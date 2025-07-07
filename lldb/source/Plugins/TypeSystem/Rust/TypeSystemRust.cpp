//===-- TypeSystemRust.cpp
//----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TypeSystemRust.h"

#include "Plugins/SymbolFile/DWARF/DWARFAttribute.h"
#include "Plugins/SymbolFile/DWARF/DWARFDIE.h"
#include "Plugins/SymbolFile/DWARF/DWARFDeclContext.h"
#include "Plugins/SymbolFile/DWARF/DWARFFormValue.h"
#include "Plugins/SymbolFile/DWARF/DWARFUnit.h"
#include "Plugins/SymbolFile/DWARF/SymbolFileDWARF.h"
#include "Plugins/SymbolFile/DWARF/UniqueDWARFASTType.h"
#include "Plugins/SymbolFile/NativePDB/PdbSymUid.h"
#include "Plugins/SymbolFile/NativePDB/PdbUtil.h"
#include "Plugins/SymbolFile/NativePDB/SymbolFileNativePDB.h"
#include "lldb/Core/PluginManager.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Core/dwarf.h"
#include "lldb/Expression/DWARFExpression.h"
#include "lldb/Symbol/CompileUnit.h"
#include "lldb/Symbol/CompilerType.h"
#include "lldb/Symbol/Type.h"
#include "lldb/Utility/FileSpec.h"
#include "lldb/Utility/StreamString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-private-enumerations.h"
#include "lldb/lldb-types.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/bit.h"
#include "llvm/BinaryFormat/Dwarf.h"
// #include "llvm/DebugInfo/PDB/IPDBEnumChildren.h"
// #include "llvm/DebugInfo/PDB/IPDBLineNumber.h"
// #include "llvm/DebugInfo/PDB/IPDBSession.h"
// #include "llvm/DebugInfo/PDB/IPDBSourceFile.h"
#include "llvm/DebugInfo/CodeView/CodeView.h"
#include "llvm/DebugInfo/CodeView/RecordName.h"
#include "llvm/DebugInfo/CodeView/SymbolDeserializer.h"
#include "llvm/DebugInfo/CodeView/SymbolRecord.h"
#include "llvm/DebugInfo/CodeView/SymbolRecordHelpers.h"
#include "llvm/DebugInfo/CodeView/TypeDeserializer.h"
#include "llvm/DebugInfo/CodeView/TypeRecord.h"
#include "llvm/DebugInfo/PDB/Native/DbiStream.h"
#include "llvm/DebugInfo/PDB/Native/GlobalsStream.h"
#include "llvm/DebugInfo/PDB/Native/PDBStringTable.h"
#include "llvm/DebugInfo/PDB/Native/SymbolStream.h"
#include "llvm/DebugInfo/PDB/Native/TpiStream.h"
#include "llvm/DebugInfo/PDB/PDBSymbol.h"

// #include "llvm/DebugInfo/PDB/PDBSymbolFunc.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeArray.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeBaseClass.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeBuiltin.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeEnum.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeFunctionArg.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeFunctionSig.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypePointer.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeTypedef.h"
// #include "llvm/DebugInfo/PDB/PDBSymbolTypeUDT.h"
// #include "llvm/DebugInfo/PDB/PDBTypes.h"
// #include "llvm/Support/Format.h"
#include "llvm/Support/BinaryStreamReader.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/FormatVariadic.h"
#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string>

using namespace llvm::codeview;
using namespace llvm::dwarf;
using namespace llvm::pdb;
using namespace lldb;
using namespace lldb_private::plugin::dwarf;
using namespace lldb_private::npdb;

LLDB_PLUGIN_DEFINE(TypeSystemRust)

namespace lldb_private {

/// Returns a pair containing the root name of the type, and the template args
/// of that type.
///
/// Only gets the first "level" of template/generic args from a type name.
///
/// e.g. `"T<A, B<C>, D>"` -> `("T", ["A", "B<C>", "D"])`
///
/// Useful for PDB debug info, which does not store template args in any
/// convenient way.
static std::pair<llvm::StringRef, std::vector<llvm::StringRef>>
GetTemplateArgs(llvm::StringRef name) {

  auto [root_name, args] = name.split('<');

  if (args.size() == 0) {
    return {root_name, {}};
  }

  // this can happen for msvc sum-types (e.g. enum2$<sample::Number>::NAMES)
  if (!args.ends_with('>')) {
    return {name, {}};
  }

  args.consume_back(">");
  args = args.trim();
  // args = args.substr(0, args.size() - 1);

  std::vector<llvm::StringRef> arg_vec{};
  uint32_t len = args.size();
  uint32_t start = 0;
  uint32_t depth = 0;

  for (uint32_t i = 0; i < len; ++i) {
    switch (args[i]) {
    case ',':
      if (depth == 0) {
        arg_vec.push_back(args.substr(start, i - start).trim());
        start = i + 1;
      }
      break;
    case '<':
    // also account for tuples
    case '(':
      depth += 1;
      break;
    case '>':
    // also account for tuples
    case ')':
      depth -= 1;
      break;
    default:
      break;
    }
  }

  // we already cut off the trailing `>` above, so now we need to handle the
  // case where there wasn't a trailing `,`
  auto last = args.substr(start).trim();
  if (last.size() != 0) {
    arg_vec.push_back(last);
  }

  return {root_name, arg_vec};
}

// -------------------------------------------------------------------------- //
//                                 Bookkeeping                                //
// -------------------------------------------------------------------------- //

char TypeSystemRust::ID;

TypeSystemRust::TypeSystemRust()
    : DWARFASTParser(Kind::DWARFASTParserRust), m_pointer_byte_size(0) {}

lldb::TypeSystemSP TypeSystemRust::CreateInstance(
    lldb::LanguageType language,
    Module* module,
    Target* target
) {
  if (language == eLanguageTypeRust) {
    ArchSpec arch;
    std::shared_ptr<TypeSystemRust> astc;
    if (module) {
      arch = module->GetArchitecture();
      astc = std::shared_ptr<TypeSystemRust>(new TypeSystemRust);
    } else if (target) {
      arch = target->GetArchitecture();
      astc = std::shared_ptr<TypeSystemRust>(new TypeSystemRust);
      astc->m_target_wp = target->shared_from_this();
      // astc = std::shared_ptr<TypeSystemRustForExpr>(
      //     new TypeSystemRustForExpr(target->shared_from_this())
      // );
    }

    if (arch.IsValid()) {
      astc->m_pointer_byte_size = arch.GetAddressByteSize();
      astc->primitive_types.SetPointerByteSize(astc->m_pointer_byte_size);
      return astc;
    }
  }
  return lldb::TypeSystemSP();
}

void TypeSystemRust::Initialize() {
  LanguageSet supported_languages_for_types;
  supported_languages_for_types.Insert(lldb::eLanguageTypeRust);
  LanguageSet supported_languages_for_expressions;
  PluginManager::RegisterPlugin(
      GetPluginNameStatic(),
      "Rust AST context plug-in",
      CreateInstance,
      supported_languages_for_types,
      supported_languages_for_expressions
  );
}

TypeSystemRust::~TypeSystemRust() { Finalize(); }

void TypeSystemRust::Finalize() { m_compile_unit_ctx.reset(); }

void TypeSystemRust::Terminate() {
  PluginManager::UnregisterPlugin(CreateInstance);
}

// -------------------------------------------------------------------------- //
//                              DWARF AST Parsing                             //
// -------------------------------------------------------------------------- //

TypeSP TypeSystemRust::ParseTypeFromDWARF(
    const SymbolContext& sc,
    const DWARFDIE& die,
    bool* type_is_new_ptr
) {
  if (type_is_new_ptr) {
    *type_is_new_ptr = false;
  }

  if (!die) {
    return nullptr;
  }

  SymbolFileDWARF* dwarf = die.GetDWARF();

  // Set a bit that lets us know that we are currently parsing this
  if (auto [it, inserted] =
          dwarf->GetDIEToType().try_emplace(die.GetDIE(), DIE_IS_BEING_PARSED);
      !inserted) {
    if (it->getSecond() == nullptr || it->getSecond() == DIE_IS_BEING_PARSED) {
      return nullptr;
    }

    return it->getSecond()->shared_from_this();
  }

  TypeSP type_sp;

  if (type_is_new_ptr) {
    *type_is_new_ptr = true;
  }

  const dw_tag_t tag = die.Tag();

  switch (tag) {
  case DW_TAG_base_type:
    type_sp = ParseBasicType(die);
    break;
  case DW_TAG_typedef:
    type_sp = ParseTypedefType(die);
    break;
  case DW_TAG_pointer_type:
  case DW_TAG_reference_type:
  case DW_TAG_rvalue_reference_type:
    type_sp = ParseIndirectionType(die);
    break;
  case DW_TAG_const_type:
    break;
  case DW_TAG_structure_type:
  case DW_TAG_union_type:
  case DW_TAG_class_type:
    type_sp = ParseStructureType(die);
    break;
  case DW_TAG_enumeration_type:
    type_sp = ParseCStyleEnum(die);
    break;

  case DW_TAG_inlined_subroutine:
  case DW_TAG_subprogram:
  case DW_TAG_subroutine_type:
    type_sp = ParseFunctionType(die);
    break;

  case DW_TAG_array_type:
    type_sp = ParseArrayType(die);
    break;
  default:
    break;
  }

  if (type_sp) {
    // printf("parsed type: %s\n", type_sp->GetName().AsCString());
    dwarf->GetDIEToType()[die.GetDIE()] = type_sp.get();

    DWARFDIE sc_parent_die = SymbolFileDWARF::GetParentSymbolContextDIE(die);
    dw_tag_t sc_parent_tag = sc_parent_die.Tag();

    SymbolContextScope* symbol_context_scope = NULL;
    if (sc_parent_tag == DW_TAG_compile_unit) {
      symbol_context_scope = sc.comp_unit;
    } else if (sc.function != NULL && sc_parent_die) {
      symbol_context_scope =
          sc.function->GetBlock(true).FindBlockByID(sc_parent_die.GetID());
      if (symbol_context_scope == NULL)
        symbol_context_scope = sc.function;
    }

    if (symbol_context_scope != NULL) {
      type_sp->SetSymbolContextScope(symbol_context_scope);
    }

    // // We are ready to put this type into the uniqued list up at the module
    // // level
    // dwarf->GetTypeList()->Insert(type_sp);
  }

  return type_sp;
}

BasicAttributes
TypeSystemRust::ParseBaseAttributes(const plugin::dwarf::DWARFDIE& die) {
  DWARFAttributes attrs = die.GetAttributes(DWARFBaseDIE::Recurse::yes);
  size_t size = attrs.Size();

  lldb::user_id_t encoding_uid = LLDB_INVALID_UID;
  uint64_t byte_size = 0;
  uint64_t encoding = 0;
  ConstString type_name;

  for (size_t i = 0; i < size; ++i) {
    dw_attr_t tag = attrs.AttributeAtIndex(i);
    DWARFFormValue form_value;
    if (!attrs.ExtractFormValueAtIndex(i, form_value)) {
      continue;
    }
    switch (tag) {
    case DW_AT_encoding:
      encoding = form_value.Unsigned();
      break;
    case DW_AT_byte_size:
      byte_size = form_value.Unsigned();
      break;
    case DW_AT_type:
      encoding_uid = form_value.Reference().GetID();
      break;
    case DW_AT_name:
      type_name.SetCString(form_value.AsCString());
      break;
    default:
      break;
    }
  }

  return {encoding_uid, byte_size, static_cast<TypeKind>(encoding), type_name};
}

TypeSP TypeSystemRust::ParseBasicType(const DWARFDIE& die) {
  auto [encoding_uid, byte_size, encoding, type_name] =
      ParseBaseAttributes(die);

  SymbolFileDWARF* dwarf = die.GetDWARF();
  CompilerType compiler_type;
  Type::EncodingDataType encoding_data_type = Type::eEncodingIsUID;

  RustType* rt;

  if (byte_size == 0 && type_name && type_name == UNIT_TYPE_NAME) {
    rt = primitive_types.unit();
  } else {
    switch (static_cast<TypeKind>(encoding)) {
    case DW_ATE_boolean:
      rt = primitive_types.Bool();
      break;
    case DW_ATE_float:
      switch (byte_size) {
      case 2:
        rt = primitive_types.f16();
        break;
      case 4:
        rt = primitive_types.f32();
        break;
      case 8:
        rt = primitive_types.f64();
        break;
      case 16:
        rt = primitive_types.f128();
        break;
      default:
        // TODO leak
        rt = new RustType{RustType::NewFloat(type_name, byte_size)};
      }
      break;
    case DW_ATE_signed:
      if (type_name == ISIZE_NAME) {
        rt = primitive_types.isize();
        rt->m_size = byte_size;
        m_pointer_byte_size = byte_size;
        break;
      }
      switch (byte_size) {
      case 1:
        rt = primitive_types.i8();
        break;
      case 2:
        rt = primitive_types.i16();
        break;
      case 4:
        rt = primitive_types.i32();
        break;
      case 8:
        rt = primitive_types.i64();
        break;
      case 16:
        rt = primitive_types.i128();
        break;
      default:
        // TODO leak
        rt = new RustType{RustType::NewInt(type_name, byte_size)};
      }
      break;
    case DW_ATE_unsigned_char:
    case DW_ATE_unsigned: {
      if (type_name == USIZE_NAME) {
        rt = primitive_types.usize();
        rt->m_size = byte_size;
        m_pointer_byte_size = byte_size;
        break;
      }
      auto rust_type = RustType{RustType::NewUInt(type_name, byte_size)};
      switch (byte_size) {
      case 1:
        rt = primitive_types.u8();
        break;
      case 2:
        rt = primitive_types.u16();
        break;
      case 4:
        rt = primitive_types.u32();
        break;
      case 8:
        rt = primitive_types.u64();
        break;
      case 16:
        rt = primitive_types.u128();
        break;
      default:
        // TODO leak
        rt = new RustType{RustType::NewUInt(type_name, byte_size)};
      }
    } break;
    case DW_ATE_UTF:
      rt = primitive_types.Char();
      break;
    default:
      // nothing else should make it here
      assert(0);
      rt = nullptr;
    }
  }

  compiler_type = CompilerType(weak_from_this(), rt);

  return dwarf->MakeType(
      die.GetID(),
      type_name,
      byte_size,
      nullptr,
      encoding_uid,
      encoding_data_type,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );
}

TypeSP TypeSystemRust::ParseTypedefType(const DWARFDIE& die) {
  auto [encoding_uid, byte_size, encoding, type_name] =
      ParseBaseAttributes(die);

  SymbolFileDWARF* dwarf = die.GetDWARF();
  Type::ResolveState resolve_state = Type::ResolveState::Unresolved;
  CompilerType compiler_type;
  Type::EncodingDataType encoding_data_type = Type::eEncodingIsTypedefUID;

  RustType* rt;

  Type* under_t = dwarf->ResolveTypeUID(encoding_uid);
  if (under_t) {
    CompilerType underlying_type = under_t->GetForwardCompilerType();
    rt = new RustType{RustType::NewTypedef(type_name, underlying_type)};
    // TODO clang doesn't do this and neither did the codelldb dwarfastparser?
    // Idk why or what this is even used for
    // resolve_state = Type::ResolveState::Full;
    compiler_type = CompilerType(weak_from_this(), rt);
  } else {
    compiler_type = CompilerType();
  }

  return dwarf->MakeType(
      die.GetID(),
      type_name,
      byte_size,
      NULL,
      encoding_uid,
      encoding_data_type,
      Declaration(),
      compiler_type,
      resolve_state
  );
}

TypeSP TypeSystemRust::ParseIndirectionType(const DWARFDIE& die) {

  auto [encoding_uid, byte_size, encoding, type_name] =
      ParseBaseAttributes(die);

  SymbolFileDWARF* dwarf = die.GetDWARF();
  Type::ResolveState resolve_state = Type::ResolveState::Unresolved;
  CompilerType compiler_type;

  dw_tag_t tag = die.Tag();

  Type::EncodingDataType encoding_data_type = Type::eEncodingIsPointerUID;

  switch (tag) {
  case DW_TAG_reference_type:
    encoding_data_type = Type::eEncodingIsLValueReferenceUID;
    break;
  case DW_TAG_rvalue_reference_type:
    encoding_data_type = Type::eEncodingIsRValueReferenceUID;
    break;
  default:
    break;
  }

  RustType* rt;

  Type* under_t = dwarf->ResolveTypeUID(encoding_uid);

  if (under_t) {
    CompilerType underlying_type = under_t->GetForwardCompilerType();
    int byte_size = die.GetCU()->GetAddressByteSize();
    this->m_pointer_byte_size = byte_size;

    rt = new RustType{
        RustType::NewIndirection(type_name, byte_size, underlying_type, tag)
    };

    // TODO clang doesn't do this and neither did the codelldb dwarfastparser?
    // Idk why or what this is even used for
    // resolve_state = Type::ResolveState::Full;
    compiler_type = CompilerType(weak_from_this(), rt);
  } else {
    compiler_type = CompilerType();
  }

  return dwarf->MakeType(
      die.GetID(),
      type_name,
      byte_size,
      NULL,
      encoding_uid,
      encoding_data_type,
      Declaration(),
      compiler_type,
      resolve_state
  );
}

TypeSP TypeSystemRust::ParseCStyleEnum(const DWARFDIE& die) {
  DWARFAttributes attrs = die.GetAttributes(DWARFBaseDIE::Recurse::yes);
  size_t size = attrs.Size();

  CompilerType underlying_type;
  // TODO might not need these since we can get the data we need from the
  // underlying type
  // uint64_t byte_size = 0;
  // uint64_t byte_align = 0;
  ConstString type_name;

  // Grab the name and underlying type of the enum
  for (size_t i = 0; i < size; ++i) {
    dw_attr_t tag = attrs.AttributeAtIndex(i);
    DWARFFormValue form_value;
    if (!attrs.ExtractFormValueAtIndex(i, form_value)) {
      continue;
    }
    switch (tag) {
      // case DW_AT_byte_size:
      //   byte_size = attrs.ValueAtIndex(i).value.uval;
      //   break;
      // case DW_AT_alignment:
      //   byte_align = attrs.ValueAtIndex(i).value.uval;
      break;
    case DW_AT_name:
      type_name.SetCString(form_value.AsCString());
      break;
    case DW_AT_type:
      if (Type* type = die.ResolveTypeUID(form_value.Reference())) {
        underlying_type = type->GetFullCompilerType();
      }
      break;
    default:
      break;
    }
  }

  type_name = QualifyTypeName(type_name, die);

  RustType* rt =
      new RustType{RustType::NewCStyleEnum(type_name, underlying_type)};

  RustCStyleEnumType* renum = rt->AsCStyleEnum();

  // We also need to grab all the individual "children" of the enum so we can
  // display their names
  for (auto& child : die.children()) {
    if (child.Tag() != DW_TAG_enumerator) {
      continue;
    }

    DWARFAttributes child_attrs =
        child.GetAttributes(DWARFBaseDIE::Recurse::yes);
    size_t child_size = child_attrs.Size();

    ConstString name;
    uint64_t value;
    bool saw_value = false;

    for (size_t i = 0; i < child_size; ++i) {
      dw_attr_t tag = child_attrs.AttributeAtIndex(i);
      DWARFFormValue form_value;
      if (!child_attrs.ExtractFormValueAtIndex(i, form_value)) {
        continue;
      }
      switch (tag) {
      case DW_AT_name:
        name = ConstString(form_value.AsCString());
        break;
      case DW_AT_const_value:
        value = form_value.Unsigned();
        saw_value = true;
        break;
      default:
        break;
      }
    }

    if (saw_value && !name.IsEmpty()) {
      renum->variants[value] = name;
    }
  }

  CompilerType compiler_type = CompilerType(weak_from_this(), rt);

  // We pass in 0 as the size since we're essentially treating them as
  // "typedefs" of a builtin type
  return die.GetDWARF()->MakeType(
      die.GetID(),
      type_name,
      0,
      NULL,
      LLDB_INVALID_UID,
      Type::eEncodingIsUID,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );
}

TypeSP TypeSystemRust::ParseStructureType(const DWARFDIE& die) {
  /* There are a handful of different things that make it to this point:
    * structs
    * tuple structs
    * tuples
    * unions
    * sum-type enums
    * sum-type enum variants

  Handling them all via 1 function is really messy. I've narrowed it to 2 main
  paths - sum-type enums and everything else. There will be some code
  duplication, but it's less obnoxious than the tangled mess necessary to keep
  everything straight in 1 function. I tried to also eliminate recursion, but
  unfortunately it's not (easily) possible. Since we store CompilerTypes for
  things like struct fields, we need to call DWARFDIE::ResolveTypeUID, which
  (after a bunch of song and dance) calls
  TypeSystem::GetDWARFParser().ParseTypeFromDWARF if the type hasn't already
  been parsed. That should only be an issue for sum-type enum variants, but
  since those are only ever referenced directly by the enum itself (afaik), it
  shouldn't be an issue and we can pretend that this doesn't recurse at all.

*/
  DWARFAttributes attrs = die.GetAttributes(DWARFBaseDIE::Recurse::yes);
  size_t size = attrs.Size();

  ConstString type_name;
  uint64_t byte_size = -1;
  uint64_t byte_align;
  // TODO maybe?
  // AccessType access;
  Declaration decl{};

  for (size_t i = 0; i < size; ++i) {
    dw_attr_t tag = attrs.AttributeAtIndex(i);
    DWARFFormValue form_value;
    if (!attrs.ExtractFormValueAtIndex(i, form_value)) {
      continue;
    }
    switch (tag) {
    case DW_AT_name:
      type_name.SetCString(form_value.AsCString());
      break;
    case DW_AT_byte_size:
      byte_size = form_value.Unsigned();
      break;
    case DW_AT_accessibility:
      // access = attrs.ValueAtIndex(i).value.uval == DW_ACCESS_private
      //              ? AccessType::eAccessPrivate
      //              : AccessType::eAccessPublic;
      break;
    case DW_AT_alignment:
      byte_align = form_value.Unsigned();
      break;
    case DW_AT_decl_file:
      decl.SetFile(attrs.CompileUnitAtIndex(i)->GetFile(form_value.Unsigned()));
      break;
    case DW_AT_decl_line:
      decl.SetLine(form_value.Unsigned());
      break;
    case DW_AT_decl_column:
      decl.SetColumn(form_value.Unsigned());
      break;
    default:
      break;
    }
  }

  SymbolFileDWARF* dwarf = die.GetDWARF();

  bool is_union = die.Tag() == DW_TAG_union_type;
  bool is_tuple = !type_name.IsNull() && type_name.AsCString()[0] == '(';
  bool is_enum = die.GetFirstChild().Tag() == DW_TAG_variant_part;

  // Anonymous tuples e.g. `(u8, u16)` do not need to be scope-qualified,
  // everything else does. We need to do this before we look up the type in
  // the unique type map because when we insert the type, we'll be using the
  // qualified name.
  if (!is_tuple) {
    type_name = QualifyTypeName(type_name, die);
  }

  // if we have a name and it's in the unique type map, just return that type.
  if (!type_name.IsNull()) {
    if (UniqueDWARFASTType* unique_ast_entry_type =
            dwarf->GetUniqueDWARFASTTypeMap()
                .Find(type_name, die, &decl, byte_size, false)) {
      if (TypeSP type_sp = unique_ast_entry_type->m_type_sp) {
        dwarf->GetDIEToType()[die.GetDIE()] = type_sp.get();
        return type_sp;
      }
    }
  }

  CompilerType compiler_type;

  if (!is_enum) {
    // We can't determine if it's a tuple struct until we see the fields, so
    // this will do for now
    AggregateKind agg_kind = is_union   ? AggregateKind::Union
                             : is_tuple ? AggregateKind::Tuple
                                        : AggregateKind::Struct;

    RustType* rt = new RustType{
        RustType::NewAggregate(type_name, byte_size, byte_align, agg_kind)
    };

    auto* inner = rt->AsAggregate();

    ParseStructFields(die, inner->fields, inner->template_args, is_tuple);

    if (inner->fields.size() > 0 && inner->fields[0].name == "0") {
      inner->kind = AggregateKind::TupleStruct;
    }

    compiler_type = CompilerType(weak_from_this(), rt);
  } else {
    compiler_type = ParseSumType(die, type_name, size);
  }

  // TODO is forward resolvestate necessary? Rust doesn't do forward
  // declarations
  TypeSP type_sp = dwarf->MakeType(
      die.GetID(),
      type_name,
      byte_size,
      NULL,
      LLDB_INVALID_UID,
      Type::eEncodingIsUID,
      &decl,
      compiler_type,
      Type::ResolveState::Forward
  );

  // UniqueDWARFASTType is large, so don't create a local variables on the
  // stack, put it on the heap. This function is often called recursively and
  // clang isn't good at sharing the stack space for variables in different
  // blocks.
  auto ast_entry = std::make_unique<UniqueDWARFASTType>();
  // Add our type to the unique type map so we don't
  // end up creating many copies of the same type over
  // and over in the ASTContext for our module

  ast_entry->m_type_sp = type_sp;
  ast_entry->m_die = die;
  ast_entry->m_declaration = decl;
  ast_entry->m_byte_size = byte_size;
  // this should be accurate. Rust doesn't do forward decls as far as i can
  // tell
  ast_entry->m_is_forward_declaration = false;

  dwarf->GetUniqueDWARFASTTypeMap().Insert(type_name, *ast_entry);

  // TODO clang puts it in the ForwardDeclCompilerTypeToDIE() map. dunno if we
  // need to do that.

  return type_sp;
}

std::pair<FieldAttributes, plugin::dwarf::DWARFFormValue>
TypeSystemRust::ParseFieldAttributes(const DWARFDIE& die) {
  DWARFAttributes attrs = die.GetAttributes(DWARFBaseDIE::Recurse::yes);
  size_t size = attrs.Size();

  FieldAttributes field;
  DWARFFormValue encoding;

  ModuleSP module_sp = die.GetModule();

  for (size_t i = 0; i < size; ++i) {
    dw_attr_t tag = attrs.AttributeAtIndex(i);
    DWARFFormValue form_value;
    if (!attrs.ExtractFormValueAtIndex(i, form_value)) {
      continue;
    }
    switch (tag) {
    case DW_AT_name:
      field.name.SetCString(form_value.AsCString());
      break;
    case DW_AT_type:
      attrs.ExtractFormValueAtIndex(i, encoding);
      break;
    case DW_AT_alignment:
      field.byte_align = form_value.Unsigned();
      break;
    case DW_AT_data_member_location:
      if (!form_value.BlockData()) {
        field.byte_offset = form_value.Unsigned();
      } else {
        // I'm not entirely sure what this does, but DWARFASTParserClang does
        // this in ExtractDataMemberLocation and vadimcn's DWARFASTParserRust
        // did this in ParseFields, so it's probably important.
        const DWARFDataExtractor& debug_info_data = die.GetData();
        uint32_t block_length = form_value.Unsigned();
        uint32_t block_offset =
            form_value.BlockData() - debug_info_data.GetDataStart();

        Value initial_value(0);

        llvm::Expected<Value> member_offset = DWARFExpression::Evaluate(
            NULL,
            NULL,
            module_sp,
            DataExtractor(debug_info_data, block_offset, block_length),
            die.GetCU(),
            RegisterKind::eRegisterKindDWARF,
            &initial_value,
            nullptr
        );
        if (member_offset) {
          field.byte_offset = member_offset->ResolveValue(nullptr).UInt();
        }
      }
      break;
    case DW_AT_accessibility:
      field.access = GetAccessTypeFromDWARF(form_value.Unsigned());
      break;
    case DW_AT_artificial:
      field.artificial = form_value.Unsigned();
      break;
    default:
      break;
    }
  }

  return std::make_pair(field, encoding);
}

void TypeSystemRust::ParseStructFields(
    const DWARFDIE& die,
    std::vector<FieldAttributes>& fields,
    std::vector<std::pair<llvm::StringRef, std::optional<CompilerType>>>&
        template_args,
    bool is_tuple
) {
  for (auto& child : die.children()) {
    dw_tag_t tag = child.Tag();
    // TODO rust doesn't output template_value_parameter, but probably should
    if (tag == DW_TAG_template_type_parameter) {
      // TODO could this be replaced with child.ResolveType()?
      auto encoding_ref = child.GetAttributeValueAsReferenceDIE(DW_AT_type);
      Type* templ = child.ResolveTypeUID(encoding_ref);

      template_args.push_back(std::make_pair(
          templ->GetName().GetStringRef(),
          templ->GetLayoutCompilerType()
      ));

    } else if (tag == DW_TAG_member) {
      auto [field, encoding] = ParseFieldAttributes(child);

      auto f_name = field.name.GetStringRef();

      // normalize tuple field names here so that it doesn't have to be done
      // repeatedly by synthetic providers
      // is_tuple is cheap to check, so it's still worth including as a guard,
      // but we need to catch tuple-structs and tuple-struct-enum-variants, so
      // we do some string inspection.
      if (is_tuple ||
          (f_name.starts_with("__") &&
           std::all_of(f_name.begin() + 2, f_name.end(), ::isdigit))) {
        auto substring = f_name.substr(2);
        field.name = ConstString(substring);
      }

      Type* member_type =
          child.GetDWARF()->ResolveTypeUID(encoding.Reference().GetID());
      if (member_type) {
        field.underlying_type = member_type->GetFullCompilerType();
      }
      fields.push_back(field);
    }
    // TODO DW_TAG_subprogram for member functions?
    // TypeSystemRust::GetNumMemberFunctions and
    // TypeSystemRust::GetMemberFunctionAtIndex
  }
}

CompilerType TypeSystemRust::ParseSumType(
    const DWARFDIE& die,
    const ConstString& type_name,
    uint64_t size
) {
  // per rust docs, enums look like this, with the arrow pointing to the
  // passed in `die`:
  //
  //  ---> DW_TAG_structure_type     (top-level type for enum)
  //         DW_TAG_variant_part     (variant part)
  //           DW_AT_discr           (reference to discriminant DW_TAG_member)
  //           DW_TAG_member         (discriminant member)
  //           DW_TAG_variant        (variant 1)
  //           DW_TAG_variant        (variant 2)
  //           DW_TAG_variant        (variant 3)
  //         DW_TAG_structure_type   (type of variant 1)
  //         DW_TAG_structure_type   (type of variant 2)
  //         DW_TAG_structure_type   (type of variant 3)
  //
  // That means this outter loop must handle the `variant_part` and
  // `structure_type` tags from a "macro" level
  //
  // Importantly, even if one of the enum members is carrying no extra data,
  // all variant types are output as DW_TAG_structure_type
  //
  // If the enum has a generic arg, it is not output for the top level
  // DW_TAG_structure_type, but *is* output for each of the variant types.

  //
  std::pair<CompilerType, std::vector<std::optional<uint64_t>>> discr_info;
  std::vector<EnumVariant> variant_types{};

  // used to match the expected naming conventions of the variant
  uint64_t variant_idx = 0;

  for (auto& child : die.children()) {
    auto tag = child.Tag();
    switch (tag) {
    case DW_TAG_variant_part:
      discr_info = ParseVariantPart(child);
      break;
    case DW_TAG_structure_type: {
      // this is our 1 explicit instance of recursion, as variant structs are
      // ~identical to regular structs
      //
      // these should output in the same order as the discr_info vector, so we
      // can easily associate the CompilerType with its discriminant value
      CompilerType underlying_type =
          ParseStructureType(child)->GetFullCompilerType();

      // matches the expected naming format for current rust SyntheticProviders

      // TODO does not match PDB output
      ConstString name =
          ConstString(llvm::formatv("$variant{0}$", variant_idx).str());

      variant_types.push_back({underlying_type, name});

      variant_idx += 1;
    } break;
    default:
      break;
    }
  }

  // if discr_info.second.size() != variant_types.size(), the rust compiler
  // has majorly fucked up so we'll assume that just doesn't happen =)
  uint64_t len = variant_types.size();

  RustType* rt = new RustType{RustType::NewSumType(
      type_name,
      size,
      std::move(variant_types),
      discr_info.first
  )};

  auto* sum_type = rt->AsSumType();

  for (uint64_t i = 0; i < len; ++i) {
    auto& discr_value = discr_info.second[i];

    if (discr_value.has_value()) {
      sum_type->discr_map[{discr_value.value(), 0}] = i;
    } else {
      sum_type->untagged_variant = std::make_optional(i);
    }
  }

  return CompilerType(weak_from_this(), rt);
}

std::pair<CompilerType, std::vector<std::optional<uint64_t>>>
TypeSystemRust::ParseVariantPart(const DWARFDIE& die) {
  // per rust docs, enums look like this, with the arrow pointing to the
  // passed in `die`
  //
  //       DW_TAG_structure_type     (top-level type for enum)
  //    ---> DW_TAG_variant_part     (variant part)
  //           DW_AT_discr           (reference to discriminant DW_TAG_member)
  //           DW_TAG_member         (discriminant member)
  //           DW_TAG_variant        (variant 1)
  //           DW_TAG_variant        (variant 2)
  //           DW_TAG_variant        (variant 3)
  //         DW_TAG_structure_type   (type of variant 1)
  //         DW_TAG_structure_type   (type of variant 2)
  //         DW_TAG_structure_type   (type of variant 3)
  //
  // This loop handles DW_TAG_member, and any number of DW_TAG_variant.
  // Handling for DW_TAG_structure_type is taken care of via ParseVariantType.
  //
  // DW_AT_discr can be ignored, as it only contains a pointer to the
  // DW_TAG_member. Since there's only 1 member, there's no ambiguity that
  // requires that pointer

  CompilerType discr_type;
  std::vector<std::optional<uint64_t>> discr_values{};

  for (auto& child : die.children()) {
    auto tag = child.Tag();
    if (tag == DW_TAG_member) {
      // The underlying type is the only thing we really need from the member
      auto encoding_id =
          child.GetAttributeValueAsReferenceDIE(DW_AT_type).GetID();
      Type* raw_type = child.GetDWARF()->ResolveTypeUID(encoding_id);
      discr_type = raw_type->GetForwardCompilerType();

    } else if (tag == DW_TAG_variant) {
      // per rust docs, this is the layout of the variant die:
      //
      // DW_TAG_variant
      //   DW_AT_discr_value           0
      //   DW_TAG_member
      //     DW_AT_name                  None
      //     DW_AT_type                  <0x000002a1>
      //     DW_AT_alignment             0x00000002
      //     DW_AT_data_member_location  0
      //
      // "The DW_AT_discr_value is optional, and is omitted if
      //   - This is the only variant of a univariant enum (i.e. their is no
      //   discriminant)
      //   - This is the "untagged" variant of a niche-layout enum
      //     (where only the other variants are identified by a single value)"

      // NOTE: we do NOT want to resolve the types of the variants. Since
      // DW_TAG_variant_part comes before the DW_TAG_structure_type, we should
      // always be in this function before the variant structures are parsed.
      // Calling GetLayoutCompilerType on a non-resolved type will "take it
      // from the top" and call ParseTypeFromDWARF with that `die`,
      // effectively recursing us. This whole house of cards was made to *not*
      // recurse in any meaningful way.
      //
      // Variants are stored in the same order as their `structure_type`
      // equivalents, so we should be okay to store the discriminant values as
      // a raw array

      std::optional<uint64_t> tag =
          child.GetAttributeValueAsOptionalUnsigned(DW_AT_discr_value);

      discr_values.push_back(tag);
    }
  }

  return {discr_type, discr_values};
}

TypeSP TypeSystemRust::ParseArrayType(const DWARFDIE& die) {
  SymbolFileDWARF* dwarf = die.GetDWARF();

  auto element_type_id =
      die.GetAttributeValueAsReferenceDIE(DW_AT_type).GetID();
  Type* raw_type = dwarf->ResolveTypeUID(element_type_id);
  CompilerType element_type = raw_type->GetForwardCompilerType();

  ConstString element_name = element_type.GetTypeName();

  uint64_t len =
      die.GetFirstChild().GetAttributeValueAsUnsigned(DW_AT_count, 0);

  ConstString type_name =
      ConstString(llvm::formatv("[{0}; {1}]", element_name, len).str());

  RustType* rt = new RustType{RustType::NewArray(type_name, element_type, len)};

  CompilerType compiler_type = CompilerType(weak_from_this(), rt);

  TypeSP type_sp = dwarf->MakeType(
      die.GetID(),
      type_name,
      element_type.GetByteSize(NULL),
      NULL,
      element_type_id,
      Type::eEncodingIsUID,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );

  type_sp->SetEncodingType(raw_type);

  return type_sp;
}

TypeSP TypeSystemRust::ParseFunctionType(const DWARFDIE& die) {
  SymbolFileDWARF* dwarf = die.GetDWARF();

  // Get the basic attributes of the function

  DWARFAttributes attrs = die.GetAttributes();
  uint64_t attr_len = attrs.Size();
  Declaration decl;
  CompilerType return_type;
  ConstString type_name;
  ConstString mangled;

  for (uint64_t i = 0; i < attr_len; ++i) {
    dw_attr_t tag = attrs.AttributeAtIndex(i);
    DWARFFormValue form_value;
    if (!attrs.ExtractFormValueAtIndex(i, form_value)) {
      continue;
    }
    switch (tag) {
      // case DW_AT_low_pc:
      // case DW_AT_high_pc:
      // case DW_AT_frame_base:
      // case DW_AT_external:
    //   break;
    case DW_AT_name:
      type_name.SetCString(form_value.AsCString());
      break;
    case DW_AT_linkage_name:
      mangled.SetCString(form_value.AsCString());
      break;
    case DW_AT_decl_file:
      decl.SetFile(attrs.CompileUnitAtIndex(i)->GetFile(form_value.Unsigned()));
      break;
    case DW_AT_decl_line:
      decl.SetLine(form_value.Unsigned());
      break;
    case DW_AT_type: {
      Type* t = die.ResolveTypeUID(form_value.Reference());
      if (t) {
        return_type = t->GetForwardCompilerType();
      }
      break;
    }
    default:
      break;
    }
  }

  // If there isn't a return type, set the return type to `()`
  if (!return_type.IsValid()) {
    RustType* rt = new RustType{
        RustType::NewAggregate(UNIT_TYPE_NAME, 0, 1, AggregateKind::Tuple)
    };
    return_type = CompilerType(weak_from_this(), rt);
  }

  // Retrieve param and template types
  std::vector<CompilerType> param_types;
  std::vector<CompilerType> template_args;

  for (auto& child : die.children()) {
    switch (child.Tag()) {
    case DW_TAG_formal_parameter: {
      Type* t = child.ResolveTypeUID(attrs.FormValueAsReference(DW_AT_type));
      if (t) {
        param_types.push_back(t->GetForwardCompilerType());
      }
      break;
    }
    case DW_TAG_template_type_parameter: {
      Type* t = child.ResolveTypeUID(attrs.FormValueAsReference(DW_AT_type));
      if (t) {
        template_args.push_back(t->GetForwardCompilerType());
      }
      break;
    }
    default:
      break;
    }
  }

  RustType* rt = new RustType{RustType::NewFunction(
      type_name,
      std::move(param_types),
      std::move(template_args),
      return_type
  )};

  CompilerType compiler_type = CompilerType(weak_from_this(), rt);

  // Create a FunctionDecl to be used later elsewhere

  auto* containing_decl_ctx = static_cast<RustDecl*>(
      GetDeclContextForUIDFromDWARF(die).GetOpaqueDeclContext()
  );

  if (!containing_decl_ctx) {
    containing_decl_ctx = m_compile_unit_ctx.get();
  }
  auto* func_decl = new RustDecl{
      type_name,
      mangled,
      containing_decl_ctx,
      FnDecl{llvm::DenseMap<ConstString, RustDecl*>(), compiler_type}
  };

  containing_decl_ctx->AddItem(func_decl);

  return dwarf->MakeType(
      die.GetID(),
      type_name,
      std::nullopt,
      NULL,
      LLDB_INVALID_UID,
      Type::eEncodingIsUID,
      decl,
      compiler_type,
      Type::ResolveState::Full
  );
}

Function* TypeSystemRust::ParseFunctionFromDWARF(
    CompileUnit& comp_unit,
    const DWARFDIE& die,
    const AddressRange& range
) {

  // Implementation more or less taken 1:1 from DWARFASTParserClang
  DWARFRangeList func_ranges;
  const char* name = nullptr;
  const char* mangled = nullptr;
  std::optional<int> decl_file;
  std::optional<int> decl_line;
  std::optional<int> decl_column;
  std::optional<int> call_file;
  std::optional<int> call_line;
  std::optional<int> call_column;
  DWARFExpressionList frame_base;

  const dw_tag_t tag = die.Tag();

  auto parent = die.GetParent();
  // printf("DIE: %s, Parent: %s\n", die.GetName(), parent.GetName());

  if (tag != DW_TAG_subprogram) {
    return nullptr;
  }

  if (!die.GetDIENamesAndRanges(
          name,
          mangled,
          func_ranges,
          decl_file,
          decl_line,
          decl_column,
          call_file,
          call_line,
          call_column,
          &frame_base
      )) {
    return nullptr;
  }

  Mangled func_name;
  if (mangled) {
    func_name.SetValue(ConstString(mangled));
  } else {
    func_name.SetValue(ConstString(name));
  }

  FunctionSP func_sp;
  std::unique_ptr<Declaration> decl;

  if (decl_file || decl_line || decl_column) {
    decl = std::make_unique<Declaration>(
        die.GetCU()->GetFile(decl_file ? *decl_file : 0),
        decl_line ? *decl_line : 0,
        decl_column ? *decl_column : 0
    );
  }

  SymbolFileDWARF* dwarf = die.GetDWARF();
  Type* func_type = dwarf->GetDIEToType().lookup(die.GetDIE());

  const user_id_t func_user_id = die.GetID();
  func_sp = std::make_shared<Function>(
      &comp_unit,
      func_user_id, // UserID is the DIE offset
      func_user_id,
      func_name,
      func_type,
      range
  ); // first address range

  if (func_sp.get() != nullptr) {
    if (frame_base.IsValid())
      func_sp->GetFrameBaseExpression() = frame_base;
    comp_unit.AddFunction(func_sp);
    return func_sp.get();
  }

  return nullptr;
}

ConstString TypeSystemRust::ConstructDemangledNameFromDWARF(const DWARFDIE& die
) {
  StreamString sstr;
  DWARFDeclContext decl_ctx = die.GetDWARFDeclContext();
  sstr << decl_ctx.GetQualifiedName();
  return ConstString(sstr.GetString());
}

CompilerDecl TypeSystemRust::GetDecl(
    CompilerDeclContext parent,
    const ConstString& name,
    const ConstString& mangled
) {
  if (!parent)
    return CompilerDecl();
  TypeSystemRust* ast =
      llvm::dyn_cast_or_null<TypeSystemRust>(parent.GetTypeSystem());
  if (!ast)
    return CompilerDecl();

  RustDecl* dc = (RustDecl*)parent.GetOpaqueDeclContext();
  RustDecl* base = dc->FindByName(name);
  if (base) {
    return CompilerDecl(this, base);
  }

  // TODO leak
  auto* new_ns = new RustDecl{name, mangled, dc, UnknownDecl{}};
  dc->AddItem(new_ns);

  return CompilerDecl(this, new_ns);
}

CompilerDecl
TypeSystemRust::GetDeclForUIDFromDWARF(const plugin::dwarf::DWARFDIE& die) {
  if (m_decls.contains(die.GetDIE())) {
    return m_decls[die.GetDIE()];
  }

  CompilerDecl result;
  switch (die.Tag()) {

  case DW_TAG_null:
  case DW_TAG_array_type:
  case DW_TAG_class_type:
  case DW_TAG_entry_point:
  case DW_TAG_enumeration_type:
  case DW_TAG_formal_parameter:
  case DW_TAG_imported_declaration:
  case DW_TAG_label:
  case DW_TAG_lexical_block:
  case DW_TAG_member:
  case DW_TAG_pointer_type:
  case DW_TAG_reference_type:
  case DW_TAG_compile_unit:
  case DW_TAG_string_type:
  case DW_TAG_structure_type:
  case DW_TAG_subroutine_type:
  case DW_TAG_typedef:
  case DW_TAG_union_type:
  case DW_TAG_unspecified_parameters:
  case DW_TAG_variant:
  case DW_TAG_common_block:
  case DW_TAG_common_inclusion:
  case DW_TAG_inheritance:
  case DW_TAG_inlined_subroutine:
  case DW_TAG_module:
  case DW_TAG_ptr_to_member_type:
  case DW_TAG_set_type:
  case DW_TAG_subrange_type:
  case DW_TAG_with_stmt:
  case DW_TAG_access_declaration:
  case DW_TAG_base_type:
  case DW_TAG_catch_block:
  case DW_TAG_const_type:
  case DW_TAG_constant:
  case DW_TAG_enumerator:
  case DW_TAG_file_type:
  case DW_TAG_friend:
  case DW_TAG_namelist:
  case DW_TAG_namelist_item:
  case DW_TAG_packed_type:
  case DW_TAG_subprogram:
  case DW_TAG_template_type_parameter:
  case DW_TAG_template_value_parameter:
  case DW_TAG_thrown_type:
  case DW_TAG_try_block:
  case DW_TAG_variant_part:
  case DW_TAG_variable:
  case DW_TAG_volatile_type:
  case DW_TAG_dwarf_procedure:
  case DW_TAG_restrict_type:
  case DW_TAG_interface_type:
  case DW_TAG_namespace:
  case DW_TAG_imported_module:
  case DW_TAG_unspecified_type:
  case DW_TAG_partial_unit:
  case DW_TAG_imported_unit:
  case DW_TAG_condition:
  case DW_TAG_shared_type:
  case DW_TAG_type_unit:
  case DW_TAG_rvalue_reference_type:
  case DW_TAG_template_alias:
  case DW_TAG_coarray_type:
  case DW_TAG_generic_subrange:
  case DW_TAG_dynamic_type:
  case DW_TAG_atomic_type:
  case DW_TAG_call_site:
  case DW_TAG_call_site_parameter:
  case DW_TAG_skeleton_unit:
  case DW_TAG_immutable_type:
  case DW_TAG_MIPS_loop:
  case DW_TAG_format_label:
  case DW_TAG_function_template:
  case DW_TAG_class_template:
  case DW_TAG_GNU_BINCL:
  case DW_TAG_GNU_EINCL:
  case DW_TAG_GNU_template_template_param:
  case DW_TAG_GNU_template_parameter_pack:
  case DW_TAG_GNU_formal_parameter_pack:
  case DW_TAG_GNU_call_site:
  case DW_TAG_GNU_call_site_parameter:
  case DW_TAG_APPLE_property:
  case DW_TAG_SUN_function_template:
  case DW_TAG_SUN_class_template:
  case DW_TAG_SUN_struct_template:
  case DW_TAG_SUN_union_template:
  case DW_TAG_SUN_indirect_inheritance:
  case DW_TAG_SUN_codeflags:
  case DW_TAG_SUN_memop_info:
  case DW_TAG_SUN_omp_child_func:
  case DW_TAG_SUN_rtti_descriptor:
  case DW_TAG_SUN_dtor_info:
  case DW_TAG_SUN_dtor:
  case DW_TAG_SUN_f90_interface:
  case DW_TAG_SUN_fortran_vax_structure:
  case DW_TAG_SUN_hi:
  case DW_TAG_LLVM_ptrauth_type:
  case DW_TAG_ALTIUM_circ_type:
  case DW_TAG_ALTIUM_mwa_circ_type:
  case DW_TAG_ALTIUM_rev_carry_type:
  case DW_TAG_ALTIUM_rom:
  case DW_TAG_LLVM_annotation:
  case DW_TAG_GHS_namespace:
  case DW_TAG_GHS_using_namespace:
  case DW_TAG_GHS_using_declaration:
  case DW_TAG_GHS_template_templ_param:
  case DW_TAG_UPC_shared_type:
  case DW_TAG_UPC_strict_type:
  case DW_TAG_UPC_relaxed:
  case DW_TAG_PGI_kanji_type:
  case DW_TAG_PGI_interface_block:
  case DW_TAG_BORLAND_property:
  case DW_TAG_BORLAND_Delphi_string:
  case DW_TAG_BORLAND_Delphi_dynamic_array:
  case DW_TAG_BORLAND_Delphi_set:
  case DW_TAG_BORLAND_Delphi_variant:
  case DW_TAG_lo_user:
  case DW_TAG_hi_user:
  case DW_TAG_user_base:
    break;
  }
  if (die.Tag() == DW_TAG_variable || die.Tag() == DW_TAG_constant) {
    const char* name = die.GetName();
    if (name) {
      const char* mangled = die.GetMangledName();
      CompilerDeclContext parent = GetDeclContextContainingUIDFromDWARF(die);
      result = GetDecl(parent, ConstString(name), ConstString(mangled));

      if (result) {
        m_decls[die.GetDIE()] = result;
      }
    }
  }

  return result;
}

CompilerDeclContext
TypeSystemRust::GetDeclContextForUIDFromDWARF(const plugin::dwarf::DWARFDIE& die
) {
  if (m_decl_contexts.contains(die.GetDIE())) {
    return m_decl_contexts[die.GetDIE()];
  }

  auto declkind = RustDecl::Namespace;

  CompilerDeclContext result;
  switch (die.Tag()) {
  case DW_TAG_compile_unit:
    if (!m_compile_unit_ctx) {
      m_compile_unit_ctx.reset(new RustDecl{
          ConstString(""),
          ConstString(),
          nullptr,
          CompUnitDecl{
              llvm::DenseMap<ConstString, RustDecl*>{},
          }
      });
    }

    result = CompilerDeclContext(this, m_compile_unit_ctx.get());
    break;
  case DW_TAG_union_type:
  case DW_TAG_structure_type:
    declkind = RustDecl::Type;
    [[clang::fallthrough]];
  case DW_TAG_namespace: {
    auto name = ConstString(die.GetName());
    if (!name) {
      result = CompilerDeclContext();
      break;
    }

    CompilerDeclContext parent = GetDeclContextContainingUIDFromDWARF(die);
    if (!parent) {
      result = CompilerDeclContext();
      break;
    }

    auto* dc = static_cast<RustDecl*>(parent.GetOpaqueDeclContext());

    if (dc) {
      if (auto* children = dc->GetChildren()) {
        if (children->contains(name)) {
          result = CompilerDeclContext(this, (*children)[name]);
        }
      }
    }

    // auto new_ctx = RustDeclContext(name, dc, declkind);

    RustDecl::DeclInner variant =
        declkind == RustDecl::Type
            ? RustDecl::DeclInner{TypeDecl{CompilerType()}}
            : RustDecl::DeclInner{
                  NamespaceDecl{llvm::DenseMap<ConstString, RustDecl*>()}
              };

    auto* new_ns = new RustDecl{name, ConstString(), dc, std::move(variant)};
    dc->AddItem(new_ns);
    result = CompilerDeclContext(this, new_ns);
  } break;
  case DW_TAG_lexical_block:
    declkind = RustDecl::Block;
    [[clang::fallthrough]];
  case DW_TAG_subprogram: {
    auto parent = GetDeclContextContainingUIDFromDWARF(die);

    auto attrs = die.GetAttributes();

    DWARFFormValue form_val;
    DWARFRangeList range_list;

    const char* c_name = nullptr;
    const char* mangled = nullptr;
    std::optional<int> decl_file;
    std::optional<int> decl_line;
    std::optional<int> decl_column;
    std::optional<int> call_file;
    std::optional<int> call_line;
    std::optional<int> call_column;

    // TODO check if we can get type information?
    auto data = die.GetDIENamesAndRanges(
        c_name,
        mangled,
        range_list,
        decl_file,
        decl_line,
        decl_column,
        call_file,
        call_line,
        call_column,
        nullptr
    );

    auto* dc = static_cast<RustDecl*>(parent.GetOpaqueDeclContext());

    // we should be able to uniquely identify lexical blocks by their address
    // range
    auto range_begin = range_list.GetEntryAtIndex(0)->base;
    auto range_end = range_list.GetEntryAtIndex(0)->base;

    auto name =
        ConstString(llvm::formatv("{0}..{1}", range_begin, range_end).str());
    if (dc) {
      if (auto* children = dc->GetChildren()) {
        if (children->contains(name)) {
          auto* decl = (*children)[name];
          if (decl->IsContext()) {
            result = CompilerDeclContext(this, decl);
          }
        }
      }
    }

    // TODO leak
    auto* new_ns = new RustDecl{
        name,
        ConstString(),
        dc,
        FnDecl{llvm::DenseMap<ConstString, RustDecl*>(), CompilerType()}
    };

    dc->AddItem(new_ns);
    result = CompilerDeclContext(this, new_ns);
  }
    result = GetDeclContextContainingUIDFromDWARF(die);
    break;
  default:
    break;
  }

  if (result) {
    m_decl_contexts[die.GetDIE()] = result;
    m_decl_ctx_to_die.emplace(result, die);
  }

  return result;
}

CompilerDeclContext
TypeSystemRust::GetDeclContextContainingUIDFromDWARF(const DWARFDIE& die) {
  return GetDeclContextForUIDFromDWARF(
      die.GetDWARF()->GetDeclContextDIEContainingDIE(die)
  );
}

void TypeSystemRust::EnsureAllDIEsInDeclContextHaveBeenParsed(
    CompilerDeclContext decl_context
) {
  for (auto it = m_decl_ctx_to_die.find(decl_context);
       it != m_decl_ctx_to_die.end() && it->first == decl_context;
       it = m_decl_ctx_to_die.erase(it))
    for (DWARFDIE decl = it->second.GetFirstChild(); decl;
         decl = decl.GetSibling())
      GetDeclForUIDFromDWARF(decl);
  return;
}

std::string
TypeSystemRust::GetDIEClassTemplateParams(const plugin::dwarf::DWARFDIE& die) {
  return "";
}

// -------------------------------------------------------------------------- //
//                            TypeSystem Interface                            //
// -------------------------------------------------------------------------- //

// ------------------------------- DeclContext ------------------------------ //

CompilerType
TypeSystemRust::DeclGetFunctionArgumentType(void* opaque_decl, size_t arg_idx) {
  if (!opaque_decl) {
    return CompilerType();
  }

  auto* rd = static_cast<RustDecl*>(opaque_decl)->AsFn();

  if (!rd) {
    return CompilerType();
  }

  // this should never fail since function decls can only be made from function
  // types in `ParseFunctionType`
  auto* rt = static_cast<RustType*>(rd->type.GetOpaqueQualType())->AsFunction();

  return rt->args[arg_idx];
}

ConstString TypeSystemRust::DeclGetName(void* opaque_decl) {
  if (!opaque_decl) {
    return ConstString();
  }
  return static_cast<RustDecl*>(opaque_decl)->name;
}

ConstString TypeSystemRust::DeclGetMangledName(void* opaque_decl) {
  if (!opaque_decl) {
    return ConstString();
  }

  return static_cast<RustDecl*>(opaque_decl)->mangled;
}

CompilerDeclContext TypeSystemRust::DeclGetDeclContext(void* opaque_decl) {
  if (!opaque_decl) {
    return CompilerDeclContext();
  }
  RustDecl* dc = static_cast<RustDecl*>(opaque_decl);
  return CompilerDeclContext(this, dc->parent);
}

CompilerType TypeSystemRust::DeclGetFunctionReturnType(void* opaque_decl) {
  if (!opaque_decl) {
    return CompilerType();
  }

  auto* rd = static_cast<RustDecl*>(opaque_decl)->AsFn();

  if (!rd) {
    return CompilerType();
  }

  // this should never fail since function decls can only be made from function
  // types in `ParseFunctionType`
  auto* rt = static_cast<RustType*>(rd->type.GetOpaqueQualType())->AsFunction();

  return rt->return_type;
}

size_t TypeSystemRust::DeclGetFunctionNumArguments(void* opaque_decl) {
  if (!opaque_decl) {
    return 0;
  }

  auto* rd = static_cast<RustDecl*>(opaque_decl)->AsFn();

  if (!rd) {
    return 0;
  }

  // this should never fail since function decls can only be made from function
  // types in `ParseFunctionType`
  auto* rt = static_cast<RustType*>(rd->type.GetOpaqueQualType())->AsFunction();

  return rt->args.size();
}

std::vector<lldb_private::CompilerContext>
TypeSystemRust::DeclGetCompilerContext(void* opaque_decl) {
  std::vector<lldb_private::CompilerContext> context;
  // TODO
  // ConstString decl_name = DeclGetName(opaque_decl);
  // if (decl_name) {
  //   RustDeclBase *decl = static_cast<RustDeclBase*>(opaque_decl);
  //   // Add the entire decl context first
  //   RustDeclContext *decl_ctx = decl->Context();

  //   while (decl_ctx != m_compile_unit_ctx.get()) {
  //     context.push_back({})
  //   }
  //   // Now add the decl information
  //   auto compiler_kind =
  //       GetCompilerKind(decl->getKind(), dyn_cast<DeclContext>(decl));
  //   context.push_back({compiler_kind, decl_name});
  // }
  return context;
}

Scalar TypeSystemRust::DeclGetConstantValue(void* opaque_decl) {
  if (!opaque_decl) {
    return Scalar();
  }

  auto* rd = static_cast<RustDecl*>(opaque_decl)->AsVal();

  if (!rd) {
    return Scalar();
  }

  return rd->value;
}

CompilerType TypeSystemRust::GetTypeForDecl(void* opaque_decl) {
  if (!opaque_decl) {
    return CompilerType();
  }

  auto* rd = static_cast<RustDecl*>(opaque_decl);

  return rd->GetType();
}

// ---------------------- CompilerDeclContext functions --------------------- //

std::vector<CompilerDecl> TypeSystemRust::DeclContextFindDeclByName(
    void* opaque_decl_ctx,
    ConstString name,
    const bool ignore_imported_decls
) {
  if (!opaque_decl_ctx) {
    return {};
  }

  std::vector<CompilerDecl> result;
  SymbolFile* symbol_file = GetSymbolFile();

  if (symbol_file) {
    symbol_file->ParseDeclsForContext(CompilerDeclContext(this, opaque_decl_ctx)
    );

    auto* dc = static_cast<RustDecl*>(opaque_decl_ctx);
    RustDecl* base = dc->FindByName(name);
    if (base) {
      result.push_back(CompilerDecl(this, base));
    }
  }
  return result;
}

ConstString TypeSystemRust::DeclContextGetName(void* opaque_decl_ctx) {
  if (!opaque_decl_ctx) {
    return ConstString();
  }

  auto* dc = static_cast<RustDecl*>(opaque_decl_ctx);
  return dc->name;
}

ConstString
TypeSystemRust::DeclContextGetScopeQualifiedName(void* opaque_decl_ctx) {
  if (!opaque_decl_ctx) {
    return ConstString();
  }

  auto* dc = static_cast<RustDecl*>(opaque_decl_ctx);

  if (!dc->IsContext()) {
    return ConstString();
  }

  return dc->QualifiedName();
}

bool TypeSystemRust::DeclContextIsContainedInLookup(
    void* opaque_decl_ctx,
    void* other_opaque_decl_ctx
) {
  auto* decl_ctx = static_cast<RustDecl*>(opaque_decl_ctx);
  auto* other = static_cast<RustDecl*>(other_opaque_decl_ctx);

  if (!decl_ctx || !other) {
    return false;
  }

  do {
    // A decl context always includes its own contents in its lookup.
    if (decl_ctx == other)
      return true;
  } while ((other = other->parent)); // breaks if the parent is nullptr

  return false;
}

// ----------------------------- Type Properties ---------------------------- //

bool TypeSystemRust::IsArrayType(
    lldb::opaque_compiler_type_t type,
    CompilerType* element_type,
    uint64_t* size,
    bool* is_incomplete
) {
  auto* rt = static_cast<RustType*>(type);
  auto* array = rt->AsArray();

  if (!array) {
    if (element_type) {
      element_type->Clear();
    }
    if (size) {
      *size = 0;
    }

    return false;
  }

  if (element_type) {
    *element_type = array->element_type;
  }
  if (size) {
    *size = array->len;
  }
  if (is_incomplete) {
    *is_incomplete = false;
  }
  return true;
}

bool TypeSystemRust::IsAggregateType(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);

  return rt->IsAggregate() || rt->IsSumType();
}

bool TypeSystemRust::IsAnonymousType(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);
  return !rt->m_name.IsNull() && rt->m_name.AsCString()[0] == '(';
}

bool TypeSystemRust::IsCharType(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);
  return rt->IsChar();
}

bool TypeSystemRust::IsCompleteType(lldb::opaque_compiler_type_t type) {
  return bool(type);
}

bool TypeSystemRust::IsDefined(lldb::opaque_compiler_type_t type) {
  return type != nullptr;
}

bool TypeSystemRust::IsFloatingPointType(
    lldb::opaque_compiler_type_t type,
    uint32_t& count,
    bool& is_complex
) {
  if (!type) {
    return false;
  }
  is_complex = false;

  auto* rt = static_cast<RustType*>(type);

  if (!rt->IsFloat()) {
    count = 0;
    return false;
  }

  count = 1;
  return true;
}

bool TypeSystemRust::IsFunctionType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return false;
  }

  return static_cast<RustType*>(type)->IsFunction();
}

size_t
TypeSystemRust::GetNumberOfFunctionArguments(lldb::opaque_compiler_type_t type
) {
  if (!type) {
    return 0;
  }

  auto* rt = static_cast<RustType*>(type)->AsFunction();

  if (!rt) {
    return 0;
  }

  return rt->args.size();
}

CompilerType TypeSystemRust::GetFunctionArgumentAtIndex(
    lldb::opaque_compiler_type_t type,
    const size_t index
) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type)->AsFunction();

  if (!rt || rt->args.size() <= index) {
    return CompilerType();
  }

  return rt->args[index];
}

bool TypeSystemRust::IsFunctionPointerType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return false;
  }

  auto* rt = static_cast<RustType*>(type)->AsIndirection();

  if (!rt) {
    return false;
  }

  if (rt->pointee_type.IsFunctionType()) {
    return true;
  }

  return false;
}

bool TypeSystemRust::IsMemberFunctionPointerType(
    lldb::opaque_compiler_type_t type
) {
  return false;
}

bool TypeSystemRust::IsBlockPointerType(
    lldb::opaque_compiler_type_t type,
    CompilerType* function_pointer_type_ptr
) {
  return false;
}

bool TypeSystemRust::IsIntegerType(
    lldb::opaque_compiler_type_t type,
    bool& is_signed
) {
  auto* rt = static_cast<RustType*>(type);

  is_signed = rt->IsInt();

  return rt->IsUInt() || rt->IsInt();
}

bool TypeSystemRust::IsEnumerationType(
    lldb::opaque_compiler_type_t type,
    bool& is_signed
) {
  auto* rt = static_cast<RustType*>(type);
  auto* sum_type = rt->AsSumType();
  auto* enum_type = rt->AsCStyleEnum();

  if (sum_type) {
    is_signed = sum_type->discr_type.IsSigned();
  }

  if (enum_type) {
    is_signed = enum_type->underlying_type.IsSigned();
  }

  return sum_type || enum_type;
}

bool TypeSystemRust::IsScopedEnumerationType(lldb::opaque_compiler_type_t type
) {
  auto* rt = static_cast<RustType*>(type);

  return rt->IsSumType() || rt->IsCStyleEnum();
}

bool TypeSystemRust::IsPossibleDynamicType(
    lldb::opaque_compiler_type_t type,
    CompilerType* target_type, // Can pass NULL
    bool check_cplusplus,
    bool check_objc
) {
  return false;
}

bool TypeSystemRust::IsPointerType(
    lldb::opaque_compiler_type_t type,
    CompilerType* pointee_type
) {
  if (!type) {
    return false;
  }
  auto* rt = static_cast<RustType*>(type);
  auto* ptr = rt->AsIndirection();
  if (!ptr) {
    if (pointee_type) {
      pointee_type->Clear();
    }
    return false;
  }

  if (pointee_type) {
    *pointee_type = ptr->pointee_type;
  }
  return true;
}

bool TypeSystemRust::IsReferenceType(
    lldb::opaque_compiler_type_t type,
    CompilerType* pointee_type,
    bool* is_rvalue
) {
  if (is_rvalue) {
    *is_rvalue = false;
  }

  if (!type) {
    return false;
  }

  auto* rt = static_cast<RustType*>(type);
  auto* ptr = rt->AsIndirection();

  if (!ptr || ptr->kind < IndirectionKind::Reference) {
    if (pointee_type) {
      pointee_type->Clear();
    }

    return false;
  }

  if (pointee_type) {
    *pointee_type = ptr->pointee_type;
  }

  return true;
}

bool TypeSystemRust::IsPointerOrReferenceType(
    lldb::opaque_compiler_type_t type,
    CompilerType* pointee_type
) {
  return IsPointerType(type, pointee_type);
}

bool TypeSystemRust::IsScalarType(lldb::opaque_compiler_type_t type) {
  return !IsAggregateType(type);
}

bool TypeSystemRust::IsBooleanType(lldb::opaque_compiler_type_t type) {
  if (type)
    return static_cast<RustType*>(type)->IsBool();
  return false;
}

bool TypeSystemRust::IsVoidType(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);
  return rt->m_name == UNIT_TYPE_NAME;
}

bool TypeSystemRust::CanPassInRegisters(const CompilerType& type) {
  // Rust does not have the exception for types with "non-trivial"
  // constructors.
  return true;
}

bool TypeSystemRust::IsTemplateType(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);
  if (auto* agg = rt->AsAggregate()) {
    return !agg->template_args.empty();
  }
  if (auto* sum = rt->AsSumType()) {
    // each variant gets the template type even if only 1 variant actually
    // uses it
    return !sum->variants[0].underlying_type.IsTemplateType();
  }

  return false;
}

bool TypeSystemRust::IsBeingDefined(lldb::opaque_compiler_type_t type) {
  return false;
}

bool TypeSystemRust::IsConst(lldb::opaque_compiler_type_t type) {
  return false;
}

bool TypeSystemRust::IsTypedefType(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);
  return rt->IsTypedef();
}

bool TypeSystemRust::IsVectorType(
    lldb::opaque_compiler_type_t type,
    CompilerType* element_type,
    uint64_t* size
) {
  // we could maybe fill this out? But i don't think it's used for anything
  // super important
  if (element_type)
    element_type->Clear();
  if (size)
    *size = 0;
  return false;
}

CompilerType TypeSystemRust::GetFunctionArgumentTypeAtIndex(
    lldb::opaque_compiler_type_t type,
    size_t idx
) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type)->AsFunction();
  if (rt && rt->args.size() > idx) {
    return rt->args[idx];
  }

  return CompilerType();
}

int TypeSystemRust::GetFunctionArgumentCount(lldb::opaque_compiler_type_t type
) {
  if (!type) {
    return -1;
  }

  auto* rt = static_cast<RustType*>(type);
  if (auto* func = rt->AsFunction()) {
    return func->args.size();
  }

  return -1;
}

CompilerType
TypeSystemRust::GetFunctionReturnType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);
  if (auto* func = rt->AsFunction()) {
    return func->return_type;
  }

  return CompilerType();
}

// ---------------------------- Type Completion --------------------------- //

bool TypeSystemRust::GetCompleteType(lldb::opaque_compiler_type_t type) {
  return bool(type);
}

bool TypeSystemRust::IsForcefullyCompleted(lldb::opaque_compiler_type_t type) {
  return false;
}

// -------------------------- AST related queries ------------------------- //

uint32_t TypeSystemRust::GetPointerByteSize() { return m_pointer_byte_size; }

// ------------------------------- Accessors ------------------------------ //

ConstString
TypeSystemRust::GetTypeName(lldb::opaque_compiler_type_t type, bool BaseOnly) {
  if (!type) {
    return ConstString();
  }

  return static_cast<RustType*>(type)->m_name;
}

ConstString TypeSystemRust::GetDisplayTypeName(lldb::opaque_compiler_type_t type
) {
  return GetTypeName(type, false);
}

uint32_t TypeSystemRust::GetTypeInfo(
    lldb::opaque_compiler_type_t type,
    CompilerType* pointee_or_element_compiler_type
) {
  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Bool:
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsScalar;
  case RustType::UInt:
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsScalar | eTypeIsInteger;
  case RustType::Int:
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsScalar | eTypeIsInteger |
           eTypeIsSigned;
  case RustType::Float:
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsFloat;
  case RustType::Char:
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsScalar | eTypeIsInteger;
  case RustType::Typedef:
    return eTypeIsTypedef;
  case RustType::Indirection:
    if (pointee_or_element_compiler_type) {
      *pointee_or_element_compiler_type = rt->AsIndirection()->pointee_type;
    }
    return eTypeIsBuiltIn | eTypeHasValue | eTypeIsPointer |
           // disgusting
           (rt->AsIndirection()->kind >= IndirectionKind::Reference
                ? eTypeIsReference
                : 0);
  case RustType::Aggregate:
  case RustType::SumType:
    return eTypeHasChildren | eTypeIsStructUnion;
  case RustType::CStyleEmum:
    return eTypeHasValue | eTypeIsEnumeration | eTypeIsScalar;
  case RustType::Array:
    if (pointee_or_element_compiler_type) {
      *pointee_or_element_compiler_type = rt->AsArray()->element_type;
    }
    return eTypeHasChildren | eTypeIsArray;
  case RustType::Function:
    return eTypeIsFuncPrototype | eTypeHasValue;
  }

  // should be impossible
  return 0;
}

TypeClass TypeSystemRust::GetTypeClass(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);

  switch (rt->VariantKind()) {
  case RustType::Bool:
  case RustType::UInt:
  case RustType::Int:
  case RustType::Float:
  case RustType::Char:
    return eTypeClassBuiltin;
  case RustType::Typedef:
    return eTypeClassTypedef;
  case RustType::Indirection:
    return rt->AsIndirection()->IsReference() ? eTypeClassReference
                                              : eTypeClassPointer;
  case RustType::Aggregate:
    return rt->AsAggregate()->kind == AggregateKind::Union ? eTypeClassUnion
                                                           : eTypeClassStruct;
  case RustType::SumType:
    return eTypeClassStruct;
  case RustType::CStyleEmum:
    return eTypeClassEnumeration;
  case RustType::Array:
    return eTypeClassArray;
  case RustType::Function:
    return eTypeClassFunction;
  }

  // shouldn't be possible to get here
  return lldb::eTypeClassInvalid;
}

// ------------------------ Modified Type Creation ------------------------ //

CompilerType TypeSystemRust::GetArrayElementType(
    lldb::opaque_compiler_type_t type,
    ExecutionContextScope* exe_scope
) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);
  if (auto* array = rt->AsArray()) {
    return array->element_type;
  }

  return CompilerType();
}

CompilerType
TypeSystemRust::GetArrayType(lldb::opaque_compiler_type_t type, uint64_t size) {
  if (!type) {
    return CompilerType();
  }

  auto compiler_type = CompilerType(weak_from_this(), type);
  ConstString type_name = ConstString(
      llvm::formatv("[{0}; {1}]", compiler_type.GetTypeName(), size).str()
  );

  RustType* rt =
      new RustType{RustType::NewArray(type_name, compiler_type, size)};

  return CompilerType(weak_from_this(), rt);
}

CompilerType TypeSystemRust::GetCanonicalType(lldb::opaque_compiler_type_t type
) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);
  if (auto* td = rt->AsTypedef()) {
    return td->underlying_type;
  }

  return CompilerType(weak_from_this(), type);
}

CompilerType
TypeSystemRust::GetEnumerationIntegerType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);
  if (auto* td = rt->AsCStyleEnum()) {
    return td->underlying_type;
  }
  if (auto* td = rt->AsSumType()) {
    return td->discr_type;
  }

  return CompilerType();
}

CompilerType TypeSystemRust::GetPointeeType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);
  if (auto* ptr = rt->AsIndirection()) {
    return ptr->pointee_type;
  }

  return CompilerType();
}

CompilerType TypeSystemRust::GetPointerType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return CompilerType();
  }

  auto compiler_type = CompilerType(weak_from_this(), type);
  ConstString type_name =
      ConstString(llvm::formatv("*mut {0}", compiler_type.GetTypeName()).str());

  RustType* rt = new RustType{RustType::NewIndirection(
      type_name,
      m_pointer_byte_size,
      compiler_type,
      DW_TAG_pointer_type
  )};

  return CompilerType(weak_from_this(), rt);
}

CompilerType
TypeSystemRust::GetLValueReferenceType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return CompilerType();
  }

  auto* pointee_type = static_cast<RustType*>(type);
  ConstString type_name =
      ConstString(llvm::formatv("&mut {0}", pointee_type->m_name).str());

  auto compiler_type = CompilerType(weak_from_this(), type);
  RustType* rt = new RustType{RustType::NewIndirection(
      type_name,
      m_pointer_byte_size,
      compiler_type,
      DW_TAG_reference_type
  )};

  return CompilerType(weak_from_this(), rt);
}

CompilerType
TypeSystemRust::GetRValueReferenceType(lldb::opaque_compiler_type_t type) {
  return GetLValueReferenceType(type);
}

CompilerType TypeSystemRust::GetAtomicType(lldb::opaque_compiler_type_t type) {
  return CompilerType(weak_from_this(), type);
}

CompilerType TypeSystemRust::AddConstModifier(lldb::opaque_compiler_type_t type
) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);

  if (auto* ptr = rt->AsIndirection()) {
    Tag tag;
    llvm::StringRef name;
    switch (ptr->kind) {
    case IndirectionKind::ConstPointer:
    case IndirectionKind::Reference:
      return CompilerType(weak_from_this(), rt);
    case IndirectionKind::MutPointer:
      tag = DW_TAG_pointer_type;
      name = "*const";
      break;
    case IndirectionKind::MutReference:
      tag = DW_TAG_reference_type;
      name = "&";
      break;
    }

    ConstString type_name = ConstString(
        llvm::formatv("&mut {0}", name, ptr->pointee_type.GetTypeName()).str()
    );

    RustType* new_ptr = new RustType{RustType::NewIndirection(
        type_name,
        m_pointer_byte_size,
        ptr->pointee_type,
        tag
    )};

    return CompilerType(weak_from_this(), new_ptr);
  }

  return CompilerType(weak_from_this(), rt);
}

CompilerType
TypeSystemRust::AddVolatileModifier(lldb::opaque_compiler_type_t type) {
  return CompilerType(weak_from_this(), type);
}

CompilerType
TypeSystemRust::AddRestrictModifier(lldb::opaque_compiler_type_t type) {
  return CompilerType(weak_from_this(), type);
}

CompilerType TypeSystemRust::AddPtrAuthModifier(
    lldb::opaque_compiler_type_t type,
    uint32_t payload
) {
  return CompilerType(weak_from_this(), type);
}

CompilerType TypeSystemRust::CreateTypedef(
    lldb::opaque_compiler_type_t type,
    const char* name,
    const CompilerDeclContext& decl_ctx,
    uint32_t opaque_payload
) {
  if (!type) {
    return CompilerType();
  }

  CompilerType underlying_type = CompilerType(weak_from_this(), type);

  RustType* rt =
      new RustType{RustType::NewTypedef(ConstString(name), underlying_type)};

  return CompilerType(weak_from_this(), rt);
}

const llvm::fltSemantics& TypeSystemRust::GetFloatTypeSemantics(size_t byte_size
) {
  switch (byte_size) {
  case 2:
    return llvm::APFloatBase::IEEEhalf();
  case 4:
    return llvm::APFloatBase::IEEEsingle();
  case 8:
    return llvm::APFloatBase::IEEEdouble();
  case 16:
    return llvm::APFloatBase::IEEEquad();
  default:
    return llvm::APFloatBase::Bogus();
  }
}

std::optional<uint64_t> TypeSystemRust::GetBitSize(
    lldb::opaque_compiler_type_t type,
    ExecutionContextScope* exe_scope
) {
  if (!type) {
    return std::nullopt;
  }
  return static_cast<RustType*>(type)->m_size * 8;
}

lldb::Encoding TypeSystemRust::GetEncoding(
    lldb::opaque_compiler_type_t type,
    uint64_t& count
) {
  count = 1;
  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Bool:
    return eEncodingUint;
  case RustType::Char:
  case RustType::UInt:
    return eEncodingUint;
  case RustType::Int:
    return eEncodingSint;
  case RustType::Float:
    return eEncodingIEEE754;
  default:
    return eEncodingInvalid;
  }
}

lldb::Format TypeSystemRust::GetFormat(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Bool:
    return eFormatBoolean;
  case RustType::UInt:
    return eFormatUnsigned;
  case RustType::Int:
    return eFormatDecimal;
  case RustType::Float:
    return eFormatFloat;
  case RustType::Char:
    return eFormatUnicode32;
  case RustType::Typedef:
    return eFormatBytes;
  case RustType::Indirection:
    return eFormatPointer;
  case RustType::Aggregate:
  case RustType::SumType:
    return eFormatBytes;
  case RustType::CStyleEmum:
    return eFormatEnum;
  case RustType::Array:
    return eFormatBytes;
  case RustType::Function:
    return eFormatPointer;
  }

  return lldb::eFormatDefault;
}

llvm::Expected<uint32_t> TypeSystemRust::GetNumChildren(
    lldb::opaque_compiler_type_t type,
    bool omit_empty_base_classes,
    const ExecutionContext* exe_ctx
) {
  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Typedef: {
    auto* t = rt->AsTypedef();
    return t->underlying_type.GetNumChildren(omit_empty_base_classes, exe_ctx);
  } break;
  case RustType::Indirection: {
    auto* t = rt->AsIndirection();
    return t->pointee_type.GetNumChildren(omit_empty_base_classes, exe_ctx);
  } break;
  case RustType::Aggregate: {
    auto* t = rt->AsAggregate();
    return t->fields.size();
  }
  case RustType::SumType: {
    auto* t = rt->AsSumType();
    // extra is for discr
    return t->variants.size() + 1;
  }
  case RustType::Array: {
    auto* t = rt->AsArray();
    return t->len;
  }
  default:
    return 0;
  }
}

CompilerType TypeSystemRust::GetBuiltinTypeByName(ConstString name) {
  static const llvm::StringMap<RustType> g_type_map = {
      {"()",
       RustType{
           UNIT_TYPE_NAME,
           0,
           RustAggregate{{}, {}, {}, 1, AggregateKind::Tuple}
       }},

      {"i8", RustType{I8_NAME, 1, RustInt{}}},
      {"u8", RustType{U8_NAME, 1, RustUInt{}}},

      {"i16", RustType{I16_NAME, 2, RustInt{}}},
      {"u16", RustType{U16_NAME, 2, RustUInt{}}},

      {"i32", RustType{I32_NAME, 4, RustInt{}}},
      {"u32", RustType{U32_NAME, 4, RustUInt{}}},

      {"i64", RustType{I64_NAME, 8, RustInt{}}},
      {"u64", RustType{U64_NAME, 8, RustUInt{}}},

      {"i128", RustType{I128_NAME, 16, RustInt{}}},
      {"u128", RustType{U128_NAME, 16, RustUInt{}}},

      {"f16", RustType{F16_NAME, 2, RustFloat{}}},
      {"f32", RustType{F32_NAME, 4, RustFloat{}}},
      {"f64", RustType{F64_NAME, 8, RustFloat{}}},

      {"char", RustType{U8_NAME, 1, RustChar{}}},
      {"bool", RustType{I8_NAME, 1, RustBool{}}},
  };

  auto iter = g_type_map.find(name);
  if (iter == g_type_map.end())
    return CompilerType();

  return CompilerType(weak_from_this(), new RustType{iter->second});
}

lldb::BasicType
TypeSystemRust::GetBasicTypeEnumeration(lldb::opaque_compiler_type_t type) {
  if (GetTypeClass(type) != eTypeClassBuiltin) {
    return eBasicTypeInvalid;
  }

  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Bool:
    return eBasicTypeBool;
  case RustType::UInt: {
    switch (rt->m_size) {
    case 1:
      return eBasicTypeUnsignedChar;
    case 2:
      return eBasicTypeUnsignedShort;
    case 3:
      return eBasicTypeUnsignedInt;
    case 4:
      return eBasicTypeUnsignedLongLong;
    case 8:
      return eBasicTypeUnsignedInt128;
    default:
      return eBasicTypeOther;
    }
  }
  case RustType::Int: {
    switch (rt->m_size) {
    case 1:
      return eBasicTypeSignedChar;
    case 2:
      return eBasicTypeShort;
    case 4:
      return eBasicTypeInt;
    case 8:
      return eBasicTypeLongLong;
    case 16:
      return eBasicTypeInt128;
    default:
      return eBasicTypeOther;
    }
  }
  case RustType::Float: {
    switch (rt->m_size) {
    case 2:
      return eBasicTypeHalf;
    case 4:
      return eBasicTypeFloat;
    case 8:
      return eBasicTypeDouble;
      // eBasicTypeLongDouble is typically the 80-bit extended precision
      // float, not f128. If f128 ever gets stabilized, it may be worth
      // hijacking the tag it
    default:
      return eBasicTypeOther;
    }
  }
  case RustType::Char:
    return eBasicTypeChar32;
  default:
    return eBasicTypeInvalid;
  }
}

void TypeSystemRust::ForEachEnumerator(
    lldb::opaque_compiler_type_t type,
    std::function<bool(
        const CompilerType& integer_type,
        ConstString name,
        const llvm::APSInt& value
    )> const& callback
) {
  auto* rt = static_cast<RustType*>(type);
  if (auto* t = rt->AsCStyleEnum()) {
    bool is_signed = t->underlying_type.IsSigned();
    for (auto& v : t->variants) {
      if (!callback(
              t->underlying_type,
              ConstString(v.second),
              llvm::APSInt(
                  llvm::APInt(rt->m_size * 8, v.first.value_or(0), is_signed)
              )
          )) {
        break;
      }
    }
  }
}

uint32_t TypeSystemRust::GetNumFields(lldb::opaque_compiler_type_t type) {
  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Typedef: {
    auto* t = rt->AsTypedef();
    return t->underlying_type.GetNumFields();
  } break;
  case RustType::Aggregate: {
    auto* t = rt->AsAggregate();
    return t->fields.size();
  }
  case RustType::SumType: {
    auto* t = rt->AsSumType();
    // extra 1 is so that we can treat the discr as a field
    return t->variants.size() + 1;
  }
  default:
    return 0;
  }
}

CompilerType TypeSystemRust::GetFieldAtIndex(
    lldb::opaque_compiler_type_t type,
    size_t idx,
    std::string& name,
    uint64_t* bit_offset_ptr,
    uint32_t* bitfield_bit_size_ptr,
    bool* is_bitfield_ptr
) {
  if (bit_offset_ptr)
    *bit_offset_ptr = 0;
  if (bitfield_bit_size_ptr)
    *bitfield_bit_size_ptr = 0;
  if (is_bitfield_ptr)
    *is_bitfield_ptr = false;

  if (!type)
    return CompilerType();

  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Typedef: {
    auto* t = rt->AsTypedef();
    return t->underlying_type.GetFieldAtIndex(
        idx,
        name,
        bit_offset_ptr,
        bitfield_bit_size_ptr,
        is_bitfield_ptr
    );
  } break;
  case RustType::Aggregate: {
    auto* t = rt->AsAggregate();
    if (idx < t->fields.size()) {
      auto* field = &t->fields[idx];

      *bit_offset_ptr = field->byte_offset * 8;
      name = field->name.GetStringRef();
      return field->underlying_type;
    }
    return CompilerType();
  }
  case RustType::SumType: {
    // bit offset will always be 0 because the fields of the variants have
    // offset values based on the discr value (e.g. the first field of a variant
    // with a u8 discriminant will already have a byte offset value of 1)
    auto* t = rt->AsSumType();
    if (idx < t->variants.size()) {
      EnumVariant& variant = t->variants[idx];

      name = variant.name.GetString();
      return variant.underlying_type;
    }

    if (idx == t->variants.size()) {
      // match rust Synthetic Provider expected name
      name = "$discr$";
      return t->discr_type;
    }

    return CompilerType();
  }
  default:
    return CompilerType();
  }
}

CompilerDecl TypeSystemRust::GetStaticFieldWithName(
    lldb::opaque_compiler_type_t type,
    llvm::StringRef name
) {
  return CompilerDecl();
  // if (!type) {
  //   return CompilerDecl();
  // }

  // auto* rt = static_cast<RustType*>(type);

  // auto* at = rt->AsAggregate();

  // if (!at) {
  //   return CompilerDecl();
  // }
}

llvm::Expected<CompilerType> TypeSystemRust::GetChildCompilerTypeAtIndex(
    lldb::opaque_compiler_type_t type,
    ExecutionContext* exe_ctx,
    size_t idx,
    // output option, "should we auto-deref pointers?"
    // if not, the only child is the derefed pointer
    bool transparent_pointers,
    bool omit_empty_base_classes,
    bool ignore_array_bounds,
    std::string& child_name,
    uint32_t& child_byte_size,
    int32_t& child_byte_offset,
    uint32_t& child_bitfield_bit_size,
    uint32_t& child_bitfield_bit_offset,
    bool& child_is_base_class,
    bool& child_is_deref_of_parent,
    ValueObject* valobj,
    uint64_t& language_flags
) {
  child_name.clear();
  child_byte_size = 0;
  child_byte_offset = 0;
  child_bitfield_bit_size = 0;
  child_bitfield_bit_offset = 0;
  child_is_base_class = false;
  child_is_deref_of_parent = false;
  language_flags = 0;

  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);

  switch (rt->VariantKind()) {
  case RustType::Typedef: {
    auto* t = rt->AsTypedef();

    return GetChildCompilerTypeAtIndex(
        t->underlying_type.GetOpaqueQualType(),
        exe_ctx,
        idx,
        transparent_pointers,
        omit_empty_base_classes,
        ignore_array_bounds,
        child_name,
        child_byte_size,
        child_byte_offset,
        child_bitfield_bit_size,
        child_bitfield_bit_offset,
        child_is_base_class,
        child_is_deref_of_parent,
        valobj,
        language_flags
    );
  } break;
  case RustType::Indirection: {
    auto* t = rt->AsIndirection();

    if (transparent_pointers) {
      bool tmp = false;

      return t->pointee_type.GetChildCompilerTypeAtIndex(
          exe_ctx,
          idx,
          transparent_pointers,
          omit_empty_base_classes,
          ignore_array_bounds,
          child_name,
          child_byte_size,
          child_byte_offset,
          child_bitfield_bit_size,
          child_bitfield_bit_offset,
          child_is_base_class,
          tmp,
          valobj,
          language_flags
      );
    }

    child_is_deref_of_parent = true;

    auto* parent_name = valobj ? valobj->GetName().AsCString() : nullptr;

    if (parent_name) {
      child_name.assign(llvm::formatv("*{0}", parent_name));
    }

    if (idx == 0) {
      auto size = t->pointee_type.GetByteSize(
          exe_ctx ? exe_ctx->GetBestExecutionContextScope() : nullptr
      );
      if (!size.has_value()) {
        return llvm::createStringError("no size info for field");
      }

      child_byte_size = size.value();
      child_byte_offset = 0;
      return {t->pointee_type};
    }

  } break;
  case RustType::Aggregate:
  case RustType::SumType: {
    uint64_t bit_offset;
    CompilerType child =
        GetFieldAtIndex(type, idx, child_name, &bit_offset, nullptr, nullptr);
    auto size = child.GetByteSize(
        exe_ctx ? exe_ctx->GetBestExecutionContextScope() : nullptr
    );
    if (!size.has_value()) {
      return llvm::createStringError("no size info for field");
    }

    child_byte_size = size.value();
    child_byte_offset = bit_offset / 8;
    return child;
  } break;
  case RustType::Array: {
    auto* t = rt->AsArray();

    if (ignore_array_bounds || idx < t->len) {
      child_name.assign(llvm::formatv("[{0}]", idx));

      CompilerType element_type = t->element_type;

      auto size = element_type.GetByteSize(
          exe_ctx ? exe_ctx->GetBestExecutionContextScope() : nullptr
      );
      if (!size.has_value()) {
        return llvm::createStringError("no size info for field");
      }

      child_byte_size = size.value();
      child_byte_offset =
          static_cast<int32_t>(idx) * static_cast<int32_t>(child_byte_size);

      return element_type;
    }
  } break;
  default:
    break;
  }

  return CompilerType();
}

uint32_t TypeSystemRust::GetIndexOfChildWithName(
    lldb::opaque_compiler_type_t type,
    llvm::StringRef name,
    bool omit_empty_base_classes
) {
  if (!(type && !name.empty())) {
    return UINT32_MAX;
  }

  auto* rt = static_cast<RustType*>(type);
  switch (rt->VariantKind()) {
  case RustType::Typedef: {
    auto* t = rt->AsTypedef();
    return t->underlying_type.GetIndexOfChildWithName(
        name,
        omit_empty_base_classes
    );
  } break;
  case RustType::Aggregate: {
    auto* t = rt->AsAggregate();
    uint64_t len = t->fields.size();
    for (uint64_t i = 0; i < len; ++i) {
      if (t->fields[i].name == name) {
        return i;
      }
    }
  } break;
  case RustType::SumType: {
    if (name == "$discr$") {
      return rt->AsSumType()->variants.size();
    }

    if (name.starts_with("$variant$")) {
      auto slice = name.slice(9, name.size());
      return std::stoull(slice.str());
    }

    break;
  }
  case RustType::Indirection: {
    auto* t = rt->AsIndirection();
    return t->pointee_type.GetIndexOfChildWithName(
        name,
        omit_empty_base_classes
    );
  }
  default:
    break;
  }

  return UINT32_MAX;
}

size_t TypeSystemRust::GetIndexOfChildMemberWithName(
    lldb::opaque_compiler_type_t type,
    llvm::StringRef name,
    bool omit_empty_base_classes,
    std::vector<uint32_t>& child_indexes
) {
  uint32_t index = GetIndexOfChildWithName(type, name, omit_empty_base_classes);
  if (index == UINT_MAX)
    return 0;
  child_indexes.push_back(index);
  return 1;
}

CompilerType TypeSystemRust::GetDirectNestedTypeWithName(
    lldb::opaque_compiler_type_t type,
    llvm::StringRef name
) {
  return CompilerType();
}

size_t TypeSystemRust::GetNumTemplateArguments(
    lldb::opaque_compiler_type_t type,
    bool expand_pack
) {
  if (!type) {
    return 0;
  }

  auto* rt = static_cast<RustType*>(type);

  switch (rt->VariantKind()) {
  case RustType::Aggregate:
    return rt->AsAggregate()->template_args.size();
  case RustType::SumType:
    return rt->AsSumType()->variants[0].underlying_type.GetNumTemplateArguments(
    );
  case RustType::Function:
    return rt->AsFunction()->template_args.size();
  default:
    return 0;
  }
}

lldb::TemplateArgumentKind TypeSystemRust::GetTemplateArgumentKind(
    lldb::opaque_compiler_type_t type,
    size_t idx,
    bool expand_pack
) {
  if (GetTypeTemplateArgument(type, idx, expand_pack).IsValid()) {
    // TODO rust compiler doesn't output generic values, only generic types
    // if that ever changes, we'll need to add handling here
    return lldb::eTemplateArgumentKindType;
  }

  return lldb::eTemplateArgumentKindType;
}

CompilerType TypeSystemRust::GetTypeTemplateArgument(
    lldb::opaque_compiler_type_t type,
    size_t idx,
    bool expand_pack
) {
  if (!type) {
    return CompilerType();
  }

  auto* rt = static_cast<RustType*>(type);

  switch (rt->VariantKind()) {
  case RustType::Aggregate: {
    auto& args = rt->AsAggregate()->template_args;
    if (idx < args.size()) {
      auto template_arg = args[idx];

      if (!template_arg.second) {
        TypeQuery query = TypeQuery(template_arg.first);
        query.SetFindOne(true);
        query.AddLanguage(eLanguageTypeRust);
        TypeResults results{};
        GetSymbolFile()->FindTypes(query, results);
        if (auto r = results.GetFirstType()) {
          auto arg = r->GetFullCompilerType();
          args[idx].second = arg;
          return arg;
        }

        return CompilerType();
      }

      return template_arg.second.value();
    }

    return CompilerType();
  }
  case RustType::SumType: {
    return rt->AsSumType()->variants[0].underlying_type.GetTypeTemplateArgument(
        idx
    );
  }
  case RustType::Function: {
    std::vector<CompilerType>& args = rt->AsFunction()->template_args;
    if (idx < args.size()) {
      return args[idx];
    }

    return CompilerType();
  }
  default:
    return CompilerType();
  }
}

std::optional<CompilerType::IntegralTemplateArgument>
TypeSystemRust::GetIntegralTemplateArgument(
    lldb::opaque_compiler_type_t type,
    size_t idx,
    bool expand_pack
) {
  // TODO rust compiler doesn't output generic values, only generic types
  // if that ever changes, we'll need to add handling here
  return std::nullopt;
}

// TODO old TypeSystemRust didn't implement
CompilerType TypeSystemRust::GetBasicTypeFromAST(lldb::BasicType basic_type) {
  return CompilerType();
}

// TODO old TypeSystemRust didn't implement
CompilerType TypeSystemRust::GetBuiltinTypeForEncodingAndBitSize(
    lldb::Encoding encoding,
    size_t bit_size
) {
  // switch (encoding) {
  // case eEncodingUint:
  //   // TODO add scoped_name_to_type map,
  // case eEncodingSint:
  // case eEncodingIEEE754:
  // case eEncodingVector:
  //   break;
  // }
  return CompilerType();
}

// TODO probably not used?
CompilerType TypeSystemRust::CreateGenericFunctionPrototype() {
  return CompilerType();
}

uint32_t TypeSystemRust::IsHomogeneousAggregate(
    lldb::opaque_compiler_type_t type,
    CompilerType* base_type_ptr
) {
  // TODO
  // per old TypeSystemRust:
  // "FIXME should detect "homogeneous floating-point aggregates"."
  return 0;
}

bool TypeSystemRust::IsPolymorphicClass(lldb::opaque_compiler_type_t type) {
  return false;
}

// If the current object represents a typedef type, get the underlying type.
CompilerType TypeSystemRust::GetTypedefedType(lldb::opaque_compiler_type_t type
) {
  if (!type) {
    return CompilerType();
  }

  if (auto* t = static_cast<RustType*>(type)->AsTypedef()) {
    return t->underlying_type;
  }

  return CompilerType();
}

CompilerType
TypeSystemRust::GetFullyUnqualifiedType(lldb::opaque_compiler_type_t type) {
  // FIXME this works because rust doesn't output qualifiers atm, but it will
  // eventually for pointers
  return CompilerType(weak_from_this(), type);
}

CompilerType
TypeSystemRust::GetNonReferenceType(lldb::opaque_compiler_type_t type) {
  if (!type) {
    return CompilerType();
  }

  if (auto* t = static_cast<RustType*>(type)->AsIndirection()) {
    if (t->IsReference()) {
      return t->pointee_type;
    }
  }

  return CompilerType(weak_from_this(), type);
}

// --------------------------------- Dumping -------------------------------- //

bool TypeSystemRust::DumpTypeValue(
    lldb::opaque_compiler_type_t type,
    Stream& s,
    lldb::Format format,
    const DataExtractor& data,
    lldb::offset_t data_offset,
    size_t data_byte_size,
    uint32_t bitfield_bit_size,
    uint32_t bitfield_bit_offset,
    ExecutionContextScope* exe_scope
) {
  if (!type) {
    return false;
  }
  if (IsAggregateType(type)) {
    return false;
  }

  uint32_t item_count = 1;

  auto* rt = static_cast<RustType*>(type);

  // Print enum variant names instead of their values
  if (format == eFormatEnum && rt->IsCStyleEnum()) {
    auto* et = static_cast<RustType*>(type)->AsCStyleEnum();
    // even if it's represented in rust as a signed value, it's stored in
    // RustType as an unsigned value
    uint64_t discr = data.GetMaxU64Bitfield(
        &data_offset,
        data_byte_size,
        bitfield_bit_size,
        bitfield_bit_offset
    );

    if (et->variants.contains(discr)) {
      s.Printf(
          "%s::%s",
          rt->m_name.AsCString(),
          et->variants[discr].AsCString()
      );
    } else {
      s.Printf("<invalid> %llu", discr);
    }

    return true;
  }

  // // Format sum types by determining which variant to display
  // if (rt->IsSumType()) {
  //   auto* st = rt->AsSumType();
  //   uint64_t discr_size = st->discr_type.GetByteSize(exe_scope).value();

  //   uint64_t discr = data.GetMaxU64(&data_offset, discr_size);

  //   CompilerType variant = st->GetVariant(discr);
  //   auto* variant_type = static_cast<RustType*>(variant.GetOpaqueQualType());

  //   // this should always succeed since variants are stored as Struct nodes
  //   if (variant_type->IsAggregate()) {
  //     auto* vt = variant_type->AsAggregate();
  //     s.Printf("%s", variant_type->m_name.AsCString());
  //   }
  // }

  // LLDB's default unicode formatting sucks. Everything is formatted as the
  // `U00000001` raw unicode value which is unhelpful. This reformats chars to
  // their appropriate ascii representation/escape code when appropriate.
  if (format == lldb::eFormatCharPrintable ||
      (format == eFormatUnicode32 && rt->IsChar())) {
    uint64_t value = data.GetMaxU64Bitfield(
        &data_offset,
        data_byte_size,
        bitfield_bit_size,
        bitfield_bit_offset
    );

    switch (value) {
    case '\n':
      s.PutCString("'\\n'");
      break;
    case '\r':
      s.PutCString("'\\r'");
      break;
    case '\t':
      s.PutCString("'\\t'");
      break;
    case '\\':
      s.PutCString("'\\\\'");
      break;
    case '\0':
      s.PutCString("'\\0'");
      break;
    case '\'':
      s.PutCString("'\\''");
      break;

    default:
      if (value < 128 && isprint(value)) {
        s.Printf("'%c'", char(value));
      } else {
        s.Printf("'\\u{%x}'", unsigned(value));
      }
      break;
    }

    return true;
  }

  // if (rt->IsIndirection()) {
  //   auto* pt = rt->AsIndirection();

  //   switch (pt->kind) {
  //   case IndirectionKind::ConstPointer:
  //     s.Printf("*const ");
  //     break;
  //   case IndirectionKind::MutPointer:
  //     s.Printf("*mut ");
  //     break;
  //   case IndirectionKind::Reference:
  //     s.Printf("&");
  //     break;
  //   case IndirectionKind::MutReference:
  //     s.Printf("&mut ");
  //     break;
  //   }

  //   return true;
  // }

  return DumpDataExtractor(
      data,
      &s,
      data_offset,
      format,
      data_byte_size,
      item_count,
      UINT32_MAX,
      LLDB_INVALID_ADDRESS,
      bitfield_bit_size,
      bitfield_bit_offset,
      exe_scope
  );
}

CompilerType TypeSystemRust::GetTypeForFormatters(void* type) {
  if (!type) {
    return CompilerType();
  }

  return CompilerType(weak_from_this(), type);
}

LazyBool
TypeSystemRust::ShouldPrintAsOneLiner(void* type, ValueObject* valobj) {
  // TODO ?
  return eLazyBoolNo;
}

std::optional<llvm::json::Value> TypeSystemRust::ReportStatistics() {
  // TODO ?
  return std::nullopt;
}

// const int EEF = 0;

// void TypeSystemRust::DumpTypeDescription(
//     lldb::opaque_compiler_type_t type,
//     lldb::DescriptionLevel level
// ) {
//   if (!type) {
//     return;
//   }
//   StreamFile s(stdout, false);
//   s.PutCString(static_cast<RustType*>(type)->m_name.GetStringRef());
// }

// void TypeSystemRust::DumpTypeDescription(
//     lldb::opaque_compiler_type_t type,
//     Stream& s,
//     lldb::DescriptionLevel level = lldb::eDescriptionLevelFull
// ) {

// }

/* -------------------------------------------------------------------------- */
/*                                   Helpers                                  */
/* -------------------------------------------------------------------------- */

/// Turns an unqualified name ("Bar") into a qualified name
/// ("crate_name::Foo::Bar")
ConstString
TypeSystemRust::QualifyTypeName(const ConstString& name, const DWARFDIE& die) {
  SymbolFileDWARF* dwarf = die.GetDWARF();
  lldb::user_id_t id = die.GetID();
  CompilerDeclContext ctx = dwarf->GetDeclContextContainingUID(id);
  ConstString ctx_name = ctx.GetScopeQualifiedName();
  if (!ctx_name) {
    return name;
  }
  std::string qual_name =
      std::string(ctx_name.AsCString()) + "::" + name.AsCString();
  return ConstString(qual_name.c_str());
}

void TypeSystemRust::PrintDeclContexts() {
  for (auto& decl : m_decl_contexts) {
    auto* rd = static_cast<RustDecl*>(decl.getSecond().GetOpaqueDeclContext());

    printf(
        "name: %s\nfullname:%s",
        rd->name.AsCString(),
        rd->full_name.AsCString()
    );

    printf("children {\n");
    for (auto& child : *rd->GetChildren()) {
      printf("\t%s,\n", child.getSecond()->name.AsCString());
    }

    printf("}\n\n");
  }
}

TypeSP TypeSystemRust::ParseTypeFromPDB(npdb::PdbTypeSymId type) {
  // printf("Parsing Type Index: %#x\n", type.index.getIndex());
  // auto uid = toOpaqueUid(type);

  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );

  PdbIndex& index = pdb->GetIndex();

  // printf("Parsing Type Index: %#x\n", type.index.getIndex());

  // if (type.index.getIndex() == 0x1099) {
  //   printf("Parsing &sample::Number\n");
  // }

  // there aren't a ton of forward decls in Rust, but SumType parsing ends up
  // with a few for the variant types (e.g. enum2$<example::Enum>::Variant0)

  // TODO check cache first

  std::optional<PdbTypeSymId> fwd_ref = {};

  lldb::user_id_t uid = toOpaqueUid(type);
  if (pdb->GetTypeMap().contains(uid)) {
    // printf("<found in type map>\n");
    return pdb->GetTypeMap()[uid];
  }

  TypeSP type_sp;
  if (type.index.isSimple()) {
    type_sp = ParseSimpleTypePDB(type);
    if (type_sp) {
      pdb->GetTypeMap()[uid] = type_sp;
      pdb->InsertNameToType(type_sp);
    }
    return type_sp;
  }

  auto t = index.tpi().findFullDeclForForwardRef(type.index);
  if (t) {
    fwd_ref = type;
    type = PdbTypeSymId(t.get());
    uid = toOpaqueUid(type);
    if (fwd_ref.value().index != type.index) {
      // printf("FwdRef of: %#x\n", type.index.getIndex());
    }
    if (pdb->GetTypeMap().contains(uid)) {
      // printf("<found in type map>\n");
      return pdb->GetTypeMap()[uid];
    }
  }

  if (pdb->ParsingStarted(uid)) {
    // printf("<parsing already started>\n");
    return nullptr;
  }

  pdb->StartParsing(uid);

  CVType cvt = index.tpi().getType(type.index);

  switch (cvt.kind()) {
  case LF_MODIFIER: {
    ModifierRecord modifier;
    llvm::cantFail(
        TypeDeserializer::deserializeAs<ModifierRecord>(cvt, modifier)
    );

    type_sp = ParseModifierTypePDB(modifier);
  } break;
  case LF_POINTER: {
    PointerRecord pointer;
    llvm::cantFail(TypeDeserializer::deserializeAs<PointerRecord>(cvt, pointer)
    );
    type_sp = ParsePointerTypePDB(type, pointer);
  } break;
  case LF_CLASS:
  case LF_STRUCTURE:
  case LF_UNION:
  case LF_ENUM: {
    CVTagRecord tag = CVTagRecord::create(cvt);
    if (tag.kind() == CVTagRecord::Union) {
      auto u_tag = tag.asUnion();

      type_sp = ParseAggregateTypePDB(type, u_tag, u_tag.getSize());
    } else if (tag.kind() == CVTagRecord::Enum) {
      type_sp = ParseEnumTypePDB(type.index, tag.asEnum());
    } else {
      auto c_tag = tag.asClass();
      type_sp = ParseAggregateTypePDB(type.index, c_tag, c_tag.getSize());
    }

  } break;
  case LF_ARRAY: {
    ArrayRecord record;
    llvm::cantFail(TypeDeserializer::deserializeAs<ArrayRecord>(cvt, record));

    auto underlying = ParseTypeFromPDB(record.ElementType);

    // size is the total size of the array in bytes, not the element count
    uint64_t element_count =
        record.Size / underlying->GetByteSize(nullptr).value_or(1);

    // TODO leak
    // we normalize the type name here so that the output is identical to
    // DWARF data, thus we don't have to have any special handling down
    // the line
    RustType* rt = new RustType{RustType::NewArray(
        ConstString(
            llvm::formatv("[{0};{1}]", underlying->GetName(), element_count)
                .str()
        ),
        underlying->GetFullCompilerType(),
        element_count
    )};

    // these should be equal already but just in case
    rt->m_size = record.Size;

    auto compiler_type = CompilerType(weak_from_this(), rt);

    type_sp = GetSymbolFile()->MakeType(
        toOpaqueUid(type),
        compiler_type.GetTypeName(),
        rt->m_size,
        nullptr,
        LLDB_INVALID_UID,
        Type::eEncodingIsUID,
        Declaration(),
        compiler_type,
        Type::ResolveState::Full
    );
  } break;
  case LF_PROCEDURE: {
    ProcedureRecord pr;
    llvm::cantFail(TypeDeserializer::deserializeAs<ProcedureRecord>(cvt, pr));
    type_sp = ParseFunctionTypePDB(type, pr);
  } break;
  case LF_MFUNCTION:
    break;
  default:
    break;
  }

  if (type_sp) {
    pdb->GetTypeMap()[uid] = type_sp;
    pdb->InsertNameToType(type_sp);
    if (fwd_ref) {
      pdb->GetTypeMap()[toOpaqueUid(*fwd_ref)] = type_sp;
    }
  }

  pdb->DoneParsing(uid);
  // printf("Done parsing: %#llx\n", uid);

  return type_sp;
}

void TypeSystemRust::FillIndirectionTypes(
    CompilerType type,
    lldb::user_id_t type_idx
) {
  auto* pointee = static_cast<RustType*>(type.GetOpaqueQualType());

  auto* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );

  { // &
    // TODO leak

    RustType* ref = new RustType{RustType::NewIndirection(
        ConstString(llvm::formatv("&{0}", pointee->m_name).str()),
        m_pointer_byte_size,
        type,
        DW_TAG_reference_type
    )};

    auto ct = CompilerType(weak_from_this(), ref);

    pdb->InsertNameToType(pdb->MakeType(
        type_idx,
        ref->m_name,
        ref->m_size,
        nullptr,
        LLDB_INVALID_UID,
        Type::eEncodingIsLValueReferenceUID,
        Declaration(),
        ct,
        Type::ResolveState::Full
    ));
  }

  { // &mut
    RustType* mut_ref = new RustType{RustType::NewIndirection(
        ConstString(llvm::formatv("&mut {0}", pointee->m_name).str()),
        m_pointer_byte_size,
        type,
        DW_TAG_reference_type
    )};

    auto ct = CompilerType(weak_from_this(), mut_ref);

    pdb->InsertNameToType(pdb->MakeType(
        type_idx,
        mut_ref->m_name,
        mut_ref->m_size,
        nullptr,
        LLDB_INVALID_UID,
        Type::eEncodingIsLValueReferenceUID,
        Declaration(),
        ct,
        Type::ResolveState::Full
    ));
  }

  { // *const
    RustType* ptr = new RustType{RustType::NewIndirection(
        ConstString(llvm::formatv("*const {0}", pointee->m_name).str()),
        m_pointer_byte_size,
        type,
        DW_TAG_pointer_type
    )};

    auto ct = CompilerType(weak_from_this(), ptr);

    pdb->InsertNameToType(pdb->MakeType(
        type_idx,
        ptr->m_name,
        ptr->m_size,
        nullptr,
        LLDB_INVALID_UID,
        Type::eEncodingIsPointerUID,
        Declaration(),
        ct,
        Type::ResolveState::Full
    ));
  }

  { // *mut
    RustType* mut_ptr = new RustType{RustType::NewIndirection(
        ConstString(llvm::formatv("*mut {0}", pointee->m_name).str()),
        m_pointer_byte_size,
        type,
        DW_TAG_pointer_type
    )};

    auto ct = CompilerType(weak_from_this(), mut_ptr);

    pdb->InsertNameToType(pdb->MakeType(
        type_idx,
        mut_ptr->m_name,
        mut_ptr->m_size,
        nullptr,
        LLDB_INVALID_UID,
        Type::eEncodingIsPointerUID,
        Declaration(),
        ct,
        Type::ResolveState::Full
    ));
  }
}

TypeSP TypeSystemRust::ParseSimpleTypePDB(npdb::PdbTypeSymId type) {
  auto idx = type.index;
  if (idx == TypeIndex::NullptrT()) {
    // does rust even generate a nullptr type?
    assert(0);
  }

  // Indirect types are near/far/etc. pointers, which we don't really care about
  // the intricacies of. We just get the underlying type and spit out a `*mut T`
  if (idx.getSimpleMode() != SimpleTypeMode::Direct) {
    TypeSP direct_type = ParseTypeFromPDB(idx.makeDirect());
    if (!direct_type) {
      return nullptr;
    }

    uint32_t pointer_size = 0;
    switch (idx.getSimpleMode()) {
    case SimpleTypeMode::FarPointer32:
    case SimpleTypeMode::NearPointer32:
      pointer_size = 4;
      break;
    case SimpleTypeMode::NearPointer64:
      pointer_size = 8;
      break;
    default:
      // 128-bit and 16-bit pointers unsupported.
      return nullptr;
    }

    FillIndirectionTypes(direct_type->GetFullCompilerType(), toOpaqueUid(type));

    auto compiler_type =
        GetPointerType(direct_type->GetFullCompilerType().GetOpaqueQualType());

    return GetSymbolFile()->MakeType(
        toOpaqueUid(type),
        compiler_type.GetTypeName(),
        pointer_size,
        nullptr,
        LLDB_INVALID_UID,
        Type::eEncodingIsPointerUID,
        Declaration(),
        compiler_type,
        Type::ResolveState::Full
    );
  }

  auto simple_type = idx.getSimpleKind();

  if (simple_type == SimpleTypeKind::NotTranslated) {
    return nullptr;
  }

  size_t size = GetTypeSizeForSimpleKind(simple_type);
  RustType* rt;
  // Even though some of these are invalid for Rust (e.g. bool can only be 8
  // bits), I'm hesitant to ignore any. I'll leave out the complex types since
  // rust for sure doesn't use those
  switch (simple_type) {
  case SimpleTypeKind::Void:
    rt = primitive_types.unit();
    break;
  case SimpleTypeKind::Boolean128:
  case SimpleTypeKind::Boolean16:
  case SimpleTypeKind::Boolean32:
  case SimpleTypeKind::Boolean64:
    // TODO invalid, ignore, panic, treat as a regular bool?
    // TODO leak
    rt = new RustType{
        RustType::NewBool(ConstString("<invalid large-boolean type>"))
    };
    break;
  case SimpleTypeKind::Boolean8:
    rt = primitive_types.Bool();
    break;
  case SimpleTypeKind::Byte:
  case SimpleTypeKind::UnsignedCharacter:
    // TODO not sure if narrowcharacter should count
  case SimpleTypeKind::NarrowCharacter:
    rt = primitive_types.u8();
    break;
  case SimpleTypeKind::SignedCharacter:
  case SimpleTypeKind::SByte:
    rt = primitive_types.i8();
    break;
  case SimpleTypeKind::Character32:
    rt = primitive_types.Char();
    break;
  case SimpleTypeKind::Character8:
    // TODO leak
    rt = new RustType{RustType::NewUInt(ConstString("<char8_t>"), 1)};
    break;
  case SimpleTypeKind::WideCharacter:
    // TODO leak
    rt = new RustType{RustType::NewUInt(ConstString("<wchar_t>"), 2)};
    break;
  case SimpleTypeKind::Character16:
    // TODO leak
    rt = new RustType{RustType::NewUInt(ConstString("<char16_t>"), 8)};
    break;

  case SimpleTypeKind::Int16Short:
  case SimpleTypeKind::Int16:
    rt = primitive_types.i16();
    break;
  case SimpleTypeKind::UInt16Short:
  case SimpleTypeKind::UInt16:
    rt = primitive_types.u16();
    break;
  case SimpleTypeKind::Int32Long:
  case SimpleTypeKind::Int32:
    rt = primitive_types.i32();
    break;
  case SimpleTypeKind::UInt32Long:
  case SimpleTypeKind::UInt32:
    rt = primitive_types.u32();
    break;
  case SimpleTypeKind::Int64Quad:
  case SimpleTypeKind::Int64:
    rt = primitive_types.i64();
    break;
  case SimpleTypeKind::UInt64Quad:
  case SimpleTypeKind::UInt64:
    rt = primitive_types.u64();
    break;
  case SimpleTypeKind::Int128Oct:
  case SimpleTypeKind::Int128:
    rt = primitive_types.i128();
    break;
  case SimpleTypeKind::UInt128Oct:
  case SimpleTypeKind::UInt128:
    rt = primitive_types.u128();
    break;
  case SimpleTypeKind::Float16:
    rt = primitive_types.f16();
    break;
  case SimpleTypeKind::Float32:
    rt = primitive_types.f32();
    break;
  case SimpleTypeKind::Float64:
    rt = primitive_types.f64();
    break;
  case SimpleTypeKind::Float128:
    rt = primitive_types.f128();
    break;
  case SimpleTypeKind::Float32PartialPrecision:
    // TODO leak
    rt = new RustType{
        RustType::NewFloat(ConstString("<float_32_partial_precision>"), 4)
    };
    break;
  case SimpleTypeKind::Float48:
    // TODO leak
    rt = new RustType{RustType::NewFloat(ConstString("<float_48>"), 6)};
    break;
  case SimpleTypeKind::Float80:
    // TODO leak
    rt = new RustType{RustType::NewFloat(ConstString("<float_80>"), 10)};
    break;
  case SimpleTypeKind::Complex16:
  case SimpleTypeKind::Complex32:
  case SimpleTypeKind::Complex32PartialPrecision:
  case SimpleTypeKind::Complex48:
  case SimpleTypeKind::Complex64:
  case SimpleTypeKind::Complex80:
  case SimpleTypeKind::Complex128:
  case SimpleTypeKind::None:
  case SimpleTypeKind::NotTranslated:
  case SimpleTypeKind::HResult:
    return nullptr;
  }

  auto compiler_type = CompilerType(weak_from_this(), rt);

  return GetSymbolFile()->MakeType(
      toOpaqueUid(type),
      rt->m_name,
      size,
      nullptr,
      LLDB_INVALID_UID,
      Type::eEncodingIsUID,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );
}

TypeSP TypeSystemRust::ParseModifierTypePDB(ModifierRecord& modifier) {
  // TODO handle modifiers once there are some
  return ParseTypeFromPDB(modifier.ModifiedType);
}

TypeSP TypeSystemRust::ParsePointerTypePDB(
    npdb::PdbTypeSymId type,
    PointerRecord& pointer
) {
  auto pointee_type = ParseTypeFromPDB(pointer.getReferentType());

  // printf("pointer type: %s\n", pointee_type->GetName().AsCString());

  // "This can happen for pointers to LF_VTSHAPE records, which we shouldn't
  // create in the AST." - Clang PdbAstBuilder implementation
  // We also ignore pointer-to-member because it shouldn't be possible in rust
  // (and is EXCEEDINGLY rare in C++ anyway. What a weird language feature)
  if (!pointee_type || pointer.isPointerToMember()) {
    return {};
  }

  FillIndirectionTypes(pointee_type->GetFullCompilerType(), toOpaqueUid(type));

  m_pointer_byte_size = pointer.getSize();

  Type::EncodingDataType encoding = Type::eEncodingIsPointerUID;
  auto* pointee_rt = static_cast<RustType*>(
      pointee_type->GetFullCompilerType().GetOpaqueQualType()
  );
  CompilerType compiler_type;

  switch (pointer.getMode()) {
  case PointerMode::RValueReference:
  case PointerMode::LValueReference: {
    encoding = Type::eEncodingIsLValueReferenceUID;

    compiler_type = GetLValueReferenceType(pointee_rt);
  } break;
  default: {
    compiler_type = GetPointerType(pointee_rt);
  } break;
  }

  // may as well avoid some of the instruction cache uncertainty of the
  // CompilerType functions
  auto* pointer_rt = static_cast<RustType*>(compiler_type.GetOpaqueQualType());

  auto type_sp = GetSymbolFile()->MakeType(
      toOpaqueUid(type),
      pointer_rt->m_name,
      pointer_rt->m_size,
      nullptr,
      LLDB_INVALID_UID,
      encoding,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );

  return type_sp;
}

TypeSP TypeSystemRust::ParseAggregateTypePDB(
    PdbTypeSymId type,
    const TagRecord& record,
    uint32_t size
) {
  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();
  // make sure we use the item stream `ipi`
  auto x = index.ipi().typeArray();

  Declaration decl;

  // iterate over the item leaves to check for a source line listing that
  // matches the type we have
  for (auto& cvt : x) {
    if (cvt.kind() == llvm::codeview::LF_UDT_MOD_SRC_LINE) {
      auto src_data = llvm::cantFail(
          TypeDeserializer::deserializeAs<UdtModSourceLineRecord>(cvt.data())
      );

      // if it matches our type, we look up the string and add the file path to
      // the decl
      if (src_data.UDT == type.index) {
        auto file = src_data.SourceFile;

        auto string_table = index.pdb().getStringTable();
        if (string_table) {
          auto st = string_table.get();
          auto file_name = st.getStringForID(file.getIndex());
          if (file_name) {
            FileSpec f = FileSpec(file_name.get());
            decl.SetFile(f);
          }
        }
        break;
      }
    }
  }

  // TODO declcontext

  auto name = ConstString(record.getName());

  // if (name.GetStringRef().contains("HashMap")) {
  //   printf("here");
  // }

  auto name_ref = name.GetStringRef();

  CompilerType compiler_type;

  // At this point we can't determine if it's a struct or a tuple struct
  // since we need the name of the first field.
  AggregateKind agg_kind;
  // "enum2$<" will always be an unqualified prefix for a sum-type, with the
  // sole "template arg" being the sum-type itself. The variant types are
  // emitted with the nested-type flag, and we absolutely don't want to put the
  // variants through ParseSumTypePDB
  if (name_ref.starts_with("enum2$<") && !record.isNested()) {
    compiler_type = ParseSumTypePDB(type, record, size);
  } else {

    if (name_ref.starts_with("tuple$<")) {
      agg_kind = AggregateKind::Tuple;
    } else {
      if (record.Kind == llvm::codeview::TypeRecordKind::Union) {
        agg_kind = AggregateKind::Union;
      } else {
        agg_kind = AggregateKind::Struct;
      }
    }

    name = NormalizeMSVCTypeName(name.GetStringRef());

    // We set the alignment to 1 because CodeView doesn't store it (very cool,
    // thanks CodeView). Rust and C both agree that the alignment is at least
    // the maximum alignemnt of any of its fields, so we'll properly populate it
    // once we've process the fields. See:
    // https://doc.rust-lang.org/reference/type-layout.html#r-layout.repr.rust.layout
    // https://learn.microsoft.com/en-us/cpp/c-language/alignment-c?view=msvc-170#remarks
    RustType* rt =
        new RustType{RustType::NewAggregate(name, size, 1, agg_kind)};

    RustAggregate* agg = rt->AsAggregate();
    ParseFieldListPDB(record, agg->fields, agg->static_fields);

    uint64_t align = 1;

    for (auto& field : agg->fields) {
      if (field.byte_align > align) {
        align = field.byte_align;
      }
    }

    agg->align = align;

    // Might be better to check if every field is named like this, but there's
    // not really a point. This field-name check is the only way to
    // differentiate a struct from a tuple struct as far as I'm aware. In the
    // degenerate case, a user can name all of the fields identically to a tuple
    // struct (e.g. `__0`,
    // `__1`, etc.) so checking more doesn't really save us.
    if (agg->kind == AggregateKind::Struct && !agg->fields.empty() &&
        agg->fields.at(0).name.GetStringRef() == "__0") {
      agg->kind = AggregateKind::TupleStruct;
    }

    // for tuples/tuple structs we normalize the name here (e.g. `__0` -> `0`)
    // so that the visualizers don't have to do it. This saves us from needing
    // to make synthetics for types that otherwise wouldn't need one, or do the
    // same string processing multiple times later.

    // we also populate the template args since we know that each of the fields
    // corresponds to one
    if (agg->kind == AggregateKind::TupleStruct ||
        agg->kind == AggregateKind::Tuple) {
      for (auto& f : agg->fields) {
        f.name = ConstString(f.name.GetStringRef().substr(2));
        agg->template_args.push_back(
            {static_cast<RustType*>(f.underlying_type.GetOpaqueQualType())
                 ->m_name,
             f.underlying_type}
        );
      }
    } else {
      // If it's not a tuple, just populate the template arg name for now, and
      // the type will be populated on demand later
      auto [_, args] = GetTemplateArgs(name);
      for (auto& t : args) {

        agg->template_args.push_back(std::make_pair(t, std::nullopt));
      }
    }

    compiler_type = CompilerType(weak_from_this(), rt);
  }

  return GetSymbolFile()->MakeType(
      toOpaqueUid(type),
      static_cast<RustType*>(compiler_type.GetOpaqueQualType())->m_name,
      static_cast<RustType*>(compiler_type.GetOpaqueQualType())->m_size,
      nullptr,
      LLDB_INVALID_UID,
      Type::eEncodingIsUID,
      decl,
      compiler_type,
      Type::ResolveState::Full
  );
}

void TypeSystemRust::ParseFieldListPDB(
    const TagRecord& record,
    std::vector<FieldAttributes>& fields,
    std::vector<std::pair<ConstString, CompilerType>>& static_fields
) {

  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();

  auto field_list_idx = record.getFieldList();

  // early exit if the field list is empty
  if (field_list_idx.isSimple()) {
    return;
  }
  auto cvt = index.tpi().getType(field_list_idx);

  auto reader = llvm::BinaryStreamReader(cvt.data(), llvm::endianness::little);
  auto field_iter = FieldListDeserializer(reader);

  bool cont = true;
  while (cont) {
    CVMemberRecord member;

    if (auto e = reader.readEnum(member.Kind)) {
      consumeError(std::move(e));
      break;
    }
    if (auto e = field_iter.visitMemberBegin(member)) {
      consumeError(std::move(e));
      break;
    }
    switch (member.Kind) {
    case LF_MEMBER: {
      DataMemberRecord field_data;
      if (auto e = field_iter.visitKnownMember(member, field_data)) {
        consumeError(std::move(e));
        cont = false;
        break;
      }

      TypeSP t = ParseTypeFromPDB(PdbTypeSymId(field_data.getType()));
      if (t) {
        CompilerType ct = t->GetFullCompilerType();

        uint32_t offset = field_data.getFieldOffset();

        fields.push_back(FieldAttributes{
            // TODO is 1 an okay default align for a field? Does align matter
            // for
            // a field?
            ct.GetTypeBitAlign(nullptr).value_or(8) / 8,
            offset,
            ConstString(field_data.getName()),
            ct,
            field_data.getAccess() == llvm::codeview::MemberAccess::Public
                ? lldb::AccessType::eAccessPublic
                : lldb::AccessType::eAccessPrivate
        });
      }
    } break;
    // TODO
    case LF_STMEMBER: {
      StaticDataMemberRecord field_data;
      if (auto e = field_iter.visitKnownMember(member, field_data)) {
        consumeError(std::move(e));
        cont = false;
        break;
      }

      TypeSP t = ParseTypeFromPDB(PdbTypeSymId(field_data.getType()));
      CompilerType ct = t->GetFullCompilerType();

      static_fields.push_back(
          std::make_pair(ConstString(field_data.getName()), ct)
      );

    } break;
    default:
      break;
    }

    if (auto e = field_iter.visitMemberEnd(member)) {
      break;
    }
  }
}

lldb::TypeSP TypeSystemRust::ParseEnumTypePDB(
    npdb::PdbTypeSymId type,
    const llvm::codeview::EnumRecord& record
) {
  CompilerType underlying_type =
      ParseTypeFromPDB(record.UnderlyingType)->GetFullCompilerType();
  RustType* rt = new RustType{
      RustType::NewCStyleEnum(ConstString(record.Name), underlying_type)
  };
  RustCStyleEnumType* enum_type = rt->AsCStyleEnum();

  // ~identical parsing to Aggregate types, but with the EnumeratorRecord. Kept
  // separate mostly for sanity reasons, as covering all the cases in 1 function
  // gets really ugly.

  // TODO maybe put this chunk in a GetFieldIter method?
  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();

  auto field_list_idx = record.getFieldList();
  auto cvt = index.tpi().getType(field_list_idx);

  auto reader = llvm::BinaryStreamReader(cvt.data(), llvm::endianness::little);
  auto field_iter = FieldListDeserializer(reader);

  bool cont = true;
  while (cont) {
    CVMemberRecord member;

    if (auto e = reader.readEnum(member.Kind)) {
      consumeError(std::move(e));
      break;
    }
    if (auto e = field_iter.visitMemberBegin(member)) {
      consumeError(std::move(e));
      break;
    }

    switch (member.Kind) {
    case llvm::codeview::LF_ENUMERATE: {
      EnumeratorRecord enum_record;
      if (auto e = field_iter.visitKnownMember(member, enum_record)) {
        consumeError(std::move(e));
        cont = false;
        break;
      }

      enum_type->variants.insert(std::pair(
          underlying_type.IsSigned() ? enum_record.Value.getExtValue()
                                     : enum_record.Value.getZExtValue(),
          ConstString(enum_record.Name)
      ));
    } break;
    default:
      break;
    }

    CVMemberRecord end;
    if (auto e = field_iter.visitMemberEnd(end)) {
      assert(0);
    }
  }

  CompilerType compiler_type = CompilerType(weak_from_this(), rt);

  return GetSymbolFile()->MakeType(
      toOpaqueUid(type),
      rt->m_name,
      rt->m_size,
      nullptr,
      LLDB_INVALID_UID,
      Type::eEncodingIsUID,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );
}

void TypeSystemRust::TemplateArgsFromTypeName(
    const llvm::StringRef name,
    std::vector<CompilerType>& template_args
) {
  auto [root_name, args] = name.split('<');

  if (args.size() == 0) {
    return;
  }

  assert(args.ends_with('>'));
  args = args.substr(0, args.size() - 1);

  std::vector<llvm::StringRef> arg_vec{};
  uint32_t len = args.size();
  uint32_t start = 0;

  for (uint32_t i = 0; i < len; ++i) {
    if (args[i] == ',') {
      auto arg = args.substr(start, i).trim();
      // index->tpi().findRecordsByName();

      start = i + 1;
    }
  }

  // we already cut off the trailing `>` above, so now we need to handle the
  // case where there wasn't a trailing `,`
  auto last = args.substr(start).trim();
  if (last.size() != 0) {
    arg_vec.push_back(last);
  }
}

CompilerType TypeSystemRust::ParseSumTypePDB(
    npdb::PdbTypeSymId type,
    const llvm::codeview::TagRecord& record,
    uint32_t size
) {
  /*
  per rust documentation, this is how the debug info for sum types looks.
  ```c
  union enum2$<{fully-qualified-name}> {
    struct Variant0 {
      struct {name-of-variant-0} {
         <variant 0 fields>
      } value;
      static VariantNames NAME = {name-of-variant-0};
      static uint64_t DISCR_EXACT = {discriminant-of-variant-0};
    } variant0;
    <other variant structs>
    int_type tag;
    enum VariantNames {
       <name-of-variant-0> = 0, // The numeric values are variant index,
       <name-of-variant-1> = 1, // not discriminant values.
       <name-of-variant-2> = 2,
       ...
    }
  }
  ```
*/

  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();

  auto field_list_idx = record.getFieldList();
  auto cvt = index.tpi().getType(field_list_idx);

  auto reader = llvm::BinaryStreamReader(cvt.data(), llvm::endianness::little);
  auto field_iter = FieldListDeserializer(reader);

  // TODO leak
  auto* rt = new RustType{RustType::NewSumType(
      ConstString(record.getName()),
      size,
      {},
      CompilerType()
  )};

  auto* st = rt->AsSumType();

  // if the discr is 128 bits, it gets split in half and needs to be
  // reconstructed keep in mind, the type of the `tag` variable is accurate,
  // while the `DISCR_*` static fields of the variants are always output as
  // u64's by the rust compiler
  bool discr_128 = false;
  bool cont = true;
  while (cont) {
    CVMemberRecord member;

    if (auto e = reader.readEnum(member.Kind)) {
      consumeError(std::move(e));
      break;
    }
    if (auto e = field_iter.visitMemberBegin(member)) {
      consumeError(std::move(e));
      break;
    }
    switch (member.Kind) {
      // all the variants and the discr fall under the `LF_MEMBER` tag
    case LF_MEMBER: {
      DataMemberRecord field_data;
      if (auto e = field_iter.visitKnownMember(member, field_data)) {
        consumeError(std::move(e));
        cont = false;
        break;
      }

      auto name = field_data.getName();

      if (name.starts_with("tag")) {
        st->discr_type = ParseTypeFromPDB(PdbTypeSymId(field_data.getType()))
                             ->GetFullCompilerType();
      } else {
        // variants are output with the following property flags:
        // * nested type
        // * forward ref
        // * unique name
        // ParseTypeFromPDB resolves the foward ref into its full decl, so we
        // can just call it as normal
        TypeSP v_typesp = ParseTypeFromPDB(PdbTypeSymId(field_data.getType()));
        CompilerType v_compilertype = v_typesp->GetFullCompilerType();

        auto* v_rt = static_cast<RustType*>(v_compilertype.GetOpaqueQualType());
        auto* v_aggregate = v_rt->AsAggregate();
        RustCStyleEnumType* v_name_enum =
            static_cast<RustType*>(
                v_aggregate->static_fields.at(0).second.GetOpaqueQualType()
            )
                ->AsCStyleEnum();

        // TODO I rely on this field ordering, but i'm not sure if it's
        // actually guaranteed.

        // the aggregate we have at this point is a struct with:
        // * a `value` of type VariantN
        // * a static field NAME
        // * one or more static fields denoting the discriminant that
        // identifies this variant

        auto& v_underlying_type = v_aggregate->fields.at(0).underlying_type;

        ConstString const_name_symbol = ConstString(
            llvm::formatv("{0}::NAME", v_compilertype.GetTypeName()).str()
        );

        // This is only ever used in for sum types, but it's used like 5 times
        // so DRY
        auto get_value_for_constant = [pdb](llvm::StringRef name) {
          std::optional<uint64_t> value;

          auto& index = pdb->GetIndex();

          auto syms =
              index.globals().findRecordsByName(name, index.symrecords());

          for (auto& s : syms) {
            auto cvs = s.second;

            if (cvs.kind() != SymbolKind::S_CONSTANT) {
              continue;
            }

            auto symbol = llvm::cantFail(
                SymbolDeserializer::deserializeAs<ConstantSym>(cvs)
            );

            value = symbol.Value.getZExtValue();
            break;
          }

          return value;
        };

        auto name_idx =
            get_value_for_constant(const_name_symbol.GetStringRef());

        // the API on this is a bit weird? Basically the symbol stores an
        // address range, but ACTUALLY the address range type should be a union
        // between an address range and the constant value. The following
        // function extracts the value, but only if the symbol isn't an address.
        // It used to take a fail value, but I changed it to return an option
        // because fail values are useless when every u64 value is a potentially
        // valid discriminant. .value_or() replicates the old behavior, and it
        // was only ever called in 1 location (SymbolFileDWARFDebugMap) anyway

        // auto name_idx = name_val->GetAddress().GetOffset();

        auto variant_name = v_name_enum
                                // TODO don't crash if we don't find the value?
                                ->variants.at(name_idx.value());

        // now we try to get the discriminant for this variant. The name can be
        // one of several values:
        // * DISCR_EXACT
        // * DISCR_BEGIN
        // * DISCR_END
        // * DISCR128_EXACT_LO
        // * DISCR128_EXACT_HI
        // * DISCR128_BEGIN_LO
        // * DISCR128_BEGIN_HI
        // * DISCR128_END_LO
        // * DISCR128_END_HI

        if (discr_128 ||
            // for 128 bit discrs, the character will be `_` instead, faster
            // than checking a whole range of characters
            v_aggregate->static_fields.at(1).first.GetStringRef()[5] == '1') {
          /* --------------------- 128 bit discriminant --------------------- */

          discr_128 = true;

          // char at [9] is `B` for discr ranges, otherwise is `E`
          if (v_aggregate->static_fields.at(1).first.GetStringRef()[9] == 'B') {
            /* ---------------------------- range --------------------------- */

            // Discr ranges are equivalent to the untagged variant in DWARF. As
            // far as I'm aware, only 1 variant is allowed to have a range, all
            // the rest must be exact values, therefore we can just not bother
            // getting the actual range begin and end values

            st->untagged_variant = st->variants.size();
            st->variants.push_back(EnumVariant{
                v_underlying_type,
                // to match DWARF output
                ConstString(
                    llvm::formatv("$variant{0}$", name_idx.value()).str()
                )
            });

          } else {
            /* ---------------------------- exact --------------------------- */

            auto discr_hi = get_value_for_constant(
                ConstString(llvm::formatv(
                                "{0}::DISCR128_EXACT_HI",
                                v_compilertype.GetTypeName()
                            )
                                .str())
                    .GetStringRef()
            );

            auto discr_lo = get_value_for_constant(
                ConstString(llvm::formatv(
                                "{0}::DISCR128_EXACT_LO",
                                v_compilertype.GetTypeName()
                            )
                                .str())
                    .GetStringRef()
            );

            std::pair<uint64_t, uint64_t> discr = {
                discr_lo.value(),
                discr_hi.value()
            };

            st->variants.push_back(EnumVariant{
                v_underlying_type, // to match DWARF output
                ConstString(
                    llvm::formatv("$variant{0}$", name_idx.value()).str()
                )
            });

            st->discr_map.insert({discr, st->variants.size() - 1});
          }
        } else {
          /* ---------------------- 64 bit discriminant --------------------- */

          // char at [6] is `B` for discr ranges, otherwise is `E`
          if (v_aggregate->static_fields.at(1).first.GetStringRef()[6] == 'B') {
            /* ---------------------------- range --------------------------- */

            // Discr ranges are equivalent to the untagged variant in DWARF. As
            // far as I'm aware, only 1 variant is allowed to have a range, all
            // the rest must be exact values, therefore we can just not bother
            // getting the actual range begin and end values

            st->untagged_variant = st->variants.size();
            st->variants.push_back(EnumVariant{
                v_underlying_type, // to match DWARF output
                ConstString(
                    llvm::formatv("$variant{0}$", name_idx.value()).str()
                )
            });

          } else {
            /* ---------------------------- exact --------------------------- */

            auto discr_lo = get_value_for_constant(
                ConstString(llvm::formatv(
                                "{0}::DISCR_EXACT",
                                v_compilertype.GetTypeName()
                            )
                                .str())
                    .GetStringRef()
            );

            assert(discr_lo);
            std::pair<uint64_t, uint64_t> discr{discr_lo.value(), 0};

            st->variants.push_back(EnumVariant{v_underlying_type, variant_name}
            );

            st->discr_map.insert({discr, st->variants.size() - 1});
          }
        }
      }
    } break;
    default:
      break;
    }

    if (auto e = field_iter.visitMemberEnd(member)) {
      break;
    }
  }

  rt->m_name = NormalizeMSVCTypeName(rt->m_name.GetStringRef());
  auto compiler_type = CompilerType(weak_from_this(), rt);

  return compiler_type;
}

lldb::TypeSP TypeSystemRust::ParseFunctionTypePDB(
    npdb::PdbTypeSymId type,
    llvm::codeview::ProcedureRecord pr
) {
  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();
  TpiStream& stream = index.tpi();
  CVType args_cvt = stream.getType(pr.getArgumentList());
  ArgListRecord args;
  llvm::cantFail(TypeDeserializer::deserializeAs<ArgListRecord>(args_cvt, args)
  );

  llvm::ArrayRef<TypeIndex> arg_indices = llvm::ArrayRef(args.ArgIndices);

  std::vector<CompilerType> arg_types;
  arg_types.reserve(arg_indices.size());

  for (TypeIndex arg_index : arg_indices) {
    auto arg_type = ParseTypeFromPDB(arg_index);
    if (!arg_type)
      continue;
    arg_types.push_back(arg_type->GetForwardCompilerType());
  }

  auto ret_type = ParseTypeFromPDB(pr.ReturnType);

  if (!ret_type) {
  }

  // TODO leak
  // TODO function name?
  RustType* rt = new RustType{RustType::NewFunction(
      ConstString("func"),
      arg_types,
      {},
      ret_type->GetForwardCompilerType()
  )};

  auto compiler_type = CompilerType(weak_from_this(), rt);

  return GetSymbolFile()->MakeType(
      toOpaqueUid(type),
      rt->m_name,
      rt->m_size,
      nullptr,
      LLDB_INVALID_UID,
      Type::eEncodingIsUID,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );
}

// TODO
lldb::TypeSP TypeSystemRust::ParseTypedefDecl(const npdb::PdbGlobalSymId& symbol
) {
  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();

  CVSymbol cvs = index.ReadSymbolRecord(symbol);
  if (cvs.kind() != S_UDT) {
    return nullptr;
  }

  // taken largely from PdbAstBuilder::GetOrCreateSymbolForId
  UDTSym udt(SymbolRecordKind::UDTSym);
  llvm::cantFail(SymbolDeserializer::deserializeAs(cvs, udt));

  auto inner_type = ParseTypeFromPDB(udt.Type);
  // TODO leak
  auto* rt = new RustType{RustType::NewTypedef(
      ConstString(udt.Name),
      inner_type->GetFullCompilerType()
  )};

  auto compiler_type = CompilerType(weak_from_this(), rt);

  auto type_sp = GetSymbolFile()->MakeType(
      toOpaqueUid(symbol),
      rt->m_name,
      rt->m_size,
      nullptr,
      inner_type->GetID(),
      Type::eEncodingIsTypedefUID,
      Declaration(),
      compiler_type,
      Type::ResolveState::Full
  );
}

// TODO
ConstString
TypeSystemRust::ConstructDemangledNameFromPDB(const npdb::PdbSymUid& symbol) {
  return ConstString();
}

// TODO probably not necessary since SymbolFileNativePDB takes care of this
Function* TypeSystemRust::ParseFunctionFromPDB(npdb::PdbCompilandSymId func_id
) {
  return nullptr;
}

// TODO
bool TypeSystemRust::CompleteTypeFromPDB(
    const npdb::PdbTypeSymId symbol,
    Type* type,
    CompilerType& compiler_type
) {
  return true;
}

// TODO
CompilerDecl TypeSystemRust::GetDeclForUIDFromPDB(const npdb::PdbSymUid& symbol
) {
  // TODO check cache

  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();

  switch (symbol.kind()) {
    // taken largely from PdbAstBuilder::GetOrCreateSymbolForId
  case PdbSymUidKind::CompilandSym: {
    CVSymbol cvs = index.ReadSymbolRecord(symbol.asCompilandSym());
    // if (cvs.kind() == S_REGISTER) {
    //   clang::DeclContext* scope = GetParentDeclContext(id);
    //   if (!scope)
    //     return nullptr;
    //   clang::Decl* scope_decl = clang::Decl::castFromDeclContext(scope);
    //   PdbCompilandSymId scope_id =
    //       PdbSymUid(m_decl_to_status[scope_decl].uid).asCompilandSym();
    //   return GetOrCreateVariableDecl(scope_id, id);
    // }

    switch (cvs.kind()) {
      // local variables
    case S_REGISTER:
    case S_REGREL32:
    case S_LOCAL: {
      CompilerDeclContext scope = GetDeclContextContainingUIDFromPDB(symbol);
      if (!scope) {
        return CompilerDecl();
      }

      RustDecl* decl_context =
          static_cast<RustDecl*>(scope.GetOpaqueDeclContext());

      if (!decl_context) {
        return CompilerDecl();
      }

      CVSymbol cvs = index.ReadSymbolRecord(symbol.asCompilandSym());

      VariableInfo var_info = GetVariableNameInfo(cvs);
      TypeSP var_type = ParseTypeFromPDB(var_info.type);

      auto* var_decl = new RustDecl(
          ConstString(var_info.name),
          ConstString(),
          decl_context,
          VarDecl{var_type->GetFullCompilerType()}
      );

      decl_context->AddItem(var_decl);

      return CompilerDecl(this, var_decl);
    } break;
    // functions
    case S_GPROC32:
    case S_LPROC32: {
      CompilerDeclContext scope = GetDeclContextContainingUIDFromPDB(symbol);
      if (!scope) {
        return CompilerDecl();
      }

      RustDecl* decl_context =
          static_cast<RustDecl*>(scope.GetOpaqueDeclContext());

      auto context_name = decl_context->QualifiedName();

      CVSymbol cvs = index.ReadSymbolRecord(symbol.asCompilandSym());

      ProcSym func(static_cast<SymbolRecordKind>(cvs.kind()));
      llvm::cantFail(SymbolDeserializer::deserializeAs<ProcSym>(cvs, func));

      TypeSP func_type = ParseTypeFromPDB(func.FunctionType);

      if (!func_type) {
        return CompilerDecl();
      }

      llvm::StringRef func_name = func.Name;
      func_name.consume_front(context_name);
      func_name.consume_front("::");

      // there's a bunch of processing in PdbAstBuilder::CreateFunctionDecl
      // that essentially processes member functions. Luckily for us, Rust
      // (afaik) doesn't output member functions at all and we don't bother
      // storing them in RustType, so we can ignore all of that

      auto* new_decl = new RustDecl(
          ConstString(func_name),
          ConstString(),
          decl_context,
          FnDecl{llvm::DenseMap<ConstString, RustDecl*>(), CompilerType()}
          // TODO We can probably find the type?
      );

      decl_context->AddItem(new_decl);
      return CompilerDecl(this, new_decl);
    } break;

    // global variables
    case S_GDATA32:
    case S_LDATA32:
    case S_GTHREAD32:
    case S_CONSTANT:
      return CompilerDecl();
    // block scopes
    case S_BLOCK32:

      // inlined functions
    case S_INLINESITE:
      // TODO

    default:
      return CompilerDecl();
    }

  } break;
  case PdbSymUidKind::Compiland:
  case PdbSymUidKind::PublicSym:
  case PdbSymUidKind::GlobalSym:
  case PdbSymUidKind::Type:
  case PdbSymUidKind::FieldListMember:
    break;
  }
  return CompilerDecl();
}

// TODO from GetOrCreateDeclContextForUid
CompilerDeclContext
TypeSystemRust::GetDeclContextForUIDFromPDB(const npdb::PdbSymUid& symbol) {
  if (symbol.kind() == PdbSymUidKind::CompilandSym) {
    if (symbol.asCompilandSym().offset == 0)
      // TODO this might be wrong
      return CompilerDeclContext(this, m_compile_unit_ctx.get());
  }

  auto decl = GetDeclForUIDFromPDB(symbol);

  if (!decl) {
    return CompilerDeclContext();
  }

  auto* rust_decl = static_cast<RustDecl*>(decl.GetOpaqueDecl());

  if (rust_decl->IsContext()) {
    return CompilerDeclContext(this, rust_decl);
  }

  return CompilerDeclContext();
}

// TODO probably matches GetParentDeclContext?`
CompilerDeclContext
TypeSystemRust::GetDeclContextContainingUIDFromPDB(const npdb::PdbSymUid& symbol
) {
  // Taken directly from PdbAstBuilder::GetParentDeclContext

  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();

  auto kind = symbol.kind();
  switch (kind) {
  case PdbSymUidKind::CompilandSym: {
    std::optional<PdbCompilandSymId> scope =
        pdb->FindSymbolScope(symbol.asCompilandSym());
    if (scope)
      return GetDeclContextForUIDFromPDB(*scope);

    // TODO
    CVSymbol sym = index.ReadSymbolRecord(symbol.asCompilandSym());
    // return CreateDeclInfoForUndecoratedName(getSymbolName(sym)).first;
  } break;
  case PdbSymUidKind::Type: {
    // It could be a namespace, class, or global.  We don't support nested
    // functions yet.  Anyway, we just need to consult the parent type map.
    PdbTypeSymId type_id = symbol.asTypeSym();
    std::optional<TypeIndex> parent_index = pdb->GetParentType(type_id.index);
    if (!parent_index)
      return CompilerDeclContext(this, m_compile_unit_ctx.get());
    return GetDeclContextForUIDFromPDB(PdbTypeSymId(*parent_index));
  } break;
  case PdbSymUidKind::FieldListMember:
    // In this case the parent DeclContext is the one for the class that
    // this member is inside of.
    break;
  case PdbSymUidKind::GlobalSym: {
    // If this refers to a compiland symbol, just recurse in with that
    // symbol. The only other possibilities are S_CONSTANT and S_UDT, in
    // which case we need to parse the undecorated name to figure out the
    // scope, then look that up in the TPI stream.  If it's found, it's a
    // type, othewrise it's a series of namespaces.
    // FIXME: do this.
    CVSymbol global = index.ReadSymbolRecord(symbol.asGlobalSym());
    switch (global.kind()) {
    case SymbolKind::S_GDATA32:
    case SymbolKind::S_LDATA32:
      // TODO
      // return
      // CreateDeclInfoForUndecoratedName(getSymbolName(global)).first;
    case SymbolKind::S_PROCREF:
    case SymbolKind::S_LPROCREF: {
      ProcRefSym ref{global.kind()};
      llvm::cantFail(SymbolDeserializer::deserializeAs<ProcRefSym>(global, ref)
      );
      PdbCompilandSymId cu_sym_id{ref.modi(), ref.SymOffset};
      return GetDeclContextContainingUIDFromPDB(cu_sym_id);
    }
    case SymbolKind::S_CONSTANT:
    case SymbolKind::S_UDT:
    // TODO
    // return CreateDeclInfoForUndecoratedName(getSymbolName(global)).first;
    default:
      break;
    }
  } break;
  default:
    break;
  }

  return CompilerDeclContext(this, m_compile_unit_ctx.get());
}

void TypeSystemRust::EnsureAllSymbolsInDeclContextHaveBeenParsed(
    CompilerDeclContext decl_context
) {
  // Taken from PdbAstBuilder::ParseDeclsForContext
  // return;
  auto* context = static_cast<RustDecl*>(decl_context.GetOpaqueDeclContext());

  if (!context) {
    return;
  }

  if (context->IsCompUnit()) {
    llvm::call_once(m_parse_all_types, [this]() {
      SymbolFileNativePDB* pdb = static_cast<SymbolFileNativePDB*>(
          GetSymbolFile()->GetBackingSymbolFile()
      );
      PdbIndex& index = pdb->GetIndex();
      TypeIndex ti{index.tpi().TypeIndexBegin()};
      for (const CVType& cvt : index.tpi().typeArray()) {
        PdbTypeSymId tid{ti};
        ++ti;

        if (!IsTagRecord(cvt))
          continue;

        ParseTypeFromPDB(tid);
      }
    });
    llvm::call_once(m_parse_functions_and_non_local_vars, [this]() {
      SymbolFileNativePDB* pdb = static_cast<SymbolFileNativePDB*>(
          GetSymbolFile()->GetBackingSymbolFile()
      );
      PdbIndex& index = pdb->GetIndex();
      uint32_t module_count = index.dbi().modules().getModuleCount();
      for (uint16_t modi = 0; modi < module_count; ++modi) {
        CompilandIndexItem& cii = index.compilands().GetOrCreateCompiland(modi);
        const CVSymbolArray& symbols = cii.m_debug_stream.getSymbolArray();
        auto iter = symbols.begin();
        while (iter != symbols.end()) {
          PdbCompilandSymId sym_id{modi, iter.offset()};

          switch (iter->kind()) {
          case S_GPROC32:
          case S_LPROC32:
            CreateFunctionDecl(sym_id);
            iter = symbols.at(getScopeEndOffset(*iter));
            break;
          case S_GDATA32:
          case S_GTHREAD32:
          case S_LDATA32:
          case S_LTHREAD32:
            CreateVariableDecl(PdbCompilandSymId(modi, 0), sym_id);
            ++iter;
            break;
          // default:
          //   ++iter;
          //   continue;
          case S_COMPILE:
          case S_REGISTER_16t:
          case S_CONSTANT_16t:
          case S_UDT_16t:
          case S_SSEARCH:
          case S_SKIP:
          case S_CVRESERVE:
          case S_OBJNAME_ST:
          case S_ENDARG:
          case S_COBOLUDT_16t:
          case S_MANYREG_16t:
          case S_RETURN:
          case S_ENTRYTHIS:
          case S_BPREL16:
          case S_LDATA16:
          case S_GDATA16:
          case S_PUB16:
          case S_LPROC16:
          case S_GPROC16:
          case S_THUNK16:
          case S_BLOCK16:
          case S_WITH16:
          case S_LABEL16:
          case S_CEXMODEL16:
          case S_VFTABLE16:
          case S_REGREL16:
          case S_BPREL32_16t:
          case S_LDATA32_16t:
          case S_GDATA32_16t:
          case S_PUB32_16t:
          case S_LPROC32_16t:
          case S_GPROC32_16t:
          case S_THUNK32_ST:
          case S_BLOCK32_ST:
          case S_WITH32_ST:
          case S_LABEL32_ST:
          case S_CEXMODEL32:
          case S_VFTABLE32_16t:
          case S_REGREL32_16t:
          case S_LTHREAD32_16t:
          case S_GTHREAD32_16t:
          case S_SLINK32:
          case S_LPROCMIPS_16t:
          case S_GPROCMIPS_16t:
          case S_PROCREF_ST:
          case S_DATAREF_ST:
          case S_ALIGN:
          case S_LPROCREF_ST:
          case S_OEM:
          case S_TI16_MAX:
          case S_REGISTER_ST:
          case S_CONSTANT_ST:
          case S_UDT_ST:
          case S_COBOLUDT_ST:
          case S_MANYREG_ST:
          case S_BPREL32_ST:
          case S_LDATA32_ST:
          case S_GDATA32_ST:
          case S_PUB32_ST:
          case S_LPROC32_ST:
          case S_GPROC32_ST:
          case S_VFTABLE32:
          case S_REGREL32_ST:
          case S_LTHREAD32_ST:
          case S_GTHREAD32_ST:
          case S_LPROCMIPS_ST:
          case S_GPROCMIPS_ST:
          case S_COMPILE2_ST:
          case S_MANYREG2_ST:
          case S_LPROCIA64_ST:
          case S_GPROCIA64_ST:
          case S_LOCALSLOT_ST:
          case S_PARAMSLOT_ST:
          case S_GMANPROC_ST:
          case S_LMANPROC_ST:
          case S_RESERVED1:
          case S_RESERVED2:
          case S_RESERVED3:
          case S_RESERVED4:
          case S_LMANDATA_ST:
          case S_GMANDATA_ST:
          case S_MANFRAMEREL_ST:
          case S_MANREGISTER_ST:
          case S_MANSLOT_ST:
          case S_MANMANYREG_ST:
          case S_MANREGREL_ST:
          case S_MANMANYREG2_ST:
          case S_MANTYPREF:
          case S_UNAMESPACE_ST:
          case S_ST_MAX:
          case S_WITH32:
          case S_MANYREG:
          case S_LPROCMIPS:
          case S_GPROCMIPS:
          case S_MANYREG2:
          case S_LPROCIA64:
          case S_GPROCIA64:
          case S_LOCALSLOT:
          case S_PARAMSLOT:
          case S_MANFRAMEREL:
          case S_MANREGISTER:
          case S_MANSLOT:
          case S_MANMANYREG:
          case S_MANREGREL:
          case S_MANMANYREG2:
          case S_DATAREF:
          case S_ANNOTATIONREF:
          case S_TOKENREF:
          case S_GMANPROC:
          case S_LMANPROC:
          case S_ATTR_FRAMEREL:
          case S_ATTR_REGISTER:
          case S_ATTR_REGREL:
          case S_ATTR_MANYREG:
          case S_SEPCODE:
          case S_LOCAL_2005:
          case S_DEFRANGE_2005:
          case S_DEFRANGE2_2005:
          case S_DISCARDED:
          case S_LPROCMIPS_ID:
          case S_GPROCMIPS_ID:
          case S_LPROCIA64_ID:
          case S_GPROCIA64_ID:
          case S_DEFRANGE_HLSL:
          case S_GDATA_HLSL:
          case S_LDATA_HLSL:
          case S_LOCAL_DPC_GROUPSHARED:
          case S_DEFRANGE_DPC_PTR_TAG:
          case S_DPC_SYM_TAG_MAP:
          case S_POGODATA:
          case S_INLINESITE2:
          case S_MOD_TYPEREF:
          case S_REF_MINIPDB:
          case S_PDBMAP:
          case S_GDATA_HLSL32:
          case S_LDATA_HLSL32:
          case S_GDATA_HLSL32_EX:
          case S_LDATA_HLSL32_EX:
          case S_FASTLINK:
          case S_INLINEES:
          case S_END:
          case S_INLINESITE_END:
          case S_PROC_ID_END:
          case S_THUNK32:
          case S_TRAMPOLINE:
          case S_SECTION:
          case S_COFFGROUP:
          case S_EXPORT:
          case S_LPROC32_ID:
          case S_GPROC32_ID:
          case S_LPROC32_DPC:
          case S_LPROC32_DPC_ID:
          case S_REGISTER:
          case S_PUB32:
          case S_PROCREF:
          case S_LPROCREF:
          case S_ENVBLOCK:
          case S_INLINESITE:
          case S_LOCAL:
          case S_DEFRANGE:
          case S_DEFRANGE_SUBFIELD:
          case S_DEFRANGE_REGISTER:
          case S_DEFRANGE_FRAMEPOINTER_REL:
          case S_DEFRANGE_SUBFIELD_REGISTER:
          case S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE:
          case S_DEFRANGE_REGISTER_REL:
          case S_BLOCK32:
          case S_LABEL32:
          case S_OBJNAME:
          case S_COMPILE2:
          case S_COMPILE3:
          case S_FRAMEPROC:
          case S_CALLSITEINFO:
          case S_FILESTATIC:
          case S_HEAPALLOCSITE:
          case S_FRAMECOOKIE:
          case S_ARMSWITCHTABLE:
          case S_CALLEES:
          case S_CALLERS:
          case S_UDT:
          case S_COBOLUDT:
          case S_BUILDINFO:
          case S_BPREL32:
          case S_REGREL32:
          case S_CONSTANT:
          case S_MANCONSTANT:
          case S_LMANDATA:
          case S_GMANDATA:
          case S_UNAMESPACE:
          case S_ANNOTATION:
            break;
          }
        }
      }
    });
    return;
  }

  if (context->IsNamespace()) {
    // TODO
    // ParseNamespace(context);
    return;
  }

  // We don't need to resolve types any further here since we don't do
  // partial type completion.

  if (context->IsFn() || context->IsBlock()) {
    if (auto search = m_decl_ctx_to_uid.find(decl_context);
        search != m_decl_ctx_to_uid.end()) {
      PdbCompilandSymId block_id = PdbSymUid(search->second).asCompilandSym();
      ParseBlockChildren(block_id);
    }

    return;
  }
}

// TODO
std::string
TypeSystemRust::GetPDBClassTemplateParams(const npdb::PdbTypeSymId& type) {
  return {};
}

RustDecl* TypeSystemRust::CreateFunctionDecl(PdbCompilandSymId symbol) {
  if (m_uid_to_decl.contains(toOpaqueUid(symbol))) {
    return m_uid_to_decl[toOpaqueUid(symbol)];
  }

  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();

  CompilerDeclContext scope = GetDeclContextContainingUIDFromPDB(symbol);
  if (!scope) {
    return nullptr;
  }

  RustDecl* decl_context = static_cast<RustDecl*>(scope.GetOpaqueDeclContext());

  auto context_name = decl_context->QualifiedName();

  CVSymbol cvs = index.ReadSymbolRecord(symbol);

  ProcSym func(static_cast<SymbolRecordKind>(cvs.kind()));
  llvm::cantFail(SymbolDeserializer::deserializeAs<ProcSym>(cvs, func));

  TypeSP func_type = ParseTypeFromPDB(func.FunctionType);

  if (!func_type) {
    return nullptr;
  }

  llvm::StringRef func_name = func.Name;
  func_name.consume_front(context_name);
  func_name.consume_front("::");

  // there's a bunch of processing in PdbAstBuilder::CreateFunctionDecl that
  // essentially processes member functions. Luckily for us, Rust (afaik)
  // doesn't output member functions at all and we don't bother storing them
  // in RustType, so we can ignore all of that

  auto* new_decl = new RustDecl(
      ConstString(func_name),
      ConstString(),
      decl_context,
      FnDecl{llvm::DenseMap<ConstString, RustDecl*>(), CompilerType()}
      // TODO We can probably find the type?
  );

  m_uid_to_decl[toOpaqueUid(symbol)] = new_decl;
  decl_context->AddItem(new_decl);
  return new_decl;
}

RustDecl* TypeSystemRust::CreateVariableDecl(
    PdbCompilandSymId scope_id,
    PdbCompilandSymId var_id
) {
  if (m_uid_to_decl.contains(toOpaqueUid(var_id))) {
    return m_uid_to_decl[toOpaqueUid(var_id)];
  }

  CompilerDeclContext scope = GetDeclContextForUIDFromPDB(scope_id);
  if (!scope)
    return nullptr;

  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );
  PdbIndex& index = pdb->GetIndex();
  CVSymbol sym = index.ReadSymbolRecord(var_id);

  VariableInfo var_info = GetVariableNameInfo(sym);

  TypeSP var_type = ParseTypeFromPDB(var_info.type);
  if (!var_type) {
    return nullptr;
  }

  // TODO leak
  RustDecl* var_decl = new RustDecl(
      ConstString(var_info.name),
      ConstString(),
      static_cast<RustDecl*>(scope.GetOpaqueDeclContext()),
      {VarDecl{var_type->GetFullCompilerType()}}
  );

  m_uid_to_decl[toOpaqueUid(var_id)] = var_decl;

  return var_decl;
}

void TypeSystemRust::ParseBlockChildren(PdbCompilandSymId block_id) {
  SymbolFileNativePDB* pdb =
      static_cast<SymbolFileNativePDB*>(GetSymbolFile()->GetBackingSymbolFile()
      );

  PdbIndex& index = pdb->GetIndex();
  CVSymbol sym = index.ReadSymbolRecord(block_id);
  lldbassert(
      sym.kind() == S_GPROC32 || sym.kind() == S_LPROC32 ||
      sym.kind() == S_BLOCK32 || sym.kind() == S_INLINESITE
  );
  CompilandIndexItem& cii =
      index.compilands().GetOrCreateCompiland(block_id.modi);
  CVSymbolArray symbols =
      cii.m_debug_stream.getSymbolArrayForScope(block_id.offset);

  // Function parameters should already have been created when the function
  // was parsed.
  if (sym.kind() == S_GPROC32 || sym.kind() == S_LPROC32) {
    auto* decl = m_uid_to_decl[toOpaqueUid(block_id)];
    if (auto* fn_decl = decl->AsFn()) {
      auto params = fn_decl->type.GetFunctionArgumentCount();
      if (params != 0) {
        while (!symbols.empty()) {
          if (params == 0) {
            break;
          }

          CVSymbol sym = *symbols.begin();
          symbols.drop_front();

          switch (sym.kind()) {
          case S_REGISTER:
          case S_REGREL32:
          case S_LOCAL:
            continue;
          default:
            --params;
          }
        }
      }
    }
  }

  symbols.drop_front();
  auto begin = symbols.begin();
  while (begin != symbols.end()) {
    PdbCompilandSymId child_sym_id(block_id.modi, begin.offset());
    GetDeclForUIDFromPDB(child_sym_id);
    if (begin->kind() == S_BLOCK32 || begin->kind() == S_INLINESITE) {
      ParseBlockChildren(child_sym_id);
      begin = symbols.at(getScopeEndOffset(*begin));
    }
    ++begin;
  }
}

enum class NameKind {
  Bare,
  Enum,
  Ref,
  RefMut,
  PtrConst,
  PtrMut,
  Tuple,
  Slice,
  Never,
  Pat,
  Dyn,
  Assoc,
  RecursiveType,
  Impl,
  VTable,
  VTableType,
};

ConstString TypeSystemRust::NormalizeMSVCTypeName(llvm::StringRef name) {
  if (name.empty()) {
    return ConstString();
  }

  if (msvc_normalization_cache.contains(name)) {
    return msvc_normalization_cache[name];
  }

  auto original = name.substr(0);

  NameKind kind = NameKind::Bare;
  if (name.starts_with("enum2$<")) {
    // we skip processing sum-type's nested types (e.g.
    // enum2$<sample::Number>::NAMES) because it makes finding them in the
    // symbol table annoying
    if (!name.ends_with('>')) {
      return ConstString(name);
    }
    kind = NameKind::Enum;
    name = name.substr(7);
    name.consume_back(">");
  } else if (name.starts_with("ref$<")) {
    kind = NameKind::Ref;
    name = name.substr(5);
    name.consume_back(">");
  } else if (name.starts_with("ref_mut$<")) {
    kind = NameKind::RefMut;
    name = name.substr(9);
    name.consume_back(">");
  } else if (name.starts_with("ptr_const$<")) {
    kind = NameKind::PtrConst;
    name = name.substr(11);
    name.consume_back(">");
  } else if (name.starts_with("ptr_mut$<")) {
    kind = NameKind::PtrMut;
    name = name.substr(9);
    name.consume_back(">");
  } else if (name.starts_with("tuple$<")) {
    kind = NameKind::Tuple;
    // intentionally leave the `<` so that we can process the generics the same
    // as the other types
    name = name.substr(6);
  } else if (name.starts_with("slice2$<")) {
    kind = NameKind::Slice;
    name = name.substr(8);
    name.consume_back(">");
  }

  name = name.trim();

  if (kind != NameKind::Bare) {
    name = NormalizeMSVCTypeName(name);
  }

  auto [base_name, args] = GetTemplateArgs(name);

  std::string normalized = "";

  switch (kind) {
  case NameKind::Ref:
    normalized.push_back('&');
    break;
  case NameKind::RefMut:
    normalized.append("&mut ");
    break;
  case NameKind::PtrConst:
    normalized.append("*const ");
    break;
  case NameKind::PtrMut:
    normalized.append("*mut ");
    break;
  case NameKind::Slice:
    normalized.append("[");
    break;
  default:
    break;
  }

  normalized.append(base_name.trim());

  if (!args.empty() || kind == NameKind::Tuple) {
    switch (kind) {
    case NameKind::Tuple:
      normalized.push_back('(');
      break;
    default:
      normalized.push_back('<');
      break;
    }

    for (auto arg : args) {
      normalized.append(NormalizeMSVCTypeName(arg).GetStringRef());
      normalized.push_back(',');
    }
    // remove trailing ','
    if (!args.empty()) {
      normalized.pop_back();
    }

    switch (kind) {
    case NameKind::Tuple:
      normalized.push_back(')');
      break;
    case NameKind::Slice:
      normalized.push_back(']');
      break;
    default:
      normalized.push_back('>');
      break;
    }
  }

  switch (kind) {
  case NameKind::Slice:
    normalized.push_back(']');
    break;
  default:
    break;
  }

  if (!normalized.empty() && normalized.back() == '$') {
    normalized.pop_back();
  }

  auto result = ConstString(normalized);

  msvc_normalization_cache[original] = result;

  return result;
}

} // namespace lldb_private