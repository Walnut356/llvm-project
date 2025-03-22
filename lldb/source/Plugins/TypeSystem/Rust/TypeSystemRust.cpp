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
#include "lldb/Core/PluginManager.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Core/dwarf.h"
#include "lldb/Expression/DWARFExpression.h"
#include "lldb/Symbol/CompileUnit.h"
#include "lldb/Symbol/CompilerType.h"
#include "lldb/Utility/StreamString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-private-enumerations.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/FormatVariadic.h"
#include <cstdint>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string>

using namespace lldb;

using namespace llvm::dwarf;

using namespace lldb_private::plugin::dwarf;

LLDB_PLUGIN_DEFINE(TypeSystemRust)

namespace lldb_private {
// TODO maybe these don't work?
const ConstString UNIT_TYPE_NAME{"()"};
const ConstString I8_NAME{"i8"};
const ConstString U8_NAME{"u8"};
const ConstString I16_NAME{"i16"};
const ConstString U16_NAME{"u16"};
const ConstString I32_NAME{"i32"};
const ConstString U32_NAME{"u32"};
const ConstString I64_NAME{"i64"};
const ConstString U64_NAME{"u64"};
const ConstString I128_NAME{"i128"};
const ConstString U128_NAME{"u128"};

const ConstString F16_NAME{"f16"};
const ConstString F32_NAME{"f32"};
const ConstString F64_NAME{"f64"};
const ConstString BOOL_NAME{"bool"};
const ConstString CHAR_NAME{"char"};

// enum class IndirectionKind {
//   /// `*const T`
//   ConstPointer,
//   /// `*mut T`
//   MutPointer,
//   /// `&T`
//   Reference,
//   /// `&mut T`
//   MutReference,
// };
// // enum class Access { Private, Pub };
// enum class AggregateKind {
//   Struct,
//   Tuple,
//   Union
// };

// /// A single variant, for use within RustCStyleEnum
// struct Enumerator {
//   ConstString name;
//   /// May need to be casted to a signed type
//   uint64_t value;
// };

// /// Corresponds to the Rust `bool`
// struct RustBool {};
// /// Represents u8/u16/u32/u64/u128/usize
// struct RustUInt {};
// /// Represents i8/i16/i32/i64/i128/isize
// struct RustInt {};
// /// Represents f32/f64
// struct RustFloat {};
// /// Represents the Rust `char` type (i.e. char32_t)
// struct RustChar {};
// /// Represents typedefs, contains the underlying type
// struct RustTypedef {
//   CompilerType underlying_type;
// };
// /// Represents &/&mut and *const/*mut. Contains the pointed-to type.
// struct RustIndirection {
//   CompilerType pointee_type;
//   IndirectionKind kind;

//   bool IsReference() { return kind >= IndirectionKind::Reference; }
// };
// /// Represents Structs, Tuples, and Unions
// struct RustAggregate {
//   std::vector<FieldAttributes> fields;
//   std::vector<CompilerType> template_args;
//   uint64_t align;
//   AggregateKind kind;
// };
// // struct RustStruct {};
// // struct RustTuple {};
// // struct RustUnion {};

// /// Represents a single variant of a sum-type enum
// struct RustVariant {
//   CompilerType underlying_type;
//   std::optional<uint64_t> discr_value;
// };

// /// Represents non-C-style enums

// struct RustType {
// public:
//   /// std::variant has an `index` function which returns the 0-indexed value
//   /// that corresponds to the type of the variant. This allows us to switch
//   on
//   /// the variant type (similar to a match) rather than a thousand if-elif
//   /// checks. That should result in both cleaner code and better codegen
//   enum Kind {
//     Bool,
//     UInt,
//     Int,
//     Float,
//     Char,
//     Typedef,
//     Indirection,
//     Aggregate,
//     SumType,
//     CStyleEmum,
//     Array,
//     Function,
//   };
//   // RustType(const ConstString &name, RustTypeKind);

//   static RustType NewBool(const ConstString& name) {
//     return RustType{name, 1, RustBool()};
//   }

//   static RustType NewChar(const ConstString& name) {
//     return RustType{name, 4, RustChar()};
//   }
//   static RustType NewUInt(const ConstString& name, uint64_t size) {
//     return RustType{name, size, RustUInt{}};
//   }

//   static RustType NewInt(const ConstString& name, uint64_t size) {
//     return RustType{name, size, RustInt{}};
//   }

//   static RustType NewFloat(const ConstString& name, uint64_t size) {
//     return RustType{name, size, RustFloat{}};
//   }

//   static RustType
//   NewTypedef(const ConstString& name, const CompilerType& underlying_type) {
//     return RustType{
//         name,
//         underlying_type.GetByteSize(nullptr).value_or(0),
//         RustTypedef{underlying_type}
//     };
//   }

//   static RustType NewIndirection(
//       const ConstString& name,
//       uint64_t byte_size,
//       const CompilerType& underlying_type,
//       Tag tag
//   ) {
//     // eventually the compiler should start outputting const qualifiers for
//     // refs and pointers, but until then we can do some ugly string checks.

//     IndirectionKind kind;
//     if (tag == DW_TAG_reference_type || name.GetStringRef().starts_with('&'))
//     {
//       bool is_const = !(name.GetStringRef().starts_with("&mut "));

//       kind =
//           is_const ? IndirectionKind::Reference :
//           IndirectionKind::MutReference;
//     } else {
//       bool is_const = name.GetStringRef().starts_with("*const ");
//       kind = is_const ? IndirectionKind::ConstPointer
//                       : IndirectionKind::MutPointer;
//     }
//     return RustType{name, byte_size, RustIndirection{underlying_type, kind}};
//   }

//   static RustType NewAggregate(
//       const ConstString& name,
//       uint64_t size,
//       uint64_t align,
//       AggregateKind kind
//   ) {
//     return RustType{name, size, RustAggregate{{}, {}, align, kind}};
//   }

//   static RustType NewSumType(
//       const ConstString& name,
//       uint64_t size,
//       std::vector<CompilerType> variants,
//       CompilerType discr_type
//   ) {
//     return RustType{
//         name,
//         size,
//         RustSumType{
//             variants,
//             llvm::DenseMap<uint64_t, uint64_t>(),
//             discr_type,
//             {}
//         }
//     };
//   }

//   static RustType
//   NewCStyleEnum(const ConstString& name, CompilerType& underlying_type) {
//     return RustType{
//         name,
//         underlying_type.GetByteSize(nullptr).value_or(0),
//         RustCStyleEnumType{
//             llvm::DenseMap<uint64_t, std::string>(),
//             underlying_type,
//         }
//     };
//   }

//   static RustType
//   NewArray(const ConstString& name, CompilerType element_type, uint64_t len)
//   {
//     return RustType{
//         name,
//         element_type.GetByteSize(nullptr).value_or(0) * len,
//         RustArrayType{element_type, len}
//     };
//   }

//   static RustType NewFunction(
//       const ConstString& name,
//       std::vector<CompilerType> args,
//       std::vector<CompilerType> template_args,
//       CompilerType return_type
//   ) {
//     return RustType{
//         name,
//         0, // should this be pointer sized?
//         RustFunctionType{args, template_args, return_type}
//     };
//   }

//   Kind VariantKind() { return static_cast<Kind>(m_variant.index()); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustBool* AsBool() { return std::get_if<RustBool>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustUInt* AsUInt() { return std::get_if<RustUInt>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustInt* AsInt() { return std::get_if<RustInt>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustFloat* AsFloat() { return std::get_if<RustFloat>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustChar* AsChar() { return std::get_if<RustChar>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustTypedef* AsTypedef() { return std::get_if<RustTypedef>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustIndirection* AsIndirection() {
//     return std::get_if<RustIndirection>(&m_variant);
//   }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustAggregate* AsAggregate() {
//     return std::get_if<RustAggregate>(&m_variant);
//   }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustSumType* AsSumType() { return std::get_if<RustSumType>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustCStyleEnumType* AsCStyleEnum() {
//     return std::get_if<RustCStyleEnumType>(&m_variant);
//   }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustArrayType* AsArray() { return std::get_if<RustArrayType>(&m_variant); }

//   /// Returns a null pointer if this RustType does not contain the requested
//   /// variant
//   RustFunctionType* AsFunction() {
//     return std::get_if<RustFunctionType>(&m_variant);
//   }

//   bool IsBool() { return std::holds_alternative<RustBool>(m_variant); }

//   bool IsUInt() { return std::holds_alternative<RustUInt>(m_variant); }

//   bool IsInt() { return std::holds_alternative<RustInt>(m_variant); }

//   bool IsFloat() { return std::holds_alternative<RustFloat>(m_variant); }

//   bool IsChar() { return std::holds_alternative<RustChar>(m_variant); }

//   bool IsTypedef() { return std::holds_alternative<RustTypedef>(m_variant); }

//   bool IsIndirection() {
//     return std::holds_alternative<RustIndirection>(m_variant);
//   }

//   bool IsAggregate() {
//     return std::holds_alternative<RustAggregate>(m_variant);
//   }

//   bool IsSumType() { return std::holds_alternative<RustSumType>(m_variant); }

//   bool IsCStyleEnum() {
//     return std::holds_alternative<RustCStyleEnumType>(m_variant);
//   }

//   bool IsArray() { return std::holds_alternative<RustArrayType>(m_variant); }

//   bool IsFunction() {
//     return std::holds_alternative<RustFunctionType>(m_variant);
//   }

//   ConstString m_name;
//   /// in bytes
//   uint64_t m_size;

//   std::variant<
//       RustBool,
//       RustUInt,
//       RustInt,
//       RustFloat,
//       RustChar,
//       RustTypedef,
//       RustIndirection,
//       RustAggregate,
//       RustSumType,
//       RustCStyleEnumType,
//       RustArrayType,
//       RustFunctionType>
//       m_variant;
// };

void RustDeclContext::AddItem(std::shared_ptr<RustDeclBase>&& item) {
  ConstString name = item->Name();
  child_decls[name] = std::move(item);
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
      //   astc = std::shared_ptr<TypeSystemRustForExpr>(
      //       new TypeSystemRustForExpr(target->shared_from_this()));
    }

    if (arch.IsValid()) {
      astc->m_pointer_byte_size = arch.GetAddressByteSize();
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
  if (die.Tag() == DW_TAG_namespace) {
    printf("namespace");
  }
  //   PrintDeclContexts();
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
    printf("parsed type: %s\n", type_sp->GetName().AsCString());
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
    rt = new RustType{
        RustType::NewAggregate(type_name, 0, 1, AggregateKind::Tuple)
    };
  } else {
    switch (static_cast<TypeKind>(encoding)) {
    case DW_ATE_boolean:
      rt = new RustType{RustType::NewBool(type_name)};
      break;
    case DW_ATE_float:
      rt = new RustType{RustType::NewFloat(type_name, byte_size)};
      break;
    case DW_ATE_signed:
      rt = new RustType{RustType::NewInt(type_name, byte_size)};
      break;
    case DW_ATE_unsigned:
      rt = new RustType{RustType::NewUInt(type_name, byte_size)};
      break;
    case DW_ATE_unsigned_char:
      rt = new RustType{RustType::NewUInt(type_name, byte_size)};
      break;
    case DW_ATE_UTF:
      rt = new RustType{RustType::NewChar(type_name)};
      break;
    default:
      // nothing else should make it here
      assert(0);
      break;
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

    std::string name = "";
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
        name = form_value.AsCString();
        break;
      case DW_AT_const_value:
        value = form_value.Unsigned();
        saw_value = true;
        break;
      default:
        break;
      }
    }

    if (saw_value && !name.empty()) {
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
    AggregateKind agg_kind = is_union   ? AggregateKind::Union
                             : is_tuple ? AggregateKind::Tuple
                                        : AggregateKind::Struct;

    RustType* rt = new RustType{
        RustType::NewAggregate(type_name, byte_size, byte_align, agg_kind)
    };

    auto* inner = rt->AsAggregate();

    ParseStructFields(die, inner->fields, inner->template_args, is_tuple);

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

FieldAttributes TypeSystemRust::ParseFieldAttributes(const DWARFDIE& die) {
  DWARFAttributes attrs = die.GetAttributes(DWARFBaseDIE::Recurse::yes);
  size_t size = attrs.Size();

  FieldAttributes field;

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
      attrs.ExtractFormValueAtIndex(i, field.encoding);
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

  return field;
}

void TypeSystemRust::ParseStructFields(
    const DWARFDIE& die,
    std::vector<FieldAttributes>& fields,
    std::vector<CompilerType>& template_args,
    bool is_tuple
) {
  for (auto& child : die.children()) {
    dw_tag_t tag = child.Tag();
    // TODO rust doesn't output template_value_parameter, but probably should
    if (tag == DW_TAG_template_type_parameter) {
      // TODO could this be replaced with child.ResolveType()?
      auto encoding_ref = child.GetAttributeValueAsReferenceDIE(DW_AT_type);
      Type* templ = child.ResolveTypeUID(encoding_ref);

      template_args.push_back(templ->GetLayoutCompilerType());

    } else if (tag == DW_TAG_member) {
      auto field = ParseFieldAttributes(child);

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
          child.GetDWARF()->ResolveTypeUID(field.encoding.Reference().GetID());
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
  // That means this outter look must handle the `variant_part` and
  // `structure_type` tags from a "macro" level
  //
  // Importantly, even if one of the enum members is carrying no extra data,
  // all variant types are output as DW_TAG_structure_type
  //
  // If the enum has a generic arg, it is not output for the top level
  // DW_TAG_structure_type, but *is* output for each of the variant types.

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
      ConstString name =
          ConstString(llvm::formatv("$variant${0}", variant_idx).str());

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
      sum_type->discr_map[discr_value.value()] = i;
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

  auto* containing_decl_ctx = static_cast<RustDeclContext*>(
      GetDeclContextForUIDFromDWARF(die).GetOpaqueDeclContext()
  );

  if (!containing_decl_ctx) {
    containing_decl_ctx = m_compile_unit_ctx.get();
  }
  auto* func_decl = new RustDeclBase{
      RustDecl{type_name, mangled, containing_decl_ctx, compiler_type}
  };
  containing_decl_ctx->AddItem(std::unique_ptr<RustDeclBase>(func_decl));

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
  printf("DIE: %s, Parent: %s\n", die.GetName(), parent.GetName());

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

  RustDeclContext* dc = (RustDeclContext*)parent.GetOpaqueDeclContext();
  RustDeclBase* base = dc->FindByName(name);
  if (base) {
    if (RustDecl* ctx = base->AsDecl()) {
      return CompilerDecl(this, ctx);
    }
  }

  auto* new_ns = new RustDeclBase{RustDecl(name, mangled, dc, CompilerType())};
  dc->AddItem(std::unique_ptr<RustDeclBase>(new_ns));

  return CompilerDecl(this, new_ns);
}

CompilerDecl
TypeSystemRust::GetDeclForUIDFromDWARF(const plugin::dwarf::DWARFDIE& die) {
  if (m_decls.contains(die.GetDIE())) {
    return m_decls[die.GetDIE()];
  }

  CompilerDecl result;
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

  auto declkind = RustDeclContext::Namespace;

  CompilerDeclContext result;
  switch (die.Tag()) {
  case DW_TAG_compile_unit:
    if (!m_compile_unit_ctx) {
      m_compile_unit_ctx.reset(new RustDeclContext{
          llvm::DenseMap<ConstString, std::shared_ptr<RustDeclBase>>(),
          ConstString(""),
          ConstString(),
          nullptr,
          RustDeclContext::CompileUnit
      });
    }

    result = CompilerDeclContext(this, m_compile_unit_ctx.get());
    break;
  case DW_TAG_union_type:
  case DW_TAG_structure_type:
    declkind = RustDeclContext::Struct;
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

    auto* dc = static_cast<RustDeclContext*>(parent.GetOpaqueDeclContext());

    if (dc && dc->child_decls.contains(name)) {
      auto* decl = dc->child_decls[name].get();
      if (RustDeclContext* ctx = decl->AsDeclContext()) {
        result = CompilerDeclContext(this, ctx);
      }
    }

    // auto new_ctx = RustDeclContext(name, dc, declkind);

    auto* new_ns = new RustDeclBase{RustDeclContext{
        llvm::DenseMap<ConstString, std::shared_ptr<RustDeclBase>>(),
        name,
        ConstString(),
        dc,
        declkind
    }};

    dc->AddItem(std::shared_ptr<RustDeclBase>(new_ns));
    result = CompilerDeclContext(this, new_ns);

  } break;
  case DW_TAG_lexical_block:
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

    auto* dc = static_cast<RustDeclContext*>(parent.GetOpaqueDeclContext());

    // we should be able to uniquely identify lexical blocks by their address
    // range
    auto range_begin = range_list.GetEntryAtIndex(0)->base;
    auto range_end = range_list.GetEntryAtIndex(0)->base;

    auto name =
        ConstString(llvm::formatv("{0}..{1}", range_begin, range_end).str());
    if (dc && dc->child_decls.contains(name)) {
      auto* decl = dc->child_decls[name].get();
      if (RustDeclContext* ctx = decl->AsDeclContext()) {
        result = CompilerDeclContext(this, ctx);
      }
    }

    auto* new_ns = new RustDeclBase{RustDeclContext{
        llvm::DenseMap<ConstString, std::shared_ptr<RustDeclBase>>(),
        name,
        ConstString(),
        dc,
        declkind
    }};

    dc->AddItem(std::shared_ptr<RustDeclBase>(new_ns));
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

  auto* rd = static_cast<RustDeclBase*>(opaque_decl)->AsDecl();

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
  RustDeclBase* dc = static_cast<RustDeclBase*>(opaque_decl);
  return dc->Name();
}

ConstString TypeSystemRust::DeclGetMangledName(void* opaque_decl) {
  if (!opaque_decl) {
    return ConstString();
  }

  RustDeclBase* dc = static_cast<RustDeclBase*>(opaque_decl);
  return dc->AsDecl()->mangled;
}

CompilerDeclContext TypeSystemRust::DeclGetDeclContext(void* opaque_decl) {
  if (!opaque_decl) {
    return CompilerDeclContext();
  }
  RustDeclBase* dc = static_cast<RustDeclBase*>(opaque_decl);
  return CompilerDeclContext(this, dc->Context());
}

CompilerType TypeSystemRust::DeclGetFunctionReturnType(void* opaque_decl) {
  if (!opaque_decl) {
    return CompilerType();
  }

  auto* rd = static_cast<RustDeclBase*>(opaque_decl)->AsDecl();

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

  auto* rd = static_cast<RustDeclBase*>(opaque_decl)->AsDecl();

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
  // TODO ?
  return Scalar();
}

CompilerType TypeSystemRust::GetTypeForDecl(void* opaque_decl) {
  if (!opaque_decl) {
    return CompilerType();
  }

  auto* rd = static_cast<RustDeclBase*>(opaque_decl)->AsDecl();

  if (!rd) {
    return CompilerType();
  }

  return rd->type;
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

    auto* dc = static_cast<RustDeclBase*>(opaque_decl_ctx)->AsDeclContext();
    RustDeclBase* base = dc->FindByName(name);
    if (RustDecl* decl = base ? base->AsDecl() : nullptr) {
      result.push_back(CompilerDecl(this, decl));
    }
  }
  return result;
}

ConstString TypeSystemRust::DeclContextGetName(void* opaque_decl_ctx) {
  if (!opaque_decl_ctx) {
    return ConstString();
  }

  auto* dc = static_cast<RustDeclBase*>(opaque_decl_ctx);

  return dc->Name();
}

ConstString
TypeSystemRust::DeclContextGetScopeQualifiedName(void* opaque_decl_ctx) {
  if (!opaque_decl_ctx) {
    return ConstString();
  }

  auto* dc = static_cast<RustDeclBase*>(opaque_decl_ctx);

  if (!dc->IsDeclContext()) {
    return ConstString();
  }

  return dc->AsDeclContext()->QualifiedName();
}

bool TypeSystemRust::DeclContextIsContainedInLookup(
    void* opaque_decl_ctx,
    void* other_opaque_decl_ctx
) {
  auto* decl_ctx = static_cast<RustDeclBase*>(opaque_decl_ctx)->AsDeclContext();
  auto* other =
      static_cast<RustDeclBase*>(other_opaque_decl_ctx)->AsDeclContext();

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

  auto compiler_type = CompilerType(weak_from_this(), type);
  ConstString type_name =
      ConstString(llvm::formatv("&mut {0}", compiler_type.GetTypeName()).str());

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
           RustAggregate{{}, {}, 1, AggregateKind::Tuple}
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
              llvm::APSInt(llvm::APInt(rt->m_size * 8, v.first, is_signed))
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
  // TODO only really need to handle discr
  return CompilerDecl();
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
      return t->pointee_type;
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
    std::vector<CompilerType>& args = rt->AsAggregate()->template_args;
    if (idx < args.size()) {
      return args[idx];
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
  switch (encoding) {
  case eEncodingUint:
    // TODO add scoped_name_to_type map,
  case eEncodingSint:
  case eEncodingIEEE754:
  case eEncodingVector:
    break;
  }
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
      s.Printf("%s::%s", rt->m_name.AsCString(), et->variants[discr].c_str());
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
      format == eFormatUnicode32 && rt->IsChar()) {
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
    auto* rd =
        static_cast<RustDeclContext*>(decl.getSecond().GetOpaqueDeclContext());

    printf(
        "name: %s\nfullname:%s",
        rd->name.AsCString(),
        rd->full_name.AsCString()
    );

    printf("children {\n");
    for (auto& child : rd->child_decls) {
      printf("\t%s,\n", child.getSecond()->Name().AsCString());
    }

    printf("}\n\n");
  }
}

} // namespace lldb_private