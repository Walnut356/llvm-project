//===-- TypeSystemRust.h --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

/*
    This is a Rust implementation of the TypeSystem interface. There's a lot of
moving parts and most of LLDB/LLVM's parts are undocumented. To mitigate that,
I've commented this code *heavily* to hopefully make it easier to
extend/contribute to.
    For prior art, please see:
        * TypeSystemClang
        * Apple's TypeSystemSwift
(https://github.com/swiftlang/llvm-project/tree/next/lldb/source/Plugins/TypeSystem/Swift)
        * vadimcn's TypeSystemRust
(https://archive.softwareheritage.org/browse/origin/directory/?branch=refs/heads/codelldb/16.x&origin_url=https://github.com/vadimcn/llvm-project&path=lldb/source/Plugins/TypeSystem/Rust&timestamp=2023-09-11T04:55:10Z)

TypeSystems rely on DWARF/PDB parsers to provide them with type information.
They transform that information into a custom struct (RustType in our case),
which the TypeSystem then interacts with to power its interface. That interface
is what's used by the public API (SBValue, SBType, etc.) to provide facilities
for pretty-printing and type/value inspection.

All 3 of the TypeSystem implementations above use the "has-a" rather than "is-a"
relationship with DWARFASTParser. The LLDB docs recommend subclassing. At time
of writing, I have somewhere in the realm of 20 vscode tabs and half a dozen
browser tabs to keep track of all this nonsense. As such, I've subclassed to
keep things within 1 file if possible. There are big comment blocks to denote
the beginning of DWARF/PDB parsing code.

---
Important LLVM Types
---

ConstString - a handle to an interned string. ConstStrings exist for the
lifetime of the program once created (but can still be created dynamically at
run-time) and compare for equality via pointer. Has mechanisms to convert to and
from C Strings



---
DWARFASTParser Notes
---

Most important struct is DWARFDIE (DWARF Debug Info Entry - yes there's also a
class called DWARFDebugInfoEntry that seemingly does something different, no I
don't know why). It gives very low level access to the dwarf type information.
If you'd like to see some of that information yourself, please check out
godbolt: https://godbolt.org/z/c99Mrs44c as well as the DWARF v5 standard:
https://dwarfstd.org/doc/DWARF5.pdf

I personaly found dwex (python-based GUI dwarf explorer) to be very helpful.

Tags and attributes are available under the namespace llvm::dwarf

Most of Rust's debug info generation takes place here:
https://github.com/rust-lang/rust/blob/master/compiler/rustc_codegen_llvm/src/debuginfo/metadata.rs

When parsing attributes, even though the underlying value is accessible, be sure
to access via the DWARFFormValue as there's extra processing done to get the
correct output that isn't necessarily obvious.


Type:

TypeSystemRust::ParseTypeFromDWARF is the primary access point, and parses the
raw DWARF into RustType objects. RustType is as close to a Rust enum as I could
get it, and contains the specific versions Rust types such as RustBool or
RustFloat. Those RustTypes are then wrapped in a CompilerType - an opaque
wrapper to allow custom types to be handled by the bookkeeping parts of LLDB
while still having bespoke handling within the TypeSystem. In Rust terms,
CompilerType is literally just an `(Arc<TypeSystem>, Box<RustType>)`. It has a
similar interface to TypeSystem, but internally just calls TypeSystem's
functions and passes in the RustType.

CompilerType is then passed to SymbolFileDWARF::MakeType, which seemingly
updates some bookkeeping values and returns a TypeSP (i.e.
Arc<lldb::Type>). lldb::Type is what underpins the SBType
interface in the public API.

Decl/DeclContext:

Decls are declarations of variables, structs, functions, etc.

DeclContexts are, in essense, scopes. They can be namespaces (i.e. crates) or
struct impl blocks or lexical blocks that contain variable definitions

Neither are used very extensively within the `TypeSystem` itself, but it appears
they're sued pretty heavily by other parts of the stack to keep track of things
like which variables to populate when printing a frame, scope qualifying certain
type names, etc.

---
PDBASTParser Notes
---
PDBASTParser looks pretty confusing to say the least, so I'm looking into ways
to prefer the native PDB parser, or create a unified abstraction layer over the
top of both. Having to implement 3 parsers for 1 type system seems a bit silly.

---
NativePDBASTParser Notes
---
`PdbSymUid` seems to be the `DWARFDIE` equivalent. It can be converted to/from
more specific types:

PdbSymCompilandId
PdbCompileandSymId
PdbGlobalSymId
PdbTypeSymId
PdbFieldListMemberId

SymbolFileNativePDB::GetOrCreateType appears to store types by `PdbSymUid`, so
`MakeType()` should use this value


---
TypeSystem Notes
---

TypeSystems don't technically do anything. They're like a manually-implemented
v-table for RustType. Most of the raw processing is done via DWARFASTParser,
that relevant information is trivially interpreted by the TypeSystem (Which
could be implemented on the types themselves if this was architected
differently), most of the "prettifying" happens via the `Language` plugin, and
expressions are handled by the `Expression` plugin. Technically the `TypeSystem`
"unifies" all those plugins, but I'd hardly say that justifies the lead billing
it gets.



*/

#ifndef liblldb_TypeSystemRust_h_
#define liblldb_TypeSystemRust_h_

#include "Plugins/ExpressionParser/Rust/RustUserExpression.h"
#include "Plugins/SymbolFile/DWARF/DWARFASTParser.h"
#include "Plugins/SymbolFile/DWARF/DWARFDebugInfoEntry.h"
#include "Plugins/SymbolFile/DWARF/DWARFFormValue.h"
#include "Plugins/SymbolFile/NativePDB/PdbAstBuilder.h"

#include "Plugins/SymbolFile/NativePDB/PdbSymUid.h"
#include "lldb/Core/DumpDataExtractor.h"
#include "lldb/Expression/UtilityFunction.h"
#include "lldb/Symbol/CompilerType.h"
#include "lldb/Symbol/TypeSystem.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-types.h"
#include "llvm/ADT/APInt.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/DebugInfo/CodeView/TypeIndex.h"
#include "llvm/DebugInfo/CodeView/TypeRecord.h"
#include <array>
#include <memory>
#include <optional>
#include <unordered_map>
#include <vector>

namespace lldb_private {

const ConstString UNIT_TYPE_NAME{"()"};
const ConstString UNIT_TYPE_NAME_MSVC{"tuple$<>"};
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
const ConstString ISIZE_NAME{"isize"};
const ConstString USIZE_NAME{"usize"};

const ConstString F16_NAME{"f16"};
const ConstString F32_NAME{"f32"};
const ConstString F64_NAME{"f64"};
const ConstString F128_NAME{"f128"};
const ConstString BOOL_NAME{"bool"};
const ConstString CHAR_NAME{"char"};

// used for std::visit
template <class... Ts> struct overload : Ts... {
  using Ts::operator()...;
};

template <class... Ts> overload(Ts...) -> overload<Ts...>;

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
//                                 Rust Decls                                 //
// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// Declaration of a Rust symbol.
struct RustDecl;

struct UnknownDecl {};

struct CompUnitDecl {
  llvm::DenseMap<ConstString, RustDecl*> children;
};

struct NamespaceDecl {
  llvm::DenseMap<ConstString, RustDecl*> children;
};

struct FnDecl {
  llvm::DenseMap<ConstString, RustDecl*> children;
  CompilerType type;
};

struct BlockDecl {
  llvm::DenseMap<ConstString, RustDecl*> children;
};

struct VarDecl {
  CompilerType type;
};

struct ValDecl {
  Scalar value;
};

/// Includes typedefs
struct TypeDecl {
  CompilerType type;
};

struct RustDecl {
  ConstString name;
  ConstString mangled;
  ConstString full_name;
  RustDecl* parent;

  typedef std::variant<
      UnknownDecl,
      CompUnitDecl,
      NamespaceDecl,
      FnDecl,
      BlockDecl,
      VarDecl,
      ValDecl,
      TypeDecl>
      DeclInner;

  DeclInner variant;

  enum Kind {
    Unknown,
    CompUnit,
    Namespace,
    Fn,
    Block,
    Var,
    Val,
    Type,
  };

  RustDecl(
      const ConstString& name,
      const ConstString& mangled,
      RustDecl* parent,
      DeclInner variant
  )
      : name(name), mangled(mangled), parent(parent), variant(variant) {}

  CompUnitDecl* AsCompUnit() { return std::get_if<CompUnitDecl>(&variant); }
  NamespaceDecl* AsNamespace() { return std::get_if<NamespaceDecl>(&variant); }
  FnDecl* AsFn() { return std::get_if<FnDecl>(&variant); }
  BlockDecl* AsBlock() { return std::get_if<BlockDecl>(&variant); }
  VarDecl* AsVar() { return std::get_if<VarDecl>(&variant); }
  ValDecl* AsVal() { return std::get_if<ValDecl>(&variant); }
  TypeDecl* AsType() { return std::get_if<TypeDecl>(&variant); }

  bool IsCompUnit() { return std::holds_alternative<CompUnitDecl>(variant); }
  bool IsNamespace() { return std::holds_alternative<NamespaceDecl>(variant); }
  bool IsFn() { return std::holds_alternative<FnDecl>(variant); }
  bool IsBlock() { return std::holds_alternative<BlockDecl>(variant); }
  bool IsVar() { return std::holds_alternative<VarDecl>(variant); }
  bool IsVal() { return std::holds_alternative<ValDecl>(variant); }
  bool IsType() { return std::holds_alternative<TypeDecl>(variant); }

  bool IsContext() {
    switch (variant.index()) {
    case CompUnit:
    case Namespace:
    case Fn:
    case Block:
      return true;
    default:
      return false;
    }
  }

  llvm::DenseMap<ConstString, RustDecl*>* GetChildren() {
    // jfc C++, get your shit together
    const auto options = overload{
        [](UnknownDecl& v) {
          return static_cast<llvm::DenseMap<ConstString, RustDecl*>*>(nullptr);
        },
        [](CompUnitDecl& v) { return &v.children; },
        [](NamespaceDecl& v) { return &v.children; },
        [](FnDecl& v) { return &v.children; },
        [](BlockDecl& v) { return &v.children; },
        [](VarDecl& v) {
          return static_cast<llvm::DenseMap<ConstString, RustDecl*>*>(nullptr);
        },
        [](ValDecl& v) {
          return static_cast<llvm::DenseMap<ConstString, RustDecl*>*>(nullptr);
        },
        [](TypeDecl& v) {
          return static_cast<llvm::DenseMap<ConstString, RustDecl*>*>(nullptr);
        },
    };

    return std::visit(options, variant);
  }

  CompilerType GetType() {
    const auto options = overload{
        [](UnknownDecl& v) { return CompilerType(); },
        [](CompUnitDecl& v) { return CompilerType(); },
        [](NamespaceDecl& v) { return CompilerType(); },
        [](FnDecl& v) { return v.type; },
        [](BlockDecl& v) { return CompilerType(); },
        [](VarDecl& v) { return v.type; },
        [](ValDecl& v) {
          return CompilerType();
          // TODO
          // switch (v.value.GetType()) {

          // case Scalar::e_void:
          // return CompilerType();
          // case Scalar::e_int:

          // case Scalar::e_float:
          //   break;
          // }
        },
        [](TypeDecl& v) { return v.type; },
    };

    return std::visit(options, variant);
  }

  void AddItem(RustDecl* decl) {
    if (auto* children = GetChildren()) {
      auto name = decl->name;
      (*children)[name] = decl;
    }
  }

  RustDecl* FindByName(const ConstString& name) {
    if (auto* children = GetChildren()) {
      if (children->contains(name)) {
        return (*children)[name];
      }
    }

    return nullptr;
  }

  ConstString QualifiedName() {
    if (!parent) {
      return name;
    }
    if (!full_name) {
      ConstString basename = parent->QualifiedName();
      if (basename) {
        std::string qual =
            std::string(basename.AsCString()) + "::" + name.AsCString();
        full_name = ConstString(qual.c_str());
      } else {
        full_name = name;
      }
    }
    return full_name;
  }
};

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
//                                  RustTypes                                 //
// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

/// Contains the relevant dwarf attribute tags for `DW_TAG_base_type` DIE
/// parsing
struct BasicAttributes {
  lldb::user_id_t encoding_uid = LLDB_INVALID_UID;
  uint64_t byte_size = 0;
  llvm::dwarf::TypeKind encoding;
  ConstString type_name;
};

/// Contains the relevant dwarf attribute tags for `DW_TAG_member_type` DIE
/// parsing
struct FieldAttributes {
  uint64_t byte_align;
  uint64_t byte_offset;
  /// The name of the field itself
  ConstString name;
  CompilerType underlying_type;
  lldb::AccessType access = lldb::AccessType::eAccessPrivate;
  bool artificial = false;
};

/// mut/immutable reference/pointer. Pointer values will compare as less-than
/// reference values, which can be used to easily test if you have a pointer or
/// a reference (e.g. if `indir_kind < Reference` you have a pointer)
enum class IndirectionKind {
  /// `*const T`
  ConstPointer,
  /// `*mut T`
  MutPointer,
  /// `&T`
  Reference,
  /// `&mut T`
  MutReference,
};

/// Used for RustAggregateType, differentiates between Structs, Tuples, and
/// Unions.
enum class AggregateKind {
  Struct,
  Tuple,
  Union,
  TupleStruct
};

/// A single variant, for use within RustCStyleEnum
struct Enumerator {
  ConstString name;
  /// May need to be casted to a signed type
  uint64_t value;
};

/// Corresponds to the Rust `bool`
struct RustBool {};
/// Represents u8/u16/u32/u64/u128/usize
struct RustUInt {};
/// Represents i8/i16/i32/i64/i128/isize
struct RustInt {};
/// Represents f32/f64
struct RustFloat {};
/// Represents the Rust `char` type (i.e. char32_t)
struct RustChar {};
/// Represents typedefs, contains the underlying type
struct RustTypedef {
  CompilerType underlying_type;
};
/// Represents &/&mut and *const/*mut. Contains the pointed-to type.
struct RustIndirection {
  CompilerType pointee_type;
  IndirectionKind kind;

  bool IsReference() { return kind >= IndirectionKind::Reference; }
  bool IsPointer() { return kind < IndirectionKind::Reference; }
};
/// Represents Structs, Tuples, and Unions
struct RustAggregate {
  std::vector<FieldAttributes> fields;
  // SAFETY: the StringRefs are derived directly from the ConstString type name
  // of either the compiler type, or the containing type, thus are safe to hold
  std::vector<std::pair<llvm::StringRef, std::optional<CompilerType>>>
      template_args;
  std::vector<std::pair<ConstString, CompilerType>> static_fields;
  uint64_t align;
  AggregateKind kind;
};

struct EnumVariant {
  CompilerType underlying_type;
  ConstString name;
};

/// u128.0 is the low bits, u128.1 is the high bits
/// used for RustSumType discriminant values, since types such as
/// `NonZero<u128>` can have a 128-bit discriminant. C/C++ seemingly don't have
/// a uint128_t so it is what it is.
///
/// I deliberately avoid using llvm::APInt because all bytes after the first u64
/// are allocated, which is a giant waste when we know exactly the size of int
/// we want.
///
/// I use std::pair instead of a bespoke u128 struct because it'll work with
/// llvm::densemap without me doing any extra work.
typedef std::pair<uint64_t, uint64_t> u128;

// TODO make this compatible with existing rust synthetic providers

/// Represents a Rust Enum where at least 1 variant carries data e.g.:
///
/// ```rust
/// enum Eef {
///   Freef(u8),
///   Sbubby,
/// }
/// ```
///
/// If all variants are "bare" (like `Sbubby` in the above example),
/// RustCStyleEnum is used instead
struct RustSumType {
  std::vector<EnumVariant> variants;
  /// For a given discr, returns the index of the variant it corresponds to.
  /// We have to use llvm::APInt here because types like `NonZero<u128>` have
  /// 128-bit
  llvm::DenseMap<std::pair<uint64_t, uint64_t>, uint64_t> discr_map;
  CompilerType discr_type;
  /// The index of the variant that can take a range of discr values, if there
  /// is one
  std::optional<uint64_t> untagged_variant;

  CompilerType GetVariant(uint64_t discr) {
    return GetVariant(std::make_pair(discr, 0));
  }

  CompilerType GetVariant(std::pair<uint64_t, uint64_t> discr) {
    if (discr_map.contains(discr)) {
      return variants[discr_map[discr]].underlying_type;
    }
    if (untagged_variant.has_value()) {
      return variants[untagged_variant.value()].underlying_type;
    }

    return CompilerType();
  }
};

/// Represents a C-Style enum. For sum types, see RustSumType
struct RustCStyleEnumType {
  llvm::DenseMap<std::optional<uint64_t>, ConstString> variants;
  CompilerType underlying_type;
};

/// Represents a raw, sized array. Slices and dynamic arrays are structs and
/// thus are handled via RustAggregateType
struct RustArrayType {
  CompilerType element_type;
  uint64_t len;
};

struct RustFunctionType {
  std::vector<CompilerType> args;
  std::vector<CompilerType> template_args;
  CompilerType return_type;
};

// /// TODO make this compatible with existing rust synthetic providers
// struct RustSumType {
//   std::vector<CompilerType> variants;
//   /// For a given discr, returns the index of the variant it corresponds to.
//   llvm::DenseMap<uint64_t, uint64_t> discr_map;
//   CompilerType discr_type;
//   std::optional<uint64_t> untagged_variant;

//   CompilerType GetVariant(uint64_t discr) {
//     if (discr_map.contains(discr)) {
//       return variants[discr_map[discr]];
//     }
//     if (untagged_variant.has_value()) {
//       return variants[untagged_variant.value()];
//     }

//     return CompilerType();
//   }
// };

// /// Represents a C-Style enum. For sum types, see RustSumType
// struct RustCStyleEnumType {
//   llvm::DenseMap<uint64_t, std::string> variants;
//   CompilerType underlying_type;
// };

// struct RustArrayType {
//   CompilerType element_type;
//   uint64_t len;
// };

// struct RustFunctionType {
//   std::vector<CompilerType> args;
//   std::vector<CompilerType> template_args;
//   CompilerType return_type;
// };

/// In rust terms, this struct can be thought of as:
///
/// ```rust
///   struct RustType {
///     m_name: String,
///     m_variant: Kind,
///   }
///
///   enum Kind {
///     Bool{...},
///     UInt{...},
///     Int{...},
///     Float{...},
///     Char{...},
///     Typedef{...},
///     Indirection{...},
///     Aggregate{...},
///     CStyleEmum{...},
///     SumType{...},
///     Array{...},
///   }
///
/// ```
///
/// All NewX functions are trivial to keep as much of the processing as
/// possible as close as possible to the dwarf parsing.
struct RustType {
public:
  /// std::variant has an `index` function which returns the 0-indexed value
  /// that corresponds to the type of the variant. This allows us to switch on
  /// the variant type (similar to a match) rather than a thousand if-elif
  /// checks. That should result in both cleaner code and better codegen
  enum Kind {
    Bool,
    UInt,
    Int,
    Float,
    Char,
    Typedef,
    Indirection,
    Aggregate,
    SumType,
    CStyleEmum,
    Array,
    Function,
  };
  // RustType(const ConstString &name, RustTypeKind);

  static RustType NewBool(const ConstString& name) {
    return RustType{name, 1, RustBool()};
  }

  static RustType NewChar(const ConstString& name) {
    return RustType{name, 4, RustChar()};
  }
  static RustType NewUInt(const ConstString& name, uint64_t size) {
    return RustType{name, size, RustUInt{}};
  }

  static RustType NewInt(const ConstString& name, uint64_t size) {
    return RustType{name, size, RustInt{}};
  }

  static RustType NewFloat(const ConstString& name, uint64_t size) {
    return RustType{name, size, RustFloat{}};
  }

  static RustType
  NewTypedef(const ConstString& name, const CompilerType& underlying_type) {
    return RustType{
        name,
        underlying_type.GetByteSize(nullptr).value_or(0),
        RustTypedef{underlying_type}
    };
  }

  static RustType NewIndirection(
      const ConstString& name,
      uint64_t byte_size,
      const CompilerType& underlying_type,
      llvm::dwarf::Tag tag
  ) {
    // eventually the compiler should start outputting const qualifiers for
    // refs and pointers, but until then we can do some ugly string checks.

    IndirectionKind kind;
    if (tag == llvm::dwarf::DW_TAG_reference_type ||
        name.GetStringRef().starts_with('&')) {
      bool is_const = !(name.GetStringRef().starts_with("&mut "));

      kind =
          is_const ? IndirectionKind::Reference : IndirectionKind::MutReference;
    } else {
      bool is_const = name.GetStringRef().starts_with("*const ");
      kind = is_const ? IndirectionKind::ConstPointer
                      : IndirectionKind::MutPointer;
    }
    return RustType{name, byte_size, RustIndirection{underlying_type, kind}};
  }

  static RustType NewAggregate(
      const ConstString& name,
      uint64_t size,
      uint64_t align,
      AggregateKind kind
  ) {
    return RustType{name, size, RustAggregate{{}, {}, {}, align, kind}};
  }

  static RustType NewSumType(
      const ConstString& name,
      uint64_t size,
      std::vector<EnumVariant> variants,
      CompilerType discr_type
  ) {
    return RustType{
        name,
        size,
        RustSumType{
            variants,
            llvm::DenseMap<std::pair<uint64_t, uint64_t>, uint64_t>(),
            discr_type,
            {}
        }
    };
  }

  static RustType
  NewCStyleEnum(const ConstString& name, CompilerType& underlying_type) {
    return RustType{
        name,
        underlying_type.GetByteSize(nullptr).value_or(0),
        RustCStyleEnumType{
            llvm::DenseMap<std::optional<uint64_t>, ConstString>(),
            underlying_type,
        }
    };
  }

  static RustType
  NewArray(const ConstString& name, CompilerType element_type, uint64_t len) {
    return RustType{
        name,
        element_type.GetByteSize(nullptr).value_or(0) * len,
        RustArrayType{element_type, len}
    };
  }

  static RustType NewFunction(
      const ConstString& name,
      std::vector<CompilerType> args,
      std::vector<CompilerType> template_args,
      CompilerType return_type
  ) {
    return RustType{
        name,
        0, // TODO should this be pointer sized?
        RustFunctionType{args, template_args, return_type}
    };
  }

  Kind VariantKind() { return static_cast<Kind>(m_variant.index()); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustBool* AsBool() { return std::get_if<RustBool>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustUInt* AsUInt() { return std::get_if<RustUInt>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustInt* AsInt() { return std::get_if<RustInt>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustFloat* AsFloat() { return std::get_if<RustFloat>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustChar* AsChar() { return std::get_if<RustChar>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustTypedef* AsTypedef() { return std::get_if<RustTypedef>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustIndirection* AsIndirection() {
    return std::get_if<RustIndirection>(&m_variant);
  }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustAggregate* AsAggregate() {
    return std::get_if<RustAggregate>(&m_variant);
  }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustSumType* AsSumType() { return std::get_if<RustSumType>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustCStyleEnumType* AsCStyleEnum() {
    return std::get_if<RustCStyleEnumType>(&m_variant);
  }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustArrayType* AsArray() { return std::get_if<RustArrayType>(&m_variant); }

  /// Returns a null pointer if this RustType does not contain the requested
  /// variant
  RustFunctionType* AsFunction() {
    return std::get_if<RustFunctionType>(&m_variant);
  }

  bool IsBool() { return std::holds_alternative<RustBool>(m_variant); }

  bool IsUInt() { return std::holds_alternative<RustUInt>(m_variant); }

  bool IsInt() { return std::holds_alternative<RustInt>(m_variant); }

  bool IsFloat() { return std::holds_alternative<RustFloat>(m_variant); }

  bool IsChar() { return std::holds_alternative<RustChar>(m_variant); }

  bool IsTypedef() { return std::holds_alternative<RustTypedef>(m_variant); }

  bool IsIndirection() {
    return std::holds_alternative<RustIndirection>(m_variant);
  }

  bool IsAggregate() {
    return std::holds_alternative<RustAggregate>(m_variant);
  }

  bool IsSumType() { return std::holds_alternative<RustSumType>(m_variant); }

  bool IsCStyleEnum() {
    return std::holds_alternative<RustCStyleEnumType>(m_variant);
  }

  bool IsArray() { return std::holds_alternative<RustArrayType>(m_variant); }

  bool IsFunction() {
    return std::holds_alternative<RustFunctionType>(m_variant);
  }

  ConstString m_name;
  /// in bytes
  uint64_t m_size;

  std::variant<
      RustBool,
      RustUInt,
      RustInt,
      RustFloat,
      RustChar,
      RustTypedef,
      RustIndirection,
      RustAggregate,
      RustSumType,
      RustCStyleEnumType,
      RustArrayType,
      RustFunctionType>
      m_variant;
};

/// Contains unique_ptr RustTypes for the built-in numeric types, bool, and char
class RustPrimitives {
  /// in order: i8, i16, i32, i64, i128
  std::unique_ptr<RustType> int_types[5]{
      std::make_unique<RustType>(RustType{I8_NAME, 1, RustInt{}}),
      std::make_unique<RustType>(RustType{I16_NAME, 2, RustInt{}}),
      std::make_unique<RustType>(RustType{I32_NAME, 4, RustInt{}}),
      std::make_unique<RustType>(RustType{I64_NAME, 8, RustInt{}}),
      std::make_unique<RustType>(RustType{I128_NAME, 16, RustInt{}}),
  };
  std::unique_ptr<RustType> isize_type =
      std::make_unique<RustType>(RustType{ISIZE_NAME, 8, RustInt{}});
  /// in order: u8, u16, u32, u64, u128
  std::unique_ptr<RustType> uint_types[5]{
      std::make_unique<RustType>(RustType{U8_NAME, 1, RustUInt{}}),
      std::make_unique<RustType>(RustType{U16_NAME, 2, RustUInt{}}),
      std::make_unique<RustType>(RustType{U32_NAME, 4, RustUInt{}}),
      std::make_unique<RustType>(RustType{U64_NAME, 8, RustUInt{}}),
      std::make_unique<RustType>(RustType{U128_NAME, 16, RustUInt{}}),
  };
  std::unique_ptr<RustType> usize_type =
      std::make_unique<RustType>(RustType{USIZE_NAME, 8, RustUInt{}});
  /// in order: f16, f32, f64, f128
  std::unique_ptr<RustType> float_types[4]{
      std::make_unique<RustType>(RustType{F16_NAME, 2, RustFloat{}}),
      std::make_unique<RustType>(RustType{F32_NAME, 4, RustFloat{}}),
      std::make_unique<RustType>(RustType{F64_NAME, 8, RustFloat{}}),
      std::make_unique<RustType>(RustType{F128_NAME, 16, RustFloat{}}),
  };
  std::unique_ptr<RustType> bool_type =
      std::make_unique<RustType>(RustType{BOOL_NAME, 1, RustBool{}});
  std::unique_ptr<RustType> char_type =
      std::make_unique<RustType>(RustType{CHAR_NAME, 4, RustChar{}});
  std::unique_ptr<RustType> unit_type = std::make_unique<RustType>(RustType{
      RustType::NewAggregate(UNIT_TYPE_NAME, 0, 1, AggregateKind::Tuple)
  });

public:
  void SetPointerByteSize(uint64_t size) {
    isize_type->m_size = size;
    usize_type->m_size = size;
  }

  RustType* i8() { return int_types[0].get(); }
  RustType* i16() { return int_types[1].get(); }
  RustType* i32() { return int_types[2].get(); }
  RustType* i64() { return int_types[3].get(); }
  RustType* i128() { return int_types[4].get(); }
  RustType* isize() { return isize_type.get(); }
  RustType* u8() { return uint_types[0].get(); }
  RustType* u16() { return uint_types[1].get(); }
  RustType* u32() { return uint_types[2].get(); }
  RustType* u64() { return uint_types[3].get(); }
  RustType* u128() { return uint_types[4].get(); }
  RustType* usize() { return usize_type.get(); }
  RustType* f16() { return float_types[0].get(); }
  RustType* f32() { return float_types[1].get(); }
  RustType* f64() { return float_types[2].get(); }
  RustType* f128() { return float_types[3].get(); }
  RustType* Bool() { return bool_type.get(); }
  RustType* Char() { return char_type.get(); }
  RustType* unit() { return unit_type.get(); }
};

const ConstString SUM_TYPE_DISCR_NAME = ConstString{"tag"};

// TODO hey wait, doesn't every single `new RustType` leak? How long does
// `TypeSystem` live?

// -------------------------------------------------------------------------- //
//                                                                            //
//                                                                            //
//                                                                            //
//                               TypeSystemRust                               //
//                                                                            //
//                                                                            //
//                                                                            //
// -------------------------------------------------------------------------- //

/// Primary hub for Rust language support. Doesn't "do much" by itself, but
/// inherits the parsers for the various debug file formats.
class TypeSystemRust : public TypeSystem,
                       public plugin::dwarf::DWARFASTParser,
                       public npdb::NativePDBASTParser {
public:
  // ------------------------------------------------------------------------ //
  //                         Constructors/Destructors                         //
  // ------------------------------------------------------------------------ //
  TypeSystemRust();
  ~TypeSystemRust() override;

  /// Called just before destruction
  void Finalize() override;

  // LLVM RTTI (Run Time Type Information) support
  static char ID;
  bool isA(const void* ClassID) const override { return ClassID == &ID; };
  static bool classof(const TypeSystem* ts) { return ts->isA(&ID); }

  // ------------------------------------------------------------------------ //
  //                              PluginInterface                             //
  // ------------------------------------------------------------------------ //

  /// Main entrypoint. Can take a `Module` or a `Target`, with `Targets` having
  /// the additional expectation of Expression support. Expression support can
  /// be kept separate from the base `TypeSystem`
  static lldb::TypeSystemSP
  CreateInstance(lldb::LanguageType language, Module* module, Target* target);

  /// Passes the metadata and creation callback to the LLDB `PluginManager`
  static void Initialize();

  /// Unregisters the plugin with the LLDB `PluginManager`
  static void Terminate();

  /// Returns "rust"
  llvm::StringRef GetPluginName() override { return GetPluginNameStatic(); }

  /// Returns "rust"
  static llvm::StringRef GetPluginNameStatic() { return "rust"; }

  // ------------------------------------------------------------------------ //
  //                           TypeSystem Interface                           //
  // ------------------------------------------------------------------------ //

  /// This function is used to determine whether or not this type system should
  /// be used for a given target. Make sure that TypeSystemClang's version of
  /// this function does not have eLanguageTypeRust (as it currently does on
  /// stable), or else TypeSystemClang will always take priority over this
  /// TypeSystem
  bool SupportsLanguage(lldb::LanguageType language) override {
    return language == lldb::LanguageType::eLanguageTypeRust;
  };

  /// This function is used to determine whether or not this type system should
  /// be used for a given target. Make sure that TypeSystemClang's version of
  /// this function does not have eLanguageTypeRust (as it currently does on
  /// stable), or else TypeSystemClang will always take priority over this
  /// TypeSystem
  static bool SupportsLanguageStatic(lldb::LanguageType language) {
    return language == lldb::LanguageType::eLanguageTypeRust;
  };

  // ------------------------------ AST Parsers ----------------------------- //

  /// Other type system implementations keep a separate ASTParser, but this
  /// implementation has it as a parent class, so it just returns itself.
  plugin::dwarf::DWARFASTParser* GetDWARFParser() override { return this; }

  /// Other type system implementations keep a separate ASTParser, but this
  /// implementation has it as a parent class, so it just returns itself.
  PDBASTParser* GetPDBParser() override { return nullptr; }

  npdb::NativePDBASTParser* GetNativePDBParser() override { return this; }

  // ------------------------------ Symbol File ----------------------------- //

  SymbolFile* GetSymbolFile() const override { return m_sym_file; }

  void SetSymbolFile(SymbolFile* sym_file) override { m_sym_file = sym_file; }

  // ------------------------ CompilerDecl Functions ------------------------ //

  CompilerType
  DeclGetFunctionArgumentType(void* opaque_decl, size_t arg_idx) override;

  ConstString DeclGetName(void* opaque_decl) override;

  ConstString DeclGetMangledName(void* opaque_decl) override;

  CompilerDeclContext DeclGetDeclContext(void* opaque_decl) override;

  /// Returns an invalid CompilerType if the decl is not a RustFunctionDecl
  CompilerType DeclGetFunctionReturnType(void* opaque_decl) override;

  /// Returns 0 if the decl is not a RustFunctionDecl
  size_t DeclGetFunctionNumArguments(void* opaque_decl) override;

  std::vector<lldb_private::CompilerContext>
  DeclGetCompilerContext(void* opaque_decl) override;

  Scalar DeclGetConstantValue(void* opaque_decl) override;

  CompilerType GetTypeForDecl(void* opaque_decl) override;

  // --------------------- CompilerDeclContext functions -------------------- //
  // TODO all of these
  std::vector<CompilerDecl> DeclContextFindDeclByName(
      void* opaque_decl_ctx,
      ConstString name,
      const bool ignore_imported_decls
  ) override;

  ConstString DeclContextGetName(void* opaque_decl_ctx) override;

  ConstString DeclContextGetScopeQualifiedName(void* opaque_decl_ctx) override;

  // TODO
  bool DeclContextIsClassMethod(void* opaque_decl_ctx) override {
    return false;
  };

  bool DeclContextIsContainedInLookup(
      void* opaque_decl_ctx,
      void* other_opaque_decl_ctx
  ) override;

  lldb::LanguageType DeclContextGetLanguage(void* opaque_decl_ctx) override {
    return lldb::eLanguageTypeRust;
  };

  // TODO
  CompilerDeclContext GetCompilerDeclContextForType(const CompilerType& type
  ) override {
    return {};
  };

  // TODO
  std::vector<lldb_private::CompilerContext>
  DeclContextGetCompilerContext(void* opaque_decl_ctx) override {
    return {};
  };

  // --------------------------------- Tests -------------------------------- //
#ifndef NDEBUG
  /// Verify the integrity of the type to catch CompilerTypes that mix
  /// and match invalid TypeSystem/Opaque type pairs.
  bool Verify(lldb::opaque_compiler_type_t type) override { return true; };
#endif

  // ---------------------------- Type Properties --------------------------- //

  /// Returns `true` if the type is a `RustArrayType`. The additional parameters
  /// are populated if possible. `is_incomplete` is always set to false.
  bool IsArrayType(
      lldb::opaque_compiler_type_t type,
      CompilerType* element_type,
      uint64_t* size,
      bool* is_incomplete
  ) override;

  /// Returns `true` if the type is a `RustAggregate` OR a `RustSumType`
  bool IsAggregateType(lldb::opaque_compiler_type_t type) override;

  /// Returns `true` if the type is a plain tuple (not a tuple struct or tuple
  /// enum variant)
  bool IsAnonymousType(lldb::opaque_compiler_type_t type) override;

  /// Returns `true` if the type is `RustChar` (does not include `u8`)
  bool IsCharType(lldb::opaque_compiler_type_t type) override;

  /// Returns `true` if the type is not `nullptr` since Rust doesn't have
  /// incomplete types afaik
  bool IsCompleteType(lldb::opaque_compiler_type_t type) override;

  /// Returns `true` if the type is not `nullptr` since Rust doesn't have
  /// separate definitions and declarations.
  bool IsDefined(lldb::opaque_compiler_type_t type) override;

  /// Returns true if the type is `RustFloat`
  bool IsFloatingPointType(
      lldb::opaque_compiler_type_t type,
      uint32_t& count,
      bool& is_complex
  ) override;

  /// Returns true if the type is `RustFunctionType`
  bool IsFunctionType(lldb::opaque_compiler_type_t type) override;

  /// Returns 0 if this isn't a function. This is supposed to be used for
  /// function prototypes
  size_t GetNumberOfFunctionArguments(lldb::opaque_compiler_type_t type
  ) override;

  /// Returns an invalid `CompilerType` if the type isn't a function type, or if
  /// the index is out of range. This is supposed to be used for function
  /// prototypes
  CompilerType GetFunctionArgumentAtIndex(
      lldb::opaque_compiler_type_t type,
      const size_t index
  ) override;

  /// Returns true if the passed in type is a `RustIndirection` whose underlying
  /// type is a `RustFunctionType`
  bool IsFunctionPointerType(lldb::opaque_compiler_type_t type) override;

  /// TODO always returns false - maybe check declcontexts to see if it's part
  /// of a struct?
  bool IsMemberFunctionPointerType(lldb::opaque_compiler_type_t type) override;

  /// TODO always returns false - is this used for closures?
  bool IsBlockPointerType(
      lldb::opaque_compiler_type_t type,
      CompilerType* function_pointer_type_ptr
  ) override;

  /// Returns true if the type is `RustUInt` or `RustInt`
  bool
  IsIntegerType(lldb::opaque_compiler_type_t type, bool& is_signed) override;

  /// Returns true if the type is `RustCStyleEnum` or `RustSumType`. is_signed
  /// is set based on the underlying `repr` of the type
  bool IsEnumerationType(lldb::opaque_compiler_type_t type, bool& is_signed)
      override;

  /// All enums in rust are scoped enums, so this returns the same as
  /// `IsEnumerationType`
  bool IsScopedEnumerationType(lldb::opaque_compiler_type_t type) override;

  /// TODO always returns false - used for `dyn`?
  bool IsPossibleDynamicType(
      lldb::opaque_compiler_type_t type,
      CompilerType* target_type, // Can pass NULL
      bool check_cplusplus,
      bool check_objc
  ) override;

  /// Returns true if the type is a `RustIndirection` of kind `ConstPointer` or
  /// `MutPointer`
  bool IsPointerType(
      lldb::opaque_compiler_type_t type,
      CompilerType* pointee_type
  ) override;

  /// Returns true if the type is a `RustIndirection` of kind `Reference` or
  /// `MutReference`
  bool IsReferenceType(
      lldb::opaque_compiler_type_t type,
      CompilerType* pointee_type,
      bool* is_rvalue
  ) override;

  /// Returns true if the type is a `RustIndirection`
  bool IsPointerOrReferenceType(
      lldb::opaque_compiler_type_t type,
      CompilerType* pointee_type
  ) override;

  /// Returns true if the type is NOT a `RustAggregate`
  bool IsScalarType(lldb::opaque_compiler_type_t type) override;

  /// Returns true if the type is `RustBool`
  bool IsBooleanType(lldb::opaque_compiler_type_t type);

  /// Returns true if the type is Rust's unit type: `()`
  bool IsVoidType(lldb::opaque_compiler_type_t type) override;

  /// Always returns true - rust doesn't have the exception for types with
  /// "non-trivial" constructors
  bool CanPassInRegisters(const CompilerType& type) override;

  /// Slightly poorly worded name. Per TypeSystemClang, "does this type have
  /// template args?". In Rust terms, is this a generic type (e.g. Vec<T>)?
  bool IsTemplateType(lldb::opaque_compiler_type_t type) override;

  /// Always returns false. This is used by swift.
  bool IsRuntimeGeneratedType(lldb::opaque_compiler_type_t type) override {
    return false;
  }

  /// TODO Always returns false
  bool IsBeingDefined(lldb::opaque_compiler_type_t type) override;

  // TODO always returns false - pointers should eventually have const
  // information
  bool IsConst(lldb::opaque_compiler_type_t type) override;

  /// Returns true if the type is `RustTypedef`
  bool IsTypedefType(lldb::opaque_compiler_type_t type) override;

  /// TODO always returns false - not sure if this is important
  bool IsVectorType(
      lldb::opaque_compiler_type_t type,
      CompilerType* element_type,
      uint64_t* size
  ) override;

  // Per TypeSystem:
  // "Type systems can have types that are placeholder types, which are meant to
  // indicate the presence of a type, but offer no actual information about
  // said types, and leave the burden of actually figuring type information out
  // to dynamic type resolution. For instance a language with a generics
  // system, can use placeholder types to indicate "type argument goes here",
  // without promising uniqueness of the placeholder, nor attaching any
  // actually idenfiable information to said placeholder. This API allows type
  // systems to tell LLDB when such a type has been encountered In response,
  // the debugger can react by not using this type as a cache entry in any
  // type-specific way For instance, LLDB will currently not cache any
  // formatters that are discovered on such a type as attributable to the
  // meaningless type itself, instead preferring to use the dynamic type"
  bool IsMeaninglessWithoutDynamicResolution(void* type) override {
    return false;
  }

  // TODO always returns 0
  // per old TypeSystemRust:
  // "FIXME should detect "homogeneous floating-point aggregates"."
  uint32_t IsHomogeneousAggregate(
      lldb::opaque_compiler_type_t type,
      CompilerType* base_type_ptr
  ) override;

  /// TODO always returns false - maybe `dyn`?
  bool IsPolymorphicClass(lldb::opaque_compiler_type_t type) override;

  /// Returns an invalid `CompilerType` if the type isn't a function or if the
  /// index is out of range
  CompilerType GetFunctionArgumentTypeAtIndex(
      lldb::opaque_compiler_type_t type,
      size_t idx
  ) override;

  /// Returns -1 if this isn't a function of if the function doesn't have a
  /// prototype. Returns a value >= 0 if there is a prototype.
  int GetFunctionArgumentCount(lldb::opaque_compiler_type_t type) override;

  /// Returns an invalid `CompilerType` if the type isn't a function
  CompilerType GetFunctionReturnType(lldb::opaque_compiler_type_t type
  ) override;

  // TODO ParseStructFields handle DW_TAG_subprogram
  size_t GetNumMemberFunctions(lldb::opaque_compiler_type_t type) override {
    return 0;
  };

  // TODO ParseStructFields handle DW_TAG_subprogram
  TypeMemberFunctionImpl GetMemberFunctionAtIndex(
      lldb::opaque_compiler_type_t type,
      size_t idx
  ) override {
    return TypeMemberFunctionImpl();
  };

  // ---------------------------- Type Completion --------------------------- //

  /// Returns true if type is not `nullptr`. Rust doesn't have incomplete types
  /// AFAIK
  bool GetCompleteType(lldb::opaque_compiler_type_t type) override;

  /// Always returns false, rust doesn't have incomplete types afaik
  bool IsForcefullyCompleted(lldb::opaque_compiler_type_t type) override;

  // -------------------------- AST related queries ------------------------- //

  /// Returns the internally cached pointer byte size
  uint32_t GetPointerByteSize() override;

  /// I have literally no idea what this means
  unsigned GetPtrAuthKey(lldb::opaque_compiler_type_t type) override {
    return 0;
  }

  /// I have literally no idea what this means
  unsigned GetPtrAuthDiscriminator(lldb::opaque_compiler_type_t type) override {
    return 0;
  }

  /// I have literally no idea what this means
  bool GetPtrAuthAddressDiversity(lldb::opaque_compiler_type_t type) override {
    return false;
  }

  // ------------------------------- Accessors ------------------------------ //

  /// Returns the name of the type. TODO if BaseOnly is false, returns the scope
  /// qualified name of the type
  ConstString
  GetTypeName(lldb::opaque_compiler_type_t type, bool BaseOnly) override;

  /// TODO returns the scope-qualified name of the type
  ConstString GetDisplayTypeName(lldb::opaque_compiler_type_t type) override;

  /// Returns an lldb::TypeFlags bitfield describing the type
  uint32_t GetTypeInfo(
      lldb::opaque_compiler_type_t type,
      CompilerType* pointee_or_element_compiler_type
  ) override;

  /// Returns eLanguageTypeRust
  lldb::LanguageType GetMinimumLanguage(lldb::opaque_compiler_type_t type
  ) override {
    return lldb::eLanguageTypeRust;
  }

  lldb::TypeClass GetTypeClass(lldb::opaque_compiler_type_t type) override;

  // ------------------------ Modified Type Creation ------------------------ //

  /// Returns an invalid CompilerType if the type is not an array
  CompilerType GetArrayElementType(
      lldb::opaque_compiler_type_t type,
      ExecutionContextScope* exe_scope
  ) override;

  /// For a type T and size N, returns a type [T; N]. Returns an invalid
  /// CompilerType if type is `nullptr`
  CompilerType
  GetArrayType(lldb::opaque_compiler_type_t type, uint64_t size) override;

  /// If the type is a typedef, strips the typedef. If not, returns the type
  /// passed in.
  CompilerType GetCanonicalType(lldb::opaque_compiler_type_t type) override;

  /// Gets the underlying `repr` of the enum or sum type. Returns an invalid
  /// CompilerType if the type is not a enum or sum type
  CompilerType GetEnumerationIntegerType(lldb::opaque_compiler_type_t type
  ) override;

  /// If the passed in type is a pointer, returns the type pointed-to type. If
  /// not, returns an invalid CompilerType
  CompilerType GetPointeeType(lldb::opaque_compiler_type_t type) override;

  /// Wraps the given type in a RustIndirectionType
  CompilerType GetPointerType(lldb::opaque_compiler_type_t type) override;

  /// Wraps the given type in a RustIndirectionType
  CompilerType GetLValueReferenceType(lldb::opaque_compiler_type_t type
  ) override;

  /// Wraps the given type in a RustIndirectionType
  CompilerType GetRValueReferenceType(lldb::opaque_compiler_type_t type
  ) override;

  /// Returns the same type that was passed in
  CompilerType GetAtomicType(lldb::opaque_compiler_type_t type) override;

  /// If the passed in type is a pointer/ref, returns the immutable version of
  /// that pointer/ref
  CompilerType AddConstModifier(lldb::opaque_compiler_type_t type) override;

  /// Returns the same type that was put in
  CompilerType AddVolatileModifier(lldb::opaque_compiler_type_t type) override;

  /// Returns the same type that was put in
  CompilerType AddRestrictModifier(lldb::opaque_compiler_type_t type) override;

  /// Returns the same type that was put in
  CompilerType AddPtrAuthModifier(
      lldb::opaque_compiler_type_t type,
      uint32_t payload
  ) override;

  /// Returns the given type wrapped in a RustTypedefType
  ///
  /// \param opaque_payload      The m_payload field of Type, which may
  /// carry TypeSystem-specific extra information.
  CompilerType CreateTypedef(
      lldb::opaque_compiler_type_t type,
      const char* name,
      const CompilerDeclContext& decl_ctx,
      uint32_t opaque_payload
  ) override;

  /// TODO old TypeSystemRust didn't implement
  CompilerType GetBasicTypeFromAST(lldb::BasicType basic_type) override;

  // TODO old TypeSystemRust didn't implement, probably not necessary
  CompilerType CreateGenericFunctionPrototype() override;

  /// Returns the built-in numeric type of the appropriate size.
  /// `eEncodingVector` results in an invalid CompilerType
  CompilerType GetBuiltinTypeForEncodingAndBitSize(
      lldb::Encoding encoding,
      size_t bit_size
  ) override;

  // ---------------------------- Type Inspection --------------------------- //

  /// TODO
  const llvm::fltSemantics& GetFloatTypeSemantics(size_t byte_size) override;

  std::optional<uint64_t> GetBitSize(
      lldb::opaque_compiler_type_t type,
      ExecutionContextScope* exe_scope
  ) override;

  lldb::Encoding
  GetEncoding(lldb::opaque_compiler_type_t type, uint64_t& count) override;

  lldb::Format GetFormat(lldb::opaque_compiler_type_t type) override;

  /// "Sees through" typedefs and pointers, populates their underlying type's
  /// children. Also gets children for Aggregates, Sum Types, and Arrays. All
  /// other types return 0
  llvm::Expected<uint32_t> GetNumChildren(
      lldb::opaque_compiler_type_t type,
      bool omit_empty_base_classes,
      const ExecutionContext* exe_ctx
  ) override;

  /// Only includes the basic numeric types, char, and bool.
  CompilerType GetBuiltinTypeByName(ConstString name) override;

  /// Returned values are straightforward. i8 returns eBasicTypeSignedChar,
  /// Short is assumed to be 16 bits, Int is assumed to be 32 bits, and LongLong
  /// is assumed to be 64 bits
  lldb::BasicType GetBasicTypeEnumeration(lldb::opaque_compiler_type_t type
  ) override;

  /// Calls the given function on each enum variant
  void ForEachEnumerator(
      lldb::opaque_compiler_type_t type,
      std::function<bool(
          const CompilerType& integer_type,
          ConstString name,
          const llvm::APSInt& value
      )> const& callback
  ) override;

  /// "looks through" typedefs to report the underlying type's field count. Sum
  /// types report their variant count + 1 so that the discriminant can be
  /// treated as a field.
  uint32_t GetNumFields(lldb::opaque_compiler_type_t type) override;

  /// "looks through" typedefs to report the underlying type's field. Sum types
  /// have a "bonus field" at the end for their discriminant value.
  CompilerType GetFieldAtIndex(
      lldb::opaque_compiler_type_t type,
      size_t idx,
      std::string& name,
      uint64_t* bit_offset_ptr,
      uint32_t* bitfield_bit_size_ptr,
      bool* is_bitfield_ptr
  ) override;

  /// Always returns 0
  uint32_t GetNumDirectBaseClasses(lldb::opaque_compiler_type_t type) override {
    return 0;
  }

  /// Always returns 0 TODO maybe use for num of trait implementations?
  uint32_t GetNumVirtualBaseClasses(lldb::opaque_compiler_type_t type
  ) override {
    return 0;
  }

  /// Always returns an invalid compiler type.
  CompilerType GetDirectBaseClassAtIndex(
      lldb::opaque_compiler_type_t type,
      size_t idx,
      uint32_t* bit_offset_ptr
  ) override {
    return CompilerType();
  }

  /// Always returns an invalid compiler type
  CompilerType GetVirtualBaseClassAtIndex(
      lldb::opaque_compiler_type_t type,
      size_t idx,
      uint32_t* bit_offset_ptr
  ) override {
    return CompilerType();
  };

  /// TODO always returns an invalid CompilerDecl
  CompilerDecl GetStaticFieldWithName(
      lldb::opaque_compiler_type_t type,
      llvm::StringRef name
  ) override;

  /// "Looks through" typedefs, if `transparent_pointers` is true, also looks
  /// through pointers
  llvm::Expected<CompilerType> GetChildCompilerTypeAtIndex(
      lldb::opaque_compiler_type_t type,
      ExecutionContext* exe_ctx,
      size_t idx,
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
  ) override;

  /// Lookup a child given a name.
  uint32_t GetIndexOfChildWithName(
      lldb::opaque_compiler_type_t type,
      llvm::StringRef name,
      bool omit_empty_base_classes
  ) override;

  // Lookup a child member given a name. Returns UINT32_MAX as a fail value
  size_t GetIndexOfChildMemberWithName(
      lldb::opaque_compiler_type_t type,
      llvm::StringRef name,
      bool omit_empty_base_classes,
      std::vector<uint32_t>& child_indexes
  ) override;

  /// Always returns an invalid CompilerType TODO maybe should return sum type
  /// variant types?
  CompilerType GetDirectNestedTypeWithName(
      lldb::opaque_compiler_type_t type,
      llvm::StringRef name
  ) override;

  /// Returns the number of generic parameters for a given type
  size_t GetNumTemplateArguments(
      lldb::opaque_compiler_type_t type,
      bool expand_pack
  ) override;

  /// Returns eTemplateArgumentKindType if the template argument is valid,
  /// otherwise returns eTemplateArgumentKindNull
  /// TODO rust compiler doesn't output generic values, only generic types
  /// if that ever changes, we'll need to add handling here
  lldb::TemplateArgumentKind GetTemplateArgumentKind(
      lldb::opaque_compiler_type_t type,
      size_t idx,
      bool expand_pack
  ) override;

  /// Returns an invalid CompilerType if the type or index are not valid
  CompilerType GetTypeTemplateArgument(
      lldb::opaque_compiler_type_t type,
      size_t idx,
      bool expand_pack
  ) override;

  /// Always returns `std::nullopt`
  // TODO rust compiler doesn't output generic values, only generic types
  // if that ever changes, we'll need to add handling here
  std::optional<CompilerType::IntegralTemplateArgument>
  GetIntegralTemplateArgument(
      lldb::opaque_compiler_type_t type,
      size_t idx,
      bool expand_pack
  ) override;

  /// Always returns 0. TODO if pointers ever get const qualifiers
  unsigned GetTypeQualifiers(lldb::opaque_compiler_type_t type) override {
    return 0;
  };

  std::optional<size_t> GetTypeBitAlign(
      lldb::opaque_compiler_type_t type,
      ExecutionContextScope* exe_scope
  ) override {
    auto* rt = static_cast<RustType*>(type);

    if (IsScalarType(type)) {
      return rt->m_size;
    }

    /// TODO just store the align for all values
    if (auto* t = rt->AsAggregate()) {
      return t->align;
    }

    return std::nullopt;
  };

  /// If the current object represents a typedef type, get the underlying type,
  /// otherwise return an invalid CompilerType
  CompilerType GetTypedefedType(lldb::opaque_compiler_type_t type) override;

  /// Returns the type passed in TODO if rust pointers get qualifiers
  CompilerType GetFullyUnqualifiedType(lldb::opaque_compiler_type_t type
  ) override;

  /// Returns the underlying type of a reference
  CompilerType GetNonReferenceType(lldb::opaque_compiler_type_t type) override;

  // ----------------------------- Dumping Types ---------------------------- //
#ifndef NDEBUG
  /// Convenience LLVM-style dump method for use in the debugger only.
  LLVM_DUMP_METHOD void dump(lldb::opaque_compiler_type_t type) const override {
    return;
  };
#endif

  /// This is used to control the formatting of values when printed with the `v
  /// <expr>` command (e.g. printing enum variant name instead of value,
  /// printing u8 as a byte instead of a char)
  bool DumpTypeValue(
      lldb::opaque_compiler_type_t type,
      Stream& s,
      lldb::Format format,
      const DataExtractor& data,
      lldb::offset_t data_offset,
      size_t data_byte_size,
      uint32_t bitfield_bit_size,
      uint32_t bitfield_bit_offset,
      ExecutionContextScope* exe_scope
  ) override;

  /// Dump the type to stdout.
  void DumpTypeDescription(
      lldb::opaque_compiler_type_t type,
      lldb::DescriptionLevel level = lldb::eDescriptionLevelFull
  ) override {
    return;
  };

  /// Print a description of the type to a stream. The exact implementation
  /// varies, but the expectation is that eDescriptionLevelFull returns a
  /// source-like representation of the type, whereas eDescriptionLevelVerbose
  /// does a dump of the underlying AST if applicable.
  void DumpTypeDescription(
      lldb::opaque_compiler_type_t type,
      Stream& s,
      lldb::DescriptionLevel level = lldb::eDescriptionLevelFull
  ) override {
    return;
  };

  /// Dump a textual representation of the internal TypeSystem state to the
  /// given stream.
  ///
  /// This should not modify the state of the TypeSystem if possible.
  void Dump(llvm::raw_ostream& output) override { return; };

  UserExpression* GetUserExpression(
      llvm::StringRef expr,
      llvm::StringRef prefix,
      SourceLanguage language,
      Expression::ResultType desired_type,
      const EvaluateExpressionOptions& options,
      ValueObject* ctx_obj
  ) override {
    // return nullptr;
    lldb::TargetSP target_sp = m_target_wp.lock();
    if (!target_sp)
      return nullptr;

    return new RustUserExpression(
        *target_sp.get(),
        expr,
        prefix,
        language,
        desired_type,
        options
    );
  };

  FunctionCaller* GetFunctionCaller(
      const CompilerType& return_type,
      const Address& function_address,
      const ValueList& arg_value_list,
      const char* name
  ) override {
    return nullptr;
  };

  // TODO ?
  std::unique_ptr<UtilityFunction>
  CreateUtilityFunction(std::string text, std::string name) override {
    return nullptr;
  };

  /// based on the TypeSystemClang implementation, this looks like a final step
  /// to "clean up" a type (e.g. remove qualifiers, look through typedef) before
  /// it gets formatted.
  CompilerType GetTypeForFormatters(void* type) override;

  LazyBool ShouldPrintAsOneLiner(void* type, ValueObject* valobj) override;

  std::optional<llvm::json::Value> ReportStatistics() override;

  bool ShouldTreatScalarValueAsAddress(lldb::opaque_compiler_type_t type
  ) override {
    return IsPointerOrReferenceType(type, nullptr);
  }

  PersistentExpressionState* GetPersistentExpressionState() override {
    return nullptr;
  }

  //   bool GetHasForcefullyCompletedTypes() const;

  // ------------------------------------------------------------------------ //
  //                         DWARFASTParser Interface                         //
  // ------------------------------------------------------------------------ //

  /// Main entry point for type interpretation for `*-gnu` based builds.
  lldb::TypeSP ParseTypeFromDWARF(
      const SymbolContext& sc,
      const plugin::dwarf::DWARFDIE& die,
      bool* type_is_new_ptr
  ) override;

  ConstString ConstructDemangledNameFromDWARF(const plugin::dwarf::DWARFDIE& die
  ) override;

  /// Function Types are still dispatched to ParseTypeFromDWARF, this mostly
  /// handles Decls and DeclContexts
  Function* ParseFunctionFromDWARF(
      CompileUnit& comp_unit,
      const plugin::dwarf::DWARFDIE& die,
      const AddressRange& range
  ) override;

  /// What does the return value indicate? Who knows.
  bool CompleteTypeFromDWARF(
      const plugin::dwarf::DWARFDIE& die,
      Type* type,
      CompilerType& compiler_type
  ) override {
    // We don't currently use type completion for Rust.
    return true;
  };

  CompilerDecl GetDeclForUIDFromDWARF(const plugin::dwarf::DWARFDIE& die
  ) override;

  CompilerDeclContext
  GetDeclContextForUIDFromDWARF(const plugin::dwarf::DWARFDIE& die) override;

  CompilerDeclContext
  GetDeclContextContainingUIDFromDWARF(const plugin::dwarf::DWARFDIE& die
  ) override;

  void EnsureAllDIEsInDeclContextHaveBeenParsed(CompilerDeclContext decl_context
  ) override;

  std::string GetDIEClassTemplateParams(const plugin::dwarf::DWARFDIE& die
  ) override;

  // ---------------------------- Custom helpers ---------------------------- //

  /// Returns a tuple containing the `encoding_uid`, `byte_size`, `encoding`,
  /// and `TypeName` of a dwarf node
  BasicAttributes ParseBaseAttributes(const plugin::dwarf::DWARFDIE& die);

  /// Parser for `DW_TAG_base_type`
  lldb::TypeSP ParseBasicType(const plugin::dwarf::DWARFDIE& die);

  /// Parser for `DW_TAG_typedef_type`
  ///
  /// Rust currently doesn't generate typedefs for anything except MSVC targets,
  /// but having this will help future-proof.
  lldb::TypeSP ParseTypedefType(const plugin::dwarf::DWARFDIE& die);

  /// Parser for `DW_TAG_pointer_type`, `DW_TAG_reference_type`, and
  /// `DW_TAG_rvalue_reference_type`
  ///
  /// Rust doesn't currently generate rvalue references, but they may be used in
  /// the future for nested refs
  lldb::TypeSP ParseIndirectionType(const plugin::dwarf::DWARFDIE& die);

  lldb::TypeSP ParseCStyleEnum(const plugin::dwarf::DWARFDIE& die);
  /// Parser for `DW_TAG_structure_type` and `DW_TAG_union_type`
  ///
  /// By virture of the dwarf tags, also covers tuple structs and sum types.
  /// Does NOT handle C-style enums
  lldb::TypeSP ParseStructureType(const plugin::dwarf::DWARFDIE& die);

  /// Returns a tuple containing the DWARFFormValue representing the
  /// `DW_AT_type`, the byte alignment, the byte offset, and the field name
  std::pair<FieldAttributes, plugin::dwarf::DWARFFormValue>
  ParseFieldAttributes(const plugin::dwarf::DWARFDIE& die);

  /// Called inside `TypeSystemRust::ParseStructureType()` to parse the
  /// individual fields of a struct or tuple. For enum field parsing, see
  /// `TypeSystemRust::ParseEnumFields`
  ///
  /// Corresponds roughly to `DWARFASTParserClang::ParseSingleMember`
  void ParseStructFields(
      const plugin::dwarf::DWARFDIE& die,
      std::vector<FieldAttributes>& fields,
      std::vector<std::pair<llvm::StringRef, std::optional<CompilerType>>>&
          template_args,
      bool is_tuple
  );

  /// For layout details,
  /// see:https://github.com/rust-lang/rust/blob/master/compiler/rustc_codegen_llvm/src/debuginfo/metadata/enums/native.rs#L24
  CompilerType ParseSumType(
      const plugin::dwarf::DWARFDIE& die,
      const ConstString& type_name,
      uint64_t size
  );

  /// Parses the `DW_TAG_variant_part` of a sum type. Returns a pair containing
  /// the type of the discriminant, as well as a list of each variant's
  /// discriminant value. Per rust docs:
  // "The [discriminant value] is optional, and is omitted if
  //   - This is the only variant of a univariant enum (i.e. their is no
  //   discriminant)
  //   - This is the "untagged" variant of a niche-layout enum
  //     (where only the other variants are identified by a single value)"
  std::pair<CompilerType, std::vector<std::optional<uint64_t>>>
  ParseVariantPart(const plugin::dwarf::DWARFDIE& die);

  /// Parser for `DW_TAG_array_type`
  lldb::TypeSP ParseArrayType(const plugin::dwarf::DWARFDIE& die);

  /// Parser for `DW_TAG_inlined_subroutine`, `DW_TAG_subprogram`,
  /// `DW_TAG_subroutine_type`
  lldb::TypeSP ParseFunctionType(const plugin::dwarf::DWARFDIE& die);

  // ------------------------------------------------------------------------ //
  //                          PDBASTParser Interface                          //
  // ------------------------------------------------------------------------ //

  lldb::TypeSP ParseTypeFromPDB(npdb::PdbTypeSymId type) override;

  lldb::TypeSP ParseTypedefDecl(const npdb::PdbGlobalSymId& symbol) override;

  ConstString ConstructDemangledNameFromPDB(const npdb::PdbSymUid& symbol
  ) override;

  Function* ParseFunctionFromPDB(npdb::PdbCompilandSymId func_id) override;

  bool CompleteTypeFromPDB(
      const npdb::PdbTypeSymId symbol,
      Type* type,
      CompilerType& compiler_type
  ) override;

  CompilerDecl GetDeclForUIDFromPDB(const npdb::PdbSymUid& symbol) override;

  CompilerDeclContext GetDeclContextForUIDFromPDB(const npdb::PdbSymUid& symbol
  ) override;

  CompilerDeclContext
  GetDeclContextContainingUIDFromPDB(const npdb::PdbSymUid& symbol) override;

  void
  EnsureAllSymbolsInDeclContextHaveBeenParsed(CompilerDeclContext decl_context
  ) override;

  std::string GetPDBClassTemplateParams(const npdb::PdbTypeSymId& type
  ) override;

  // --------------------------- NativePDB Helpers -------------------------- //

  /// Parses builtin and pointer types from PDB data. Equivalent to DWARF's
  /// ParseBasicType
  lldb::TypeSP ParseSimpleTypePDB(npdb::PdbTypeSymId type);

  /// For the moment, just returns the underlying type.
  lldb::TypeSP ParseModifierTypePDB(llvm::codeview::ModifierRecord& modifier);

  lldb::TypeSP ParsePointerTypePDB(
      npdb::PdbTypeSymId type,
      llvm::codeview::PointerRecord& pointer
  );

  lldb::TypeSP ParseAggregateTypePDB(
      npdb::PdbTypeSymId type,
      const llvm::codeview::TagRecord& record,
      uint32_t size
  );

  /// Fills the given `fields` vector with the parsed child-fields of the given
  /// record. Recurses into `ParseTypeFromPDB` for field types
  void ParseFieldListPDB(
      const llvm::codeview::TagRecord& record,
      std::vector<FieldAttributes>& fields,
      std::vector<std::pair<ConstString, CompilerType>>& static_fields
  );

  lldb::TypeSP ParseEnumTypePDB(
      npdb::PdbTypeSymId type,
      const llvm::codeview::EnumRecord& record
  );

  CompilerType ParseSumTypePDB(
      npdb::PdbTypeSymId type,
      const llvm::codeview::TagRecord& record,
      uint32_t size
  );

  lldb::TypeSP ParseFunctionTypePDB(
      npdb::PdbTypeSymId type,
      llvm::codeview::ProcedureRecord pr
  );

  RustDecl* CreateFunctionDecl(npdb::PdbCompilandSymId symbol);
  RustDecl* CreateVariableDecl(
      npdb::PdbCompilandSymId scope_id,
      npdb::PdbCompilandSymId var_id
  );

  void ParseBlockChildren(npdb::PdbCompilandSymId block_id);

  ConstString NormalizeMSVCTypeName(llvm::StringRef name);

  void FillIndirectionTypes(CompilerType type, lldb::user_id_t type_idx);

  // lldb::TypeSP ParseTypeFromSymbol(
  //     const lldb_private::SymbolContext& sc,
  //     const llvm::pdb::PDBSymbol& type,
  //     bool* type_is_new_ptr
  // ) override;

  // lldb_private::ConstString
  // ConstructDemangledNameFromSymbol(const llvm::pdb::PDBSymbol& symbol)
  // override;

  // lldb_private::Function* ParseFunctionFromSymbol(
  //     lldb_private::CompileUnit& comp_unit,
  //     const llvm::pdb::PDBSymbol& symbol,
  //     const lldb_private::AddressRange& range
  // ) override;

  // bool CompleteTypeFromSymbol(
  //     const llvm::pdb::PDBSymbol& symbol,
  //     lldb_private::Type* type,
  //     lldb_private::CompilerType& compiler_type
  // ) override;

  // lldb_private::CompilerDecl GetDeclForSymbol(const llvm::pdb::PDBSymbol&
  // symbol ) override;

  // lldb_private::CompilerDeclContext
  // GetDeclContextForSymbol(const llvm::pdb::PDBSymbol& symbol) override;

  // lldb_private::CompilerDeclContext
  // GetDeclContextContainingSymbol(const llvm::pdb::PDBSymbol& symbol)
  // override;

  // // virtual void EnsureAllDIEsInDeclContextHaveBeenParsed(
  // //     CompilerDeclContext decl_context) = 0;

  // // virtual std::string GetDIEClassTemplateParams(const DWARFDIE &die) =
  // 0;

  // lldb_private::Type* GetTypeForSymbol(const llvm::pdb::PDBSymbol& die);

  // void
  // ParseDeclsForDeclContext(const lldb_private::CompilerDeclContext
  // decl_context ) override;

  // static lldb::AccessType GetAccessTypeFromDWARF(uint32_t
  // dwarf_accessibility);

  // ------------------------------------------------------------------------
  // //
  //                                  Helpers //
  // ------------------------------------------------------------------------
  // //

  /// Returns a type name with its scope qualifiers. E.g. "Bar" ->
  /// "example::Foo::Bar"
  ConstString
  QualifyTypeName(const ConstString& name, const plugin::dwarf::DWARFDIE& die);

  void TemplateArgsFromTypeName(
      const llvm::StringRef name,
      std::vector<CompilerType>& template_args
  );

  CompilerDecl GetDecl(
      CompilerDeclContext parent,
      const ConstString& name,
      const ConstString& mangled
  );

  void PrintDeclContexts();

  SymbolFile* GetSymbolFile() { return m_sym_file; };

  /// Returns an invalid compiler type if the byte size is not 1, 2, 4, 8, or
  /// 16
  CompilerType IntTypeFromByteSize(uint8_t size) {
    RustType* rt;
    switch (size) {
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
      return CompilerType();
    }

    return CompilerType(weak_from_this(), rt);
  }

  /// Returns an invalid compiler type if the byte size is not 1, 2, 4, 8, or
  /// 16
  CompilerType UIntTypeFromByteSize(uint8_t size) {
    RustType* rt;
    switch (size) {
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
      return CompilerType();
    }

    return CompilerType(weak_from_this(), rt);
  }

  /// Returns an invalid compiler type if the byte size is not 2, 4, 8, or 16
  CompilerType FloatTypeFromByteSize(uint8_t size) {
    RustType* rt;

    switch (size) {
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
      return CompilerType();
    }

    return CompilerType(weak_from_this(), rt);
  }

  CompilerType BoolType() {
    return CompilerType(weak_from_this(), primitive_types.Bool());
  }

  CompilerType CharType() {
    return CompilerType(weak_from_this(), primitive_types.Char());
  }

private:
  uint64_t m_pointer_byte_size;

  /// Compile unit context, used to store all DW_TAG_compile_unit Decls
  std::unique_ptr<RustDecl> m_compile_unit_ctx;

  lldb::TargetWP m_target_wp;

  llvm::DenseMap<
      const lldb_private::plugin::dwarf::DWARFDebugInfoEntry*,
      lldb_private::CompilerDeclContext>
      m_decl_contexts;
  llvm::DenseMap<
      const lldb_private::plugin::dwarf::DWARFDebugInfoEntry*,
      lldb_private::CompilerDecl>
      m_decls;

  std::
      multimap<CompilerDeclContext, const lldb_private::plugin::dwarf::DWARFDIE>
          m_decl_ctx_to_die;

  /// Cache of primitive types since they are used frequently, particularly in
  /// expressions
  RustPrimitives primitive_types;

  llvm::DenseMap<lldb::user_id_t, RustDecl*> m_uid_to_decl;
  llvm::DenseMap<lldb::user_id_t, RustType*> m_uid_to_type;
  std::multimap<CompilerDeclContext, lldb::user_id_t> m_decl_ctx_to_uid;
  llvm::DenseMap<llvm::StringRef, ConstString> msvc_normalization_cache;
  llvm::once_flag m_parse_functions_and_non_local_vars;
  llvm::once_flag m_parse_all_types;
};
} // namespace lldb_private

#endif // liblldb_TypeSystemRust_h_