//===-- RustLanguage.cpp ----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

// C Includes
#include <memory>
#include <string.h>
// C++ Includes
#include <functional>
#include <mutex>

// Other libraries and framework includes
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Threading.h"

// Project includes
#include "Plugins/TypeSystem/Rust/TypeSystemRust.h"
#include "RustLanguage.h"
#include "lldb/Core/PluginManager.h"
#include "lldb/DataFormatters/DataVisualization.h"
#include "lldb/DataFormatters/FormattersHelpers.h"
#include "lldb/DataFormatters/TypeSummary.h"
#include "lldb/Expression/UtilityFunction.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-enumerations.h"

#include "Slice.cpp"
#include "Str.cpp"
#include "String.cpp"
#include "SumType.cpp"
#include "Vec.cpp"
#include "lldb/lldb-forward.h"

using namespace lldb;
using namespace lldb_private;
using namespace lldb_private::formatters;

LLDB_PLUGIN_DEFINE(RustLanguage)

/// Wrapper function to easily add a RustSyntheticFrontEnd. Piggybacks on
/// CXXSyntheticChildren for the time being because 1. i'm lazy and 2. it
/// doesn't do anything crazy enough to warrant writing our own version
void AddRustSynthetic(
    TypeCategoryImpl::SharedPointer category_sp,
    CXXSyntheticChildren::CreateFrontEndCallback generator,
    const char* description,
    llvm::StringRef type_name,
    ScriptedSyntheticChildren::Flags flags,
    bool regex
) {
  lldb::SyntheticChildrenSP synth_sp(
      new CXXSyntheticChildren(flags, description, generator)
  );
  FormatterMatchType match_type =
      regex ? eFormatterMatchRegex : eFormatterMatchExact;
  category_sp->AddTypeSynthetic(type_name, match_type, synth_sp);
}

/// Wrapper function to easily add a RustSummary. Piggybacks on
/// CXXFunctionSummaryFormat for the time being because 1. i'm lazy and 2. it
/// doesn't do anything crazy enough to warrant writing our own version
// void AddRustSummary(
//     TypeCategoryImpl::SharedPointer category_sp,
//     RustFunctionSummaryFormat::Callback funct,
//     const char* description,
//     llvm::StringRef type_name,
//     TypeSummaryImpl::Flags flags,
//     bool regex = false
// ) {
//   RustFunctionSummaryFormat::SharedPointer summary_sp(
//       new RustFunctionSummaryFormat(flags, funct, description)
//   );

//   FormatterMatchType match_type =
//       regex ? eFormatterMatchRegex : eFormatterMatchExact;
//   category_sp->AddTypeSummary(type_name, match_type, summary_sp);
// }

void RustLanguage::Initialize() {
  PluginManager::RegisterPlugin(
      GetPluginNameStatic(),
      "Rust Language",
      CreateInstance
  );
}

void RustLanguage::Terminate() {
  PluginManager::UnregisterPlugin(CreateInstance);
}

llvm::StringRef RustLanguage::GetPluginNameStatic() { return "Rust"; }

llvm::StringRef RustLanguage::GetPluginName() { return GetPluginNameStatic(); }

Language* RustLanguage::CreateInstance(lldb::LanguageType language) {
  if (language == eLanguageTypeRust)
    return new RustLanguage();
  return nullptr;
}

bool RustLanguage::IsSourceFile(llvm::StringRef file_path) const {
  return file_path.ends_with(".rs");
}

bool RustLanguage::SymbolNameFitsToLanguage(Mangled name) const {
  const char* mangled_name = name.GetMangledName().GetCString();
  if (!mangled_name)
    return false;

  Mangled::ManglingScheme scheme = Mangled::GetManglingScheme(mangled_name);
  return scheme == Mangled::ManglingScheme::eManglingSchemeRustV0;
}

bool RustLanguage::IsTopLevelFunction(Function& function) {
  auto ctx = function.GetDeclContext();

  auto* rc = static_cast<RustDeclContext*>(ctx.GetOpaqueDeclContext());

  return rc->kind != RustDeclContext::Struct;
}

std::unique_ptr<Language::TypeScavenger> RustLanguage::GetTypeScavenger() {
  class RustTypeScavenger : public Language::ImageListTypeScavenger {
  public:
    CompilerType AdjustForInclusion(CompilerType& candidate) override {
      LanguageType lang_type(candidate.GetMinimumLanguage());
      if (lang_type != lldb::eLanguageTypeRust)
        return CompilerType();
      if (candidate.IsTypedefType())
        return candidate.GetTypedefedType();
      return candidate;
    }
  };

  return std::unique_ptr<TypeScavenger>(new RustTypeScavenger());
}

lldb::TypeCategoryImplSP RustLanguage::GetFormatters() {
  static llvm::once_flag g_initialize;
  static TypeCategoryImplSP g_category;

  llvm::call_once(g_initialize, [this]() -> void {
    DataVisualization::Categories::GetCategory(
        ConstString(GetPluginName()),
        g_category
    );
    if (g_category) {
      this->category = g_category;
      g_category->AddLanguage(lldb::eLanguageTypeRust);

      // -------------------------------------------------------------------- //
      //                              Synthetics                              //
      // -------------------------------------------------------------------- //

      // THE ORDER OF THESE MATTERS. When they are iterated over, llvm:reverse()
      // is called on the container (see: FormattersContainer.h:182) presumably
      // so that ones added at runtime "overwrite" eixsting ones without
      // actually removing them from the map

      // ----------------------------- Sum Type ----------------------------- //

      // This needs to be first (so that it will be checked last) since it is a
      // raw wildcard.
      // AddRustSynthetic(
      //     g_category,
      //     RustSumTypeSyntheticFrontEndCreator,
      //     "Rust Enum (sum type) synthetic provider",
      //     // Unfortunately there's no way we can tell the difference between
      //     a
      //     // struct and an enum via just the type name in non-msvc-land, so
      //     we
      //     // have to test everything. It's just a simple static cast of the
      //     // internal type to check if it's a RustSumType, but it still kinda
      //     // sucks
      //     ".*",
      //     ScriptedSyntheticChildren::Flags()
      //         .SetCascades()
      //         .SetSkipPointers(false)
      //         .SetSkipReferences(false)
      //         .SetFrontEndWantsDereference(),
      //     true
      // );

      // -------------------------------- Vec ------------------------------- //

      AddRustSynthetic(
          g_category,
          RustVecSyntheticFrontEndCreator,
          "standard library Vec synthetic provider",
          "^(alloc::([a-z_]+::)+)Vec<.+>$",
          ScriptedSyntheticChildren::Flags()
              .SetCascades()
              .SetSkipPointers(false)
              .SetSkipReferences(false)
              .SetFrontEndWantsDereference(),
          true
      );

      // ------------------------------- Slice ------------------------------ //

      AddRustSynthetic(
          g_category,
          RustSliceSyntheticFrontEndCreator,
          "built-in slice synthetic provider",
          "^&(mut )?\\[.+\\]$",
          ScriptedSyntheticChildren::Flags()
              .SetCascades()
              .SetSkipPointers(false)
              .SetSkipReferences(false)
              .SetFrontEndWantsDereference(),
          true
      );

      // ------------------------------ String ------------------------------ //

      AddRustSynthetic(
          g_category,
          RustStringSyntheticFrontEndCreator,
          "built-in String synthetic provider",
          "^(alloc::([a-z_]+::)+)String$",
          ScriptedSyntheticChildren::Flags()
              .SetCascades()
              .SetSkipPointers(false)
              .SetSkipReferences(false)
              .SetFrontEndWantsDereference(),
          true
      );

      // ------------------------------- &str ------------------------------- //

      AddRustSynthetic(
          g_category,
          RustStrSyntheticFrontEndCreator,
          "built-in &str synthetic provider",
          "^&(mut )?str$",
          ScriptedSyntheticChildren::Flags()
              .SetCascades()
              .SetSkipPointers(false)
              .SetSkipReferences(false)
              .SetFrontEndWantsDereference(),
          true
      );

      // -------------------------------------------------------------------- //
      //                               Summaries                              //
      // -------------------------------------------------------------------- //

      // RustFunctionSummaryFormat::SharedPointer format(
      //     new RustFunctionSummaryFormat(
      //         TypeSummaryImpl::Flags()
      //             .SetCascades()
      //             .SetSkipPointers(false)
      //             .SetSkipReferences(false),
      //         RustStringSummary,
      //         "built-in String summary provider"
      //     )
      // );
      g_category->AddTypeSummary(
          "^(alloc::([a-z_]+::)+)String$",
          lldb::eFormatterMatchRegex,
          RustFunctionSummaryFormat::SharedPointer(
              new RustFunctionSummaryFormat(
                  TypeSummaryImpl::Flags()
                      .SetCascades()
                      .SetSkipPointers(false)
                      .SetSkipReferences(false),
                  RustStringSummary,
                  "built-in String summary provider"
              )
          )
      );

      g_category->AddTypeSummary(
          "^&(mut )?str$",
          lldb::eFormatterMatchRegex,
          RustFunctionSummaryFormat::SharedPointer(
              new RustFunctionSummaryFormat(
                  TypeSummaryImpl::Flags()
                      .SetCascades()
                      .SetSkipPointers(false)
                      .SetSkipReferences(false),
                  RustStrSummary,
                  "built-in String summary provider"
              )
          )
      );
    }
  });
  return g_category;
}

std::vector<FormattersMatchCandidate>
RustLanguage::GetPossibleFormattersMatches(
    ValueObject& valobj,
    lldb::DynamicValueType use_dynamic
) {
  // BEWARE, UNGODLY HACK BELOW:
  //
  // There are 2 main issues we need to solve:
  // 1. there are 3 options for attaching synthetic providers: match by exact
  // name, match by regex, and match by python script. Python scripts are slow,
  // and type names are not enough to differentiate a struct from a sum-type
  // enum.
  // 2. Using a ".*" regex to match on every time and return an invalid
  // synthetic for non-sum-types *works*, but is very wasteful and slow. It also
  // ends up flagging every single type as "having a synthetic" even though for
  // most of them it's invalid. This can cause issues, most notably in CodeLLDB
  // which has special handling for types with synthetic providers.
  //
  // To solve both, we hijack this function, which is seemingly called *a lot*
  // (presumably on every inspected valobj?). Since we have the valobj, we have
  // the CompilerType. With the CompilerType, we can just ask if it's a
  // sum-type. Once we've confirmed that it is one, we can add an
  // exact-name-match synthetic for the type.

  // ensure that type category "rust" is valid and has been populated
  if (!this->category) {
    GetFormatters();
  }

  auto* rt =
      static_cast<RustType*>(valobj.GetCompilerType().GetOpaqueQualType());

  // If it's a sum type and we haven't already seen it, add the synthetic provider
  if (rt && rt->IsSumType() && !this->sum_types.contains(rt->m_name)) {
    this->sum_types.insert(rt->m_name);

    category->AddTypeSynthetic(
        TypeNameSpecifierImplSP(
            new TypeNameSpecifierImpl(valobj.GetCompilerType())
        ),
        lldb::SyntheticChildrenSP(new CXXSyntheticChildren(
            ScriptedSyntheticChildren::Flags()
                .SetCascades()
                .SetSkipPointers(false)
                .SetSkipReferences(false)
                .SetFrontEndWantsDereference(),
            "sum type",
            RustSumTypeSyntheticFrontEndCreator
        ))
    );
  }

  return {};
}

// HardcodedFormatters::HardcodedSyntheticFinder
// RustLanguage::GetHardcodedSynthetics() {}

// bool RustLanguage::DemangledNameContainsPath(llvm::StringRef path,
//                                              ConstString demangled) const
//                                              {

//                                              }

bool RustFunctionSummaryFormat::FormatObject(
    ValueObject* valobj,
    std::string& dest,
    const TypeSummaryOptions& options
) {
  dest.clear();
  StreamString stream;
  if (!m_impl || !m_impl(*valobj, stream, options))
    return false;
  dest = std::string(stream.GetString());
  return true;
}

std::string RustFunctionSummaryFormat::GetDescription() {
  StreamString sstr;
  sstr.Printf(
      "%s%s%s%s%s%s%s %s",
      Cascades() ? "" : " (not cascading)",
      !DoesPrintChildren(nullptr) ? "" : " (show children)",
      !DoesPrintValue(nullptr) ? " (hide value)" : "",
      IsOneLiner() ? " (one-line printout)" : "",
      SkipsPointers() ? " (skip pointers)" : "",
      SkipsReferences() ? " (skip references)" : "",
      HideNames(nullptr) ? " (hide member names)" : "",
      m_description.c_str()
  );
  return std::string(sstr.GetString());
}