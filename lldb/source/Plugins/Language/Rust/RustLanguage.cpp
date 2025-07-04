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

  auto* rc = static_cast<RustDecl*>(ctx.GetOpaqueDeclContext());

  if (rc) {
    return rc->variant.index() != RustDecl::Type;
  }

  // usually always true
  return true;
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

      AddCXXSynthetic(
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

      AddCXXSynthetic(
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

      AddCXXSynthetic(
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

      AddCXXSynthetic(
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

      g_category->AddTypeSummary(
          "^(alloc::([a-z_]+::)+)String$",
          lldb::eFormatterMatchRegex,
          CXXFunctionSummaryFormat::SharedPointer(new CXXFunctionSummaryFormat(
              TypeSummaryImpl::Flags()
                  .SetCascades()
                  .SetSkipPointers(false)
                  .SetSkipReferences(false),
              RustStringSummary,
              "built-in String summary provider"
          ))
      );

      g_category->AddTypeSummary(
          "^&(mut )?str$",
          lldb::eFormatterMatchRegex,
          CXXFunctionSummaryFormat::SharedPointer(new CXXFunctionSummaryFormat(
              TypeSummaryImpl::Flags()
                  .SetCascades()
                  .SetSkipPointers(false)
                  .SetSkipReferences(false),
              RustStrSummary,
              "built-in &str summary provider"
          ))
      );

      g_category->AddTypeSummary(
          "^(alloc::([a-z_]+::)+)Vec<.+>$",
          lldb::eFormatterMatchRegex,
          CXXFunctionSummaryFormat::SharedPointer(new CXXFunctionSummaryFormat(
              TypeSummaryImpl::Flags()
                  .SetCascades()
                  .SetSkipPointers(false)
                  .SetSkipReferences(false)
                  .SetDontShowChildren(true),
              RustCollectionSummary,
              "built-in Vec summary provider"
          ))
      );

      g_category->AddTypeSummary(
          "^&(mut )?\\[.+\\]$",
          lldb::eFormatterMatchRegex,
          CXXFunctionSummaryFormat::SharedPointer(new CXXFunctionSummaryFormat(
              TypeSummaryImpl::Flags()
                  .SetCascades()
                  .SetSkipPointers(false)
                  .SetSkipReferences(false)
                  .SetDontShowChildren(true),
              RustCollectionSummary,
              "built-in Slice summary provider"
          ))
      );
    }
  });
  return g_category;
}

HardcodedFormatters::HardcodedSummaryFinder
RustLanguage::GetHardcodedSummaries() {
  static llvm::once_flag g_initialize;
  static HardcodedFormatters::HardcodedSummaryFinder g_formatters;

  llvm::call_once(g_initialize, []() -> void {
    g_formatters.push_back(
        [](lldb_private::ValueObject& valobj,
           lldb::DynamicValueType,
           FormatManager&) -> TypeSummaryImpl::SharedPointer {
          static CXXFunctionSummaryFormat::SharedPointer formatter_sp(
              new CXXFunctionSummaryFormat(
                  TypeSummaryImpl::Flags()
                      .SetCascades()
                      .SetSkipPointers(false)
                      .SetSkipReferences(false)
                      .SetDontShowChildren(true),
                  RustSumTypeSummary,
                  "sum-type summary provider"
              )
          );

          auto* rt = static_cast<RustType*>(
              valobj.GetCompilerType().GetOpaqueQualType()
          );

          if (rt && rt->IsSumType()) {
            return formatter_sp;
          }

          return nullptr;
        }
    );

    g_formatters.push_back(
        [](lldb_private::ValueObject& valobj,
           lldb::DynamicValueType,
           FormatManager&) -> TypeSummaryImpl::SharedPointer {
          static CXXFunctionSummaryFormat::SharedPointer formatter_sp(
              new CXXFunctionSummaryFormat(
                  TypeSummaryImpl::Flags()
                      .SetCascades()
                      .SetSkipPointers(false)
                      .SetSkipReferences(false)
                      .SetDontShowChildren(true),
                  RustAggregateSummary,
                  "aggregate summary provider"
              )
          );

          auto* rt = static_cast<RustType*>(
              valobj.GetCompilerType().GetOpaqueQualType()
          );

          if (rt && rt->IsAggregate()) {
            return formatter_sp;
          }

          return nullptr;
        }
    );

    g_formatters.push_back(
        [](lldb_private::ValueObject& valobj,
           lldb::DynamicValueType,
           FormatManager&) -> TypeSummaryImpl::SharedPointer {
          static CXXFunctionSummaryFormat::SharedPointer formatter_sp(
              new CXXFunctionSummaryFormat(
                  TypeSummaryImpl::Flags()
                      .SetCascades()
                      .SetSkipPointers(false)
                      .SetSkipReferences(false)
                      .SetDontShowChildren(true),
                  RustIndirectionSummary,
                  "ref/ptr summary provider"
              )
          );

          auto* rt = static_cast<RustType*>(
              valobj.GetCompilerType().GetOpaqueQualType()
          );

          if (rt && rt->IsIndirection()) {
            return formatter_sp;
          }

          return nullptr;
        }
    );
  });

  return g_formatters;
}

HardcodedFormatters::HardcodedSyntheticFinder
RustLanguage::GetHardcodedSynthetics() {
  static llvm::once_flag g_initialize;
  static HardcodedFormatters::HardcodedSyntheticFinder g_formatters;

  llvm::call_once(g_initialize, []() -> void {
    g_formatters.push_back(
        [](lldb_private::ValueObject& valobj,
           lldb::DynamicValueType,
           FormatManager& fmt_mgr) -> SyntheticChildren::SharedPointer {
          static lldb::SyntheticChildrenSP formatter_sp(
              new CXXSyntheticChildren(
                  ScriptedSyntheticChildren::Flags()
                      .SetCascades()
                      .SetSkipPointers(false)
                      .SetSkipReferences(false)
                      .SetFrontEndWantsDereference(),
                  "sum-type synthetic provider",
                  RustSumTypeSyntheticFrontEndCreator
              )
          );

          auto* rt = static_cast<RustType*>(
              valobj.GetCompilerType().GetOpaqueQualType()
          );

          if (rt && rt->IsSumType()) {
            return formatter_sp;
          }

          return nullptr;
        }
    );
  });

  return g_formatters;
}

std::vector<FormattersMatchCandidate>
RustLanguage::GetPossibleFormattersMatches(
    ValueObject& valobj,
    lldb::DynamicValueType use_dynamic
) {
  return {};
}
