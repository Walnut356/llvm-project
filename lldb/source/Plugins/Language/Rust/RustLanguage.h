//===-- RustLanguage.h ------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_RustLanguage_h_
#define liblldb_RustLanguage_h_

// C Includes
// C++ Includes
#include <vector>

// Other libraries and framework includes
#include "llvm/ADT/StringRef.h"

// Project includes
#include "lldb/Target/Language.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"

#include "lldb/DataFormatters/TypeCategory.h"
#include "lldb/DataFormatters/TypeFormat.h"
#include "lldb/DataFormatters/TypeSummary.h"
#include "lldb/DataFormatters/TypeSynthetic.h"

namespace lldb_private {

class RustLanguage : public Language {
public:
  /// Registers the plugin with the PluginManager
  static void Initialize();

  /// Unregisters the plugin with the PluginManager
  static void Terminate();

  /// Called by the PluginManager to instantiate the plugin. Returns a
  /// nullptr if the passed in language is not eLanguageTypeRust
  static lldb_private::Language* CreateInstance(lldb::LanguageType language);

  /// Returns "Rust"
  static llvm::StringRef GetPluginNameStatic();

  /// Returns "Rust"
  llvm::StringRef GetPluginName() override;

  /// Returns eLanguageTypeRust
  lldb::LanguageType GetLanguageType() const override {
    return lldb::eLanguageTypeRust;
  }

  /// Returns "main"
  llvm::StringRef GetUserEntryPointName() const override { return "main"; }

  /// Returns true if the file path ends with `.rs`
  bool IsSourceFile(llvm::StringRef file_path) const override;

  /// Returns true iff the given symbol name is compatible with the mangling
  /// scheme of this language.
  ///
  /// This function should only return true if there is a high confidence
  /// that the name actually belongs to this language.
  bool SymbolNameFitsToLanguage(Mangled name) const override;

  // TODO

  bool IsTopLevelFunction(Function& function) override;

  //   const Highlighter *GetHighlighter() const override;

  /// Creates the `Rust` type category and populates it with the built-in type
  /// synthetics and summaries.
  /// See:
  /// https://lldb.llvm.org/use/variable.html#synthetic-children for more info.
  ///
  /// The python scripts look very similar to the synthetic/summary
  /// implimentations, but have the advantage of accessing LLDB's internals and
  /// significantly better performance
  lldb::TypeCategoryImplSP GetFormatters() override;

  //   HardcodedFormatters::HardcodedFormatFinder GetHardcodedFormats()
  //   override;

  //   HardcodedFormatters::HardcodedSummaryFinder GetHardcodedSummaries()
  //   override;

  // HardcodedFormatters::HardcodedSyntheticFinder
  // GetHardcodedSynthetics() override;

  //   std::vector<FormattersMatchCandidate>
  //   GetPossibleFormattersMatches(ValueObject &valobj,
  //                                lldb::DynamicValueType use_dynamic)
  //                                override;

  /// TODO I'm not 100% sure what these are for, but this follows the CXX
  /// counterpart closely by stripping Typedefs
  std::unique_ptr<TypeScavenger> GetTypeScavenger() override;

  //   const char *GetLanguageSpecificTypeLookupHelp() override;

  /// An individual data formatter may apply to several types and cross language
  /// boundaries. Each of those languages may want to customize the display of
  /// values of said types by appending proper prefix/suffix information in
  /// language-specific ways. This function returns that prefix and suffix.
  ///
  /// \param[in] type_hint
  ///   A StringRef used to determine what the prefix and suffix should be. It
  ///   is called a hint because some types may have multiple variants for which
  ///   the prefix and/or suffix may vary.
  ///
  /// \return
  ///   A std::pair<StringRef, StringRef>, the first being the prefix and the
  ///   second being the suffix. They may be empty.
  //   std::pair<llvm::StringRef, llvm::StringRef>
  //   GetFormatterPrefixSuffix(llvm::StringRef type_hint) override;

  // When looking up functions, we take a user provided string which may be a
  // partial match to the full demangled name and compare it to the actual
  // demangled name to see if it matches as much as the user specified.  An
  // example of this is if the user provided A::my_function, but the
  // symbol was really B::A::my_function.  We want that to be
  // a match.  But we wouldn't want this to match AnotherA::my_function.  The
  // user is specifying a truncated path, not a truncated set of characters.
  // This function does a language-aware comparison for those purposes.
  // bool DemangledNameContainsPath(llvm::StringRef path,
  //                                ConstString demangled) const override;

  //   // if a language has a custom format for printing variable declarations
  //   that
  //   // it wants LLDB to honor it should return an appropriate closure here
  //   DumpValueObjectOptions::DeclPrintingHelper GetDeclPrintingHelper()
  //   override;

  //   LazyBool IsLogicalTrue(ValueObject &valobj, Status &error) override;

  //   // for a ValueObject of some "reference type", if the value points to the
  //   // nil/null object, this method returns true
  //   bool IsNilReference(ValueObject &valobj) override;

  //   /// Returns the summary string for ValueObjects for which
  //   IsNilReference() is
  //   /// true.
  //   llvm::StringRef GetNilReferenceSummaryString() override;

  //   // original doc comment:
  //   // "for a ValueObject of some "reference type", if the language provides
  //   a
  //   // technique to decide whether the reference has ever been assigned to
  //   some
  //   // object, this method will return true if such detection is possible,
  //   and if
  //   // the reference has never been assigned"
  //   //
  //   // It is against Rust's compiler rules for a reference to be invalid, so
  //   this
  //   // will always return false
  //   bool IsUninitializedReference(ValueObject &valobj) override { return
  //   false; };

  //   bool GetFunctionDisplayName(const SymbolContext *sc,
  //                               const ExecutionContext *exe_ctx,
  //                               FunctionNameRepresentation representation,
  //                               Stream &s) override;

  //   ConstString
  //   GetDemangledFunctionNameWithoutArguments(Mangled mangled) const override;

  //   ConstString GetDisplayDemangledName(Mangled mangled) const override;

  //   /// TODO probably unused
  //   void GetExceptionResolverDescription(bool catch_on, bool throw_on,
  //                                        Stream &s) override = 0;

  //   //
  //   --------------------------------------------------------------------------
  //   // //
  //   //                              General Accessors //
  //   //
  //   --------------------------------------------------------------------------
  //   // //

  //   /// TODO probably not used
  //   std::vector<ConstString>
  //   GenerateAlternateFunctionManglings(const ConstString mangled) const
  //   override;

  //   /// TODO probably not used
  //   ConstString FindBestAlternateFunctionMangledName(
  //       const Mangled mangled, const SymbolContext &sym_ctx) const override;

  //   /// Returns "self", i.e. the variable name that refers to the object that
  //   /// called a struct's function
  //   llvm::StringRef GetInstanceVariableName() override { return "self"; };

  //   /// Returns true if this SymbolContext should be ignored when setting
  //   /// breakpoints by line (number or regex). Helpful for languages that
  //   create
  //   /// artificial functions without meaningful user code associated with
  //   them
  //   /// (e.g. code that gets expanded in late compilation stages, like by
  //   /// CoroSplitter).
  //   /// TODO maybe useful for macros or something?
  //   bool IgnoreForLineBreakpoints(const SymbolContext &) const override;

  //   /// Returns true if this Language supports exception breakpoints on throw
  //   via
  //   /// a corresponding LanguageRuntime plugin.
  //   /// TODO probably always false?
  //   bool SupportsExceptionBreakpointsOnThrow() const override { return false;
  //   }

  //   /// Returns true if this Language supports exception breakpoints on catch
  //   via
  //   /// a corresponding LanguageRuntime plugin.
  //   /// TODO probably always false?
  //   bool SupportsExceptionBreakpointsOnCatch() const override { return false;
  //   }

  //   /// Returns the keyword used for throw statements in this language, e.g.
  //   /// Python uses \b raise. Defaults to \b throw.
  //   /// TODO eef
  //   llvm::StringRef GetThrowKeyword() const override;

  //   /// Returns the keyword used for catch statements in this language, e.g.
  //   /// Python uses \b except. Defaults to \b catch.
  //   /// TODO eef
  //   virtual llvm::StringRef GetCatchKeyword() const override;
};

// -------------------------------------------------------------------------- //
//                               Type Synthetics                              //
// -------------------------------------------------------------------------- //

/// Synthetics implemented via C++ classes
class RustSyntheticChildren : public SyntheticChildren {
  bool IsScripted() override { return false; }
  typedef std::function<
      SyntheticChildrenFrontEnd*(RustSyntheticChildren*, lldb::ValueObjectSP)>
      CreateFrontEndCallback;

  SyntheticChildrenFrontEnd::AutoPointer GetFrontEnd(ValueObject& backend
  ) override {
    return SyntheticChildrenFrontEnd::AutoPointer(
        m_create_callback(this, backend.GetSP())
    );
  }

  CreateFrontEndCallback m_create_callback;
  std::string m_description;

private:
  RustSyntheticChildren(const RustSyntheticChildren&) = delete;
  const RustSyntheticChildren& operator=(const RustSyntheticChildren&) = delete;
};

// -------------------------------------------------------------------------- //
//                               Type Summaries                               //
// -------------------------------------------------------------------------- //

/// summaries implemented via a C++ function
struct RustFunctionSummaryFormat : public TypeSummaryImpl {
  // we should convert these to SBValue and SBStream if we ever cross the
  // boundary towards the external world
  typedef std::function<bool(ValueObject&, Stream&, const TypeSummaryOptions&)>
      Callback;

  Callback m_impl;
  std::string m_description;

  RustFunctionSummaryFormat(
      const TypeSummaryImpl::Flags& flags,
      Callback impl,
      const char* description
  )
      : TypeSummaryImpl(Kind::eCallback, flags), m_impl(impl),
        m_description(description ? description : "") {};

  ~RustFunctionSummaryFormat() override = default;

  Callback GetBackendFunction() const { return m_impl; }

  const char* GetTextualInfo() const { return m_description.c_str(); }

  void SetBackendFunction(Callback cb_func) { m_impl = std::move(cb_func); }

  void SetTextualInfo(const char* descr) {
    if (descr)
      m_description.assign(descr);
    else
      m_description.clear();
  }

  bool FormatObject(
      ValueObject* valobj,
      std::string& dest,
      const TypeSummaryOptions& options
  ) override;

  std::string GetDescription() override;

  static bool classof(const TypeSummaryImpl* S) {
    return S->GetKind() == Kind::eCallback;
  }

  typedef std::shared_ptr<RustFunctionSummaryFormat> SharedPointer;

private:
  RustFunctionSummaryFormat(const RustFunctionSummaryFormat&) = delete;
  const RustFunctionSummaryFormat&
  operator=(const RustFunctionSummaryFormat&) = delete;
};
} // namespace lldb_private

#endif // liblldb_RustLanguage_h_
