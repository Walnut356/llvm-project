//===-- SumType.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Plugins/TypeSystem/Rust/TypeSystemRust.h"
#include "Utils.h"
#include "lldb/Core/FormatEntity.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/DumpValueObjectOptions.h"
#include "lldb/DataFormatters/TypeSynthetic.h"
#include "lldb/DataFormatters/ValueObjectPrinter.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-forward.h"
#include <cstdio>
#include <optional>

using namespace lldb;
using namespace lldb_private;

namespace lldb_private {
namespace formatters {
class SumTypeSyntheticFrontEnd : public SyntheticChildrenFrontEnd {
public:
  SumTypeSyntheticFrontEnd(ValueObjectSP valobj_sp);

  llvm::Expected<uint32_t> CalculateNumChildren() override {
    return variant->GetNumChildren();
  };

  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

  ChildCacheState Update() override;

  bool MightHaveChildren() override { return true; };

  size_t GetIndexOfChildWithName(ConstString name) override;

  ValueObject* variant;
};

SumTypeSyntheticFrontEnd::SumTypeSyntheticFrontEnd(ValueObjectSP valobj_sp)
    : SyntheticChildrenFrontEnd(*valobj_sp) {
  if (valobj_sp) {
    Update();
  }
}

ChildCacheState SumTypeSyntheticFrontEnd::Update() {
  if (!m_backend.GetChildMemberWithName("$discr$")) {
    assert(0);
  }
  bool success = false;
  uint64_t discr = m_backend.GetChildMemberWithName("$discr$")
                       ->GetValueAsUnsigned(0, &success);

  if (!success) {
    assert(0);
  }

  CompilerType t = m_backend.GetCompilerType();

  auto* rt = static_cast<RustType*>(t.GetOpaqueQualType());

  uint64_t variant_idx = rt->AsSumType()->discr_map[discr];

  // discr is always the last child, variants are always in order and 0 indexed,
  // so we can just grab the child member at the variant index
  variant = m_backend.GetChildAtIndex(variant_idx).get();

  return ChildCacheState::eRefetch;
}

ValueObjectSP SumTypeSyntheticFrontEnd::GetChildAtIndex(uint32_t idx) {
  return variant->GetChildAtIndex(idx);
}

size_t SumTypeSyntheticFrontEnd::GetIndexOfChildWithName(ConstString name) {
  return variant->GetIndexOfChildWithName(name);
}

} // namespace formatters
} // namespace lldb_private

SyntheticChildrenFrontEnd* RustSumTypeSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
) {
  if (!valobj_sp)
    return nullptr;
  CompilerType type = valobj_sp->GetCompilerType();
  if (!type.IsValid())
    return nullptr;

  auto* rt = static_cast<RustType*>(type.GetOpaqueQualType());

  if (!rt->IsSumType()) {
    return nullptr;
  }

  return new lldb_private::formatters::SumTypeSyntheticFrontEnd(valobj_sp);
}

bool RustSumTypeSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
) {
  // accessing the SyntheticFrontEnd isn't trivial here

  uint64_t discr = valobj.GetNonSyntheticValue()
                       ->GetChildMemberWithName("$discr$")
                       ->GetValueAsUnsigned(0);

  auto* rt =
      static_cast<RustType*>(valobj.GetCompilerType().GetOpaqueQualType());

  // shouldn't be possible for this to fail, but we'll be safe
  if (!rt || !rt->IsSumType()) {
    return false;
  }

  auto* st = rt->AsSumType();
  auto variant = st->GetVariant(discr);

  // Prefix the output with the variant name
  stream.PutCString(GetUnqualifiedName(variant.GetTypeName().GetStringRef()));

  uint32_t num_children = valobj.GetNumChildrenIgnoringErrors();

  // don't output empty brackets if it's a non-data-carrying variant
  if (num_children == 0) {
    return true;
  }

  // here we take advantage of any existing summary providers to print the
  // children. Instead of iterating over the variant's children and printing
  // those, we just use the summary provider of the variant itself.
  auto idx = st->discr_map.contains(discr) ? st->discr_map[discr]
                                           : st->untagged_variant.value_or(0);

  auto v = valobj.GetNonSyntheticValue()->GetChildAtIndex(idx);

  ValueObjectPrinter printer(
      *v.get(),
      &stream,
      DumpValueObjectOptions(*v.get())
          .SetRevealEmptyAggregates(false)
          .SetHideRootType(true)
          .SetHideRootName(true)
          .SetAllowOnelinerMode(true)
          .SetShowSummary(true)
  );

  auto e = printer.PrintValueObject();

  if (e) {
    assert(0);
  }

  return true;
}