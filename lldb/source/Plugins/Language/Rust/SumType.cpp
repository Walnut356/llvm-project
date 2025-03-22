//===-- SumType.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Plugins/TypeSystem/Rust/TypeSystemRust.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/FormattersHelpers.h"
#include "lldb/DataFormatters/TypeSynthetic.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-forward.h"
#include <cstdio>
#include <optional>

using namespace lldb;
using namespace lldb_private;
using namespace lldb_private::formatters;

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

  return new SumTypeSyntheticFrontEnd(valobj_sp);
}

} // namespace formatters
} // namespace lldb_private