//===-- Str.cpp -----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/FormattersHelpers.h"
#include "lldb/DataFormatters/TypeSynthetic.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-forward.h"
#include <optional>

using namespace lldb;
using namespace lldb_private;
using namespace lldb_private::formatters;

namespace lldb_private {
namespace formatters {
class StrSyntheticFrontEnd : public SyntheticChildrenFrontEnd {
public:
  StrSyntheticFrontEnd(ValueObjectSP valobj_sp);

//   ConstString GetSyntheticTypeName() override { return ConstString("&str"); };

  llvm::Expected<uint32_t> CalculateNumChildren() override { return len; };

  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

  ChildCacheState Update() override;

  bool MightHaveChildren() override { return true; };

  size_t GetIndexOfChildWithName(ConstString name) override;

  ValueObject* data_ptr;
  uint64_t len;
};

StrSyntheticFrontEnd::StrSyntheticFrontEnd(ValueObjectSP valobj_sp)
    : SyntheticChildrenFrontEnd(*valobj_sp) {
  if (valobj_sp) {
    Update();
  }
}

ChildCacheState StrSyntheticFrontEnd::Update() {
  len = m_backend.GetChildMemberWithName("length")->GetValueAsUnsigned(0);
  data_ptr = m_backend.GetChildMemberWithName("data_ptr").get();

  return ChildCacheState::eRefetch;
}

ValueObjectSP StrSyntheticFrontEnd::GetChildAtIndex(uint32_t idx) {
  if (!data_ptr || idx > len) {
    return ValueObjectSP();
  }

  uint64_t offset = idx + data_ptr->GetPointerValue();

  StreamString name;
  name.Printf("[%" PRIu64 "]", (uint64_t)idx);

  auto val = CreateValueObjectFromAddress(
      name.GetString(),
      offset,
      m_backend.GetExecutionContextRef(),
      data_ptr->GetCompilerType().GetPointeeType()
  );

  val->SetFormat(lldb::eFormatCharPrintable);

  return val;
}

size_t StrSyntheticFrontEnd::GetIndexOfChildWithName(ConstString name) {
  if (!data_ptr) {
    return UINT32_MAX;
  }

  return ExtractIndexFromString(name.GetCString());
}

static SyntheticChildrenFrontEnd* RustStrSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
) {
  if (!valobj_sp)
    return nullptr;
  CompilerType type = valobj_sp->GetCompilerType();
  if (!type.IsValid())
    return nullptr;
  return new StrSyntheticFrontEnd(valobj_sp);
}

} // namespace formatters
} // namespace lldb_private