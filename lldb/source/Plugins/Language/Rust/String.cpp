//===-- String.cpp --------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Utils.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Core/ValueObjectSyntheticFilter.h"
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

// String is just a wrapper around a Vec. While it could just delegate to the
// internal `Vec`, that's significantly slower than just replicating the vec's
// behavior here. For any other type I wouldn't care, but strings are so common
// that speed is extremely important
class StringSyntheticFrontEnd : public SyntheticChildrenFrontEnd {
public:
  StringSyntheticFrontEnd(ValueObjectSP valobj_sp);

  ConstString GetSyntheticTypeName() override { return ConstString("String"); };

  llvm::Expected<uint32_t> CalculateNumChildren() override { return len; };

  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

  ChildCacheState Update() override;

  bool MightHaveChildren() override { return true; };

  size_t GetIndexOfChildWithName(ConstString name) override;

  static TypeSummaryImplSP summary;

  ValueObject* inner_vec = nullptr;
  ValueObject* data_ptr = nullptr;
  std::vector<uint8_t> buffer = {};
  uint64_t len;
  CompilerType element_type;
};

TypeSummaryImplSP StringSyntheticFrontEnd::summary =
    CXXFunctionSummaryFormat::SharedPointer(new CXXFunctionSummaryFormat(
        TypeSummaryImpl::Flags()
            .SetCascades()
            .SetSkipPointers(false)
            .SetSkipReferences(false),
        PrintableByteSummary,
        "summary for u8's that should be treated as characters"
    ));

StringSyntheticFrontEnd::StringSyntheticFrontEnd(ValueObjectSP valobj_sp)
    : SyntheticChildrenFrontEnd(*valobj_sp) {
  if (valobj_sp) {
    Update();
  }
}

ChildCacheState StringSyntheticFrontEnd::Update() {
  inner_vec =
      m_backend.GetChildMemberWithName("vec")->GetNonSyntheticValue().get();

  len = inner_vec->GetChildMemberWithName("len")->GetValueAsUnsigned(0);
  // should always be `u8`
  element_type = inner_vec->GetCompilerType().GetTypeTemplateArgument(0);
  // element size is always 1 so we don't need to bother with that

  data_ptr = inner_vec->GetChildMemberWithName("buf")
                 ->GetChildMemberWithName("inner")
                 ->GetChildMemberWithName("ptr")
                 ->GetChildMemberWithName("pointer")
                 ->GetChildMemberWithName("pointer")
                 .get();

  if (len > 0) {
    buffer.resize(len);
    auto process = data_ptr->GetProcessSP();

    Status err = Status();
    process->ReadMemory(data_ptr->GetPointerValue(), buffer.data(), len, err);
  }

  return ChildCacheState::eRefetch;
}

ValueObjectSP StringSyntheticFrontEnd::GetChildAtIndex(uint32_t idx) {
  if (!data_ptr || !element_type || idx > buffer.size()) {
    return ValueObjectSP();
  }

  StreamString name;
  name.Printf("[%" PRIu64 "]", (uint64_t)idx);

  DataExtractor d = DataExtractor(
      &buffer[idx],
      1,
      lldb::eByteOrderLittle,
      data_ptr->GetByteSize().value_or(8)
  );

  auto child = CreateValueObjectFromData(
      name.GetString(),
      d,
      m_backend.GetExecutionContextRef(),
      element_type
  );

  child->SetFormat(eFormatCharPrintable);

  child->SetSummaryFormat(StringSyntheticFrontEnd::summary);

  return child;
}

size_t StringSyntheticFrontEnd::GetIndexOfChildWithName(ConstString name) {
  if (!data_ptr) {
    return UINT32_MAX;
  }

  auto idx = ExtractIndexFromString(name.GetCString());

  return idx;
}

static SyntheticChildrenFrontEnd* RustStringSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
) {
  if (!valobj_sp)
    return nullptr;
  CompilerType type = valobj_sp->GetCompilerType();
  if (!type.IsValid())
    return nullptr;
  return new StringSyntheticFrontEnd(valobj_sp);
}

static bool RustStringSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
) {

  auto size = valobj.GetNonSyntheticValue()
                  ->GetChildMemberWithName("vec")
                  ->GetChildMemberWithName("len")
                  ->GetValueAsUnsigned(0);

  stream.PutChar('"');
  for (unsigned int i = 0; i < size; ++i) {
    auto child = valobj.GetChildAtIndex(i);
    if (child) {
      stream.PutChar(child->GetValueAsUnsigned(0));
    } else {
      stream.PutCString("<?>");
    }
  }
  stream.PutChar('"');

  return true;
}

} // namespace formatters
} // namespace lldb_private