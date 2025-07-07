//===-- Str.cpp -----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "RustStdLib.h"

#include "Utils.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/FormattersHelpers.h"
#include "lldb/DataFormatters/TypeSynthetic.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-forward.h"


using namespace lldb;
using namespace lldb_private;
using namespace lldb_private::formatters;

namespace {
class StrSyntheticFrontEnd : public SyntheticChildrenFrontEnd {
public:
  StrSyntheticFrontEnd(ValueObjectSP valobj_sp);

  //   ConstString GetSyntheticTypeName() override { return ConstString("&str");
  //   };

  llvm::Expected<uint32_t> CalculateNumChildren() override { return len; };

  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

  ChildCacheState Update() override;

  bool MightHaveChildren() override { return true; };

  size_t GetIndexOfChildWithName(ConstString name) override;

  static TypeSummaryImplSP summary;

  std::vector<uint8_t> buffer;
  ValueObject* data_ptr;
  uint64_t len;
};
} // namespace

TypeSummaryImplSP StrSyntheticFrontEnd::summary =
    CXXFunctionSummaryFormat::SharedPointer(new CXXFunctionSummaryFormat(
        TypeSummaryImpl::Flags()
            .SetCascades()
            .SetSkipPointers(false)
            .SetSkipReferences(false),
        PrintableByteSummary,
        "summary for u8's that should be treated as characters"
    ));

StrSyntheticFrontEnd::StrSyntheticFrontEnd(ValueObjectSP valobj_sp)
    : SyntheticChildrenFrontEnd(*valobj_sp) {
  if (valobj_sp) {
    Update();
  }
}

ChildCacheState StrSyntheticFrontEnd::Update() {
  len = m_backend.GetChildMemberWithName("length")->GetValueAsUnsigned(0);
  data_ptr = m_backend.GetChildMemberWithName("data_ptr").get();

    if (len > 0) {
    buffer.resize(len);
    auto process = data_ptr->GetProcessSP();

    Status err = Status();
    process->ReadMemory(data_ptr->GetPointerValue(), buffer.data(), len, err);
  }

  return ChildCacheState::eReuse;
}

ValueObjectSP StrSyntheticFrontEnd::GetChildAtIndex(uint32_t idx) {
  if (!data_ptr || idx > len) {
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
      data_ptr->GetCompilerType().GetPointeeType()
  );

  child->SetFormat(eFormatCharPrintable);

  // child->SetSummaryFormat(StringSyntheticFrontEnd::summary);

  return child;
}

size_t StrSyntheticFrontEnd::GetIndexOfChildWithName(ConstString name) {
  if (!data_ptr) {
    return UINT32_MAX;
  }

  return ExtractIndexFromString(name.GetCString());
}

SyntheticChildrenFrontEnd* formatters::RustStrSyntheticFrontEndCreator(
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

bool formatters::RustStrSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
) {

  auto size = valobj.GetNonSyntheticValue()
                  ->GetChildMemberWithName("length")
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