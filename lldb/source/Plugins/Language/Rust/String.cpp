//===-- String.cpp --------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "RustLanguage.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/FormattersHelpers.h"
#include "lldb/DataFormatters/StringPrinter.h"
#include "lldb/DataFormatters/TypeSynthetic.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-forward.h"
#include <optional>

using namespace lldb;
using namespace lldb_private;
using namespace lldb_private::formatters;

bool PrintableByteSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
) {
  uint64_t value = valobj.GetValueAsUnsigned(0);
  switch (value) {
  case '\n':
    stream.PutCString("'\\n'");
    break;
  case '\r':
    stream.PutCString("'\\r'");
    break;
  case '\t':
    stream.PutCString("'\\t'");
    break;
  case '\\':
    stream.PutCString("'\\\\'");
    break;
  case '\0':
    stream.PutCString("'\\0'");
    break;
  case '\'':
    stream.PutCString("'\\''");
    break;

  default:
    if (value < 128 && isprint(value)) {
      stream.Printf("'%c'", char(value));
    } else {
      stream.Printf("'\\u{%x}'", unsigned(value));
    }
    break;
  }

  return true;
}

namespace lldb_private {
namespace formatters {

// String is just a wrapper around a Vec, so most of its functionality will
// delegate directly to the internal vec
class StringSyntheticFrontEnd : public SyntheticChildrenFrontEnd {
public:
  StringSyntheticFrontEnd(ValueObjectSP valobj_sp);

  ConstString GetSyntheticTypeName() override { return ConstString("String"); };

  llvm::Expected<uint32_t> CalculateNumChildren() override {
    return inner_vec->GetNumChildrenIgnoringErrors();
  };

  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

  ChildCacheState Update() override;

  bool MightHaveChildren() override { return true; };

  size_t GetIndexOfChildWithName(ConstString name) override;

  ValueObject* inner_vec;
  TypeSummaryImplSP summary;
};

StringSyntheticFrontEnd::StringSyntheticFrontEnd(ValueObjectSP valobj_sp)
    : SyntheticChildrenFrontEnd(*valobj_sp) {
  summary =
      CXXFunctionSummaryFormat::SharedPointer(new CXXFunctionSummaryFormat(
          TypeSummaryImpl::Flags()
              .SetCascades()
              .SetSkipPointers(false)
              .SetSkipReferences(false),
          PrintableByteSummary,
          "summary for u8's that should be treated as characters"
      ));
  if (valobj_sp) {
    Update();
  }
}

ChildCacheState StringSyntheticFrontEnd::Update() {
  inner_vec =
      m_backend.GetChildMemberWithName("vec")->GetSyntheticValue().get();
  return ChildCacheState::eRefetch;
}

ValueObjectSP StringSyntheticFrontEnd::GetChildAtIndex(uint32_t idx) {
  auto child = inner_vec->GetChildAtIndex(idx);
  if (!child) {
    return child;
  }
  child->SetFormat(eFormatCharPrintable);

  child->SetSummaryFormat(summary);

  return child;
}

size_t StringSyntheticFrontEnd::GetIndexOfChildWithName(ConstString name) {
  return inner_vec->GetIndexOfChildWithName(name);
}

SyntheticChildrenFrontEnd* RustStringSyntheticFrontEndCreator(
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

bool RustStringSummary(
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
    stream.PutChar(valobj.GetChildAtIndex(i)->GetValueAsUnsigned(0));
  }
  stream.PutChar('"');

  return true;
}

} // namespace formatters
} // namespace lldb_private