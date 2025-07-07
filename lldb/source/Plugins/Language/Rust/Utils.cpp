//===-- Utils.cpp
//----------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "Plugins/TypeSystem/Rust/TypeSystemRust.h"
#include "Utils.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/DumpValueObjectOptions.h"
#include "lldb/Utility/Stream.h"

using namespace lldb_private;

llvm::StringRef GetUnqualifiedName(llvm::StringRef str) {
  auto idx = str.rfind(':') + 1;

  return str.substr(idx);
}

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