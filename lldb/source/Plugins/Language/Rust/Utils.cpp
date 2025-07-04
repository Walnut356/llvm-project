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

bool RustAggregateSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
) {
  auto* rt =
      static_cast<RustType*>(valobj.GetCompilerType().GetOpaqueQualType());

  if (!rt) {
    return false;
  }

  auto* at = rt->AsAggregate();

  if (!at) {
    return false;
  }

  bool tuple_struct = at->kind == AggregateKind::TupleStruct;

  uint32_t child_count = valobj.GetNumChildrenIgnoringErrors();

  // this is just a cute way to follow rust's conventions for a more "natural"
  // feel
  char delim_start = tuple_struct ? '(' : '{';
  char delim_end = tuple_struct ? ')' : '}';

  stream.PutChar(delim_start);

  if (child_count != 0) {
    // child_count - 1 because we don't want ", " after the last element
    for (uint32_t i = 0; i < (child_count - 1); ++i) {
      // from what I understand, getting the child value doesn't get the
      // synthetic? Maybe i'm wrong and this is a waste, since that's what it
      // does in python IIRC, but either way we need it for the proper summary
      auto child_reg = valobj.GetChildAtIndex(i);
      auto child = child_reg->GetSyntheticValue();
      if (!child) {
        child = child_reg;
      }

      // no point printing out the numeric tuple "names"
      if (!tuple_struct) {
        stream.PutCString(child->GetName());
        stream.PutChar(':');
      }

      // first we check for a summary. If there isn't one, it returns a nullptr.
      // Then we check for value (afaik, "does the typesystem `DumpTypeValue`
      // return anything?"). If *that* returns a nullptr, we just put in a
      // filler value
      auto* summary = child->GetSummaryAsCString();
      if (!summary) {
        summary = child->GetValueAsCString();
      }
      if (!summary) {
        summary = "{...}";
      }
      stream.PutCString(summary);
      stream.PutCString(", ");
    }

    auto last = valobj.GetChildAtIndex(child_count - 1);

    if (!tuple_struct) {
      stream.PutCString(last->GetName());
      stream.PutChar(':');
    }

    auto* summary = last->GetSummaryAsCString();
    if (!summary) {
      summary = last->GetValueAsCString();
    }
    if (!summary) {
      summary = "{...}";
    }

    stream.PutCString(summary);
  }

  stream.PutChar(delim_end);

  return true;
}

bool RustCollectionSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
) {

  stream.PutCString("[");

  auto size = valobj.GetNumChildrenIgnoringErrors();

  if (size > 0) {
    for (uint32_t i = 0; i < size - 1; ++i) {
      auto child_reg = valobj.GetChildAtIndex(i);
      auto child = child_reg;
      if (child_reg) {
        if (auto c = child_reg->GetSyntheticValue()) {
          child = c;
        }
      }

      if (child) {
        auto* summary = child->GetSummaryAsCString();
        if (!summary) {
          summary = child->GetValueAsCString();
        }
        if (!summary) {
          summary = "{...}";
        }
        stream.PutCString(summary);
        stream.PutCString(", ");
      } else {
        stream.PutCString("<cannot access child>");
      }
    }
    auto child_reg = valobj.GetChildAtIndex(size - 1);
    auto child = child_reg;
    if (child_reg) {
      if (auto c = child_reg->GetSyntheticValue()) {
        child = c;
      }
    }

    if (child) {
      auto* summary = child->GetSummaryAsCString();
      if (!summary) {
        summary = child->GetValueAsCString();
      }
      if (!summary) {
        summary = "{...}";
      }

      stream.PutCString(summary);
    } else {
      stream.PutCString("<cannot access child>");
    }
  }

  stream.PutChar(']');
  return true;
}

bool RustIndirectionSummary(
    lldb_private::ValueObject& valobj,
    lldb_private::Stream& stream,
    const lldb_private::TypeSummaryOptions& summary_options
) {

  // I'm undecided if I want the following section. It feels somewhat helpful,
  // but also a bit redundant

  // auto* rt =
  // static_cast<RustType*>(valobj.GetCompilerType().GetOpaqueQualType());

  // auto kind = rt->AsIndirection()->kind;

  //
  // switch (kind) {
  // case IndirectionKind::ConstPointer:
  //   stream.PutCString("*const ");
  //   break;
  // case IndirectionKind::MutPointer:
  //   stream.PutCString("*mut ");
  //   break;
  // case IndirectionKind::Reference:
  //   stream.PutChar('&');
  //   break;
  // case IndirectionKind::MutReference:
  //   stream.PutCString("&mut ");
  //   break;
  // }

  Status e;
  auto deref = valobj.Dereference(e);

  // If we can't dereference for whatever reason, just output (presumably) the
  // address
  if (e.Fail()) {
    stream.PutCString(valobj.GetValueAsCString());
    return true;
  }

  auto* summary = deref->GetSummaryAsCString();
  if (!summary) {
    summary = deref->GetValueAsCString();
  }
  if (!summary) {
    summary = "{...}";
  }

  stream.PutCString(summary);

  return true;
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