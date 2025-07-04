//===-- Utils.h
//------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_RustLanguage_utils_h_
#define liblldb_RustLanguage_utils_h_

#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"
#include "llvm/ADT/StringRef.h"

/// Strips all scope qualifiers from a name (e.g. `Foo::Bar::Baz` -> `Baz`)
llvm::StringRef GetUnqualifiedName(llvm::StringRef str);

/// Summary provider for non-tuple, non-sum-type aggregates (i.e. raw structs,
/// tuple-structs, and unions)
bool RustAggregateSummary(
    lldb_private::ValueObject& valobj,
    lldb_private::Stream& stream,
    const lldb_private::TypeSummaryOptions& summary_options
);

/// Summary provider for array-like collections (e.g. Slice, Vec). Slightly
/// specialized version of RustAggregateSummary. See RustAggregateSummary for
/// implementation logic comments
bool RustCollectionSummary(
    lldb_private::ValueObject& valobj,
    lldb_private::Stream& stream,
    const lldb_private::TypeSummaryOptions& summary_options
);

/// Dereferences the object and returns that value's summary if possible
bool RustIndirectionSummary(
    lldb_private::ValueObject& valobj,
    lldb_private::Stream& stream,
    const lldb_private::TypeSummaryOptions& summary_options
);

bool PrintableByteSummary(
    lldb_private::ValueObject& valobj,
    lldb_private::Stream& stream,
    const lldb_private::TypeSummaryOptions& summary_options
);

#endif