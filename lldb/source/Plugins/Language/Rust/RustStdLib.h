//===-- StdLib.h ------------------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLDB_SOURCE_PLUGINS_LANGUAGE_RUST_RUSTSTDLIB_H
#define LLDB_SOURCE_PLUGINS_LANGUAGE_RUST_RUSTSTDLIB_H

#include "lldb/Core/ValueObject.h"
#include "lldb/DataFormatters/TypeSummary.h"
#include "lldb/DataFormatters/TypeSynthetic.h"
#include "lldb/Utility/Stream.h"

namespace lldb_private {
namespace formatters {
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

SyntheticChildrenFrontEnd* RustStringSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
);
bool RustStringSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
);

SyntheticChildrenFrontEnd* RustSliceSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
);

bool RustStrSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
);

SyntheticChildrenFrontEnd* RustStrSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
);

SyntheticChildrenFrontEnd* RustSumTypeSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
);

bool RustSumTypeSummary(
    ValueObject& valobj,
    Stream& stream,
    const TypeSummaryOptions& summary_options
);

SyntheticChildrenFrontEnd* RustVecSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
);

SyntheticChildrenFrontEnd* RustHashMapSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
);

} // namespace formatters
} // namespace lldb_private

#endif