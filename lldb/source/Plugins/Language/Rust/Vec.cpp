//===-- Vec.cpp -----------------------------------------------------------===//
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
class VecSyntheticFrontEnd : public SyntheticChildrenFrontEnd {
public:
  VecSyntheticFrontEnd(ValueObjectSP valobj_sp);

  ConstString GetSyntheticTypeName() override {
    return ConstString(llvm::formatv("Vec<{0}>", element_name).str());
  };

  llvm::Expected<uint32_t> CalculateNumChildren() override { return len; };

  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

  ChildCacheState Update() override;

  bool MightHaveChildren() override { return true; };

  size_t GetIndexOfChildWithName(ConstString name) override;

  ValueObject* data_ptr;
  uint64_t len;
  CompilerType element_type;
  uint32_t element_size;
  ConstString element_name;
};

VecSyntheticFrontEnd::VecSyntheticFrontEnd(ValueObjectSP valobj_sp)
    : SyntheticChildrenFrontEnd(*valobj_sp), element_type() {
  if (valobj_sp) {
    Update();
  }
}

ChildCacheState VecSyntheticFrontEnd::Update() {
  len = m_backend.GetChildMemberWithName("len")->GetValueAsUnsigned(0);

  element_type = m_backend.GetCompilerType().GetTypeTemplateArgument(0);

  element_size = element_type.GetByteSize(nullptr).value_or(0);

  if (element_size > 0) {
    data_ptr = m_backend.GetChildMemberWithName("buf")
                   ->GetChildMemberWithName("inner")
                   ->GetChildMemberWithName("ptr")
                   ->GetChildMemberWithName("pointer")
                   ->GetChildMemberWithName("pointer")
                   .get();

    assert(data_ptr->IsPointerType());
    // assert(data_ptr->)
  }

  // This creates an arbitrary value of type `element_type` so that we can call
  // the GetSyntheticTypeName of that value. The alternative is a gigantic
  // function that recursively checks generic args to try to format them
  // correctly which is both a pain in the ass and very slow
  // auto ptr_type = element_type.GetPointerType();
  // auto temp = data_ptr->Cast(ptr_type);

  // Status err;
  // auto deref_val = data_ptr->Dereference(err);
  // auto synth = deref_val->GetSyntheticValue();
  element_name = element_type.GetTypeName();

  return ChildCacheState::eRefetch;
}

ValueObjectSP VecSyntheticFrontEnd::GetChildAtIndex(uint32_t idx) {
  if (!data_ptr || idx > len) {
    return ValueObjectSP();
  }

  uint64_t offset = idx * element_size;

  offset = offset + data_ptr->GetPointerValue();
  StreamString name;
  name.Printf("[%" PRIu64 "]", (uint64_t)idx);

  return CreateValueObjectFromAddress(
      name.GetString(),
      offset,
      m_backend.GetExecutionContextRef(),
      element_type
  );
}

size_t VecSyntheticFrontEnd::GetIndexOfChildWithName(ConstString name) {
  if (!data_ptr) {
    return UINT32_MAX;
  }

  return ExtractIndexFromString(name.GetCString());
}

static SyntheticChildrenFrontEnd* RustVecSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
) {
  if (!valobj_sp)
    return nullptr;
  CompilerType type = valobj_sp->GetCompilerType();
  if (!type.IsValid() || type.GetNumTemplateArguments() == 0)
    return nullptr;
  return new VecSyntheticFrontEnd(valobj_sp);
}

} // namespace formatters
} // namespace lldb_private