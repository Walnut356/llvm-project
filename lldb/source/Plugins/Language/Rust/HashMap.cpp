//===-- HashMap.cpp -------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "RustStdLib.h"
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

namespace {

/// Synthetic provider for hashbrown HashMaps
class HashMapSyntheticFrontEnd : public SyntheticChildrenFrontEnd {
public:
  HashMapSyntheticFrontEnd(ValueObjectSP valobj_sp);

  ConstString GetSyntheticTypeName() override {
    if (type_name) {
      return type_name;
    }

    auto key_name = m_backend.GetCompilerType().GetTypeTemplateArgument(0).GetTypeName();
    auto elmt_name = m_backend.GetCompilerType().GetTypeTemplateArgument(1).GetTypeName();

    ConstString result;
    if (m_backend.GetCompilerType().GetNumTemplateArguments() == 2) {
      result =
          ConstString(llvm::formatv(
                          "HashSet<{0}>",
                          key_name
          )
                          .str());
    } else {
      result = ConstString(
          llvm::formatv("HashMap<{0},{1}>", key_name, elmt_name).str()
      );
    }

    type_name = result;
    return result;
  }

  llvm::Expected<uint32_t> CalculateNumChildren() override { return len; };

  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

  ChildCacheState Update() override;

  bool MightHaveChildren() override { return true; };

  size_t GetIndexOfChildWithName(ConstString name) override;

  std::vector<uint64_t> valid_indices;
  CompilerType pair_type;
  ValueObject* data_ptr;
  ValueObject* ctrl;
  ConstString type_name;
  uint64_t pair_size;
  uint64_t len;
  uint64_t capacity;
  bool new_layout;
};
} // namespace

HashMapSyntheticFrontEnd::HashMapSyntheticFrontEnd(ValueObjectSP valobj_sp)
    : SyntheticChildrenFrontEnd(*valobj_sp) {
  if (valobj_sp) {
    Update();
  }
}

ChildCacheState HashMapSyntheticFrontEnd::Update() {
  auto base = m_backend.GetChildMemberWithName("base");
  auto table = base->GetChildMemberWithName("table");

  if (!table) {
    table = base->GetChildMemberWithName("map")->GetChildMemberWithName("table");
  }

  auto inner_table = table->GetChildMemberWithName("table");

  auto mask = inner_table->GetChildMemberWithName("bucket_mask");
                  capacity = mask->GetValueAsUnsigned(0) + 1;
  ctrl = inner_table->GetChildMemberWithName("ctrl")->GetChildAtIndex(0).get();

  len = inner_table->GetChildMemberWithName("items")->GetValueAsUnsigned(0);
  pair_type = table->GetCompilerType().GetTypeTemplateArgument(0);
  pair_size = pair_type.GetByteSize(nullptr).value_or(0);

  // TODO is this necessary?
  data_ptr = inner_table->GetChildMemberWithName("data").get();
  if (!data_ptr) {
    data_ptr = ctrl->Cast(pair_type.GetPointerType()).get();
    new_layout = true;
  } else {
    data_ptr = data_ptr->GetChildAtIndex(0).get();
    new_layout = false;
  }

  std::vector<uint8_t> buff = {};
  buff.resize(capacity);
  auto process = data_ptr->GetProcessSP();

  Status err = Status();
  process->ReadMemory(ctrl->GetPointerValue(), buff.data(), capacity, err);

  for (uint64_t i = 0; i < capacity; ++i) {
    // value is present
    if ((buff[i] & 128) == 0) {
      valid_indices.push_back(i);
    }
  }

  return lldb::eRefetch;
}

ValueObjectSP HashMapSyntheticFrontEnd::GetChildAtIndex(uint32_t idx) {
  if (idx >= valid_indices.size()) {
    return nullptr;
  }

  // map the given index to the slot in the hashmap
  auto i = valid_indices[idx];

  if (new_layout) {
    i = -(i + 1);
  }

  auto addr = data_ptr->GetPointerValue() + (i * pair_size);

  printf("%d: %#llx", idx, addr);
  StreamString name;
  name.Printf("[%" PRIu64 "]", (uint64_t)idx);
  auto n = name.GetString();

  auto element = CreateValueObjectFromAddress(
      n,
      addr,
      m_backend.GetExecutionContextRef(),
      pair_type
  );

  // hashset only has 2 template args
  if (m_backend.GetCompilerType().GetNumTemplateArguments() == 2) {
    auto key = element->GetChildAtIndex(0);
    DataExtractor d = {};
    Status err = {};
    key->GetData(d, err);
    return CreateValueObjectFromData(
        n,
        d,
        m_backend.GetExecutionContextRef(),
        key->GetCompilerType()
    );
  }

  return element;
}

size_t HashMapSyntheticFrontEnd::GetIndexOfChildWithName(ConstString name) {
  if (!data_ptr) {
    return UINT32_MAX;
  }

  auto idx =
      lldb_private::formatters::ExtractIndexFromString(name.GetCString());

  return idx;
}

SyntheticChildrenFrontEnd* formatters::RustHashMapSyntheticFrontEndCreator(
    CXXSyntheticChildren*,
    lldb::ValueObjectSP valobj_sp
) {
  if (!valobj_sp)
    return nullptr;
  CompilerType type = valobj_sp->GetCompilerType();
  if (!type.IsValid() || type.GetNumTemplateArguments() == 0)
    return nullptr;
  return new HashMapSyntheticFrontEnd(valobj_sp);
}