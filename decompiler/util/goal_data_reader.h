#pragma once

#include <string>
#include <cstring>
#include "common/common_types.h"

namespace decompiler {
class DecompilerTypeSystem;
class LinkedObjectFile;
}  // namespace decompiler

class Type;

struct Ref {
  const decompiler::LinkedObjectFile* data = nullptr;
  int seg = -1;
  int byte_offset = 1;
};

struct TypedRef {
  Ref ref;
  Type* type = nullptr;
};

void read_plain_data_field(const TypedRef& object,
                           const std::string& field_name,
                           const decompiler::DecompilerTypeSystem& dts,
                           int size_bytes,
                           u8* out);

template <typename T>
T read_plain_data_field(const TypedRef& object,
                        const std::string& field_name,
                        const decompiler::DecompilerTypeSystem& dts) {
  u8 data[sizeof(T)];
  read_plain_data_field(object, field_name, dts, sizeof(T), data);

  T result;
  memcpy(&result, data, sizeof(T));
  return result;
}

TypedRef get_and_check_ref_to_basic(const TypedRef& object,
                                    const std::string& field_name,
                                    const std::string& expected_type,
                                    const decompiler::DecompilerTypeSystem& dts);

std::string read_symbol_field(const TypedRef& object,
                              const std::string& field_name,
                              const decompiler::DecompilerTypeSystem& dts);

std::string read_type_field(const TypedRef& object,
                            const std::string& field_name,
                            const decompiler::DecompilerTypeSystem& dts,
                            bool ignore_field_type);

std::string read_string_field(const TypedRef& object,
                              const std::string& field_name,
                              const decompiler::DecompilerTypeSystem& dts,
                              bool ignore_field_type);

Ref get_field_ref(const TypedRef& object,
                  const std::string& field_name,
                  const decompiler::DecompilerTypeSystem& dts);

std::string get_type_of_basic(const Ref& object);

TypedRef typed_ref_from_basic(const Ref& object, const decompiler::DecompilerTypeSystem& dts);

Ref deref_label(const Ref& object);

std::string inspect_ref(const Ref& ref);
