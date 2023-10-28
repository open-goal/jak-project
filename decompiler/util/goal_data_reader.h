#pragma once

#include <cstring>
#include <string>

#include "common/common_types.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

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

  TypedRef() = default;
  TypedRef(const Ref& r, Type* t) : ref(r), type(t) {}
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

void memcpy_from_plain_data(u8* dest, const Ref& source, int size_bytes);
std::vector<u8> bytes_from_plain_data(const Ref& source, int size_bytes);

decompiler::LinkedWord::Kind get_word_kind_for_field(const TypedRef& object,
                                                     const std::string& field_name,
                                                     const decompiler::DecompilerTypeSystem& dts);

TypedRef get_and_check_ref_to_basic(const TypedRef& object,
                                    const std::string& field_name,
                                    const std::string& expected_type,
                                    const decompiler::DecompilerTypeSystem& dts);

std::string read_symbol_field(const TypedRef& object,
                              const std::string& field_name,
                              const decompiler::DecompilerTypeSystem& dts);

std::string read_symbol(const Ref& object);
std::string read_type(const Ref& object);
std::string read_string_ref(const Ref& object);

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
u32 deref_u32(const Ref& ref, int word_offset);
u16 deref_u16(const Ref& ref, int array_idx);
s8 deref_s8(const Ref& ref, int byte);
u8 deref_u8(const Ref& ref, int byte);
float deref_float(const Ref& ref, int array_idx);
u64 deref_u64(const Ref& ref, int dw_offset);
std::string inspect_ref(const Ref& ref);
std::vector<int> find_objects_with_type(const decompiler::LinkedObjectFile& file,
                                        const std::string& name);