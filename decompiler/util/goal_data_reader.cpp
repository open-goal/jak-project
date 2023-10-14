#include "goal_data_reader.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/Error.h"

void read_plain_data_field(const TypedRef& object,
                           const std::string& field_name,
                           const decompiler::DecompilerTypeSystem& dts,
                           int size_bytes,
                           u8* out) {
  FieldLookupInfo field_info = dts.ts.lookup_field_info(object.type->get_name(), field_name);

  if (field_info.field.is_dynamic() || field_info.field.is_array() ||
      field_info.field.is_inline()) {
    throw Error("Field {} is dynamic/array/inline and can't be used with read_plain_data_field",
                field_name);
  }

  auto field_type = dts.ts.lookup_type(field_info.type);
  if (field_type->is_reference()) {
    throw Error("Field {} is a reference and can't be used with read_plain_data_field", field_name);
  }

  if (field_type->is_boxed()) {
    throw Error("Field {} is boxed and can't be used with read_plain_data_field", field_name);
  }

  int field_size_bytes = field_type->get_size_in_memory();

  if (field_size_bytes != size_bytes) {
    throw Error(
        "Field {} size mismatch. Type system thinks it is {} bytes, but tried to load as {} bytes.",
        field_name, field_size_bytes, size_bytes);
  }

  const auto& words = object.ref.data->words_by_seg.at(object.ref.seg);
  for (int byte = 0; byte < size_bytes; byte++) {
    int byte_in_words = byte + object.ref.byte_offset + field_info.field.offset();

    int word_idx = byte_in_words / 4;
    int byte_in_word = byte_in_words % 4;

    const auto& word = words.at(word_idx);
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("Error reading byte {} of field {} (in data, byte {}). Didn't get plain data.",
                  byte, field_name, byte_in_words);
    }

    out[byte] = word.get_byte(byte_in_word);
  }
}

void memcpy_from_plain_data(u8* dest, const Ref& source, int size_bytes) {
  const auto& words = source.data->words_by_seg.at(source.seg);
  for (int byte = 0; byte < size_bytes; byte++) {
    int byte_in_words = byte + source.byte_offset;

    int word_idx = byte_in_words / 4;
    int byte_in_word = byte_in_words % 4;

    const auto& word = words.at(word_idx);
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("Error reading byte {} of (in data, byte {}). Didn't get plain data.", byte,
                  byte_in_words);
    }

    dest[byte] = word.get_byte(byte_in_word);
  }
}

std::vector<u8> bytes_from_plain_data(const Ref& source, int size_bytes) {
  std::vector<u8> ret(size_bytes);
  memcpy_from_plain_data(ret.data(), source, size_bytes);
  return ret;
}

decompiler::LinkedWord::Kind get_word_kind_for_field(const TypedRef& object,
                                                     const std::string& field_name,
                                                     const decompiler::DecompilerTypeSystem& dts) {
  FieldLookupInfo field_info = dts.ts.lookup_field_info(object.type->get_name(), field_name);

  if (field_info.field.is_dynamic() || field_info.field.is_array() ||
      field_info.field.is_inline()) {
    throw Error("Field {} is dynamic/array/inline and can't be used with get_word_kind_for_field",
                field_name);
  }

  int byte_in_words = object.ref.byte_offset + field_info.field.offset();
  if ((byte_in_words % 4) != 0) {
    throw Error("Field {} was misaligned.", field_name);
  }

  return object.ref.data->words_by_seg.at(object.ref.seg).at(byte_in_words / 4).kind();
}

TypedRef get_and_check_ref_to_basic(const TypedRef& object,
                                    const std::string& field_name,
                                    const std::string& expected_type,
                                    const decompiler::DecompilerTypeSystem& dts) {
  FieldLookupInfo field_info = dts.ts.lookup_field_info(object.type->get_name(), field_name);

  if (field_info.field.is_dynamic() || field_info.field.is_array() ||
      field_info.field.is_inline()) {
    throw Error(
        "Field {} is dynamic/array/inline and can't be used with get_and_check_ref_to_basic",
        field_name);
  }

  auto field_type = dts.ts.lookup_type(field_info.type);
  auto basic_type = dynamic_cast<BasicType*>(field_type);
  if (!basic_type) {
    throw Error("Field {} is not a basic and can't be used with read_plain_data_field", field_name);
  }

  int byte_in_words = object.ref.byte_offset + field_info.field.offset();
  if ((byte_in_words % 4) != 0) {
    throw Error("Field {} was misaligned.", field_name);
  }

  const auto& word = object.ref.data->words_by_seg.at(object.ref.seg).at(byte_in_words / 4);

  if (word.kind() != decompiler::LinkedWord::PTR) {
    throw Error("get_and_check_ref_to_basic did not get a label");
  }

  const auto& label = object.ref.data->labels.at(word.label_id());

  Ref ref;
  ref.data = object.ref.data;
  ref.seg = label.target_segment;
  ref.byte_offset = label.offset - 4;  // basic offset.

  if ((ref.byte_offset % 4) != 0) {
    throw Error("get_and_check_ref_to_basic had a misaligned basic");
  }

  const auto& type_tag = object.ref.data->words_by_seg.at(ref.seg).at(ref.byte_offset / 4);
  if (type_tag.kind() != decompiler::LinkedWord::TYPE_PTR) {
    throw Error("get_and_check_ref_to_basic did not find a type tag");
  }

  if (type_tag.symbol_name() != expected_type) {
    throw Error("get_and_check_ref_to_basic found type {} for field {}, expected {}",
                type_tag.symbol_name(), field_name, expected_type);
  }

  TypedRef tr;
  tr.ref = ref;
  tr.type = dts.ts.lookup_type(type_tag.symbol_name());
  return tr;
}

std::string read_symbol_field(const TypedRef& object,
                              const std::string& field_name,
                              const decompiler::DecompilerTypeSystem& dts) {
  FieldLookupInfo field_info = dts.ts.lookup_field_info(object.type->get_name(), field_name);

  if (field_info.field.is_dynamic() || field_info.field.is_array() ||
      field_info.field.is_inline()) {
    throw Error("Field {} is dynamic/array/inline and can't be used with read_symbol_field",
                field_name);
  }

  if (field_info.type != TypeSpec("symbol")) {
    throw Error("Field {} has type {} and can't be used with read_symbol_field", field_name,
                field_info.type.print());
  }

  int byte_in_words = object.ref.byte_offset + field_info.field.offset();
  if ((byte_in_words % 4) != 0) {
    throw Error("Field {} was misaligned.", field_name);
  }

  const auto& word = object.ref.data->words_by_seg.at(object.ref.seg).at(byte_in_words / 4);

  if (word.kind() != decompiler::LinkedWord::SYM_PTR) {
    throw Error("read_symbol_field did not get a symbol (offset {} words)", byte_in_words / 4);
  }

  return word.symbol_name();
}

std::string read_type_field(const TypedRef& object,
                            const std::string& field_name,
                            const decompiler::DecompilerTypeSystem& dts,
                            bool ignore_field_type) {
  FieldLookupInfo field_info = dts.ts.lookup_field_info(object.type->get_name(), field_name);

  if (field_info.field.is_dynamic() || field_info.field.is_array() ||
      field_info.field.is_inline()) {
    throw Error("Field {} is dynamic/array/inline and can't be used with read_type_field",
                field_name);
  }

  if (!ignore_field_type && field_info.type != TypeSpec("type")) {
    throw Error("Field {} has type {} and can't be used with read_type_field", field_name,
                field_info.type.print());
  }

  int byte_in_words = object.ref.byte_offset + field_info.field.offset();
  if ((byte_in_words % 4) != 0) {
    throw Error("Field {} was misaligned.", field_name);
  }

  const auto& word = object.ref.data->words_by_seg.at(object.ref.seg).at(byte_in_words / 4);

  if (word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    throw Error("read_type_field did not get a symbol (offset {} words)", byte_in_words / 4);
  }

  return word.symbol_name();
}

std::string read_symbol(const Ref& object) {
  const auto& word = object.data->words_by_seg.at(object.seg).at(object.byte_offset / 4);
  if (word.kind() != decompiler::LinkedWord::SYM_PTR) {
    throw Error("read_symbol did not get a symbol (offset {} words)", object.byte_offset / 4);
  }
  return word.symbol_name();
}

std::string read_type(const Ref& object) {
  const auto& word = object.data->words_by_seg.at(object.seg).at(object.byte_offset / 4);
  if (word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    throw Error("read_type did not get a type (offset {} words)", object.byte_offset / 4);
  }
  return word.symbol_name();
}

std::string read_string_ref(const Ref& object) {
  const auto& word = object.data->words_by_seg.at(object.seg).at(object.byte_offset / 4);
  if (word.kind() != decompiler::LinkedWord::PTR) {
    throw Error("read_string_ref did not get a pointer (offset {} words)", object.byte_offset / 4);
  }
  const auto& label = object.data->labels.at(word.label_id());
  return object.data->get_goal_string_by_label(label);
}

std::string read_string_field(const TypedRef& object,
                              const std::string& field_name,
                              const decompiler::DecompilerTypeSystem& dts,
                              bool ignore_field_type) {
  FieldLookupInfo field_info = dts.ts.lookup_field_info(object.type->get_name(), field_name);

  if (field_info.field.is_dynamic() || field_info.field.is_array() ||
      field_info.field.is_inline()) {
    throw Error("Field {} is dynamic/array/inline and can't be used with read_string_field",
                field_name);
  }

  if (!ignore_field_type && field_info.type != TypeSpec("string")) {
    throw Error("Field {} has type {} and can't be used with read_string_field", field_name,
                field_info.type.print());
  }

  int byte_in_words = object.ref.byte_offset + field_info.field.offset();
  if ((byte_in_words % 4) != 0) {
    throw Error("Field {} was misaligned.", field_name);
  }
  const auto& word = object.ref.data->words_by_seg.at(object.ref.seg).at(byte_in_words / 4);

  if (word.kind() != decompiler::LinkedWord::PTR) {
    throw Error("read_string_field did not get a label (offset {} words)", byte_in_words / 4);
  }

  const auto& label = object.ref.data->labels.at(word.label_id());
  return object.ref.data->get_goal_string_by_label(label);
}

Ref get_field_ref(const TypedRef& object,
                  const std::string& field_name,
                  const decompiler::DecompilerTypeSystem& dts) {
  FieldLookupInfo field_info = dts.ts.lookup_field_info(object.type->get_name(), field_name);
  Ref result = object.ref;
  result.byte_offset += field_info.field.offset();
  return result;
}

std::string get_type_of_basic(const Ref& object) {
  int byte_in_words = object.byte_offset;
  if ((byte_in_words % 4) != 0) {
    throw Error("Basic in get_type_of_basic was misaligned.");
  }

  const auto& word = object.data->words_by_seg.at(object.seg).at(byte_in_words / 4);

  if (word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    throw Error("get_type_of_basic did not get a type tag (offset {} words)", byte_in_words / 4);
  }

  return word.symbol_name();
}

TypedRef typed_ref_from_basic(const Ref& object, const decompiler::DecompilerTypeSystem& dts) {
  int byte_in_words = object.byte_offset;
  if ((byte_in_words % 4) != 0) {
    throw Error("Basic in typed_ref_from_basic was misaligned.");
  }

  const auto& word = object.data->words_by_seg.at(object.seg).at(byte_in_words / 4);

  if (word.kind() != decompiler::LinkedWord::TYPE_PTR) {
    throw Error("typed_ref_from_basic did not get a type tag (offset {} words). Got {}",
                byte_in_words / 4, (int)word.kind());
  }

  TypedRef result;
  result.ref = object;
  result.type = dts.ts.lookup_type(word.symbol_name());
  return result;
}

Ref deref_label(const Ref& object) {
  int byte_in_words = object.byte_offset;
  if ((byte_in_words % 4) != 0) {
    throw Error("ptr in deref_label was misaligned.");
  }

  const auto& word = object.data->words_by_seg.at(object.seg).at(byte_in_words / 4);

  if (word.kind() != decompiler::LinkedWord::PTR) {
    throw Error("deref_label did not get a label (offset {} words). Got {} {}", byte_in_words / 4,
                (int)word.kind(), word.data);
  }

  const auto& lab = object.data->labels.at(word.label_id());

  Ref result;
  result.byte_offset = lab.offset;
  result.seg = lab.target_segment;
  result.data = object.data;
  return result;
}

std::string inspect_ref(const Ref& ref) {
  auto& word = ref.data->words_by_seg.at(ref.seg).at(ref.byte_offset / 4);
  switch (word.kind()) {
    case decompiler::LinkedWord::PLAIN_DATA:
      return fmt::format("[0x{:08x} (offset {} bytes)]", word.data, ref.byte_offset % 4);
    case decompiler::LinkedWord::PTR:
      return fmt::format("[{}]", ref.data->labels.at(word.label_id()).name);
    case decompiler::LinkedWord::SYM_PTR:
      return fmt::format("['{}]", word.symbol_name());
    case decompiler::LinkedWord::TYPE_PTR:
      return fmt::format("[t'{}]", word.symbol_name());
    default:
      ASSERT(false);
  }
}

u32 deref_u32(const Ref& ref, int word_offset) {
  if ((ref.byte_offset % 4) != 0) {
    throw Error("deref_u32 bad alignment");
  }
  const auto& word = ref.data->words_by_seg.at(ref.seg).at(word_offset + (ref.byte_offset / 4));
  if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
    throw Error("deref_u32 bad kind: {}", (int)word.kind());
  }
  return word.data;
}

float deref_float(const Ref& ref, int array_idx) {
  if ((ref.byte_offset % 4) != 0) {
    throw Error("deref_u32 bad alignment");
  }
  const auto& word = ref.data->words_by_seg.at(ref.seg).at(array_idx + (ref.byte_offset / 4));
  if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
    throw Error("deref_u32 bad kind: {}", (int)word.kind());
  }
  float ret;
  memcpy(&ret, &word.data, 4);
  return ret;
}

u64 deref_u64(const Ref& ref, int dw_offset) {
  if ((ref.byte_offset % 8) != 0) {
    throw Error("deref_u64 bad alignment");
  }
  const auto& word0 = ref.data->words_by_seg.at(ref.seg).at(ref.byte_offset / 4 + (dw_offset * 2));
  const auto& word1 =
      ref.data->words_by_seg.at(ref.seg).at(1 + (ref.byte_offset / 4) + (dw_offset * 2));

  if (word0.kind() != decompiler::LinkedWord::PLAIN_DATA) {
    throw Error("deref_u64 bad kind: {}", (int)word0.kind());
  }
  if (word1.kind() != decompiler::LinkedWord::PLAIN_DATA) {
    throw Error("deref_u64 bad kind: {}", (int)word1.kind());
  }
  u64 result = word1.data;
  result <<= 32;
  result |= word0.data;
  return result;
}

u16 deref_u16(const Ref& ref, int array_idx) {
  u32 u32_offset = array_idx / 2;
  u32 u32_val = deref_u32(ref, u32_offset);
  if (array_idx & 1) {
    return u32_val >> 16;
  } else {
    return (u16)u32_val;
  }
}

s8 deref_s8(const Ref& ref, int byte) {
  u32 u32_offset = byte / 4;
  u32 u32_val = deref_u32(ref, u32_offset);
  s8 vals[4];
  memcpy(vals, &u32_val, 4);
  return vals[byte & 3];
}

u8 deref_u8(const Ref& ref, int byte) {
  u32 total_offset = ref.byte_offset + byte;
  const auto& word = ref.data->words_by_seg.at(ref.seg).at(total_offset / 4);
  if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
    throw Error("deref_u32 bad kind: {}", (int)word.kind());
  }
  u8 vals[4];
  memcpy(vals, &word.data, 4);
  return vals[total_offset & 3];
}

std::vector<int> find_objects_with_type(const decompiler::LinkedObjectFile& file,
                                        const std::string& name) {
  std::vector<int> result;
  for (size_t i = 0; i < file.words_by_seg.at(0).size(); i++) {
    const auto& word = file.words_by_seg[0][i];
    if (word.kind() == decompiler::LinkedWord::TYPE_PTR && word.symbol_name() == name) {
      result.push_back(i);
    }
  }
  return result;
}