#pragma once

#include <optional>

#include "common/goos/Object.h"
#include "common/type_system/TypeSpec.h"
#include "common/type_system/TypeSystem.h"
#include "common/versions/versions.h"

#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/IR2/LabelDB.h"
#include "decompiler/ObjectFile/LinkedWord.h"
#include "decompiler/config.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
class LinkedObjectFile;
std::optional<TypeSpec> get_type_of_label(const DecompilerLabel& label,
                                          const std::vector<std::vector<LinkedWord>>& words);

goos::Object decompile_string_at_label(const DecompilerLabel& label,
                                       const std::vector<std::vector<LinkedWord>>& words);
goos::Object decompile_at_label(const TypeSpec& type,
                                const DecompilerLabel& label,
                                const std::vector<DecompilerLabel>& labels,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts,
                                const LinkedObjectFile* file,
                                GameVersion version,
                                bool in_static_pair = false);
goos::Object decompile_at_label_with_hint(const LabelInfo& hint,
                                          const DecompilerLabel& label,
                                          const std::vector<DecompilerLabel>& labels,
                                          const std::vector<std::vector<LinkedWord>>& words,
                                          const TypeSystem& dts,
                                          const LinkedObjectFile* file,
                                          GameVersion version);
goos::Object decompile_at_label_guess_type(const DecompilerLabel& label,
                                           const std::vector<DecompilerLabel>& labels,
                                           const std::vector<std::vector<LinkedWord>>& words,
                                           const TypeSystem& ts,
                                           const LinkedObjectFile* file,
                                           GameVersion version);
goos::Object decompile_structure(const TypeSpec& actual_type,
                                 const DecompilerLabel& label,
                                 const std::vector<DecompilerLabel>& labels,
                                 const std::vector<std::vector<LinkedWord>>& words,
                                 const TypeSystem& ts,
                                 const LinkedObjectFile* file,
                                 bool use_fancy_macros,
                                 GameVersion version);
goos::Object decompile_pair(const DecompilerLabel& label,
                            const std::vector<DecompilerLabel>& labels,
                            const std::vector<std::vector<LinkedWord>>& words,
                            const TypeSystem& ts,
                            bool add_quote,
                            const LinkedObjectFile* file,
                            GameVersion version);
goos::Object decompile_boxed_array(const TypeSpec& type,
                                   const DecompilerLabel& label,
                                   const std::vector<DecompilerLabel>& labels,
                                   const std::vector<std::vector<LinkedWord>>& words,
                                   const TypeSystem& ts,
                                   const LinkedObjectFile* file,
                                   const std::optional<TypeSpec>& content_type_override,
                                   GameVersion version);
goos::Object decompile_value(const TypeSpec& type,
                             const std::vector<u8>& bytes,
                             const TypeSystem& ts,
                             const std::optional<TypeSpec> decomp_as_type = std::nullopt);
goos::Object decompile_value_array(const TypeSpec& elt_type,
                                   const Type* elt_type_info,
                                   int length,
                                   int stride,
                                   int offset,
                                   const std::vector<LinkedWord>& obj_words,
                                   const TypeSystem& ts);
goos::Object decompile_bitfield(const TypeSpec& type,
                                const BitFieldType* type_info,
                                const DecompilerLabel& label,
                                const std::vector<DecompilerLabel>& labels,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts);

struct BitFieldConstantDef {
  bool is_signed = false;
  bool is_float = false;
  u64 value = -1;
  std::optional<std::string> enum_constant;
  std::string field_name;

  struct NestedField {
    TypeSpec field_type;
    std::vector<BitFieldConstantDef> fields;
  };

  std::optional<NestedField> nested_field;
};

template <typename T>
T extract_bitfield(T input, int start_bit, int size) {
  int end_bit = start_bit + size;
  T left_shifted = input << (64 - end_bit);
  return left_shifted >> (64 - size);
}

std::vector<BitFieldConstantDef> decompile_bitfield_from_int(const TypeSpec& type,
                                                             const TypeSystem& ts,
                                                             u64 value);

std::optional<std::vector<BitFieldConstantDef>> try_decompile_bitfield_from_int(
    const TypeSpec& type,
    const TypeSystem& ts,
    u64 value,
    bool require_success,
    std::optional<int> offset);

std::vector<std::string> decompile_bitfield_enum_from_int(const TypeSpec& type,
                                                          const TypeSystem& ts,
                                                          u64 value);
std::string decompile_int_enum_from_int(const TypeSpec& type, const TypeSystem& ts, u64 value);
goos::Object bitfield_defs_print(const TypeSpec& type,
                                 const std::vector<BitFieldConstantDef>& defs);

struct ArrayFieldDecompMeta {
  enum class Kind { REF_TO_INLINE_ARR, REF_TO_INTEGER_ARR };

  TypeSpec element_type;
  int bytes_per_element;  // aka stride
  Kind kind;

  ArrayFieldDecompMeta(TypeSpec _element_type,
                       int _bytes_per_element,
                       Kind _kind = Kind::REF_TO_INLINE_ARR)
      : element_type(_element_type), bytes_per_element(_bytes_per_element), kind(_kind){};
};

extern const std::unordered_map<
    GameVersion,
    std::unordered_map<std::string, std::unordered_map<std::string, ArrayFieldDecompMeta>>>
    array_field_decomp_special_cases;
}  // namespace decompiler
