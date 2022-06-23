#pragma once

#include <optional>

#include "common/goos/Object.h"
#include "common/type_system/TypeSpec.h"
#include "common/type_system/TypeSystem.h"

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
                                const LinkedObjectFile* file);
goos::Object decompile_at_label_with_hint(const LabelInfo& hint,
                                          const DecompilerLabel& label,
                                          const std::vector<DecompilerLabel>& labels,
                                          const std::vector<std::vector<LinkedWord>>& words,
                                          DecompilerTypeSystem& dts,
                                          const LinkedObjectFile* file);
goos::Object decompile_at_label_guess_type(const DecompilerLabel& label,
                                           const std::vector<DecompilerLabel>& labels,
                                           const std::vector<std::vector<LinkedWord>>& words,
                                           const TypeSystem& ts,
                                           const LinkedObjectFile* file);
goos::Object decompile_structure(const TypeSpec& actual_type,
                                 const DecompilerLabel& label,
                                 const std::vector<DecompilerLabel>& labels,
                                 const std::vector<std::vector<LinkedWord>>& words,
                                 const TypeSystem& ts,
                                 const LinkedObjectFile* file,
                                 bool use_fancy_macros);
goos::Object decompile_pair(const DecompilerLabel& label,
                            const std::vector<DecompilerLabel>& labels,
                            const std::vector<std::vector<LinkedWord>>& words,
                            const TypeSystem& ts,
                            bool add_quote,
                            const LinkedObjectFile* file);
goos::Object decompile_boxed_array(const DecompilerLabel& label,
                                   const std::vector<DecompilerLabel>& labels,
                                   const std::vector<std::vector<LinkedWord>>& words,
                                   const TypeSystem& ts,
                                   const LinkedObjectFile* file,
                                   const std::optional<TypeSpec>& content_type_override);
goos::Object decompile_value(const TypeSpec& type,
                             const std::vector<u8>& bytes,
                             const TypeSystem& ts);
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
}  // namespace decompiler
