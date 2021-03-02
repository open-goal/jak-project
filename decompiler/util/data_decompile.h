#pragma once

#include <optional>
#include "common/goos/Object.h"
#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/ObjectFile/LinkedWord.h"
#include "common/type_system/TypeSpec.h"
#include "common/type_system/TypeSystem.h"
#include "decompiler/config.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
std::optional<TypeSpec> get_type_of_label(const DecompilerLabel& label,
                                          const std::vector<std::vector<LinkedWord>>& words);

goos::Object decompile_string_at_label(const DecompilerLabel& label,
                                       const std::vector<std::vector<LinkedWord>>& words);
goos::Object decompile_at_label(const TypeSpec& type,
                                const DecompilerLabel& label,
                                const std::vector<DecompilerLabel>& labels,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts);
goos::Object decompile_at_label_with_hint(const LabelType& hint,
                                          const DecompilerLabel& label,
                                          const std::vector<DecompilerLabel>& labels,
                                          const std::vector<std::vector<LinkedWord>>& words,
                                          DecompilerTypeSystem& dts);
goos::Object decompile_at_label_guess_type(const DecompilerLabel& label,
                                           const std::vector<DecompilerLabel>& labels,
                                           const std::vector<std::vector<LinkedWord>>& words,
                                           const TypeSystem& ts);
goos::Object decompile_structure(const TypeSpec& actual_type,
                                 const DecompilerLabel& label,
                                 const std::vector<DecompilerLabel>& labels,
                                 const std::vector<std::vector<LinkedWord>>& words,
                                 const TypeSystem& ts);
goos::Object decompile_pair(const DecompilerLabel& label,
                            const std::vector<DecompilerLabel>& labels,
                            const std::vector<std::vector<LinkedWord>>& words,
                            const TypeSystem& ts);
goos::Object decompile_boxed_array(const DecompilerLabel& label,
                                   const std::vector<DecompilerLabel>& labels,
                                   const std::vector<std::vector<LinkedWord>>& words,
                                   const TypeSystem& ts);
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
}  // namespace decompiler
