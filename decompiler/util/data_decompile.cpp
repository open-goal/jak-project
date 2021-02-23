#include "data_decompile.h"
#include "third-party/fmt/core.h"

namespace decompiler {

/*!
 * Attempt to determine the type of this label. This does not make sure that the type system
 * actually knows about the type. If the thing is not a basic or pair, it will fail.
 */
std::optional<TypeSpec> get_type_of_label(const DecompilerLabel& label,
                                          const std::vector<std::vector<LinkedWord>>& words) {
  if ((label.offset % 8) == 2) {
    return TypeSpec("pair");
  }

  // try to guess the type by looking for a type pointer.
  if (label.offset < 4) {
    return {};
  }

  if ((label.offset % 8) == 4) {
    auto& type_ptr = words.at(label.target_segment).at((label.offset / 4) - 1);
    if (type_ptr.kind != LinkedWord::TYPE_PTR) {
      return {};
    }
    return TypeSpec(type_ptr.symbol_name);
  } else {
    return {};
  }
}

/*!
 * Attempt to decompile data at the given label, without knowing the type.  This can only succeed
 * if the object is a basic or pair, and is intended to save the user time in these cases,
 * or even be run automatically.
 */
goos::Object decompile_at_label_guess_type(const DecompilerLabel& label,
                                           const std::vector<std::vector<LinkedWord>>& words,
                                           const TypeSystem& ts) {
  auto guessed_type = get_type_of_label(label, words);
  if (!guessed_type.has_value()) {
    throw std::runtime_error("Couldn't guess the type of " + label.name);
  }
  return decompile_at_label(*guessed_type, label, words, ts);
}

/*!
 * Attempt to decompile data of the given type at the given label. If the decompiler thinks the
 * types do not line up, it will fail.
 */
goos::Object decompile_at_label(const TypeSpec& type,
                                const DecompilerLabel& label,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts) {
  if (type == TypeSpec("string")) {
    return decompile_string_at_label(label, words);
  }

  throw std::runtime_error("Unimplemented decompile_at_label for " + type.print());
}

/*!
 * Special case to decompile a string into a string constant.
 */
goos::Object decompile_string_at_label(const DecompilerLabel& label,
                                       const std::vector<std::vector<LinkedWord>>& words) {
  // first, check that it's actually a string.
  if (label.offset % 4) {
    throw std::runtime_error(fmt::format("Cannot get string at label {}, alignment of label is {}",
                                         label.name, label.offset));
  }
  assert(label.offset >= 4);

  const auto& type_ptr = words.at(label.target_segment).at(label.offset - 4);
  if (type_ptr.kind != LinkedWord::TYPE_PTR) {
    throw std::runtime_error(fmt::format(
        "Cannot get string at label {}, word before is not a type pointer.", label.name));
  }

  if (type_ptr.symbol_name != "string") {
    throw std::runtime_error(fmt::format("Cannot get string at label {}, type pointer is for a {}.",
                                         label.name, type_ptr.symbol_name));
  }

  std::string result;

  auto word_idx = (label.offset / 4) - 1;
  // next should be the size
  if (word_idx + 1 >= int(words.at(label.target_segment).size())) {
    throw std::runtime_error(
        fmt::format("Cannot get string at label {}, not enough room", label.name));
  }
  const LinkedWord& size_word = words.at(label.target_segment).at(word_idx + 1);
  if (size_word.kind != LinkedWord::PLAIN_DATA) {
    // sometimes an array of string pointer triggers this!
    throw std::runtime_error(
        fmt::format("Cannot get string at label {}, size is not plain data.", label.name));
  }

  // now characters...
  for (size_t i = 0; i < size_word.data; i++) {
    int word_offset = word_idx + 2 + (i / 4);
    int byte_offset = i % 4;
    auto& word = words.at(label.target_segment).at(word_offset);
    if (word.kind != LinkedWord::PLAIN_DATA) {
      throw std::runtime_error(
          fmt::format("Cannot get string at label {}, character is not plain data.", label.name));
    }
    char cword[4];
    memcpy(cword, &word.data, 4);
    result += cword[byte_offset];
    assert(result.back() != 0);
  }
  return goos::StringObject::make_new(result);
}

// goos::Object decompile_structure(const TypeSpec& type,
//                                 const DecompilerLabel& label,
//                                 const std::vector<std::vector<LinkedWord>>& words,
//                                 const TypeSystem& ts) {
//  // first step, get type info and words
//  auto uncast_type_info = ts.lookup_type(type);
//  auto type_info = dynamic_cast<StructureType*>(uncast_type_info);
//  if (!type_info) {
//    throw std::runtime_error(fmt::format("Type {} wasn't a structure type.", type.print()));
//  }
//
//  int word_count = (type_info->get_size_in_memory() + 3) / 4;
//
//  // check alignment
//  auto offset_location = label.offset - type_info->get_offset();
//  if (offset_location % 8) {
//    throw std::runtime_error(
//        fmt::format("Structure type {} (type offset {}) has alignment {}, which is not valid.",
//                    type_info->get_name(), type_info->get_offset(), (offset_location % 8)));
//  }
//
//  // check enough room
//  if (int(words.at(label.target_segment).size()) < word_count + offset_location / 4) {
//    throw std::runtime_error(fmt::format("Structure type {} takes up {} bytes and doesn't fit.",
//                                         type_info->get_name(), type_info->get_size_in_memory()));
//  }
//
//  // get words for real
//  std::vector<LinkedWord> obj_words;
//  obj_words.insert(obj_words.begin(), words.begin() + (offset_location / 4),
//                   words.begin() + (offset_location / 4) + word_count);
//
//
//  std::vector<int> field_count_per_byte;
//
//  return goos::Object::make_integer(1);
//}
}  // namespace decompiler