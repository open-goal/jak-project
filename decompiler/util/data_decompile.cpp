#include "data_decompile.h"
#include "third-party/fmt/core.h"
#include "common/goos/PrettyPrinter.h"

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
    auto type_ptr_word_idx = (label.offset / 4) - 1;
    auto& type_ptr = words.at(label.target_segment).at(type_ptr_word_idx);
    if (type_ptr.kind != LinkedWord::TYPE_PTR) {
      return {};
    }
    if (type_ptr.symbol_name == "array") {
      auto content_type_ptr_word_idx = type_ptr_word_idx + 3;
      auto& content_type_ptr = words.at(label.target_segment).at(content_type_ptr_word_idx);
      if (content_type_ptr.kind != LinkedWord::TYPE_PTR) {
        return {};
      }
      return TypeSpec("array", {TypeSpec(content_type_ptr.symbol_name)});
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
                                           const std::vector<DecompilerLabel>& labels,
                                           const std::vector<std::vector<LinkedWord>>& words,
                                           const TypeSystem& ts) {
  auto guessed_type = get_type_of_label(label, words);
  if (!guessed_type.has_value()) {
    throw std::runtime_error("Couldn't guess the type of " + label.name);
  }
  return decompile_at_label(*guessed_type, label, labels, words, ts);
}

/*!
 * Attempt to decompile data of the given type at the given label. If the decompiler thinks the
 * types do not line up, it will fail.
 */
goos::Object decompile_at_label(const TypeSpec& type,
                                const DecompilerLabel& label,
                                const std::vector<DecompilerLabel>& labels,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts) {
  if (type == TypeSpec("string")) {
    return decompile_string_at_label(label, words);
  }

  if (ts.typecheck(TypeSpec("array"), type, "", false, false)) {
    return decompile_boxed_array(label, labels, words, ts);
  }

  if (ts.typecheck(TypeSpec("structure"), type, "", false, false)) {
    return decompile_structure(type, label, labels, words, ts);
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

  const auto& type_ptr = words.at(label.target_segment).at((label.offset - 4) / 4);
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

goos::Object decompile_structure(const TypeSpec& type,
                                 const DecompilerLabel& label,
                                 const std::vector<DecompilerLabel>& labels,
                                 const std::vector<std::vector<LinkedWord>>& words,
                                 const TypeSystem& ts) {
  // first step, get type info and words
  auto uncast_type_info = ts.lookup_type(type);
  auto type_info = dynamic_cast<StructureType*>(uncast_type_info);
  if (!type_info) {
    throw std::runtime_error(fmt::format("Type {} wasn't a structure type.", type.print()));
  }

  int word_count = (type_info->get_size_in_memory() + 3) / 4;

  // check alignment
  auto offset_location = label.offset - type_info->get_offset();
  if (offset_location % 8) {
    throw std::runtime_error(
        fmt::format("Structure type {} (type offset {}) has alignment {}, which is not valid.",
                    type_info->get_name(), type_info->get_offset(), (offset_location % 8)));
  }

  // check enough room
  if (int(words.at(label.target_segment).size()) < word_count + offset_location / 4) {
    throw std::runtime_error(fmt::format("Structure type {} takes up {} bytes and doesn't fit.",
                                         type_info->get_name(), type_info->get_size_in_memory()));
  }

  // get words for real
  std::vector<LinkedWord> obj_words;
  obj_words.insert(obj_words.begin(),
                   words.at(label.target_segment).begin() + (offset_location / 4),
                   words.at(label.target_segment).begin() + (offset_location / 4) + word_count);

  // status of each byte.
  enum ByteStatus : u8 { ZERO_UNREAD, HAS_DATA_UNREAD, ZERO_READ, HAS_DATA_READ };
  std::vector<int> field_status_per_byte;
  for (int i = 0; i < word_count; i++) {
    auto& w = obj_words.at(i);
    switch (w.kind) {
      case LinkedWord::TYPE_PTR:
      case LinkedWord::PTR:
      case LinkedWord::SYM_PTR:
        field_status_per_byte.push_back(HAS_DATA_UNREAD);
        field_status_per_byte.push_back(HAS_DATA_UNREAD);
        field_status_per_byte.push_back(HAS_DATA_UNREAD);
        field_status_per_byte.push_back(HAS_DATA_UNREAD);
        break;
      case LinkedWord::PLAIN_DATA: {
        u8 bytes[4];
        memcpy(bytes, &w.data, 4);
        for (auto b : bytes) {
          field_status_per_byte.push_back(b ? HAS_DATA_UNREAD : ZERO_UNREAD);
        }
      } break;
      default:
        throw std::runtime_error("Unsupported word in static data");
    }
  }

  std::vector<std::pair<std::string, goos::Object>> field_defs_out;
  // now iterate over fields:
  for (auto& field : type_info->fields()) {
    // first, let's see if this overlaps with anything:
    auto field_start = field.offset();
    auto field_end = field_start + ts.get_size_in_type(field);
    bool all_zero = true;
    bool any_overlap = false;
    for (int i = field_start; i < field_end; i++) {
      auto status = field_status_per_byte.at(i);
      if (status != ZERO_UNREAD && status != ZERO_READ) {
        all_zero = false;
      }
      if (status == HAS_DATA_READ || status == ZERO_READ) {
        any_overlap = true;
      }
    }

    if (all_zero) {
      // field has nothing in it, just skip it.
      continue;
    }

    if (any_overlap) {
      // for now, let's just skip fields that overlapped with the previous.
      // eventually we should do something smarter here...
      continue;
    }

    // OK - READ THE FIELD:
    for (int i = field_start; i < field_end; i++) {
      // even if our field was partially zero, we mark those zero bytes as "has data".
      field_status_per_byte.at(i) = HAS_DATA_READ;
    }

    // first, let's see if it's a value or reference (todo, where does pair fit in here...)
    auto field_type_info = ts.lookup_type(field.type());
    if (!field_type_info->is_reference()) {
      // value type. need to get bytes.
      assert(!field.is_inline());
      if (field.is_array()) {
        throw std::runtime_error(
            fmt::format("Value field array {} not yet implemented", field.name()));
      } else if (field.is_dynamic()) {
        throw std::runtime_error(
            fmt::format("Dynamic value field {} in static data not yet implemented", field.name()));
      } else {
        std::vector<u8> bytes_out;
        for (int byte_idx = field_start; byte_idx < field_end; byte_idx++) {
          bytes_out.push_back(obj_words.at(byte_idx / 4).get_byte(byte_idx % 4));
        }
        field_defs_out.emplace_back(field.name(), decompile_value(field.type(), bytes_out, ts));
      }

    } else {
      if (field.is_dynamic() || field.is_array() || field.is_inline()) {
        throw std::runtime_error(fmt::format(
            "Dynamic/array/inline reference field {} in static data not yet implemented",
            field.name()));
      } else {
        // then we expect a label.
        assert(field_end - field_start == 4);
        auto& word = obj_words.at(field_start / 4);
        if (word.kind != LinkedWord::PTR) {
          throw std::runtime_error(
              fmt::format("Field {} did not have a proper reference", field.name()));
        }

        field_defs_out.emplace_back(
            field.name(),
            decompile_at_label(field.type(), labels.at(word.label_id), labels, words, ts));
      }
    }
  }

  for (size_t i = 0; i < field_status_per_byte.size(); i++) {
    if (field_status_per_byte.at(i) == HAS_DATA_UNREAD) {
      throw std::runtime_error(
          fmt::format("In structure of type {} at label {} offset {}, there was unknown data.",
                      type.print(), label.name, i));
    }
  }

  std::vector<goos::Object> result_def = {
      pretty_print::to_symbol("new"), pretty_print::to_symbol("'static"),
      pretty_print::to_symbol(fmt::format("'{}", type.print()))};
  for (auto& f : field_defs_out) {
    result_def.push_back(pretty_print::to_symbol(fmt::format(":{}", f.first)));
    result_def.push_back(f.second);
  }
  return pretty_print::build_list(result_def);
}

goos::Object decompile_value(const TypeSpec& type,
                             const std::vector<u8>& bytes,
                             const TypeSystem& ts) {
  // try as common integer types:
  if (ts.typecheck(TypeSpec("uint32"), type, "", false, false)) {
    assert(bytes.size() == 4);
    u32 value;
    memcpy(&value, bytes.data(), 4);
    return pretty_print::to_symbol(fmt::format("#x{:x}", u64(value)));
  } else {
    throw std::runtime_error(fmt::format("decompile_value failed on a {}", type.print()));
  }
}

goos::Object decompile_boxed_array(const DecompilerLabel& label,
                                   const std::vector<DecompilerLabel>& labels,
                                   const std::vector<std::vector<LinkedWord>>& words,
                                   const TypeSystem& ts) {
  TypeSpec content_type;
  auto type_ptr_word_idx = (label.offset / 4) - 1;
  if ((label.offset % 8) == 4) {
    auto& type_ptr = words.at(label.target_segment).at(type_ptr_word_idx);
    if (type_ptr.kind != LinkedWord::TYPE_PTR) {
      throw std::runtime_error("Invalid basic in decompile_boxed_array");
    }
    if (type_ptr.symbol_name == "array") {
      auto content_type_ptr_word_idx = type_ptr_word_idx + 3;
      auto& content_type_ptr = words.at(label.target_segment).at(content_type_ptr_word_idx);
      if (content_type_ptr.kind != LinkedWord::TYPE_PTR) {
        throw std::runtime_error("Invalid content in decompile_boxed_array");
      }
      content_type = TypeSpec(content_type_ptr.symbol_name);
    } else {
      throw std::runtime_error("Wrong basic type in decompile_boxed_array");
    }
  } else {
    throw std::runtime_error("Invalid alignment in decompile_boxed_array");
  }

  // now get the size
  auto& size_word_1 = words.at(label.target_segment).at(type_ptr_word_idx + 1);
  auto& size_word_2 = words.at(label.target_segment).at(type_ptr_word_idx + 2);
  auto first_elt_word_idx = type_ptr_word_idx + 4;

  if (size_word_1.kind != LinkedWord::PLAIN_DATA || size_word_2.kind != LinkedWord::PLAIN_DATA) {
    throw std::runtime_error("Invalid size in decompile_boxed_array");
  }

  if (size_word_1.data != size_word_2.data) {
    throw std::runtime_error("Inconsistent size in decompile_boxed_array");
  }

  int array_length = size_word_1.data;

  auto content_type_info = ts.lookup_type(content_type);
  if (content_type_info->is_reference()) {
    // easy, stride of 4.
    std::vector<goos::Object> result = {
        pretty_print::to_symbol("new"), pretty_print::to_symbol("'static"),
        pretty_print::to_symbol("'boxed-array"), pretty_print::to_symbol(content_type.print()),
        pretty_print::to_symbol(fmt::format("{}", array_length))};

    for (int elt = 0; elt < array_length; elt++) {
      auto& word = words.at(label.target_segment).at(first_elt_word_idx + elt);
      if (word.kind == LinkedWord::PLAIN_DATA && word.data == 0) {
        result.push_back(pretty_print::to_symbol("0"));
      } else if (word.kind == LinkedWord::PTR) {
        result.push_back(
            decompile_at_label(content_type, labels.at(word.label_id), labels, words, ts));
      } else {
        throw std::runtime_error(
            fmt::format("Unknown content type in boxed array of references, word idx {}",
                        first_elt_word_idx + elt));
      }
    }

    return pretty_print::build_list(result);
  } else {
    // value type.
    throw std::runtime_error("boxed value type array decompile not yet implemented.");
  }
}

}  // namespace decompiler