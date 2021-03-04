#include "data_decompile.h"
#include "third-party/fmt/core.h"
#include "common/goos/PrettyPrinter.h"
#include "common/util/math_util.h"
#include "common/log/log.h"

namespace decompiler {

/*!
 * Entry point from the decompiler to decompile data.
 */
goos::Object decompile_at_label_with_hint(const LabelType& hint,
                                          const DecompilerLabel& label,
                                          const std::vector<DecompilerLabel>& labels,
                                          const std::vector<std::vector<LinkedWord>>& words,
                                          DecompilerTypeSystem& dts) {
  auto type = dts.parse_type_spec(hint.type_name);
  if (!hint.array_size.has_value()) {
    // if we don't have an array size, treat it as just a normal type.
    return decompile_at_label(type, label, labels, words, dts.ts);
  }

  if (type.base_type() == "pointer") {
    auto field_type_info = dts.ts.lookup_type(type.get_single_arg());
    if (field_type_info->is_reference()) {
      throw std::runtime_error(
          fmt::format("Type {} is not yet supported by the data decompiler.", hint.type_name));
    } else {
      auto stride = field_type_info->get_size_in_memory();

      int word_count = ((stride * (*hint.array_size)) + 3) / 4;
      std::vector<LinkedWord> obj_words;
      obj_words.insert(obj_words.begin(),
                       words.at(label.target_segment).begin() + (label.offset / 4),
                       words.at(label.target_segment).begin() + (label.offset / 4) + word_count);

      return decompile_value_array(type.get_single_arg(), field_type_info, *hint.array_size, stride,
                                   0, obj_words, dts.ts);
    }
  }

  if (type.base_type() == "inline-array") {
    auto field_type_info = dts.ts.lookup_type(type.get_single_arg());
    if (!field_type_info->is_reference()) {
      throw std::runtime_error(
          fmt::format("Type {} is invalid, the element type is not inlineable.", hint.type_name));
    } else {
      // it's an inline array.  let's figure out the len and stride
      auto len = *hint.array_size;
      // TODO - having this logic here isn't great.
      auto stride = align(field_type_info->get_size_in_memory(),
                          field_type_info->get_inline_array_stride_alignment());

      if (dynamic_cast<BasicType*>(field_type_info)) {
        throw std::runtime_error("Plan basic arrays not supported yet");
        // I just want to double check offsets....
      }

      std::vector<goos::Object> array_def = {pretty_print::to_symbol(fmt::format(
          "new 'static 'inline-array '{} {}", type.get_single_arg().print(), *hint.array_size))};
      for (int elt = 0; elt < len; elt++) {
        DecompilerLabel fake_label;
        fake_label.target_segment = label.target_segment;
        fake_label.offset = label.offset + field_type_info->get_offset() + stride * elt;
        fake_label.name = fmt::format("fake-label-{}-elt-{}", type.get_single_arg().print(), elt);
        array_def.push_back(
            decompile_at_label(type.get_single_arg(), fake_label, labels, words, dts.ts));
      }
      return pretty_print::build_list(array_def);
    }
  }

  throw std::runtime_error(
      fmt::format("Type {} is not yet supported by the data decompiler.", hint.type_name));
}

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

  if (ts.tc(TypeSpec("array"), type)) {
    return decompile_boxed_array(label, labels, words, ts);
  }

  if (ts.tc(TypeSpec("structure"), type)) {
    return decompile_structure(type, label, labels, words, ts);
  }

  if (type == TypeSpec("pair")) {
    return decompile_pair(label, labels, words, ts);
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

goos::Object decompile_value_array(const TypeSpec& elt_type,
                                   const Type* elt_type_info,
                                   int length,
                                   int stride,
                                   int offset,
                                   const std::vector<LinkedWord>& obj_words,
                                   const TypeSystem& ts) {
  std::vector<goos::Object> array_def = {
      pretty_print::to_symbol(fmt::format("new 'static 'array '{} {}", elt_type.print(), length))};

  for (int i = 0; i < length; i++) {
    auto start = offset + stride * i;
    auto end = start + elt_type_info->get_size_in_memory();
    std::vector<u8> elt_bytes;
    for (int j = start; j < end; j++) {
      auto& word = obj_words.at(j / 4);
      if (word.kind != LinkedWord::PLAIN_DATA) {
        throw std::runtime_error("Got bad word in kind in array of values");
      }
      elt_bytes.push_back(word.get_byte(j % 4));
    }
    array_def.push_back(decompile_value(elt_type, elt_bytes, ts));
  }

  return pretty_print::build_list(array_def);
}

goos::Object decompile_structure(const TypeSpec& type,
                                 const DecompilerLabel& label,
                                 const std::vector<DecompilerLabel>& labels,
                                 const std::vector<std::vector<LinkedWord>>& words,
                                 const TypeSystem& ts) {
  // first step, get type info and words
  TypeSpec actual_type = type;
  auto uncast_type_info = ts.lookup_type(actual_type);
  auto type_info = dynamic_cast<StructureType*>(uncast_type_info);
  if (!type_info) {
    throw std::runtime_error(fmt::format("Type {} wasn't a structure type.", actual_type.print()));
  }
  bool is_basic = dynamic_cast<BasicType*>(uncast_type_info);

  int word_count = (type_info->get_size_in_memory() + 3) / 4;

  // check alignment
  auto offset_location = label.offset - type_info->get_offset();
  if ((offset_location % 8) == 2) {
    // TEMP HACK
    lg::error("Data decompile was looking for a structure, but it looks like a pair instead.");
    return decompile_pair(label, labels, words, ts);
  }
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
      case LinkedWord::EMPTY_PTR:
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
  int idx = 0;
  for (auto& field : type_info->fields()) {
    if (field.skip_in_decomp()) {
      idx++;
      continue;
    }
    if (is_basic && idx == 0) {
      assert(field.name() == "type" && field.offset() == 0);
      auto& word = obj_words.at(0);
      if (word.kind != LinkedWord::TYPE_PTR) {
        throw std::runtime_error("Basic doesn't start with type pointer");
      }

      if (word.symbol_name != actual_type.base_type()) {
        // we can specify a more specific type.
        auto got_type = TypeSpec(word.symbol_name);
        if (ts.tc(actual_type, got_type)) {
          lg::info("For type {}, got more specific type {}\n", actual_type.print(),
                   got_type.print());
          actual_type = got_type;
          if (actual_type == TypeSpec("string")) {
            return decompile_string_at_label(label, words);
          }
        } else {
          throw std::runtime_error(
              fmt::format("Basic has the wrong type pointer, got {} expected {}", word.symbol_name,
                          actual_type.base_type()));
        }
      }
      for (int k = 0; k < 4; k++) {
        field_status_per_byte.at(k) = HAS_DATA_READ;
      }
      idx++;
      continue;
    }
    idx++;
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

    // first, let's see if it's a value or reference
    auto field_type_info = ts.lookup_type(field.type());
    if (!field_type_info->is_reference()) {
      // value type. need to get bytes.
      assert(!field.is_inline());
      if (field.is_array()) {
        // array of values.
        auto len = field.array_size();
        auto stride = ts.get_size_in_type(field) / len;
        assert(stride == field_type_info->get_size_in_memory());

        field_defs_out.emplace_back(
            field.name(), decompile_value_array(field.type(), field_type_info, len, stride,
                                                field_start, obj_words, ts));
      } else if (field.is_dynamic()) {
        throw std::runtime_error(
            fmt::format("Dynamic value field {} in static data type {} not yet implemented",
                        field.name(), actual_type.print()));
      } else {
        std::vector<u8> bytes_out;
        for (int byte_idx = field_start; byte_idx < field_end; byte_idx++) {
          bytes_out.push_back(obj_words.at(byte_idx / 4).get_byte(byte_idx % 4));
        }
        field_defs_out.emplace_back(field.name(), decompile_value(field.type(), bytes_out, ts));
      }

    } else {
      if (!field.is_dynamic() && !field.is_array() && field.is_inline()) {
        // inline structure!
        DecompilerLabel fake_label;
        fake_label.target_segment = label.target_segment;
        // offset from real start of outer + field offset + tag, we want to fake that.
        fake_label.offset = offset_location + field.offset() + field_type_info->get_offset();
        fake_label.name = fmt::format("fake-label-{}-{}", actual_type.print(), field.name());
        field_defs_out.emplace_back(
            field.name(), decompile_at_label(field.type(), fake_label, labels, words, ts));
      } else if (!field.is_dynamic() && field.is_array() && field.is_inline()) {
        // it's an inline array.  let's figure out the len and stride
        auto len = field.array_size();
        auto total_size = ts.get_size_in_type(field);
        auto stride = total_size / len;
        assert(stride * len == total_size);
        assert(stride == align(field_type_info->get_size_in_memory(),
                               field_type_info->get_inline_array_stride_alignment()));

        std::vector<goos::Object> array_def = {pretty_print::to_symbol(fmt::format(
            "new 'static 'inline-array '{} {}", field.type().print(), field.array_size()))};
        for (int elt = 0; elt < len; elt++) {
          DecompilerLabel fake_label;
          fake_label.target_segment = label.target_segment;
          // offset from real start of outer + field offset + tag, we want to fake that.
          fake_label.offset =
              offset_location + field.offset() + field_type_info->get_offset() + stride * elt;
          fake_label.name =
              fmt::format("fake-label-{}-{}-elt-{}", actual_type.print(), field.name(), elt);
          array_def.push_back(decompile_at_label(field.type(), fake_label, labels, words, ts));
        }
        field_defs_out.emplace_back(field.name(), pretty_print::build_list(array_def));
      } else if (!field.is_dynamic() && field.is_array() && !field.is_inline()) {
        auto len = field.array_size();
        auto total_size = ts.get_size_in_type(field);
        auto stride = total_size / len;
        assert(stride * len == total_size);
        assert(stride == 4);

        std::vector<goos::Object> array_def = {pretty_print::to_symbol(
            fmt::format("new 'static 'array '{} {}", field.type().print(), field.array_size()))};
        for (int elt = 0; elt < len; elt++) {
          auto& word = obj_words.at((field_start / 4) + elt);

          if (word.kind == LinkedWord::PTR) {
            array_def.push_back(
                decompile_at_label(field.type(), labels.at(word.label_id), labels, words, ts));
          } else if (word.kind == LinkedWord::PLAIN_DATA && word.data == 0) {
            // do nothing, the default is zero?
            array_def.push_back(pretty_print::to_symbol("0"));
          } else if (word.kind == LinkedWord::SYM_PTR) {
            if (word.symbol_name == "#f" || word.symbol_name == "#t") {
              array_def.push_back(pretty_print::to_symbol(fmt::format("{}", word.symbol_name)));
            } else {
              array_def.push_back(pretty_print::to_symbol(fmt::format("'{}", word.symbol_name)));
            }
          } else if (word.kind == LinkedWord::EMPTY_PTR) {
            array_def.push_back(pretty_print::to_symbol("'()"));
          } else {
            throw std::runtime_error(
                fmt::format("Field {} in type {} offset {} did not have a proper reference for "
                            "array element {}",
                            field.name(), actual_type.print(), field.offset(), elt));
          }
        }
        field_defs_out.emplace_back(field.name(), pretty_print::build_list(array_def));

      } else if (field.is_dynamic() || field.is_array() || field.is_inline()) {
        throw std::runtime_error(fmt::format(
            "Dynamic/array/inline reference field {} type {} in static data not yet implemented",
            field.name(), actual_type.print()));
      } else {
        // then we expect a label.
        assert(field_end - field_start == 4);
        auto& word = obj_words.at(field_start / 4);

        if (word.kind == LinkedWord::PTR) {
          field_defs_out.emplace_back(
              field.name(),
              decompile_at_label(field.type(), labels.at(word.label_id), labels, words, ts));
        } else if (word.kind == LinkedWord::PLAIN_DATA && word.data == 0) {
          // do nothing, the default is zero?
          field_defs_out.emplace_back(field.name(), pretty_print::to_symbol("0"));
        } else if (word.kind == LinkedWord::SYM_PTR) {
          if (word.symbol_name == "#f" || word.symbol_name == "#t") {
            field_defs_out.emplace_back(
                field.name(), pretty_print::to_symbol(fmt::format("{}", word.symbol_name)));
          } else {
            field_defs_out.emplace_back(
                field.name(), pretty_print::to_symbol(fmt::format("'{}", word.symbol_name)));
          }
        } else if (word.kind == LinkedWord::EMPTY_PTR) {
          field_defs_out.emplace_back(field.name(), pretty_print::to_symbol("'()"));
        } else {
          throw std::runtime_error(
              fmt::format("Field {} in type {} offset {} did not have a proper reference",
                          field.name(), actual_type.print(), field.offset()));
        }
      }
    }
  }

  for (size_t i = 0; i < field_status_per_byte.size(); i++) {
    if (field_status_per_byte.at(i) == HAS_DATA_UNREAD) {
      throw std::runtime_error(
          fmt::format("In structure of type {} at label {} offset {}, there was unknown data.",
                      actual_type.print(), label.name, i));
    }
  }

  std::vector<goos::Object> result_def = {
      pretty_print::to_symbol(fmt::format("new 'static '{}", actual_type.print()))};
  //      pretty_print::to_symbol("new"), pretty_print::to_symbol("'static"),
  //      pretty_print::to_symbol(fmt::format("'{}", actual_type.print()))};
  for (auto& f : field_defs_out) {
    auto str = f.second.print();
    if (str.length() < 40) {
      result_def.push_back(
          pretty_print::to_symbol(fmt::format(":{} {}", f.first, f.second.print())));
    } else {
      result_def.push_back(pretty_print::to_symbol(fmt::format(":{}", f.first)));
      result_def.push_back(f.second);
    }
  }
  return pretty_print::build_list(result_def);
}

goos::Object decompile_value(const TypeSpec& type,
                             const std::vector<u8>& bytes,
                             const TypeSystem& ts) {
  // try as common integer types:
  if (ts.tc(TypeSpec("uint32"), type)) {
    assert(bytes.size() == 4);
    u32 value;
    memcpy(&value, bytes.data(), 4);
    return pretty_print::to_symbol(fmt::format("#x{:x}", u64(value)));
  } else if (ts.tc(TypeSpec("int32"), type)) {
    assert(bytes.size() == 4);
    s32 value;
    memcpy(&value, bytes.data(), 4);
    if (value > 100 && value <= INT32_MAX) {
      return pretty_print::to_symbol(fmt::format("#x{:x}", value));
    } else {
      return pretty_print::to_symbol(fmt::format("{}", value));
    }
  } else if (ts.tc(TypeSpec("int8"), type)) {
    assert(bytes.size() == 1);
    s8 value;
    memcpy(&value, bytes.data(), 1);
    if (value > 5) {
      return pretty_print::to_symbol(fmt::format("#x{:x}", value));
    } else {
      return pretty_print::to_symbol(fmt::format("{}", value));
    }
  } else if (ts.tc(TypeSpec("uint64"), type)) {
    assert(bytes.size() == 8);
    u64 value;
    memcpy(&value, bytes.data(), 8);
    return pretty_print::to_symbol(fmt::format("#x{:x}", value));
  } else if (ts.tc(TypeSpec("float"), type)) {
    assert(bytes.size() == 4);
    float value;
    memcpy(&value, bytes.data(), 4);
    return pretty_print::float_representation(value);
  } else if (ts.tc(TypeSpec("uint8"), type)) {
    assert(bytes.size() == 1);
    u8 value;
    memcpy(&value, bytes.data(), 1);
    return pretty_print::to_symbol(fmt::format("#x{:x}", value));
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
    // value array
    std::vector<goos::Object> result = {
        pretty_print::to_symbol("new"), pretty_print::to_symbol("'static"),
        pretty_print::to_symbol("'boxed-array"), pretty_print::to_symbol(content_type.print()),
        pretty_print::to_symbol(fmt::format("{}", array_length))};

    auto stride = content_type_info->get_size_in_memory();
    for (int i = 0; i < array_length; i++) {
      auto start = first_elt_word_idx * 4 + stride * i;
      auto end = start + content_type_info->get_size_in_memory();
      std::vector<u8> elt_bytes;
      for (int j = start; j < end; j++) {
        auto& word = words.at(label.target_segment).at(j / 4);
        if (word.kind != LinkedWord::PLAIN_DATA) {
          throw std::runtime_error("Got bad word in kind in array of values");
        }
        elt_bytes.push_back(word.get_byte(j % 4));
      }
      result.push_back(decompile_value(content_type, elt_bytes, ts));
    }
    return pretty_print::build_list(result);
  }
}

namespace {
goos::Object decompile_pair_elt(const LinkedWord& word,
                                const std::vector<DecompilerLabel>& labels,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts) {
  if (word.kind == LinkedWord::PTR) {
    return decompile_at_label_guess_type(labels.at(word.label_id), labels, words, ts);
  } else if (word.kind == LinkedWord::PLAIN_DATA && word.data == 0) {
    // do nothing, the default is zero?
    return pretty_print::to_symbol("0");
  } else if (word.kind == LinkedWord::SYM_PTR) {
    if (word.symbol_name == "#f" || word.symbol_name == "#t") {
      return pretty_print::to_symbol(fmt::format("{}", word.symbol_name));
    } else {
      return pretty_print::to_symbol(fmt::format("'{}", word.symbol_name));
    }
  } else if (word.kind == LinkedWord::EMPTY_PTR) {
    return pretty_print::to_symbol("'()");
  } else if (word.kind == LinkedWord::PLAIN_DATA && (word.data & 0b111) == 0) {
    return pretty_print::to_symbol(fmt::format("(the binteger {})", word.data >> 3));
  } else {
    throw std::runtime_error(fmt::format("Pair elt did not have a good word kind"));
  }
}
}  // namespace

goos::Object decompile_pair(const DecompilerLabel& label,
                            const std::vector<DecompilerLabel>& labels,
                            const std::vector<std::vector<LinkedWord>>& words,
                            const TypeSystem& ts) {
  if ((label.offset % 8) != 2) {
    if ((label.offset % 4) != 0) {
      throw std::runtime_error(fmt::format("Invalid alignment for pair {}\n", label.offset % 16));
    } else {
      auto& word = words.at(label.target_segment).at(label.offset / 4);
      if (word.kind != LinkedWord::EMPTY_PTR) {
        throw std::runtime_error(
            fmt::format("Based on alignment, expected to get empty list for pair, but didn't"));
      }
      return pretty_print::to_symbol("'()");
    }
  }

  std::vector<goos::Object> list_tokens;
  auto to_print = label;

  for (int iter = 0;; iter++) {
    if (iter > 10000) {
      throw std::runtime_error(
          "Exceeded 10,000 look ups while trying to follow a linked list. Giving up, the list is "
          "possibly circular. Increase the limit in data_decompile.cpp if you really need more.");
    }

    if ((to_print.offset % 8) == 2) {
      // continue
      auto car_word = words.at(to_print.target_segment).at((to_print.offset - 2) / 4);
      list_tokens.push_back(decompile_pair_elt(car_word, labels, words, ts));

      auto cdr_word = words.at(to_print.target_segment).at((to_print.offset + 2) / 4);
      // if empty
      if (cdr_word.kind == LinkedWord::EMPTY_PTR) {
        return pretty_print::build_list(list_tokens);
      }
      // if pointer
      if (cdr_word.kind == LinkedWord::PTR) {
        to_print = labels.at(cdr_word.label_id);
        continue;
      }
      // invalid.
      lg::error(
          "There is an improper list. This is probably okay, but should be checked manually "
          "because we "
          "could not find a test case yet.");
      list_tokens.push_back(pretty_print::to_symbol("."));
      list_tokens.push_back(decompile_pair_elt(cdr_word, labels, words, ts));
      return pretty_print::build_list(list_tokens);
    } else {
      if ((to_print.offset % 4) != 0) {
        throw std::runtime_error(
            fmt::format("Invalid alignment for pair {}\n", to_print.offset % 16));
      } else {
        auto& word = words.at(to_print.target_segment).at(to_print.offset / 4);
        if (word.kind != LinkedWord::EMPTY_PTR) {
          throw std::runtime_error(
              fmt::format("Based on alignment, expected to get empty list for pair, but didn't"));
        }
        // improper list
        lg::error(
            "There is an improper list. This is probably okay, but should be checked manually "
            "because we "
            "could not find a test case yet.");
        list_tokens.push_back(pretty_print::to_symbol("."));
        list_tokens.push_back(decompile_pair_elt(
            words.at(to_print.target_segment).at(to_print.offset / 4), labels, words, ts));
        return pretty_print::build_list(list_tokens);
      }
    }
  }
}

}  // namespace decompiler