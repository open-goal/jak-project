#include "data_decompile.h"

#include <algorithm>

#include "common/goos/PrettyPrinter.h"
#include "common/log/log.h"
#include "common/type_system/Type.h"
#include "common/util/Assert.h"
#include "common/util/math_util.h"
#include "common/util/print_float.h"

#include "decompiler/IR2/Form.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/analysis/final_output.h"

#include "third-party/fmt/core.h"

namespace decompiler {

/*!
 * Entry point from the decompiler to decompile data.
 */
goos::Object decompile_at_label_with_hint(const LabelInfo& hint,
                                          const DecompilerLabel& label,
                                          const std::vector<DecompilerLabel>& labels,
                                          const std::vector<std::vector<LinkedWord>>& words,
                                          const TypeSystem& ts,
                                          const LinkedObjectFile* file,
                                          GameVersion version) {
  const auto& type = hint.result_type;
  if (!hint.array_size.has_value()) {
    // if we don't have an array size, treat it as just a normal type.
    if (hint.is_value) {
      throw std::runtime_error(
          fmt::format("Label {} was marked as a value, but is being decompiled as a reference (1).",
                      hint.name));
    }
    return decompile_at_label(type, label, labels, words, ts, file, version);
  }

  if (type.base_type() == "pointer") {
    if (hint.is_value) {
      throw std::runtime_error(
          fmt::format("Label {} was marked as a value, but is being decompiled as a reference (2).",
                      hint.name));
    }
    auto field_type_info = ts.lookup_type(type.get_single_arg());
    if (field_type_info->is_reference()) {
      throw std::runtime_error(
          fmt::format("Type {} label {} is not yet supported by the data decompiler.", type.print(),
                      hint.name));
    } else {
      auto stride = field_type_info->get_size_in_memory();

      int word_count = ((stride * (*hint.array_size)) + 3) / 4;
      int max_word = words.at(label.target_segment).size();
      int start_word = label.offset / 4;
      if (start_word + word_count > max_word) {
        throw std::runtime_error(
            fmt::format("Decompiling array of {} values of type {} would go past the end of the "
                        "file. The file has {} words, the array starts at word {}, and has length "
                        "{} words, which would make it end at {}",
                        *hint.array_size, type.get_single_arg().print(), max_word, start_word,
                        word_count, start_word + word_count));
      }

      std::vector<LinkedWord> obj_words;
      obj_words.insert(obj_words.begin(), words.at(label.target_segment).begin() + start_word,
                       words.at(label.target_segment).begin() + start_word + word_count);
      return decompile_value_array(type.get_single_arg(), field_type_info, *hint.array_size, stride,
                                   0, obj_words, ts);
    }
  }

  if (type.base_type() == "inline-array") {
    if (hint.is_value) {
      throw std::runtime_error(
          fmt::format("Label {} was marked as a value, but is being decompiled as a reference (3).",
                      hint.name));
    }
    auto field_type_info = ts.lookup_type(type.get_single_arg());
    if (!field_type_info->is_reference()) {
      throw std::runtime_error(
          fmt::format("Type {} for label {} is invalid, the element type is not inlineable.",
                      hint.result_type.print(), hint.name));
    } else {
      // it's an inline array.  let's figure out the len and stride
      auto len = *hint.array_size;
      // TODO - having this logic here isn't great.
      auto stride = align(field_type_info->get_size_in_memory(),
                          field_type_info->get_inline_array_stride_alignment());
      lg::info("decompiler {} stride {} {} = {}", field_type_info->get_name(),
               field_type_info->get_size_in_memory(),
               field_type_info->get_inline_array_stride_alignment(),
               align(field_type_info->get_size_in_memory(),
                     field_type_info->get_inline_array_stride_alignment()));

      if (dynamic_cast<BasicType*>(field_type_info)) {
        throw std::runtime_error("Plan basic arrays not supported yet");
        // I just want to double check offsets....
      }

      std::vector<goos::Object> array_def = {pretty_print::to_symbol(fmt::format(
          "new 'static 'inline-array {} {}", type.get_single_arg().print(), *hint.array_size))};
      for (int elt = 0; elt < len; elt++) {
        DecompilerLabel fake_label;
        fake_label.target_segment = label.target_segment;
        fake_label.offset = label.offset + field_type_info->get_offset() + stride * elt;
        fake_label.name = fmt::format("fake-label-{}-elt-{}", type.get_single_arg().print(), elt);
        array_def.push_back(decompile_at_label(type.get_single_arg(), fake_label, labels, words, ts,
                                               file, version));
      }
      return pretty_print::build_list(array_def);
    }
  }

  throw std::runtime_error(fmt::format(
      "Type `{}` with length {} is not yet supported by the data decompiler. (label {})",
      hint.result_type.print(), *hint.array_size, hint.name));
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
    if (type_ptr.kind() != LinkedWord::TYPE_PTR) {
      return {};
    }
    if (type_ptr.symbol_name() == "array") {
      auto content_type_ptr_word_idx = type_ptr_word_idx + 3;
      auto& content_type_ptr = words.at(label.target_segment).at(content_type_ptr_word_idx);
      if (content_type_ptr.kind() != LinkedWord::TYPE_PTR) {
        return {};
      }
      return TypeSpec("array", {TypeSpec(content_type_ptr.symbol_name())});
    }
    return TypeSpec(type_ptr.symbol_name());
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
                                           const TypeSystem& ts,
                                           const LinkedObjectFile* file,
                                           GameVersion version) {
  auto guessed_type = get_type_of_label(label, words);
  if (!guessed_type.has_value()) {
    throw std::runtime_error("(1) Could not guess the type of " + label.name);
  }
  return decompile_at_label(*guessed_type, label, labels, words, ts, file, version);
}

goos::Object decompile_function_at_label(const DecompilerLabel& label,
                                         const LinkedObjectFile* file,
                                         bool in_static_pair) {
  if (file) {
    auto other_func = file->try_get_function_at_label(label);
    if (other_func && other_func->ir2.env.has_local_vars() && other_func->ir2.top_form &&
        other_func->ir2.expressions_succeeded) {
      auto out = final_output_lambda(*other_func);
      if (in_static_pair) {
        return pretty_print::build_list("unquote", out);
      } else {
        return out;
      }
    }
  }
  return pretty_print::to_symbol(fmt::format("<lambda at {}>", label.name));
}

/*!
 * Attempt to decompile data of the given type at the given label. If the decompiler thinks the
 * types do not line up, it will fail.
 */
goos::Object decompile_at_label(const TypeSpec& type,
                                const DecompilerLabel& label,
                                const std::vector<DecompilerLabel>& labels,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts,
                                const LinkedObjectFile* file,
                                GameVersion version,
                                bool in_static_pair) {
  try {
    if (type == TypeSpec("string")) {
      return decompile_string_at_label(label, words);
    }

    if (ts.tc(TypeSpec("function"), type)) {
      return decompile_function_at_label(label, file, in_static_pair);
    }

    if (ts.tc(TypeSpec("array"), type)) {
      std::optional<TypeSpec> content_type_spec;
      if (type.has_single_arg()) {
        content_type_spec = type.get_single_arg();
      }
      return decompile_boxed_array(type, label, labels, words, ts, file, content_type_spec,
                                   version);
    }

    if (ts.tc(TypeSpec("structure"), type)) {
      return decompile_structure(type, label, labels, words, ts, file, true, version);
    }

    if (type == TypeSpec("pair")) {
      return decompile_pair(label, labels, words, ts, true, file, version);
    }
  } catch (std::exception& ex) {
    throw std::runtime_error(
        fmt::format("Unable to 'decompile_at_label' {} (using type {}), Reason: {}", label.name,
                    type.print(), ex.what()));
  }

  throw std::runtime_error(fmt::format(
      "Unimplemented decompile_at_label for Label: {} and Type: {}", label.name, type.print()));
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
  ASSERT(label.offset >= 4);

  const auto& type_ptr = words.at(label.target_segment).at((label.offset - 4) / 4);
  if (type_ptr.kind() != LinkedWord::TYPE_PTR) {
    throw std::runtime_error(fmt::format(
        "Cannot get string at label {}, word before is not a type pointer.", label.name));
  }

  if (type_ptr.symbol_name() != "string") {
    throw std::runtime_error(fmt::format("Cannot get string at label {}, type pointer is for a {}.",
                                         label.name, type_ptr.symbol_name()));
  }

  std::string result;

  auto word_idx = (label.offset / 4) - 1;
  // next should be the size
  if (word_idx + 1 >= int(words.at(label.target_segment).size())) {
    throw std::runtime_error(
        fmt::format("Cannot get string at label {}, not enough room", label.name));
  }
  const LinkedWord& size_word = words.at(label.target_segment).at(word_idx + 1);
  if (size_word.kind() != LinkedWord::PLAIN_DATA) {
    // sometimes an array of string pointer triggers this!
    throw std::runtime_error(
        fmt::format("Cannot get string at label {}, size is not plain data.", label.name));
  }

  // now characters...
  for (size_t i = 0; i < size_word.data; i++) {
    int word_offset = word_idx + 2 + (i / 4);
    int byte_offset = i % 4;
    auto& word = words.at(label.target_segment).at(word_offset);
    if (word.kind() != LinkedWord::PLAIN_DATA) {
      throw std::runtime_error(
          fmt::format("Cannot get string at label {}, character is not plain data.", label.name));
    }
    char cword[4];
    memcpy(cword, &word.data, 4);
    result += cword[byte_offset];
    ASSERT(result.back() != 0);
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
      pretty_print::to_symbol(fmt::format("new 'static 'array {} {}", elt_type.print(), length))};

  for (int i = 0; i < length; i++) {
    auto start = offset + stride * i;
    auto end = start + elt_type_info->get_size_in_memory();
    std::vector<u8> elt_bytes;
    for (int j = start; j < end; j++) {
      auto& word = obj_words.at(j / 4);
      if (word.kind() != LinkedWord::PLAIN_DATA) {
        if (word.kind() == LinkedWord::TYPE_PTR) {
          throw std::runtime_error(
              fmt::format("Got bad word in kind in array of values: expecting array of {}'s, got a "
                          "type pointer {}\n",
                          elt_type.print(), word.symbol_name()));
        }
        throw std::runtime_error(fmt::format(
            "Got bad word in kind in array of values: expecting array of {}'s, got a {}\n",
            elt_type.print(), (int)word.kind()));
      }
      elt_bytes.push_back(word.get_byte(j % 4));
    }
    ASSERT(elt_type != TypeSpec("uint128"));
    array_def.push_back(decompile_value(elt_type, elt_bytes, ts));
  }

  return pretty_print::build_list(array_def);
}

namespace {
float word_as_float(const LinkedWord& w) {
  ASSERT(w.kind() == LinkedWord::PLAIN_DATA);
  float v;
  memcpy(&v, &w.data, 4);
  return v;
}

s32 word_as_s32(const LinkedWord& w) {
  ASSERT(w.kind() == LinkedWord::PLAIN_DATA);
  return w.data;
}

std::string print_def(const goos::Object& obj) {
  if (obj.is_pair() && obj.as_pair()->car.is_symbol() &&
      obj.as_pair()->car.as_symbol() == "quote") {
    auto& rest = obj.as_pair()->cdr;
    if (rest.is_pair() && rest.as_pair()->cdr.is_empty_list()) {
      return fmt::format("'{}", rest.as_pair()->car.print());
    }
  }
  return obj.print();
}

/*!
 * Start at start_byte, and find the location of the next label.
 * Will only check labels that are in the given segment.
 */
int index_of_closest_following_label_in_segment(int start_byte,
                                                int seg,
                                                const std::vector<DecompilerLabel>& labels) {
  int result_idx = -1;
  int closest_byte = -1;
  for (int i = 0; i < (int)labels.size(); i++) {
    const auto& label = labels.at(i);
    if (label.target_segment == seg) {
      if (result_idx == -1 && label.offset > start_byte) {
        result_idx = i;
        closest_byte = label.offset;
      } else {
        if (label.offset > start_byte && label.offset < closest_byte) {
          result_idx = i;
          closest_byte = label.offset;
        }
      }
    }
  }
  return result_idx;
}

int guess_array_size_array(int start_offset,
                           int segment,
                           int stride,
                           const std::vector<std::vector<LinkedWord>>& all_words,
                           const std::vector<DecompilerLabel>& labels) {
  int end_label_idx = index_of_closest_following_label_in_segment(start_offset, segment, labels);

  int end_offset = all_words.at(segment).size() * 4;
  if (end_label_idx < 0) {
    lg::warn(
        "Failed to find label: likely just an unimplemented case for when the data is the last "
        "thing in the file.");
  } else {
    const auto& end_label = labels.at(end_label_idx);
    end_offset = end_label.offset;
    // fmt::print("end label is {}\n", end_label.name);
  }

  // lg::print("Data is from {} to {}\n", start_label.name, end_label.name);

  // now we can figure out the size
  int size_bytes = end_offset - start_offset;
  int size_elts = size_bytes / stride;
  int leftover_bytes = size_bytes % stride;
  // lg::print("Size is {} bytes ({} elts), with {} bytes left over\n", size_bytes, size_elts,
  //          leftover_bytes);

  // if we have leftover, should verify that its all zeros, or that it's the type pointer
  // of the next basic in the data section.
  // ex:
  // .word <data>
  // .type <some-other-basic's type tag>
  // L21: ; label some other basic
  // <other basic's data>
  int padding_start = end_offset - leftover_bytes;
  int padding_end = end_offset;
  for (int pad_byte_idx = padding_start; pad_byte_idx < padding_end; pad_byte_idx++) {
    auto& word = all_words.at(segment).at(pad_byte_idx / 4);
    switch (word.kind()) {
      case LinkedWord::PLAIN_DATA:
        ASSERT(word.get_byte(pad_byte_idx) == 0);
        break;
      case LinkedWord::TYPE_PTR:
        break;
      default:
        ASSERT(false);
    }
  }

  // if we end exactly on a type_ptr, take off an element.
  if (all_words.at(segment).at((end_offset - 1) / 4).kind() == LinkedWord::TYPE_PTR) {
    size_elts--;
  }
  return size_elts;
}
/*!
 * Attempt to decompile a reference to an array, without knowing the size.
 */
goos::Object decomp_ref_to_integer_array_guess_size(
    const std::vector<LinkedWord>& words,
    const std::vector<DecompilerLabel>& labels,
    int my_seg,
    int field_location,
    const TypeSystem& ts,
    const std::vector<std::vector<LinkedWord>>& all_words,
    const LinkedObjectFile* /*file*/,
    const TypeSpec& array_elt_type,
    int stride) {
  // lg::print("Decomp decomp_ref_to_inline_array_guess_size {}\n", array_elt_type.print());

  // verify types
  auto elt_type_info = ts.lookup_type(array_elt_type);
  ASSERT(stride == elt_type_info->get_size_in_memory());
  ASSERT(!elt_type_info->is_reference());

  // the input is the location of the data field.
  // we expect that to be a label:
  ASSERT((field_location % 4) == 0);
  auto& pointer_to_data = words.at(field_location / 4);

  // pointer-arrays can also be initialized as #f
  if (pointer_to_data.kind() == LinkedWord::SYM_PTR) {
    ASSERT_MSG(
        pointer_to_data.symbol_name() == "#f",
        fmt::format(
            "attempted to decompile a pointer array of '{}', but encounted a non `#f` symbol",
            array_elt_type.base_type()));
    return pretty_print::to_symbol("#f");
  }

  ASSERT(pointer_to_data.kind() == LinkedWord::PTR);

  // the data shouldn't have any labels in the middle of it, so we can find the end of the array
  // by searching for the label after the start label.
  const auto& start_label = labels.at(pointer_to_data.label_id());
  int size_elts = guess_array_size_array(start_label.offset, my_seg, stride, all_words, labels);

  return decompile_value_array(array_elt_type, elt_type_info, size_elts, stride, start_label.offset,
                               all_words.at(start_label.target_segment), ts);
}

int guess_array_size_inline_array(int start_offset,
                                  int segment,
                                  int stride,
                                  const std::vector<std::vector<LinkedWord>>& all_words,
                                  const std::vector<DecompilerLabel>& labels) {
  int end_label_idx = index_of_closest_following_label_in_segment(start_offset, segment, labels);

  int end_offset = all_words.at(segment).size() * 4;
  if (end_label_idx < 0) {
    lg::warn(
        "Failed to find label: likely just an unimplemented case for when the data is the last "
        "thing in the file.");
  } else {
    const auto& end_label = labels.at(end_label_idx);
    end_offset = end_label.offset;
    // if misaligned, round down - labels may point 2 bytes into the first word if the data is a
    // pair, and we should not treat those 2 bytes as padding for this check
    end_offset &= ~3;
    // fmt::print("detected end label of {}\n", end_label.name);
  }

  // lg::print("Data is from {} to {}\n", start_label.name, end_label.name);

  // now we can figure out the size
  int size_bytes = end_offset - start_offset;
  int size_elts = size_bytes / stride;  // 32 bytes per ocean-near-index
  int leftover_bytes = size_bytes % stride;
  // lg::print("Size is {} bytes ({} elts), with {} bytes left over\n", size_bytes,
  // size_elts,leftover_bytes);

  // if we have leftover, should verify that its all zeros, or that it's the type pointer
  // of the next basic in the data section.
  // ex:
  // .word <data>
  // .type <some-other-basic's type tag>
  // L21: ; label some other basic
  // <other basic's data>
  int padding_start = end_offset - leftover_bytes;
  int padding_end = end_offset;
  for (int pad_byte_idx = padding_start; pad_byte_idx < padding_end; pad_byte_idx++) {
    auto& word = all_words.at(segment).at(pad_byte_idx / 4);
    switch (word.kind()) {
      case LinkedWord::PLAIN_DATA:
        ASSERT(word.get_byte(pad_byte_idx % 4) == 0);
        break;
      case LinkedWord::TYPE_PTR:
        break;
      default:
        fmt::print("bad type: {}\n", (int)word.kind());
        fmt::print("data: {}\n", word.data);
        if (word.holds_string()) {
          fmt::print("str: {}\n", word.symbol_name());
        }
        if (word.kind() == LinkedWord::PTR) {
          fmt::print("ptr: {}\n", labels.at(word.label_id()).name);
        }
        ASSERT(false);
    }
  }
  return size_elts;
}

/*!
 * Attempt to decompile a reference to an inline array, without knowing the size.
 */
goos::Object decomp_ref_to_inline_array_guess_size(
    const std::vector<LinkedWord>& words,
    const std::vector<DecompilerLabel>& labels,
    int my_seg,
    int field_location,
    const TypeSystem& ts,
    const std::vector<std::vector<LinkedWord>>& all_words,
    const LinkedObjectFile* file,
    const TypeSpec& array_elt_type,
    int stride,
    GameVersion version) {
  // lg::print("Decomp decomp_ref_to_inline_array_guess_size {}\n", array_elt_type.print());

  // verify the stride matches the type system
  auto elt_type_info = ts.lookup_type(array_elt_type);
  int elt_size = align(elt_type_info->get_size_in_memory(),
                       elt_type_info->get_inline_array_stride_alignment());
  ASSERT(stride == elt_size);

  // the input is the location of the data field.
  // we expect that to be a label:
  ASSERT((field_location % 4) == 0);
  auto& pointer_to_data = words.at(field_location / 4);

  // inline-arrays can also be initialized as #f
  if (pointer_to_data.kind() == LinkedWord::SYM_PTR) {
    ASSERT_MSG(
        pointer_to_data.symbol_name() == "#f",
        fmt::format(
            "attempted to decompile an inline-array of '{}', but encounted a non `#f` symbol",
            array_elt_type.base_type()));
    return pretty_print::to_symbol("#f");
  }

  ASSERT(pointer_to_data.kind() == LinkedWord::PTR);

  // the data shouldn't have any labels in the middle of it, so we can find the end of the array
  // by searching for the label after the start label.
  const auto& start_label = labels.at(pointer_to_data.label_id());
  int start_offset = start_label.offset;

  int size_elts = guess_array_size_inline_array(start_offset, start_label.target_segment, stride,
                                                all_words, labels);

  // now disassemble:
  std::vector<goos::Object> array_def = {pretty_print::to_symbol(
      fmt::format("new 'static 'inline-array {} {}", array_elt_type.print(), size_elts))};

  for (int elt = 0; elt < size_elts; elt++) {
    // for each element, create a fake temporary label at the start to identify it
    DecompilerLabel fake_label;
    fake_label.target_segment = my_seg;  // same segment
    fake_label.offset = start_label.offset + elt * stride;
    array_def.push_back(
        decompile_at_label(array_elt_type, fake_label, labels, all_words, ts, file, version));
  }

  // build into a list.
  return pretty_print::build_list(array_def);
}

goos::Object decompile_sound_spec(const TypeSpec& type,
                                  const DecompilerLabel& label,
                                  const std::vector<DecompilerLabel>& labels,
                                  const std::vector<std::vector<LinkedWord>>& words,
                                  const TypeSystem& ts,
                                  const LinkedObjectFile* file,
                                  GameVersion version) {
  // auto normal = decompile_structure(type, label, labels, words, ts, file, false);
  // lg::print("Doing: {}\n", normal.print());
  auto uncast_type_info = ts.lookup_type(type);
  auto type_info = dynamic_cast<StructureType*>(uncast_type_info);
  if (!type_info) {
    throw std::runtime_error(fmt::format("Type {} wasn't a structure type.", type.print()));
  }
  ASSERT(type_info->get_size_in_memory() == 0x4c);

  // get words for real
  auto offset_location = label.offset - type_info->get_offset();
  int word_count = (type_info->get_size_in_memory() + 3) / 4;
  std::vector<LinkedWord> obj_words;
  obj_words.insert(obj_words.begin(),
                   words.at(label.target_segment).begin() + (offset_location / 4) + 1,
                   words.at(label.target_segment).begin() + (offset_location / 4) + word_count);

  for (int i = 0; i < word_count - 1; ++i) {
    if (i == word_count - 2 && !obj_words.at(i).data) {
      // just some default initialized sound spec, don't attempt anything fancy.
      return decompile_structure(type, label, labels, words, ts, file, false, version);
    }
    if (obj_words.at(i).data)
      break;
  }

  u16 implicit_mask = 0;
  u16 mask = obj_words.at(0).data;
  float num = word_as_float(obj_words.at(1));
  u8 group = obj_words.at(2).data;
  char sound_name[17];
  sound_name[16] = 0;
  memcpy(&sound_name[0], &obj_words.at(3).data, sizeof(u32));
  memcpy(&sound_name[4], &obj_words.at(4).data, sizeof(u32));
  memcpy(&sound_name[8], &obj_words.at(5).data, sizeof(u32));
  memcpy(&sound_name[12], &obj_words.at(6).data, sizeof(u32));
  std::string name(sound_name);

  for (int i = 0; i < 4; ++i) {
    if (obj_words.at(7 + i).data) {
      throw std::runtime_error("static sound-spec trans was not zero.");
    }
  }

  s32 volume = word_as_s32(obj_words.at(11));
  s32 pitch = word_as_s32(obj_words.at(12));
  s32 bend = word_as_s32(obj_words.at(13));
  s16 fo_min = word_as_s32(obj_words.at(14));
  s16 fo_max = word_as_s32(obj_words.at(14)) >> 16;
  s8 fo_curve = word_as_s32(obj_words.at(15));
  s8 priority = word_as_s32(obj_words.at(15)) >> 8;
  s32 auto_time = word_as_s32(obj_words.at(16));
  s32 auto_from = word_as_s32(obj_words.at(17));

  if (bend) {
    throw std::runtime_error("static sound-spec bend was not zero.");
  }
  if (fo_curve && file->version == GameVersion::Jak1) {
    throw std::runtime_error("static sound-spec fo_curve was not zero.");
  }
  if (priority) {
    throw std::runtime_error("static sound-spec priority was not zero.");
  }
  if (auto_time) {
    throw std::runtime_error("static sound-spec auto_time was not zero.");
  }
  if (auto_from) {
    throw std::runtime_error("static sound-spec auto_from was not zero.");
  }

  std::vector<goos::Object> the_macro;

  the_macro.push_back(pretty_print::to_symbol("static-sound-spec"));
  the_macro.push_back(goos::StringObject::make_new(name));
  if (num != 1) {
    the_macro.push_back(pretty_print::to_symbol(fmt::format(":num {}", num)));
  }
  if (group != 1) {
    the_macro.push_back(pretty_print::to_symbol(fmt::format(":group {}", num)));
  }
  if ((mask & 1) || volume != 1024) {
    implicit_mask |= 1 << 0;
    float volf = volume / 10.24f;
    // volume is fixed point, and floats should round towards zero, so we convert specific ints
    // to better-looking floats that end up being the same value.
    // there should be a more automated way to do this, but i am a bit lazy.
    // TODO try fixed point print i made some time ago
    switch (volume) {
      case 0x2cc:
        volf = 70;
        break;
      case 0x333:
        volf = 80;
        break;
    }
    the_macro.push_back(pretty_print::to_symbol(fmt::format(":volume {}", float_to_string(volf))));
  }
  if (pitch != 0) {
    implicit_mask |= 1 << 1;
    the_macro.push_back(pretty_print::to_symbol(fmt::format(":pitch-mod {}", pitch)));
  }
  if (fo_min != 0) {
    implicit_mask |= 1 << 6;
    the_macro.push_back(pretty_print::to_symbol(fmt::format(":fo-min {}", fo_min)));
  }
  if (fo_max != 0) {
    implicit_mask |= 1 << 7;
    the_macro.push_back(pretty_print::to_symbol(fmt::format(":fo-max {}", fo_max)));
  }
  if (fo_curve != 0) {
    implicit_mask |= (1 << 8);
    the_macro.push_back(pretty_print::to_symbol(fmt::format(":fo-curve {}", fo_curve)));
  }

  if (mask < implicit_mask) {
    throw std::runtime_error(
        fmt::format("static sound-spec too many implicit masks: #x{:x}", implicit_mask ^ mask));
  }
  u16 final_mask = mask ^ implicit_mask;
  if (final_mask) {
    lg::error(
        "final_mask in static sound-spec decomp: #x{:x}. This is fine, but should be reported.",
        final_mask);
    std::string mask_list = ":mask (";
    bool first = true;
    for (const auto& m : decompile_bitfield_enum_from_int(TypeSpec("sound-mask"), ts, final_mask)) {
      if (!first) {
        mask_list += " ";
      }
      mask_list += m;
      first = false;
    }
    mask_list += ")";

    the_macro.push_back(pretty_print::to_symbol(mask_list));
  }

  return pretty_print::build_list(the_macro);
}

}  // namespace

// TODO - add a common game version
const std::unordered_map<
    GameVersion,
    std::unordered_map<std::string, std::unordered_map<std::string, ArrayFieldDecompMeta>>>
    array_field_decomp_special_cases = {
        {GameVersion::Jak1,
         /*!
          * Decompile the data field of ocean-near-indices, which is an (inline-array
          * ocean-near-index). This is like a C++ ocean_near_index*, meaning we don't know how long
          * the array is. We know all the data in a ocean_near_index is just integers, so we can
          * guess that the end of the array is just the location of the next label. There's a chance
          * that this will include some padding in the array and make it too long, but there is no
          * harm in that.
          */
         {{"ocean-near-indices",
           {{"data", ArrayFieldDecompMeta(TypeSpec("ocean-near-index"), 32)}}},
          {"ocean-mid-masks", {{"data", ArrayFieldDecompMeta(TypeSpec("ocean-mid-mask"), 8)}}},
          {"sparticle-launcher",
           {{"init-specs", ArrayFieldDecompMeta(TypeSpec("sp-field-init-spec"), 16)}}},
          {"sparticle-launch-group",
           {{"launcher", ArrayFieldDecompMeta(TypeSpec("sparticle-group-item"), 32)}}},
          {"nav-mesh",
           {{"vertex", ArrayFieldDecompMeta(TypeSpec("nav-vertex"), 16)},
            {"poly", ArrayFieldDecompMeta(TypeSpec("nav-poly"), 8)},
            {"route", ArrayFieldDecompMeta(TypeSpec("vector4ub"), 4)}}},
          {"lightning-probe-vars", {{"probe-dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)}}},
          {"ropebridge-tuning",
           {{"col-mesh-indexes",
             ArrayFieldDecompMeta(TypeSpec("uint8"),
                                  1,
                                  ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}}}},
        {GameVersion::Jak2,
         {
             {"ocean-near-indices",
              {{"data", ArrayFieldDecompMeta(TypeSpec("ocean-near-index"), 32)}}},
             {"simple-sprite-system",
              {{"data", ArrayFieldDecompMeta(TypeSpec("sprite-glow-data"), 64)}}},
             {"ocean-mid-masks", {{"data", ArrayFieldDecompMeta(TypeSpec("ocean-mid-mask"), 8)}}},
             {"sparticle-launcher",
              {{"init-specs", ArrayFieldDecompMeta(TypeSpec("sp-field-init-spec"), 16)}}},
             {"sparticle-launch-group",
              {{"launcher", ArrayFieldDecompMeta(TypeSpec("sparticle-group-item"), 32)}}},
             {"nav-network-info",
              {{"adjacency", ArrayFieldDecompMeta(TypeSpec("nav-network-adjacency"), 16)}}},
             {"sig-path", {{"samples", ArrayFieldDecompMeta(TypeSpec("sig-path-sample"), 64)}}},
             {"rigid-body-vehicle-constants",
              {{"color-option-array", ArrayFieldDecompMeta(TypeSpec("vector"), 16)},
               {"grab-rail-array", ArrayFieldDecompMeta(TypeSpec("vehicle-grab-rail-info"), 48)}}},
             {"city-ambush-info",
              {{"array", ArrayFieldDecompMeta(TypeSpec("city-ambush-spot"), 32)}}},
             {"bombbot-path", {{"node", ArrayFieldDecompMeta(TypeSpec("bombbot-node"), 32)}}},
             {"fort-robotank-segment",
              {{"event-tbl", ArrayFieldDecompMeta(TypeSpec("fort-robotank-segment-event"), 32)}}},
             {"race-info",
              {{"turbo-pad-array", ArrayFieldDecompMeta(TypeSpec("race-turbo-pad"), 32)},
               {"racer-array", ArrayFieldDecompMeta(TypeSpec("race-racer-info"), 16)},
               {"decision-point-array",
                ArrayFieldDecompMeta(TypeSpec("race-decision-point"), 16)}}},
             {"actor-hash-bucket",
              {{"data", ArrayFieldDecompMeta(TypeSpec("actor-cshape-ptr"),
                                             16,
                                             ArrayFieldDecompMeta::Kind::REF_TO_INLINE_ARR)}}},
             {"xz-height-map",
              {{"data", ArrayFieldDecompMeta(TypeSpec("int8"),
                                             1,
                                             ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"enemy-info",
              {{"idle-anim-script",
                ArrayFieldDecompMeta(TypeSpec("idle-control-frame"),
                                     4,
                                     ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"nav-enemy-info",
              {{"idle-anim-script",
                ArrayFieldDecompMeta(TypeSpec("idle-control-frame"),
                                     4,
                                     ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"tpath-info",
              // TODO - should be able to just decompile the `anims` field
              {{"anim1", ArrayFieldDecompMeta(TypeSpec("tpath-control-frame"), 16)},
               {"anim2", ArrayFieldDecompMeta(TypeSpec("tpath-control-frame"), 16)},
               {"anim3", ArrayFieldDecompMeta(TypeSpec("tpath-control-frame"), 16)}}},
             // kinda want to add regex support now...
             {"bigmap-compressed-layers",
              {{"layer0", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer1", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer2", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer3", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer4", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer5", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer6", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer7", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer8", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer9", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                               4,
                                               ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer10", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer11", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer12", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer13", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer14", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer15", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer16", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer17", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer18", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"layer19", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                                4,
                                                ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"lightning-probe-vars",
              {{"probe-dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)}}},
             {"nav-mesh",
              {{"poly-array", ArrayFieldDecompMeta(TypeSpec("nav-poly"), 64)},
               {"nav-control-array", ArrayFieldDecompMeta(TypeSpec("nav-control"), 288)}}},
             {"trail-conn-hash",
              {{"cell", ArrayFieldDecompMeta(TypeSpec("trail-conn-hash-cell"), 4)},
               {"conn-ids", ArrayFieldDecompMeta(TypeSpec("uint16"),
                                                 2,
                                                 ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"trail-graph",
              {{"node", ArrayFieldDecompMeta(TypeSpec("trail-node"), 18)},
               {"conn", ArrayFieldDecompMeta(TypeSpec("trail-conn"), 8)},
               {"conn-ids", ArrayFieldDecompMeta(TypeSpec("uint16"),
                                                 2,
                                                 ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"visgroup", ArrayFieldDecompMeta(TypeSpec("trail-conn-hash-cell"), 4)},
               {"visnode-ids",
                ArrayFieldDecompMeta(TypeSpec("uint16"),
                                     2,
                                     ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"predator-graph",
              {{"node", ArrayFieldDecompMeta(TypeSpec("predator-node"), 48)},
               {"edge", ArrayFieldDecompMeta(TypeSpec("predator-edge"), 4)}}},
             {"sig0-course",
              {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)},
               {"speeches", ArrayFieldDecompMeta(TypeSpec("bot-speech-info"), 16)},
               {"dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)},
               {"speech-tunings", ArrayFieldDecompMeta(TypeSpec("bot-speech-tuning"), 16)}}},
             {"ashelin-course",
              {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)},
               {"speeches", ArrayFieldDecompMeta(TypeSpec("bot-speech-info"), 16)},
               {"dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)},
               {"speech-tunings", ArrayFieldDecompMeta(TypeSpec("bot-speech-tuning"), 16)}}},
             {"ai-task-pool",
              {{"tasks", ArrayFieldDecompMeta(TypeSpec("uint32"),
                                              4,
                                              ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"bot-course", {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)}}},
             {"hal3-course", {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)}}},
             {"sig5-course",
              {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)},
               {"speeches", ArrayFieldDecompMeta(TypeSpec("bot-speech-info"), 16)},
               {"dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)},
               {"speech-tunings", ArrayFieldDecompMeta(TypeSpec("bot-speech-tuning"), 16)}}},
             {"under-block-puzzle",
              {{"cells", ArrayFieldDecompMeta(TypeSpec("int32"),
                                              4,
                                              ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)},
               {"pulse-ops",
                ArrayFieldDecompMeta(TypeSpec("int8"),
                                     1,
                                     ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR)}}},
             {"turret-path",
              {{"event-tbl", ArrayFieldDecompMeta(TypeSpec("turret-path-event"), 16)}}},
             {"hal4-course",
              {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)},
               {"speeches", ArrayFieldDecompMeta(TypeSpec("bot-speech-info"), 16)},
               {"dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)},
               {"speech-tunings", ArrayFieldDecompMeta(TypeSpec("bot-speech-tuning"), 16)}}},
             {"hal2-course",
              {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)},
               {"speeches", ArrayFieldDecompMeta(TypeSpec("bot-speech-info"), 16)},
               {"dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)},
               {"speech-tunings", ArrayFieldDecompMeta(TypeSpec("bot-speech-tuning"), 16)}}},
             {"ruffian-course",
              {{"spots", ArrayFieldDecompMeta(TypeSpec("bot-spot"), 32)},
               {"speeches", ArrayFieldDecompMeta(TypeSpec("bot-speech-info"), 16)},
               {"dirs", ArrayFieldDecompMeta(TypeSpec("vector"), 16)},
               {"speech-tunings", ArrayFieldDecompMeta(TypeSpec("bot-speech-tuning"), 16)}}},
         }}};

goos::Object decompile_structure(const TypeSpec& type,
                                 const DecompilerLabel& label,
                                 const std::vector<DecompilerLabel>& labels,
                                 const std::vector<std::vector<LinkedWord>>& words,
                                 const TypeSystem& ts,
                                 const LinkedObjectFile* file,
                                 bool use_fancy_macros,
                                 GameVersion version) {
  // some structures we want to decompile to fancy macros instead of a raw static definiton
  // temp hack!!
  if (use_fancy_macros && file) {
    if (type == TypeSpec("sound-spec")) {
      return decompile_sound_spec(type, label, labels, words, ts, file, version);
    }
  }

  // first step, get type info and words
  TypeSpec actual_type = type;
  auto uncast_type_info = ts.lookup_type(actual_type);
  auto type_info = dynamic_cast<StructureType*>(uncast_type_info);
  if (!type_info) {
    throw std::runtime_error(fmt::format("Type {} wasn't a structure type.", actual_type.print()));
  }
  bool is_basic = dynamic_cast<BasicType*>(uncast_type_info);
  auto offset_location = label.offset - type_info->get_offset();

  if (is_basic) {
    const auto& word = words.at(label.target_segment).at((offset_location / 4));
    if (word.kind() != LinkedWord::TYPE_PTR) {
      throw std::runtime_error(
          fmt::format("Basic does not start with type pointer: {}", label.name));
    }

    if (word.symbol_name() != actual_type.base_type()) {
      // we can specify a more specific type.
      auto got_type = TypeSpec(word.symbol_name());
      if (ts.tc(actual_type, got_type)) {
        actual_type = got_type;

        type_info = dynamic_cast<StructureType*>(ts.lookup_type(actual_type));
        if (!type_info) {
          throw std::runtime_error(
              fmt::format("Type-tag type {} wasn't a structure type.", actual_type.print()));
        }

        // try again with the right type. this resets back to decompile_at_label because we may
        // want to get the specific function/string/etc implementations.
        return decompile_at_label(actual_type, label, labels, words, ts, file, version);
      } else {
        throw std::runtime_error(
            fmt::format("Basic has the wrong type pointer, got {} expected {} at label {}:{}",
                        word.symbol_name(), actual_type.base_type(), label.name, label.offset));
      }
    }
  }

  int word_count = (type_info->get_size_in_memory() + 3) / 4;
  int byte_count = type_info->get_size_in_memory();

  // check alignment
  if (offset_location % 8) {
    std::string error = fmt::format(
        "Decompiling a structure with type type {} (type offset {}) at label {}, but it has "
        "alignment {}, which is not valid. This might be okay for a packed inline array, but "
        "shouldn't happen for basics. {}",
        type_info->get_name(), type_info->get_offset(), label.name, (offset_location % 8),
        (offset_location & 0b10) ? "Maybe it is actually a pair?" : "");

    if (is_basic || !type_info->is_packed()) {
      throw std::runtime_error(error);
    } else {
      // lg::print("{}\n", error);
    }
  }

  // check enough room
  if (int(words.at(label.target_segment).size()) < word_count + offset_location / 4) {
    throw std::runtime_error(fmt::format(
        "Structure type {} takes up {} bytes and doesn't fit. {}:{}", type_info->get_name(),
        type_info->get_size_in_memory(), label.name, offset_location));
  }

  // get words for real
  std::vector<LinkedWord> obj_words;
  obj_words.insert(obj_words.begin(),
                   words.at(label.target_segment).begin() + (offset_location / 4),
                   words.at(label.target_segment).begin() + (offset_location / 4) + word_count);

  // status of each byte.
  enum ByteStatus : u8 { ZERO_UNREAD, HAS_DATA_UNREAD, ZERO_READ, HAS_DATA_READ };
  std::vector<int> field_status_per_byte;
  for (int i = 0; i < byte_count; i++) {
    // auto& w = obj_words.at(i / 4);
    int b = (offset_location + i);
    auto& w = words.at(label.target_segment).at(b / 4);
    switch (w.kind()) {
      case LinkedWord::TYPE_PTR:
      case LinkedWord::PTR:
      case LinkedWord::SYM_PTR:
      case LinkedWord::EMPTY_PTR:
        field_status_per_byte.push_back(HAS_DATA_UNREAD);
        break;
      case LinkedWord::PLAIN_DATA: {
        field_status_per_byte.push_back(w.get_byte(b % 4) ? HAS_DATA_UNREAD : ZERO_UNREAD);
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
      ASSERT(field.name() == "type" && field.offset() == 0);
      auto& word = obj_words.at(0);
      if (word.kind() != LinkedWord::TYPE_PTR) {
        throw std::runtime_error("Basic does not start with type pointer");
      }

      if (word.symbol_name() != actual_type.base_type()) {
        // the check above should have caught this.
        ASSERT(false);
      }
      for (int k = 0; k < 4; k++) {
        field_status_per_byte.at(k) = HAS_DATA_READ;
      }
      idx++;
      continue;
    }
    idx++;

    // O(N^2)-1 approach to the score system? but I didn't notice any slowdowns and there are
    // ultimately not many static allocs
    bool higher_score_available = false;
    for (auto& other_field : type_info->fields()) {
      if (other_field == field)
        continue;
      if (other_field.offset() == field.offset() &&
          other_field.field_score() > field.field_score()) {
        higher_score_available = true;
        break;
      }
    }
    if (higher_score_available) {
      // a higher priority field is available
      continue;
    }

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
      // special case for dynamic arrays at the end of a type
      if (!(field_start == field_end && field.is_dynamic())) {
        // field has nothing in it, just skip it.
        continue;
      }
    }

    if (any_overlap) {
      // for now, let's just skip fields that overlapped with the previous.
      // eventually we should do something smarter here...
      continue;
    }

    // first, let's see if it's a value or reference
    auto field_type_info = ts.lookup_type_allow_partial_def(field.type());
    if (!field_type_info->is_reference() && field.type() != TypeSpec("object")) {
      // value type. need to get bytes.
      ASSERT(!field.is_inline());
      if (field.is_array()) {
        // array of values.
        auto len = field.array_size();
        auto stride = ts.get_size_in_type(field) / len;
        ASSERT(stride == field_type_info->get_size_in_memory());

        field_defs_out.emplace_back(
            field.name(), decompile_value_array(field.type(), field_type_info, len, stride,
                                                field_start, obj_words, ts));
      } else if (field.is_dynamic()) {
        throw std::runtime_error(
            fmt::format("Dynamic value field {} in static data type {} not yet implemented",
                        field.name(), actual_type.print()));
      } else {
        // array field special cases, uses the map initialized above!
        // check if there is a special case for this type+field+version combination
        if (file && array_field_decomp_special_cases.count(file->version) > 0 &&
            array_field_decomp_special_cases.at(file->version).count(type.print()) > 0 &&
            array_field_decomp_special_cases.at(file->version)
                    .at(type.print())
                    .count(field.name()) > 0) {
          // We have a special case, do the things
          const auto& metadata =
              array_field_decomp_special_cases.at(file->version).at(type.print()).at(field.name());
          if (metadata.kind == ArrayFieldDecompMeta::Kind::REF_TO_INLINE_ARR) {
            field_defs_out.emplace_back(
                field.name(),
                decomp_ref_to_inline_array_guess_size(
                    obj_words, labels, label.target_segment, field_start, ts, words, file,
                    metadata.element_type, metadata.bytes_per_element, file->version));
          } else if (metadata.kind == ArrayFieldDecompMeta::Kind::REF_TO_INTEGER_ARR) {
            field_defs_out.emplace_back(
                field.name(), decomp_ref_to_integer_array_guess_size(
                                  obj_words, labels, label.target_segment, field_start, ts, words,
                                  file, metadata.element_type, metadata.bytes_per_element));
          }
        } else {  // otherwise, it's a pointer array or plain data
          if (field.type().base_type() == "pointer") {
            if (obj_words.at(field_start / 4).kind() != LinkedWord::SYM_PTR) {
              continue;
            }

            if (obj_words.at(field_start / 4).symbol_name() != "#f") {
              lg::warn("Got a weird symbol in a pointer field: {}",
                       obj_words.at(field_start / 4).symbol_name());
              continue;
            }

            field_defs_out.emplace_back(field.name(), pretty_print::to_symbol("#f"));

          } else {
            if (obj_words.at(field_start / 4).kind() != LinkedWord::PLAIN_DATA) {
              continue;
            }
            std::vector<u8> bytes_out;
            for (int byte_idx = field_start; byte_idx < field_end; byte_idx++) {
              int byte_idx_in_seg = byte_idx + label.offset - type_info->get_offset();
              bytes_out.push_back(words.at(label.target_segment)
                                      .at(byte_idx_in_seg / 4)
                                      .get_byte(byte_idx_in_seg % 4));
              // bytes_out.push_back(obj_words.at(byte_idx / 4).get_byte(byte_idx % 4));
            }

            // use more specific types for gif tags.
            bool is_gif_type =
                type.base_type() == "dma-gif-packet" || type.base_type() == "dma-gif";
            if (is_gif_type && field.name() == "gif0") {
              field_defs_out.emplace_back(field.name(),
                                          decompile_value(TypeSpec("gif-tag64"), bytes_out, ts));
            } else if (is_gif_type && field.name() == "gif1") {
              field_defs_out.emplace_back(field.name(),
                                          decompile_value(TypeSpec("gif-tag-regs"), bytes_out, ts));
            } else {
              if (field.type() == TypeSpec("uint128")) {
                throw std::runtime_error(
                    fmt::format("Failed to decompile: looking at field {} (from {}) with type {}",
                                field.name(), type_info->get_name(), field.type().print()));
              }
              field_defs_out.emplace_back(field.name(), decompile_value(field.type(), bytes_out, ts,
                                                                        field.decomp_as_type()));
            }
          }
        }
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
            field.name(),
            decompile_at_label(field.type(), fake_label, labels, words, ts, file, version));
      } else if (!field.is_dynamic() && field.is_array() && field.is_inline()) {
        // it's an inline array.  let's figure out the len and stride
        auto len = field.array_size();
        auto total_size = ts.get_size_in_type(field);
        auto stride = total_size / len;
        ASSERT(stride * len == total_size);
        ASSERT(stride == align(field_type_info->get_size_in_memory(),
                               field_type_info->get_inline_array_stride_alignment()));

        std::vector<goos::Object> array_def = {pretty_print::to_symbol(fmt::format(
            "new 'static 'inline-array {} {}", field.type().print(), field.array_size()))};
        for (int elt = 0; elt < len; elt++) {
          DecompilerLabel fake_label;
          fake_label.target_segment = label.target_segment;
          // offset from real start of outer + field offset + tag, we want to fake that.
          fake_label.offset =
              offset_location + field.offset() + field_type_info->get_offset() + stride * elt;
          fake_label.name =
              fmt::format("fake-label-{}-{}-elt-{}", actual_type.print(), field.name(), elt);
          array_def.push_back(
              decompile_at_label(field.type(), fake_label, labels, words, ts, file, version));
        }
        field_defs_out.emplace_back(field.name(), pretty_print::build_list(array_def));
      } else if (!field.is_dynamic() && field.is_array() && !field.is_inline()) {
        auto len = field.array_size();
        auto total_size = ts.get_size_in_type(field);
        auto stride = total_size / len;
        ASSERT(stride * len == total_size);
        ASSERT(stride == 4);

        std::vector<goos::Object> array_def = {pretty_print::to_symbol(
            fmt::format("new 'static 'array {} {}", field.type().print(), field.array_size()))};

        int end_elt = 0;
        for (int elt = len; elt-- > 0;) {
          auto& word = obj_words.at((field_start / 4) + elt);
          if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
            continue;
          }
          end_elt = elt + 1;
          break;
        }

        for (int elt = 0; elt < end_elt; elt++) {
          auto& word = obj_words.at((field_start / 4) + elt);

          if (word.kind() == LinkedWord::PTR) {
            array_def.push_back(decompile_at_label(field.type(), labels.at(word.label_id()), labels,
                                                   words, ts, file, version));
          } else if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
            // do nothing, the default is zero?
            array_def.push_back(pretty_print::to_symbol("0"));
          } else if (word.kind() == LinkedWord::SYM_PTR) {
            if (word.symbol_name() == "#f" || word.symbol_name() == "#t") {
              array_def.push_back(pretty_print::to_symbol(fmt::format("{}", word.symbol_name())));
            } else {
              array_def.push_back(pretty_print::to_symbol(fmt::format("'{}", word.symbol_name())));
            }
          } else if (word.kind() == LinkedWord::EMPTY_PTR) {
            array_def.push_back(pretty_print::to_symbol("'()"));
          } else {
            throw std::runtime_error(fmt::format(
                "Field {} in type {} offset {} did not have a proper reference for "
                "array element {} k = {}",
                field.name(), actual_type.print(), field.offset(), elt, (int)word.kind()));
          }
        }
        field_defs_out.emplace_back(field.name(), pretty_print::build_list(array_def));

      } else if (field.is_dynamic() && field.is_array() && !field.is_inline()) {
        // it's a dynamic array hanging off the end of the type
        auto elt_type_info = ts.lookup_type(field.type());
        int elt_size = elt_type_info->is_reference() ? 4 : elt_type_info->get_size_in_memory();

        // first byte of the array: type's offset + field's offset + basic offset (if we have it)
        int array_start_byte = offset_location + field.offset() + field_type_info->get_offset();
        // inherit segment of our data.
        int array_data_seg = label.target_segment;
        // try to find the next thing in the file.
        int num_elts =
            guess_array_size_array(array_start_byte, array_data_seg, elt_size, words, labels);

        std::vector<goos::Object> array_def = {pretty_print::to_symbol(
            fmt::format("new 'static 'array {} {}", field.type().print(), num_elts))};

        int end_elt = 0;
        for (int elt = num_elts; elt-- > 0;) {
          auto& word = words.at(array_data_seg).at((array_start_byte / 4) + elt);
          if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
            continue;
          }
          end_elt = elt + 1;
          break;
        }

        for (int elt = 0; elt < end_elt; elt++) {
          auto& word = words.at(array_data_seg).at((array_start_byte / 4) + elt);

          if (word.kind() == LinkedWord::PTR) {
            array_def.push_back(decompile_at_label(field.type(), labels.at(word.label_id()), labels,
                                                   words, ts, file, version));
          } else if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
            // do nothing, the default is zero?
            array_def.push_back(pretty_print::to_symbol("0"));
          } else if (word.kind() == LinkedWord::SYM_PTR) {
            if (word.symbol_name() == "#f" || word.symbol_name() == "#t") {
              array_def.push_back(pretty_print::to_symbol(fmt::format("{}", word.symbol_name())));
            } else {
              array_def.push_back(pretty_print::to_symbol(fmt::format("'{}", word.symbol_name())));
            }
          } else if (word.kind() == LinkedWord::EMPTY_PTR) {
            array_def.push_back(pretty_print::to_symbol("'()"));
          } else {
            throw std::runtime_error(fmt::format(
                "Field {} in type {} offset {} did not have a proper reference for "
                "array element {} k = {}",
                field.name(), actual_type.print(), field.offset(), elt, (int)word.kind()));
          }
        }
        field_defs_out.emplace_back(field.name(), pretty_print::build_list(array_def));
      } else if (field.is_dynamic() && field.is_array() && field.is_inline()) {
        // verify the stride matches the type system
        auto elt_type_info = ts.lookup_type(field.type());
        int elt_size = align(elt_type_info->get_size_in_memory(),
                             elt_type_info->get_inline_array_stride_alignment());
        // first byte of the array: type's offset + field's offset + basic offset (if we have it)
        int array_start_byte = offset_location + field.offset() + field_type_info->get_offset();
        // inherit segment of our data.
        int array_data_seg = label.target_segment;

        // the data shouldn't have any labels in the middle of it, so we can find the end of the
        // array by searching for the label after the start label.
        int size_elts = guess_array_size_inline_array(array_start_byte, array_data_seg, elt_size,
                                                      words, labels);
        if (size_elts) {
          // now disassemble:
          std::vector<goos::Object> array_def = {pretty_print::to_symbol(
              fmt::format("new 'static 'inline-array {} {}", field.type().print(), size_elts))};

          for (int elt = 0; elt < size_elts; elt++) {
            // for each element, create a fake temporary label at the start to identify it
            DecompilerLabel fake_label;
            fake_label.target_segment = array_data_seg;  // same segment
            fake_label.offset = array_start_byte + elt * elt_size;
            array_def.push_back(
                decompile_at_label(field.type(), fake_label, labels, words, ts, file, version));
          }
          // build into a list.
          field_defs_out.emplace_back(field.name(), pretty_print::build_list(array_def));
        }
      } else if (field.is_dynamic() || field.is_array() || field.is_inline()) {
        throw std::runtime_error(
            fmt::format("Field {} of type {} not supported", field.name(), type.print()));
      } else {
        // then we expect a label.
        ASSERT(field_end - field_start == 4);
        auto& word = obj_words.at(field_start / 4);

        if (word.kind() == LinkedWord::PTR) {
          if (field.type() == TypeSpec("symbol")) {
            continue;
          }
          if (field.type() == TypeSpec("object")) {
            field_defs_out.emplace_back(
                field.name(), decompile_at_label_guess_type(labels.at(word.label_id()), labels,
                                                            words, ts, file, version));
          } else {
            field_defs_out.emplace_back(
                field.name(), decompile_at_label(field.type(), labels.at(word.label_id()), labels,
                                                 words, ts, file, version));
          }
        } else if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
          // do nothing, the default is zero?
          field_defs_out.emplace_back(field.name(), pretty_print::to_symbol("0"));
        } else if (word.kind() == LinkedWord::SYM_PTR) {
          if (word.symbol_name() == "#f") {
            field_defs_out.emplace_back(
                field.name(), pretty_print::to_symbol(fmt::format("{}", word.symbol_name())));
          } else if (!ts.tc(field.type(), TypeSpec("symbol"))) {
            continue;
          } else if (word.symbol_name() == "#t") {
            field_defs_out.emplace_back(
                field.name(), pretty_print::to_symbol(fmt::format("{}", word.symbol_name())));
          } else {
            field_defs_out.emplace_back(
                field.name(), pretty_print::to_symbol(fmt::format("'{}", word.symbol_name())));
          }
        } else if (word.kind() == LinkedWord::EMPTY_PTR) {
          field_defs_out.emplace_back(field.name(), pretty_print::to_symbol("'()"));
        } else if (word.kind() == LinkedWord::TYPE_PTR) {
          if (field.type() != TypeSpec("type")) {
            throw std::runtime_error(
                fmt::format("Field {} in type {} offset {} had a reference to type {}, but the "
                            "type of the field is not type.",
                            field.name(), actual_type.print(), field.offset(), word.symbol_name()));
          }
          int method_count = ts.get_type_method_count(word.symbol_name());
          field_defs_out.emplace_back(
              field.name(), pretty_print::to_symbol(fmt::format("(type-ref {} :method-count {})",
                                                                word.symbol_name(), method_count)));
        } else {
          throw std::runtime_error(
              fmt::format("Field {} in type {} offset {} did not have a proper reference",
                          field.name(), actual_type.print(), field.offset()));
        }
      }
    }

    // OK - READ THE FIELD:
    for (int i = field_start; i < field_end; i++) {
      // even if our field was partially zero, we mark those zero bytes as "has data".
      field_status_per_byte.at(i) = HAS_DATA_READ;
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
  for (auto& f : field_defs_out) {
    auto str = f.second.print();
    bool hack = actual_type.base_type() == "sp-field-init-spec" && f.first == "object";
    if (str.length() < 40 && !hack) {
      result_def.push_back(
          pretty_print::to_symbol(fmt::format(":{} {}", f.first, print_def(f.second))));
    } else {
      result_def.push_back(pretty_print::to_symbol(fmt::format(":{}", f.first)));
      result_def.push_back(f.second);
    }
  }
  return pretty_print::build_list(result_def);
}

goos::Object bitfield_defs_print(const TypeSpec& type,
                                 const std::vector<BitFieldConstantDef>& defs) {
  std::vector<goos::Object> result;
  result.push_back(pretty_print::to_symbol(fmt::format("new 'static '{}", type.print())));
  for (auto& def : defs) {
    if (def.enum_constant) {
      result.push_back(
          pretty_print::to_symbol(fmt::format(":{} {}", def.field_name, *def.enum_constant)));
    } else if (def.is_signed) {
      result.push_back(
          pretty_print::to_symbol(fmt::format(":{} {}", def.field_name, (s64)def.value)));
    } else if (def.nested_field) {
      result.push_back(pretty_print::to_symbol(fmt::format(
          ":{} {}", def.field_name,
          bitfield_defs_print(def.nested_field->field_type, def.nested_field->fields).print())));
    } else {
      result.push_back(
          pretty_print::to_symbol(fmt::format(":{} #x{:x}", def.field_name, def.value)));
    }
  }
  return pretty_print::build_list(result);
}

goos::Object decompile_value(const TypeSpec& type,
                             const std::vector<u8>& bytes,
                             const TypeSystem& ts,
                             const std::optional<TypeSpec> decomp_as_type) {
  auto as_enum = ts.try_enum_lookup(type);
  if (as_enum) {
    ASSERT((int)bytes.size() == as_enum->get_load_size());
    ASSERT(bytes.size() <= 8);
    u64 value = 0;
    memcpy(&value, bytes.data(), bytes.size());
    if (as_enum->is_bitfield()) {
      auto defs = decompile_bitfield_enum_from_int(type, ts, value);
      std::vector<goos::Object> result_def = {pretty_print::to_symbol(type.print())};
      for (auto& x : defs) {
        result_def.push_back(pretty_print::to_symbol(x));
      }
      return pretty_print::build_list(result_def);
    } else {
      auto def = decompile_int_enum_from_int(type, ts, value);
      return pretty_print::build_list(type.print(), def);
    }
  }

  auto as_bitfield = dynamic_cast<BitFieldType*>(ts.lookup_type(type));
  if (as_bitfield) {
    if (as_bitfield->get_name() == "sound-name") {
      ASSERT(bytes.size() == 16);
      char name[17];
      memcpy(name, bytes.data(), 16);
      name[16] = '\0';

      bool got_zero = false;
      for (int i = 0; i < 16; i++) {
        if (name[i] == 0) {
          got_zero = true;
        } else {
          if (got_zero) {
            ASSERT(false);
          }
        }
      }
      return pretty_print::to_symbol(fmt::format("(static-sound-name \"{}\")", name));
    } else if (as_bitfield->get_name() != "time-frame") {  // time-frame has a special case below
      ASSERT((int)bytes.size() == as_bitfield->get_load_size());
      ASSERT(bytes.size() <= 8);
      u64 value = 0;
      memcpy(&value, bytes.data(), bytes.size());
      auto defs = decompile_bitfield_from_int(type, ts, value);
      return bitfield_defs_print(type, defs);
    }
  }

  const auto& output_type = decomp_as_type ? *decomp_as_type : type;
  // try as common integer types:
  auto print_int = [](s64 val, bool is_signed, const TypeSpec& ts, s64 hex_cutoff = 100) {
    if (ts == TypeSpec("seconds") || ts == TypeSpec("time-frame")) {
      return pretty_print::to_symbol(
          fmt::format("(seconds {})", fixed_point_to_string(val, TICKS_PER_SECOND)));
    } else if (!is_signed || val > hex_cutoff) {
      return pretty_print::to_symbol(fmt::format("#x{:x}", u64(val)));
    } else {
      return pretty_print::to_symbol(fmt::format("{}", val));
    }
  };
  auto print_float = [](double val, const TypeSpec& ts) {
    if (ts == TypeSpec("meters")) {
      double meters = val / METER_LENGTH;
      auto rep = pretty_print::float_representation(meters);
      if (rep.print().find("the-as") != std::string::npos) {
        return rep;
      } else {
        return pretty_print::to_symbol(fmt::format("(meters {})", meters_to_string(val)));
      }
    } else if (ts == TypeSpec("degrees")) {
      double degrees = val / DEGREES_LENGTH;
      auto rep = pretty_print::float_representation(degrees);
      if (rep.print().find("the-as") != std::string::npos) {
        return rep;
      } else {
        return pretty_print::to_symbol(fmt::format("(degrees {})", degrees_to_string(val)));
      }
    } else if (ts == TypeSpec("fsec")) {
      double seconds = val / TICKS_PER_SECOND;
      auto rep = pretty_print::float_representation(seconds);
      if (rep.print().find("the-as") != std::string::npos) {
        return rep;
      } else {
        return pretty_print::to_symbol(fmt::format("(fsec {})", float_to_string(seconds, false)));
      }
    } else {
      return pretty_print::float_representation(val);
    }
  };
  if (ts.tc(TypeSpec("uint32"), type)) {
    ASSERT(bytes.size() == 4);
    u32 value;
    memcpy(&value, bytes.data(), 4);
    return print_int(value, false, output_type);
  } else if (ts.tc(TypeSpec("int32"), type)) {
    ASSERT(bytes.size() == 4);
    s32 value;
    memcpy(&value, bytes.data(), 4);
    return print_int(value, true, output_type);
  } else if (ts.tc(TypeSpec("uint16"), type)) {
    ASSERT(bytes.size() == 2);
    u16 value;
    memcpy(&value, bytes.data(), 2);
    return print_int(value, false, output_type);
  } else if (ts.tc(TypeSpec("int16"), type)) {
    ASSERT(bytes.size() == 2);
    s16 value;
    memcpy(&value, bytes.data(), 2);
    return print_int(value, true, output_type);
  } else if (ts.tc(TypeSpec("uint8"), type)) {
    ASSERT(bytes.size() == 1);
    u8 value;
    memcpy(&value, bytes.data(), 1);
    return print_int(value, false, output_type);
  } else if (ts.tc(TypeSpec("int8"), type)) {
    ASSERT(bytes.size() == 1);
    s8 value;
    memcpy(&value, bytes.data(), 1);
    return print_int(value, true, output_type);
  } else if (type == TypeSpec("seconds") || type == TypeSpec("time-frame")) {
    ASSERT(bytes.size() == 8);
    s64 value;
    memcpy(&value, bytes.data(), 8);
    return print_int(value, false, output_type);
  } else if (ts.tc(TypeSpec("uint64"), type)) {
    ASSERT(bytes.size() == 8);
    u64 value;
    memcpy(&value, bytes.data(), 8);
    return print_int(value, false, output_type);
  } else if (ts.tc(TypeSpec("int64"), type)) {
    ASSERT(bytes.size() == 8);
    s64 value;
    memcpy(&value, bytes.data(), 8);
    return print_int(value, true, output_type);
  } else if (type == TypeSpec("meters")) {
    ASSERT(bytes.size() == 4);
    float value;
    memcpy(&value, bytes.data(), 4);
    return print_float(value, output_type);
  } else if (type == TypeSpec("degrees")) {
    ASSERT(bytes.size() == 4);
    float value;
    memcpy(&value, bytes.data(), 4);
    return print_float(value, output_type);
  } else if (ts.tc(TypeSpec("float"), type)) {
    ASSERT(bytes.size() == 4);
    float value;
    memcpy(&value, bytes.data(), 4);
    return print_float(value, output_type);
  } else {
    throw std::runtime_error(fmt::format("decompile_value failed on a {}", type.print()));
  }
}

goos::Object decompile_boxed_array(const TypeSpec& type,
                                   const DecompilerLabel& label,
                                   const std::vector<DecompilerLabel>& labels,
                                   const std::vector<std::vector<LinkedWord>>& words,
                                   const TypeSystem& ts,
                                   const LinkedObjectFile* file,
                                   const std::optional<TypeSpec>& content_type_override,
                                   GameVersion version) {
  TypeSpec content_type;
  auto type_ptr_word_idx = (label.offset / 4) - 1;
  if ((label.offset % 8) == 4) {
    auto& type_ptr = words.at(label.target_segment).at(type_ptr_word_idx);
    if (type_ptr.kind() != LinkedWord::TYPE_PTR) {
      throw std::runtime_error("Invalid basic in decompile_boxed_array");
    }
    // TODO - ideally this wouldn't be hard-coded
    if (type_ptr.symbol_name() == "array" || type_ptr.symbol_name() == "texture-anim-array") {
      auto content_type_ptr_word_idx = type_ptr_word_idx + 3;
      auto& content_type_ptr = words.at(label.target_segment).at(content_type_ptr_word_idx);
      if (content_type_ptr.kind() != LinkedWord::TYPE_PTR) {
        throw std::runtime_error("Invalid content in decompile_boxed_array");
      }
      content_type = TypeSpec(content_type_ptr.symbol_name());
    } else {
      throw std::runtime_error(
          fmt::format("Wrong basic type in decompile_boxed_array: got {}", type_ptr.symbol_name()));
    }
  } else {
    throw std::runtime_error("Invalid alignment in decompile_boxed_array");
  }

  std::string array_type = "boxed-array";

  if (content_type_override) {
    content_type = *content_type_override;
  }

  // Handle children of `array`
  if (type.base_type() != "array") {
    array_type = type.print();
  }

  // now get the size
  auto& size_word_1 = words.at(label.target_segment).at(type_ptr_word_idx + 1);
  auto& size_word_2 = words.at(label.target_segment).at(type_ptr_word_idx + 2);
  auto first_elt_word_idx = type_ptr_word_idx + 4;

  if (size_word_1.kind() != LinkedWord::PLAIN_DATA ||
      size_word_2.kind() != LinkedWord::PLAIN_DATA) {
    throw std::runtime_error("Invalid size in decompile_boxed_array");
  }

  int array_length = size_word_1.data;
  int array_allocated_length = size_word_2.data;

  auto content_type_info = ts.lookup_type(content_type);
  auto params_obj =
      array_length == array_allocated_length
          ? pretty_print::to_symbol(
                fmt::format("new 'static '{} :type {}", array_type, content_type.print()))
          : pretty_print::to_symbol(
                fmt::format("new 'static '{} :type {} :length {} :allocated-length {}", array_type,
                            content_type.print(), array_length, array_allocated_length));
  if (content_type_info->is_reference() || content_type == TypeSpec("object")) {
    // easy, stride of 4.
    std::vector<goos::Object> result = {params_obj};

    for (int elt = 0; elt < array_length; elt++) {
      auto& word = words.at(label.target_segment).at(first_elt_word_idx + elt);
      if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
        result.push_back(pretty_print::to_symbol("0"));
      } else if (word.kind() == LinkedWord::PTR) {
        const auto& elt_label = labels.at(word.label_id());
        if (content_type == TypeSpec("object")) {
          // if there is a type hint for the label, no need to guess!
          if (file->label_db->label_info_known_by_name(elt_label.name)) {
            result.push_back(decompile_at_label_with_hint(file->label_db->lookup(elt_label.name),
                                                          elt_label, labels, words, ts, file,
                                                          version));
          } else {
            result.push_back(
                decompile_at_label_guess_type(elt_label, labels, words, ts, file, version));
          }
        } else {
          result.push_back(
              decompile_at_label(content_type, elt_label, labels, words, ts, file, version));
        }
      } else if (word.kind() == LinkedWord::SYM_PTR) {
        result.push_back(pretty_print::to_symbol(fmt::format("'{}", word.symbol_name())));
      } else {
        if (content_type == TypeSpec("object") && word.kind() == LinkedWord::PLAIN_DATA &&
            (word.data & 0b111) == 0) {
          s32 val = word.data;
          result.push_back(pretty_print::to_symbol(fmt::format("(the binteger {})", val / 8)));
        } else if (content_type == TypeSpec("type") && word.kind() == LinkedWord::TYPE_PTR) {
          result.push_back(pretty_print::to_symbol(word.symbol_name()));
        } else {
          throw std::runtime_error(fmt::format(
              "Unknown content type in boxed array of references, word idx {} at label {}",
              first_elt_word_idx + elt, label.name));
        }
      }
    }

    return pretty_print::build_list(result);
  } else if (content_type.base_type() == "inline-array") {
    std::vector<goos::Object> result = {params_obj};

    for (int elt = 0; elt < array_length; elt++) {
      auto& word = words.at(label.target_segment).at(first_elt_word_idx + elt);
      auto segment = labels.at(word.label_id()).target_segment;
      result.push_back(decomp_ref_to_inline_array_guess_size(
          words.at(segment), labels, segment, (first_elt_word_idx + elt) * 4, ts, words, file,
          content_type.get_single_arg(), ts.get_deref_info(content_type).stride, version));
    }

    return pretty_print::build_list(result);
  } else {
    // value array
    std::vector<goos::Object> result = {params_obj};

    auto stride = content_type_info->get_size_in_memory();
    for (int i = 0; i < array_length; i++) {
      auto start = first_elt_word_idx * 4 + stride * i;
      auto end = start + content_type_info->get_size_in_memory();
      std::vector<u8> elt_bytes;
      for (int j = start; j < end; j++) {
        auto& word = words.at(label.target_segment).at(j / 4);
        if (word.kind() != LinkedWord::PLAIN_DATA) {
          throw std::runtime_error(fmt::format("Got bad word of kind {} in boxed array of values",
                                               fmt::underlying(word.kind())));
        }
        elt_bytes.push_back(word.get_byte(j % 4));
      }
      ASSERT(content_type != TypeSpec("uint128"));
      result.push_back(decompile_value(content_type, elt_bytes, ts));
    }
    return pretty_print::build_list(result);
  }
}

namespace {
goos::Object decompile_pair_elt(const LinkedWord& word,
                                const std::vector<DecompilerLabel>& labels,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts,
                                const LinkedObjectFile* file,
                                GameVersion version) {
  if (word.kind() == LinkedWord::PTR) {
    auto& label = labels.at(word.label_id());
    auto guessed_type = get_type_of_label(label, words);
    if (!guessed_type.has_value()) {
      auto& info = file->label_db->lookup(label.name);
      if (info.known) {
        guessed_type = info.result_type;
      }
    }

    if (!guessed_type.has_value()) {
      throw std::runtime_error("(1) Could not guess the type of " + label.name);
    }

    if (guessed_type == TypeSpec("pair")) {
      return decompile_pair(label, labels, words, ts, false, file, version);
    }

    return decompile_at_label(*guessed_type, label, labels, words, ts, file, version, true);
  } else if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
    // do nothing, the default is zero?
    return pretty_print::to_symbol("0");
  } else if (word.kind() == LinkedWord::SYM_PTR) {
    // never quote symbols in a list.
    return pretty_print::to_symbol(word.symbol_name());
  } else if (word.kind() == LinkedWord::EMPTY_PTR) {
    return pretty_print::to_symbol("'()");
  } else if (word.kind() == LinkedWord::PLAIN_DATA && (word.data & 0b111) == 0) {
    return pretty_print::to_symbol(fmt::format("{}", ((s32)word.data) >> 3));  // binteger assumed
  } else if (word.kind() == LinkedWord::PLAIN_DATA) {
    return pretty_print::to_symbol(fmt::format("#x{:x}", word.data));
  } else {
    throw std::runtime_error(fmt::format("Pair elt did not have a good word kind: k {} d {}",
                                         (int)word.kind(), word.data));
  }
}

bool is_pointer_to_pair(const LinkedWord& word, const std::vector<DecompilerLabel>& labels) {
  if (word.kind() != LinkedWord::PTR) {
    return false;
  }
  auto& dest_label = labels.at(word.label_id());
  return (dest_label.offset % 8) == 2;
}
}  // namespace

goos::Object decompile_pair(const DecompilerLabel& label,
                            const std::vector<DecompilerLabel>& labels,
                            const std::vector<std::vector<LinkedWord>>& words,
                            const TypeSystem& ts,
                            bool add_quote,
                            const LinkedObjectFile* file,
                            GameVersion version) {
  if ((label.offset % 8) != 2) {
    if ((label.offset % 4) != 0) {
      throw std::runtime_error(
          fmt::format("Invalid alignment for pair {} at {}\n", label.offset % 16, label.name));
    } else {
      auto& word = words.at(label.target_segment).at(label.offset / 4);
      if (word.kind() != LinkedWord::EMPTY_PTR) {
        throw std::runtime_error(
            fmt::format("Based on alignment, expected to get empty list for pair at {}, but didn't",
                        label.name));
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
      list_tokens.push_back(decompile_pair_elt(car_word, labels, words, ts, file, version));

      auto cdr_word = words.at(to_print.target_segment).at((to_print.offset + 2) / 4);
      // if empty
      if (cdr_word.kind() == LinkedWord::EMPTY_PTR) {
        if (add_quote) {
          return pretty_print::build_list("quote", pretty_print::build_list(list_tokens));
        } else {
          return pretty_print::build_list(list_tokens);
        }
      }
      // if pointer
      if (is_pointer_to_pair(cdr_word, labels)) {
        to_print = labels.at(cdr_word.label_id());
        continue;
      }
      // improper
      list_tokens.push_back(pretty_print::to_symbol("."));
      list_tokens.push_back(decompile_pair_elt(cdr_word, labels, words, ts, file, version));
      if (add_quote) {
        return pretty_print::build_list("quote", pretty_print::build_list(list_tokens));
      } else {
        return pretty_print::build_list(list_tokens);
      }
    } else {
      throw std::runtime_error(
          fmt::format("Invalid alignment for pair {}\n", to_print.offset % 16));
    }
  }
}

goos::Object decompile_bitfield(const TypeSpec& type,
                                const BitFieldType* type_info,
                                const DecompilerLabel& label,
                                const std::vector<DecompilerLabel>&,
                                const std::vector<std::vector<LinkedWord>>& words,
                                const TypeSystem& ts) {
  // read memory
  int start_byte = label.offset;
  int end_byte = start_byte + type_info->get_size_in_memory();
  std::vector<u8> elt_bytes;
  for (int j = start_byte; j < end_byte; j++) {
    auto& word = words.at(label.target_segment).at(j / 4);
    if (word.kind() != LinkedWord::PLAIN_DATA) {
      throw std::runtime_error("Got bad word in static bitfield");
    }
    elt_bytes.push_back(word.get_byte(j % 4));
  }

  // pad bytes array to 64-bits:
  while (elt_bytes.size() < 8) {
    elt_bytes.push_back(0);
  }
  ASSERT(elt_bytes.size() == 8);

  // read as u64
  u64 value = *(u64*)(elt_bytes.data());
  auto defs = decompile_bitfield_from_int(type, ts, value);
  return bitfield_defs_print(type, defs);
}

std::optional<std::vector<BitFieldConstantDef>> try_decompile_bitfield_from_int(
    const TypeSpec& type,
    const TypeSystem& ts,
    u64 value,
    bool require_success,
    std::optional<int> offset) {
  u64 touched_bits = 0;
  std::vector<BitFieldConstantDef> result;

  auto type_info = dynamic_cast<BitFieldType*>(ts.lookup_type(type));
  ASSERT(type_info);

  int start_bit = 0;

  if (offset) {
    start_bit = *offset;
  }
  int end_bit = 64 + start_bit;

  for (auto& field : type_info->fields()) {
    if (field.skip_in_decomp() || field.offset() < start_bit ||
        (field.offset() + field.size()) > end_bit) {
      continue;
    }

    u64 bitfield_value;
    bool is_signed = ts.tc(TypeSpec("int"), field.type()) && !ts.tc(TypeSpec("uint"), field.type());
    if (is_signed) {
      // signed
      s64 signed_value = value;
      bitfield_value =
          extract_bitfield<s64>(signed_value, field.offset() - start_bit, field.size());
    } else {
      // unsigned
      bitfield_value = extract_bitfield<u64>(value, field.offset() - start_bit, field.size());
    }

    if (bitfield_value != 0) {
      BitFieldConstantDef def;
      def.value = bitfield_value;
      def.field_name = field.name();
      def.is_signed = is_signed;
      def.is_float = field.type().base_type() == "float";
      auto enum_info = ts.try_enum_lookup(field.type());
      if (enum_info && !enum_info->is_bitfield()) {
        auto name = decompile_int_enum_from_int(field.type(), ts, bitfield_value);
        def.enum_constant = fmt::format("({} {})", field.type().print(), name);
      }

      auto nested_bitfield_type = dynamic_cast<BitFieldType*>(ts.lookup_type(field.type()));
      if (nested_bitfield_type) {
        BitFieldConstantDef::NestedField nested;
        nested.field_type = field.type();
        // never nested 128-bit bitfields
        nested.fields =
            *try_decompile_bitfield_from_int(field.type(), ts, bitfield_value, true, {});
        def.nested_field = nested;
      }
      result.push_back(def);
    }

    for (int i = field.offset() - start_bit; i < field.offset() + field.size() - start_bit; i++) {
      touched_bits |= (u64(1) << i);
    }
  }

  u64 untouched_but_set = value & (~touched_bits);

  if (untouched_but_set) {
    if (require_success) {
      throw std::runtime_error(fmt::format(
          "Failed to decompile static bitfield of type {}. Original value is 0x{:x} but "
          "we didn't touch",
          type.print(), value, untouched_but_set));
    }
    return {};
  }
  return result;
}

std::vector<BitFieldConstantDef> decompile_bitfield_from_int(const TypeSpec& type,
                                                             const TypeSystem& ts,
                                                             u64 value) {
  return *try_decompile_bitfield_from_int(type, ts, value, true, {});
}

std::vector<std::string> decompile_bitfield_enum_from_int(const TypeSpec& type,
                                                          const TypeSystem& ts,
                                                          u64 value) {
  u64 reconstructed = 0;
  std::vector<std::string> result;
  auto type_info = ts.try_enum_lookup(type.base_type());
  ASSERT(type_info);
  ASSERT(type_info->is_bitfield());

  std::vector<std::string> bit_sorted_names;
  for (auto& field : type_info->entries()) {
    bit_sorted_names.push_back(field.first);
  }
  std::sort(bit_sorted_names.begin(), bit_sorted_names.end(),
            [&](const std::string& a, const std::string& b) {
              return type_info->entries().at(a) < type_info->entries().at(b);
            });

  for (auto& kv : type_info->entries()) {
    u64 mask = ((u64)1) << kv.second;
    if (value & mask) {
      reconstructed |= mask;
      result.push_back(kv.first);
    }
  }

  int bit_count = 0;
  {
    u64 x = value;
    while (x) {
      if (x & 1) {
        bit_count++;
      }
      x >>= 1;
    }
  }

  if (reconstructed != value) {
    throw std::runtime_error(fmt::format(
        "Failed to decompile bitfield enum {}. Original value is 0x{:x} but we could only "
        "make 0x{:x} using the available fields.",
        type.print(), value, reconstructed));
  }

  if (bit_count == (int)result.size()) {
    // unordered map will give us these fields in a weird order, let's order them explicitly.
    // because we have exactly one name per bit, we can just order them in bit order.
    std::sort(result.begin(), result.end(), [&](const std::string& a, const std::string& b) {
      return type_info->entries().at(a) < type_info->entries().at(b);
    });
  } else {
    // we have multiple. Just sort alphabetically and complain.
    lg::warn("Enum type {} has multiple entries with the same value.", type_info->get_name());
    std::sort(result.begin(), result.end());
  }

  return result;
}

std::string decompile_int_enum_from_int(const TypeSpec& type, const TypeSystem& ts, u64 value) {
  auto type_info = ts.try_enum_lookup(type.base_type());
  ASSERT(type_info);
  ASSERT(!type_info->is_bitfield());

  std::vector<std::string> matches;
  for (auto& field : type_info->entries()) {
    if ((u64)field.second == value) {
      matches.push_back(field.first);
    }
  }

  if (matches.size() == 0) {
    throw std::runtime_error(
        fmt::format("Failed to decompile integer enum. Value {} (0x{:x}) wasn't found in enum {}",
                    value, value, type_info->get_name()));
  } else if (matches.size() == 1) {
    return matches.front();
  } else {
    std::sort(matches.begin(), matches.end());
    return matches.front();
  }
}
}  // namespace decompiler
