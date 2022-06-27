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
#include "decompiler/util/sparticle_decompile.h"

#include "third-party/fmt/core.h"

namespace decompiler {

/*!
 * Entry point from the decompiler to decompile data.
 */
goos::Object decompile_at_label_with_hint(const LabelInfo& hint,
                                          const DecompilerLabel& label,
                                          const std::vector<DecompilerLabel>& labels,
                                          const std::vector<std::vector<LinkedWord>>& words,
                                          DecompilerTypeSystem& dts,
                                          const LinkedObjectFile* file) {
  const auto& type = hint.result_type;
  if (!hint.array_size.has_value()) {
    // if we don't have an array size, treat it as just a normal type.
    if (hint.is_value) {
      throw std::runtime_error(fmt::format(
          "Label {} was marked as a value, but is being decompiled as a reference.", hint.name));
    }
    return decompile_at_label(type, label, labels, words, dts.ts, file);
  }

  if (type.base_type() == "pointer") {
    if (hint.is_value) {
      throw std::runtime_error(fmt::format(
          "Label {} was marked as a value, but is being decompiled as a reference.", hint.name));
    }
    auto field_type_info = dts.ts.lookup_type(type.get_single_arg());
    if (field_type_info->is_reference()) {
      throw std::runtime_error(
          fmt::format("Type {} label {} is not yet supported by the data decompiler.", type.print(),
                      hint.name));
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
    if (hint.is_value) {
      throw std::runtime_error(fmt::format(
          "Label {} was marked as a value, but is being decompiled as a reference.", hint.name));
    }
    auto field_type_info = dts.ts.lookup_type(type.get_single_arg());
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
      fmt::print("decompiler {} stride {} {} = {}\n", field_type_info->get_name(),
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
        array_def.push_back(
            decompile_at_label(type.get_single_arg(), fake_label, labels, words, dts.ts, file));
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
                                           const LinkedObjectFile* file) {
  auto guessed_type = get_type_of_label(label, words);
  if (!guessed_type.has_value()) {
    throw std::runtime_error("Could not guess the type of " + label.name);
  }
  return decompile_at_label(*guessed_type, label, labels, words, ts, file);
}

goos::Object decompile_function_at_label(const DecompilerLabel& label,
                                         const LinkedObjectFile* file) {
  if (file) {
    auto other_func = file->try_get_function_at_label(label);
    if (other_func) {
      return final_output_lambda(*other_func);
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
                                const LinkedObjectFile* file) {
  if (type == TypeSpec("string")) {
    return decompile_string_at_label(label, words);
  }

  if (ts.tc(TypeSpec("function"), type)) {
    return decompile_function_at_label(label, file);
  }

  if (ts.tc(TypeSpec("array"), type)) {
    std::optional<TypeSpec> content_type_spec;
    if (type.has_single_arg()) {
      content_type_spec = type.get_single_arg();
    }
    return decompile_boxed_array(label, labels, words, ts, file, content_type_spec);
  }

  if (ts.tc(TypeSpec("structure"), type)) {
    return decompile_structure(type, label, labels, words, ts, file, true);
  }

  if (type == TypeSpec("pair")) {
    return decompile_pair(label, labels, words, ts, true, file);
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
        throw std::runtime_error("Got bad word in kind in array of values");
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
      obj.as_pair()->car.as_symbol()->name == "quote") {
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

/*!
 * Attempt to decompile a reference to an inline array, without knowing the size.
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
  // fmt::print("Decomp decomp_ref_to_inline_array_guess_size {}\n", array_elt_type.print());

  // verify types
  auto elt_type_info = ts.lookup_type(array_elt_type);
  ASSERT(stride == elt_type_info->get_size_in_memory());
  ASSERT(!elt_type_info->is_reference());

  // the input is the location of the data field.
  // we expect that to be a label:
  ASSERT((field_location % 4) == 0);
  auto pointer_to_data = words.at(field_location / 4);
  ASSERT(pointer_to_data.kind() == LinkedWord::PTR);

  // the data shouldn't have any labels in the middle of it, so we can find the end of the array
  // by searching for the label after the start label.
  const auto& start_label = labels.at(pointer_to_data.label_id());
  int end_label_idx =
      index_of_closest_following_label_in_segment(start_label.offset, my_seg, labels);

  int end_offset = all_words.at(my_seg).size() * 4;
  if (end_label_idx < 0) {
    lg::warn(
        "Failed to find label: likely just an unimplemented case for when the data is the last "
        "thing in the file.");
  } else {
    const auto& end_label = labels.at(end_label_idx);
    end_offset = end_label.offset;
  }

  // fmt::print("Data is from {} to {}\n", start_label.name, end_label.name);

  // now we can figure out the size
  int size_bytes = end_offset - start_label.offset;
  int size_elts = size_bytes / stride;  // 32 bytes per ocean-near-index
  int leftover_bytes = size_bytes % stride;
  // fmt::print("Size is {} bytes ({} elts), with {} bytes left over\n", size_bytes,
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
    auto& word = all_words.at(my_seg).at(pad_byte_idx / 4);
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

  return decompile_value_array(array_elt_type, elt_type_info, size_elts, stride, start_label.offset,
                               all_words.at(start_label.target_segment), ts);
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
    int stride) {
  // fmt::print("Decomp decomp_ref_to_inline_array_guess_size {}\n", array_elt_type.print());

  // verify the stride matches the type system
  auto elt_type_info = ts.lookup_type(array_elt_type);
  int ye = align(elt_type_info->get_size_in_memory(),
                 elt_type_info->get_inline_array_stride_alignment());
  ASSERT(stride == ye);

  // the input is the location of the data field.
  // we expect that to be a label:
  ASSERT((field_location % 4) == 0);
  auto pointer_to_data = words.at(field_location / 4);
  ASSERT(pointer_to_data.kind() == LinkedWord::PTR);

  // the data shouldn't have any labels in the middle of it, so we can find the end of the array
  // by searching for the label after the start label.
  const auto& start_label = labels.at(pointer_to_data.label_id());
  int end_label_idx =
      index_of_closest_following_label_in_segment(start_label.offset, my_seg, labels);

  int end_offset = all_words.at(my_seg).size() * 4;
  if (end_label_idx < 0) {
    lg::warn(
        "Failed to find label: likely just an unimplemented case for when the data is the last "
        "thing in the file.");
  } else {
    const auto& end_label = labels.at(end_label_idx);
    end_offset = end_label.offset;
  }

  // fmt::print("Data is from {} to {}\n", start_label.name, end_label.name);

  // now we can figure out the size
  int size_bytes = end_offset - start_label.offset;
  int size_elts = size_bytes / stride;  // 32 bytes per ocean-near-index
  int leftover_bytes = size_bytes % stride;
  // fmt::print("Size is {} bytes ({} elts), with {} bytes left over\n", size_bytes,
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
    auto& word = all_words.at(my_seg).at(pad_byte_idx / 4);
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

  // now disassemble:
  std::vector<goos::Object> array_def = {pretty_print::to_symbol(
      fmt::format("new 'static 'inline-array {} {}", array_elt_type.print(), size_elts))};

  for (int elt = 0; elt < size_elts; elt++) {
    // for each element, create a fake temporary label at the start to identify it
    DecompilerLabel fake_label;
    fake_label.target_segment = my_seg;  // same segment
    fake_label.offset = start_label.offset + elt * stride;
    array_def.push_back(
        decompile_at_label(array_elt_type, fake_label, labels, all_words, ts, file));
  }

  // build into a list.
  return pretty_print::build_list(array_def);
}

/*!
 * Decompile the data field of ocean-near-indices, which is an (inline-array ocean-near-index).
 * This is like a C++ ocean_near_index*, meaning we don't know how long the array is.
 * We know all the data in a ocean_near_index is just integers, so we can guess that the end
 * of the array is just the location of the next label.
 * There's a chance that this will include some padding in the array and make it too long,
 * but there is no harm in that.
 */
goos::Object ocean_near_indices_decompile(const std::vector<LinkedWord>& words,
                                          const std::vector<DecompilerLabel>& labels,
                                          int my_seg,
                                          int field_location,
                                          const TypeSystem& ts,
                                          const std::vector<std::vector<LinkedWord>>& all_words,
                                          const LinkedObjectFile* file) {
  return decomp_ref_to_inline_array_guess_size(words, labels, my_seg, field_location, ts, all_words,
                                               file, TypeSpec("ocean-near-index"), 32);
}

goos::Object ocean_mid_masks_decompile(const std::vector<LinkedWord>& words,
                                       const std::vector<DecompilerLabel>& labels,
                                       int my_seg,
                                       int field_location,
                                       const TypeSystem& ts,
                                       const std::vector<std::vector<LinkedWord>>& all_words,
                                       const LinkedObjectFile* file) {
  return decomp_ref_to_inline_array_guess_size(words, labels, my_seg, field_location, ts, all_words,
                                               file, TypeSpec("ocean-mid-mask"), 8);
}

goos::Object sp_field_init_spec_decompile(const std::vector<LinkedWord>& words,
                                          const std::vector<DecompilerLabel>& labels,
                                          int my_seg,
                                          int field_location,
                                          const TypeSystem& ts,
                                          const std::vector<std::vector<LinkedWord>>& all_words,
                                          const LinkedObjectFile* file) {
  return decomp_ref_to_inline_array_guess_size(words, labels, my_seg, field_location, ts, all_words,
                                               file, TypeSpec("sp-field-init-spec"), 16);
}

goos::Object nav_mesh_vertex_arr_decompile(const std::vector<LinkedWord>& words,
                                           const std::vector<DecompilerLabel>& labels,
                                           int my_seg,
                                           int field_location,
                                           const TypeSystem& ts,
                                           const std::vector<std::vector<LinkedWord>>& all_words,
                                           const LinkedObjectFile* file) {
  return decomp_ref_to_inline_array_guess_size(words, labels, my_seg, field_location, ts, all_words,
                                               file, TypeSpec("nav-vertex"), 16);
}

goos::Object nav_mesh_poly_arr_decompile(const std::vector<LinkedWord>& words,
                                         const std::vector<DecompilerLabel>& labels,
                                         int my_seg,
                                         int field_location,
                                         const TypeSystem& ts,
                                         const std::vector<std::vector<LinkedWord>>& all_words,
                                         const LinkedObjectFile* file) {
  return decomp_ref_to_inline_array_guess_size(words, labels, my_seg, field_location, ts, all_words,
                                               file, TypeSpec("nav-poly"), 8);
}

goos::Object nav_mesh_route_arr_decompile(const std::vector<LinkedWord>& words,
                                          const std::vector<DecompilerLabel>& labels,
                                          int my_seg,
                                          int field_location,
                                          const TypeSystem& ts,
                                          const std::vector<std::vector<LinkedWord>>& all_words,
                                          const LinkedObjectFile* file) {
  return decomp_ref_to_inline_array_guess_size(words, labels, my_seg, field_location, ts, all_words,
                                               file, TypeSpec("vector4ub"), 4);
}

goos::Object sp_launch_grp_launcher_decompile(const std::vector<LinkedWord>& words,
                                              const std::vector<DecompilerLabel>& labels,
                                              int my_seg,
                                              int field_location,
                                              const TypeSystem& ts,
                                              const std::vector<std::vector<LinkedWord>>& all_words,
                                              const LinkedObjectFile* file) {
  return decomp_ref_to_inline_array_guess_size(words, labels, my_seg, field_location, ts, all_words,
                                               file, TypeSpec("sparticle-group-item"), 32);
}

goos::Object decompile_sound_spec(const TypeSpec& type,
                                  const DecompilerLabel& label,
                                  const std::vector<DecompilerLabel>& labels,
                                  const std::vector<std::vector<LinkedWord>>& words,
                                  const TypeSystem& ts,
                                  const LinkedObjectFile* file) {
  // auto normal = decompile_structure(type, label, labels, words, ts, file, false);
  // fmt::print("Doing: {}\n", normal.print());
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
      return decompile_structure(type, label, labels, words, ts, file, false);
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
  if (fo_curve) {
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

goos::Object decompile_structure(const TypeSpec& type,
                                 const DecompilerLabel& label,
                                 const std::vector<DecompilerLabel>& labels,
                                 const std::vector<std::vector<LinkedWord>>& words,
                                 const TypeSystem& ts,
                                 const LinkedObjectFile* file,
                                 bool use_fancy_macros) {
  // some structures we want to decompile to fancy macros instead of a raw static definiton
  if (use_fancy_macros) {
    if (type == TypeSpec("sp-field-init-spec")) {
      return decompile_sparticle_field_init(type, label, labels, words, ts, file);
    }
    if (type == TypeSpec("sparticle-group-item")) {
      return decompile_sparticle_group_item(type, label, labels, words, ts, file);
    }
    if (type == TypeSpec("sound-spec")) {
      return decompile_sound_spec(type, label, labels, words, ts, file);
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
      throw std::runtime_error("Basic does not start with type pointer");
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
        return decompile_at_label(actual_type, label, labels, words, ts, file);
      } else {
        throw std::runtime_error(
            fmt::format("Basic has the wrong type pointer, got {} expected {} at label {}:{}",
                        word.symbol_name(), actual_type.base_type(), label.name, label.offset));
      }
    }
  }

  int word_count = (type_info->get_size_in_memory() + 3) / 4;

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
      // fmt::print("{}\n", error);
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
  for (int i = 0; i < word_count; i++) {
    auto& w = obj_words.at(i);
    switch (w.kind()) {
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
      // field has nothing in it, just skip it.
      continue;
    }

    if (any_overlap) {
      // for now, let's just skip fields that overlapped with the previous.
      // eventually we should do something smarter here...
      continue;
    }

    // first, let's see if it's a value or reference
    auto field_type_info = ts.lookup_type(field.type());
    if (!field_type_info->is_reference()) {
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
        if (field.name() == "data" && type.print() == "ocean-near-indices") {
          // first, get the label:
          field_defs_out.emplace_back(
              field.name(), ocean_near_indices_decompile(obj_words, labels, label.target_segment,
                                                         field_start, ts, words, file));
        } else if (field.name() == "data" && type.print() == "ocean-mid-masks") {
          field_defs_out.emplace_back(
              field.name(), ocean_mid_masks_decompile(obj_words, labels, label.target_segment,
                                                      field_start, ts, words, file));
        } else if (field.name() == "init-specs" && type.print() == "sparticle-launcher") {
          field_defs_out.emplace_back(
              field.name(), sp_field_init_spec_decompile(obj_words, labels, label.target_segment,
                                                         field_start, ts, words, file));
        } else if (field.name() == "vertex" && type.print() == "nav-mesh") {
          field_defs_out.emplace_back(
              field.name(), nav_mesh_vertex_arr_decompile(obj_words, labels, label.target_segment,
                                                          field_start, ts, words, file));
        } else if (field.name() == "poly" && type.print() == "nav-mesh") {
          field_defs_out.emplace_back(
              field.name(), nav_mesh_poly_arr_decompile(obj_words, labels, label.target_segment,
                                                        field_start, ts, words, file));
        } else if (field.name() == "route" && type.print() == "nav-mesh") {
          field_defs_out.emplace_back(
              field.name(), nav_mesh_route_arr_decompile(obj_words, labels, label.target_segment,
                                                         field_start, ts, words, file));
        } else if (field.name() == "launcher" && type.print() == "sparticle-launch-group") {
          field_defs_out.emplace_back(field.name(), sp_launch_grp_launcher_decompile(
                                                        obj_words, labels, label.target_segment,
                                                        field_start, ts, words, file));
        } else if (field.name() == "col-mesh-indexes" && type.print() == "ropebridge-tuning") {
          field_defs_out.emplace_back(
              field.name(), decomp_ref_to_integer_array_guess_size(
                                obj_words, labels, label.target_segment, field_start, ts, words,
                                file, TypeSpec("uint8"), 1));
        } else {
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
              bytes_out.push_back(obj_words.at(byte_idx / 4).get_byte(byte_idx % 4));
            }
            field_defs_out.emplace_back(field.name(), decompile_value(field.type(), bytes_out, ts));
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
            field.name(), decompile_at_label(field.type(), fake_label, labels, words, ts, file));
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
              decompile_at_label(field.type(), fake_label, labels, words, ts, file));
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
                                                   words, ts, file));
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

      } else if (field.is_dynamic() || field.is_array() || field.is_inline()) {
        throw std::runtime_error(fmt::format(
            "Dynamic/array/inline reference field {} type {} in static data not yet implemented",
            field.name(), actual_type.print()));
      } else {
        // then we expect a label.
        ASSERT(field_end - field_start == 4);
        auto& word = obj_words.at(field_start / 4);

        if (word.kind() == LinkedWord::PTR) {
          if (field.type() == TypeSpec("symbol")) {
            continue;
          }
          field_defs_out.emplace_back(
              field.name(), decompile_at_label(field.type(), labels.at(word.label_id()), labels,
                                               words, ts, file));
        } else if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
          // do nothing, the default is zero?
          field_defs_out.emplace_back(field.name(), pretty_print::to_symbol("0"));
        } else if (word.kind() == LinkedWord::SYM_PTR) {
          if (word.symbol_name() == "#f" || word.symbol_name() == "#t") {
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
    if (str.length() < 40) {
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
                             const TypeSystem& ts) {
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

  // try as common integer types:
  if (ts.tc(TypeSpec("uint32"), type)) {
    ASSERT(bytes.size() == 4);
    u32 value;
    memcpy(&value, bytes.data(), 4);
    return pretty_print::to_symbol(fmt::format("#x{:x}", u64(value)));
  } else if (ts.tc(TypeSpec("int32"), type)) {
    ASSERT(bytes.size() == 4);
    s32 value;
    memcpy(&value, bytes.data(), 4);
    if (value > 100) {
      return pretty_print::to_symbol(fmt::format("#x{:x}", value));
    } else {
      return pretty_print::to_symbol(fmt::format("{}", value));
    }
  } else if (ts.tc(TypeSpec("uint16"), type)) {
    ASSERT(bytes.size() == 2);
    u16 value;
    memcpy(&value, bytes.data(), 2);
    return pretty_print::to_symbol(fmt::format("#x{:x}", u64(value)));
  } else if (ts.tc(TypeSpec("int16"), type)) {
    ASSERT(bytes.size() == 2);
    s16 value;
    memcpy(&value, bytes.data(), 2);
    if (value > 100) {
      return pretty_print::to_symbol(fmt::format("#x{:x}", value));
    } else {
      return pretty_print::to_symbol(fmt::format("{}", value));
    }
  } else if (ts.tc(TypeSpec("int8"), type)) {
    ASSERT(bytes.size() == 1);
    s8 value;
    memcpy(&value, bytes.data(), 1);
    if (value > 5) {
      return pretty_print::to_symbol(fmt::format("#x{:x}", value));
    } else {
      return pretty_print::to_symbol(fmt::format("{}", value));
    }
  } else if (type == TypeSpec("seconds") || type == TypeSpec("time-frame")) {
    ASSERT(bytes.size() == 8);
    s64 value;
    memcpy(&value, bytes.data(), 8);

    return pretty_print::to_symbol(
        fmt::format("(seconds {})", fixed_point_to_string(value, TICKS_PER_SECOND)));
  } else if (ts.tc(TypeSpec("uint64"), type)) {
    ASSERT(bytes.size() == 8);
    u64 value;
    memcpy(&value, bytes.data(), 8);
    return pretty_print::to_symbol(fmt::format("#x{:x}", value));
  } else if (ts.tc(TypeSpec("int64"), type)) {
    ASSERT(bytes.size() == 8);
    s64 value;
    memcpy(&value, bytes.data(), 8);
    if (value > 100) {
      return pretty_print::to_symbol(fmt::format("#x{:x}", value));
    } else {
      return pretty_print::to_symbol(fmt::format("{}", value));
    }
  } else if (type == TypeSpec("meters")) {
    ASSERT(bytes.size() == 4);
    float value;
    memcpy(&value, bytes.data(), 4);
    double meters = (double)value / METER_LENGTH;
    auto rep = pretty_print::float_representation(meters);
    if (rep.print().find("the-as") != std::string::npos) {
      return rep;
      // return pretty_print::build_list("the-as", "meters", rep);
    } else {
      return pretty_print::to_symbol(fmt::format("(meters {})", meters_to_string(value)));
    }
  } else if (type == TypeSpec("degrees")) {
    ASSERT(bytes.size() == 4);
    float value;
    memcpy(&value, bytes.data(), 4);
    double degrees = (double)value / DEGREES_LENGTH;
    return pretty_print::build_list("degrees", pretty_print::float_representation(degrees));
  } else if (ts.tc(TypeSpec("float"), type)) {
    ASSERT(bytes.size() == 4);
    float value;
    memcpy(&value, bytes.data(), 4);
    return pretty_print::float_representation(value);
  } else if (ts.tc(TypeSpec("uint8"), type)) {
    ASSERT(bytes.size() == 1);
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
                                   const TypeSystem& ts,
                                   const LinkedObjectFile* file,
                                   const std::optional<TypeSpec>& content_type_override) {
  TypeSpec content_type;
  auto type_ptr_word_idx = (label.offset / 4) - 1;
  if ((label.offset % 8) == 4) {
    auto& type_ptr = words.at(label.target_segment).at(type_ptr_word_idx);
    if (type_ptr.kind() != LinkedWord::TYPE_PTR) {
      throw std::runtime_error("Invalid basic in decompile_boxed_array");
    }
    if (type_ptr.symbol_name() == "array") {
      auto content_type_ptr_word_idx = type_ptr_word_idx + 3;
      auto& content_type_ptr = words.at(label.target_segment).at(content_type_ptr_word_idx);
      if (content_type_ptr.kind() != LinkedWord::TYPE_PTR) {
        throw std::runtime_error("Invalid content in decompile_boxed_array");
      }
      content_type = TypeSpec(content_type_ptr.symbol_name());
    } else {
      throw std::runtime_error("Wrong basic type in decompile_boxed_array");
    }
  } else {
    throw std::runtime_error("Invalid alignment in decompile_boxed_array");
  }

  if (content_type_override) {
    content_type = *content_type_override;
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
  auto params_obj = array_length == array_allocated_length
                        ? pretty_print::to_symbol(fmt::format("new 'static 'boxed-array :type {}",
                                                              content_type.print()))
                        : pretty_print::to_symbol(fmt::format(
                              "new 'static 'boxed-array :type {} :length {} :allocated-length {}",
                              content_type.print(), array_length, array_allocated_length));
  if (content_type_info->is_reference() || content_type == TypeSpec("object")) {
    // easy, stride of 4.
    std::vector<goos::Object> result = {params_obj};

    for (int elt = 0; elt < array_length; elt++) {
      auto& word = words.at(label.target_segment).at(first_elt_word_idx + elt);
      if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
        result.push_back(pretty_print::to_symbol("0"));
      } else if (word.kind() == LinkedWord::PTR) {
        if (content_type == TypeSpec("object")) {
          result.push_back(
              decompile_at_label_guess_type(labels.at(word.label_id()), labels, words, ts, file));
        } else {
          result.push_back(decompile_at_label(content_type, labels.at(word.label_id()), labels,
                                              words, ts, file));
        }
      } else if (word.kind() == LinkedWord::SYM_PTR) {
        result.push_back(pretty_print::to_symbol(fmt::format("'{}", word.symbol_name())));
      } else {
        if (content_type == TypeSpec("object") && word.kind() == LinkedWord::PLAIN_DATA &&
            (word.data & 0b111) == 0) {
          s32 val = word.data;
          result.push_back(pretty_print::to_symbol(fmt::format("(the binteger {})", val / 8)));
        } else {
          throw std::runtime_error(
              fmt::format("Unknown content type in boxed array of references, word idx {}",
                          first_elt_word_idx + elt));
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
          content_type.get_single_arg(), ts.get_deref_info(content_type).stride));
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
          throw std::runtime_error(
              fmt::format("Got bad word of kind {} in boxed array of values", word.kind()));
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
                                const LinkedObjectFile* file) {
  if (word.kind() == LinkedWord::PTR) {
    auto& label = labels.at(word.label_id());
    auto guessed_type = get_type_of_label(label, words);
    if (!guessed_type.has_value()) {
      throw std::runtime_error("Could not guess the type of " + label.name);
    }

    if (guessed_type == TypeSpec("pair")) {
      return decompile_pair(label, labels, words, ts, false, file);
    }

    return decompile_at_label(*guessed_type, label, labels, words, ts, file);
  } else if (word.kind() == LinkedWord::PLAIN_DATA && word.data == 0) {
    // do nothing, the default is zero?
    return pretty_print::to_symbol("0");
  } else if (word.kind() == LinkedWord::SYM_PTR) {
    // never quote symbols in a list.
    return pretty_print::to_symbol(fmt::format("{}", word.symbol_name()));
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
}  // namespace

goos::Object decompile_pair(const DecompilerLabel& label,
                            const std::vector<DecompilerLabel>& labels,
                            const std::vector<std::vector<LinkedWord>>& words,
                            const TypeSystem& ts,
                            bool add_quote,
                            const LinkedObjectFile* file) {
  if ((label.offset % 8) != 2) {
    if ((label.offset % 4) != 0) {
      throw std::runtime_error(fmt::format("Invalid alignment for pair {}\n", label.offset % 16));
    } else {
      auto& word = words.at(label.target_segment).at(label.offset / 4);
      if (word.kind() != LinkedWord::EMPTY_PTR) {
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
      list_tokens.push_back(decompile_pair_elt(car_word, labels, words, ts, file));

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
      if (cdr_word.kind() == LinkedWord::PTR) {
        to_print = labels.at(cdr_word.label_id());
        continue;
      }
      // invalid.
      lg::error(
          "There is an improper list. This is probably okay, but should be checked manually "
          "because we could not find a test case yet.");
      list_tokens.push_back(pretty_print::to_symbol("."));
      list_tokens.push_back(decompile_pair_elt(cdr_word, labels, words, ts, file));
      if (add_quote) {
        return pretty_print::build_list("quote", pretty_print::build_list(list_tokens));
      } else {
        return pretty_print::build_list(list_tokens);
      }
    } else {
      if ((to_print.offset % 4) != 0) {
        throw std::runtime_error(
            fmt::format("Invalid alignment for pair {}\n", to_print.offset % 16));
      } else {
        auto& word = words.at(to_print.target_segment).at(to_print.offset / 4);
        if (word.kind() != LinkedWord::EMPTY_PTR) {
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
            words.at(to_print.target_segment).at(to_print.offset / 4), labels, words, ts, file));
        if (add_quote) {
          return pretty_print::build_list("quote", pretty_print::build_list(list_tokens));
        } else {
          return pretty_print::build_list(list_tokens);
        }
      }
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
