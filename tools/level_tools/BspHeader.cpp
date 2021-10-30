#include "BspHeader.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "tools/level_tools/goal_data_reader.h"
#include "tools/level_tools/Error.h"

namespace level_tools {

void Vector::read_from_file(Ref ref) {
  if ((ref.byte_offset % 16) != 0) {
    throw Error("misaligned vector");
  }
  for (int i = 0; i < 4; i++) {
    const auto& word = ref.data->words_by_seg.at(ref.seg).at((ref.byte_offset / 4) + i);
    if (word.kind != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("vector didn't get plain data.");
    }
    memcpy(data + i, &word.data, 4);
  }
}

std::string Vector::print(int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}<vector {} {} {} {}>\n", is, data[0], data[1], data[2], data[3]);
  return result;
}

std::string Vector::print_meters(int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}<vector-meters {:.4f} {:.4f} {:.4f} {:.4f}>\n", is, data[0] / 4096.f,
                        data[1] / 4096.f, data[2] / 4096.f, data[3] / 4096.f);
  return result;
}

void FileInfo::read_from_file(TypedRef ref,
                              const decompiler::LinkedObjectFile& file,
                              const decompiler::DecompilerTypeSystem& dts) {
  file_type = read_type_field(ref, "file-type", dts, true);
  file_name = read_string_field(ref, "file-name", dts, true);
  major_version = read_plain_data_field<u32>(ref, "major-version", dts);
  minor_version = read_plain_data_field<u32>(ref, "minor-version", dts);
  maya_file_name = read_string_field(ref, "maya-file-name", dts, true);
  tool_debug = read_string_field(ref, "tool-debug", dts, true);
  mdb_file_name = read_string_field(ref, "mdb-file-name", dts, true);
}

std::string FileInfo::print(int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}file-type: {}\n", is, file_type);
  result += fmt::format("{}file-name: \"{}\"\n", is, file_name);
  result += fmt::format("{}major-version: {}\n", is, major_version);
  result += fmt::format("{}minor-version: {}\n", is, minor_version);
  result += fmt::format("{}maya-file-name: \"{}\"\n", is, maya_file_name);
  result += fmt::format("{}tool-debug: \"{}\"\n", is, tool_debug);
  result += fmt::format("{}mdb-file-name: \"{}\"\n", is, mdb_file_name);
  return result;
}

void DrawableTreeUnknown::read_from_file(TypedRef ref,
                                         const decompiler::DecompilerTypeSystem& dts) {
  type_name = ref.type->get_name();
}

std::string DrawableTreeUnknown::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}???\n", is, type_name);
  return result;
}

std::string DrawableTreeUnknown::my_type() const {
  return type_name;
}

void DrawableInlineArrayUnknown::read_from_file(TypedRef ref,
                                                const decompiler::DecompilerTypeSystem& dts) {
  type_name = ref.type->get_name();
}

std::string DrawableInlineArrayUnknown::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}???\n", is, type_name);
  return result;
}

std::string DrawableInlineArrayUnknown::my_type() const {
  return type_name;
}

std::unique_ptr<Drawable> make_draw_node_child(TypedRef ref,
                                               const decompiler::DecompilerTypeSystem& dts) {
  if (ref.type->get_name() == "draw-node") {
    auto result = std::make_unique<DrawNode>();
    result->read_from_file(ref, dts);
    return result;
  } else if (ref.type->get_name() == "tfragment") {
    auto result = std::make_unique<TFragment>();
    result->read_from_file(ref, dts);
    return result;
  } else {
    throw Error("Unknown child of draw node: {}\n", ref.type->get_name());
  }
}

int get_child_stride(const std::string& type) {
  if (type == "draw-node") {
    return 32;
  } else if (type == "tfragment") {
    return 64;
  } else {
    throw Error("unknown child for stride: {}", type);
  }
}

void TFragmentDebugData::read_from_file(Ref ref, const decompiler::DecompilerTypeSystem& dts) {
  u32 data[4];
  auto& words = ref.data->words_by_seg.at(ref.seg);
  for (int i = 0; i < 4; i++) {
    auto& word = words.at((ref.byte_offset / 4) + i);
    if (word.kind != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("debug data word type: {}\n", (int)word.kind);
    }
    data[i] = word.data;
  }

  memcpy(num_tris, data, 8);
  memcpy(num_dverts, data + 2, 8);

  auto& debug_word = words.at(4 + (ref.byte_offset / 4));
  if (debug_word.kind != decompiler::LinkedWord::PLAIN_DATA || debug_word.data != 0) {
    throw Error("got debug word.");
  }
}

std::string TFragmentDebugData::print(int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}tris: [{}, {}, {}, {}]\n", is, num_tris[0], num_tris[1], num_tris[2],
                        num_tris[3]);
  result += fmt::format("{}dverts: [{}, {}, {}, {}]\n", is, num_dverts[0], num_dverts[1],
                        num_dverts[2], num_dverts[3]);
  return result;
}

void TFragment::read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  color_index = read_plain_data_field<s16>(ref, "color-index", dts);
  debug_data.read_from_file(deref_label(get_field_ref(ref, "debug-data", dts)), dts);
  // todo color_indices
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  // todo dma
  // todo dma
  // todo dma
  auto dma_field = get_field_ref(ref, "dma-qwc", dts);
  auto& dma_word = ref.ref.data->words_by_seg.at(dma_field.seg).at(dma_field.byte_offset / 4);
  if (dma_word.kind != decompiler::LinkedWord::PLAIN_DATA) {
    throw Error("bad dma qwc word");
  }
  memcpy(dma_qwc, &dma_word.data, 4);

  // todo shader
  num_shaders = read_plain_data_field<u8>(ref, "num-shaders", dts);
  num_base_colors = read_plain_data_field<u8>(ref, "num-base-colors", dts);
  num_level0_colors = read_plain_data_field<u8>(ref, "num-level0-colors", dts);
  num_level1_colors = read_plain_data_field<u8>(ref, "num-level1-colors", dts);
  color_offset = read_plain_data_field<u8>(ref, "color-offset", dts);
  color_count = read_plain_data_field<u8>(ref, "color-count", dts);
  assert(read_plain_data_field<u8>(ref, "pad0", dts) == 0);
  assert(read_plain_data_field<u8>(ref, "pad1", dts) == 0);
  // todo generic
}

std::string TFragment::print(const PrintSettings& settings, int indent) const {
  if (!settings.print_tfrag) {
    return "";
  }
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}color-index: {}\n", is, color_index);
  result += fmt::format("{}debug-data:\n", is);
  result += debug_data.print(indent + 4);
  // debug data
  // color indices
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());
  // dma
  // dma
  // dma
  // dma qwc
  result += fmt::format("{}dma-qwc: [{}, {}, {}, {}]\n", is, dma_qwc[0], dma_qwc[1], dma_qwc[2],
                        dma_qwc[3]);
  // shader
  result += fmt::format("{}num-shaders: {}\n", is, num_shaders);
  result += fmt::format("{}num-base-colors: {}\n", is, num_base_colors);
  result += fmt::format("{}num-level0-colors: {}\n", is, num_level0_colors);
  result += fmt::format("{}num-level1-colors: {}\n", is, num_level1_colors);
  result += fmt::format("{}color-offset: {}\n", is, color_offset);
  result += fmt::format("{}color-count: {}\n", is, color_count);
  // generic
  return result;
}

void DrawNode::read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts) {
  id = read_plain_data_field<s16>(ref, "id", dts);             // 4
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));  // 16
  child_count = read_plain_data_field<u8>(ref, "child-count", dts);
  flags = read_plain_data_field<u8>(ref, "flags", dts);
  // todo child
  auto first_child = get_field_ref(ref, "child", dts);
  auto first_child_obj = deref_label(first_child);
  first_child_obj.byte_offset -= 4;

  for (int i = 0; i < child_count; i++) {
    children.push_back(make_draw_node_child(typed_ref_from_basic(first_child_obj, dts), dts));
    first_child_obj.byte_offset += get_child_stride(get_type_of_basic(first_child_obj));
  }

  distance = read_plain_data_field<float>(ref, "distance", dts);
}

std::string DrawNode::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}child-count: {}\n", is, child_count);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());
  result += fmt::format("{}flags: {}\n", is, flags);

  if (settings.expand_draw_node) {
    for (size_t i = 0; i < children.size(); i++) {
      result += fmt::format("{}children [{}] ({}):\n", is, i, children[i]->my_type());
      result += children[i]->print(settings, next_indent);
    }
  }

  return result;
}

std::string DrawNode::my_type() const {
  return "draw-node";
}

void DrawableInlineArrayNode::read_from_file(TypedRef ref,
                                             const decompiler::DecompilerTypeSystem& dts) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref obj_ref = data_ref;
    obj_ref.byte_offset += 32 * i;  // todo not a constant here
    auto type = get_type_of_basic(obj_ref);
    if (type != "draw-node") {
      throw Error("bad draw node type: {}", type);
    }
    draw_nodes.emplace_back();
    draw_nodes.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts);
  }
}

std::string DrawableInlineArrayNode::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_draw_node) {
    for (size_t i = 0; i < draw_nodes.size(); i++) {
      result += fmt::format("{}draw-nodes [{}] ({}):\n", is, i, draw_nodes[i].my_type());
      result += draw_nodes[i].print(settings, next_indent);
    }
  }

  return result;
}

std::string DrawableInlineArrayNode::my_type() const {
  return "drawable-inline-array-node";
}

void DrawableInlineArrayTFrag::read_from_file(TypedRef ref,
                                              const decompiler::DecompilerTypeSystem& dts) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref obj_ref = data_ref;
    obj_ref.byte_offset += 64 * i;  // todo not a constant here
    auto type = get_type_of_basic(obj_ref);
    if (type != "tfragment") {
      throw Error("bad draw node type: {}", type);
    }
    tfragments.emplace_back();
    tfragments.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts);
  }
}

std::string DrawableInlineArrayTFrag::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_draw_node) {
    for (size_t i = 0; i < tfragments.size(); i++) {
      result += fmt::format("{}draw-nodes [{}] ({}):\n", is, i, tfragments[i].my_type());
      result += tfragments[i].print(settings, next_indent);
    }
  }

  return result;
}

std::string DrawableInlineArrayTFrag::my_type() const {
  return "drawable-inline-array-tfrag";
}

std::unique_ptr<DrawableInlineArray> make_drawable_inline_array(
    TypedRef ref,
    const decompiler::DecompilerTypeSystem& dts) {
  if (ref.type->get_name() == "drawable-inline-array-node") {
    auto result = std::make_unique<DrawableInlineArrayNode>();
    result->read_from_file(ref, dts);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-tfrag") {
    auto result = std::make_unique<DrawableInlineArrayTFrag>();
    result->read_from_file(ref, dts);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-trans-tfrag") {
    auto result = std::make_unique<DrawableInlineArrayTransTFrag>();
    result->read_from_file(ref, dts);
    return result;
  }
  auto result = std::make_unique<DrawableInlineArrayUnknown>();
  result->read_from_file(ref, dts);
  return result;
}

void DrawableTreeTfrag::read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  if ((data_ref.byte_offset % 4) != 0) {
    throw Error("misaligned data array");
  }

  for (int idx = 0; idx < length; idx++) {
    Ref array_slot_ref = data_ref;
    array_slot_ref.byte_offset += idx * 4;

    Ref object_ref = deref_label(array_slot_ref);
    object_ref.byte_offset -= 4;

    arrays.push_back(make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts));
  }
}

std::string DrawableTreeTfrag::print(const PrintSettings& settings, int indent) const {
  if (!settings.expand_drawable_tree_tfrag && my_type() == "drawable-tree-tfrag") {
    return "";
  }

  if (!settings.expand_drawable_tree_trans_tfrag && my_type() == "drawable-tree-trans-tfrag") {
    return "";
  }
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  for (size_t i = 0; i < arrays.size(); i++) {
    result += fmt::format("{}arrays [{}] ({}):\n", is, i, arrays[i]->my_type());
    result += arrays[i]->print(settings, next_indent);
  }

  return result;
}

std::string DrawableTreeTfrag::my_type() const {
  return "drawable-tree-tfrag";
}

std::unique_ptr<DrawableTree> make_drawable_tree(TypedRef ref,
                                                 const decompiler::DecompilerTypeSystem& dts) {
  if (ref.type->get_name() == "drawable-tree-tfrag") {
    auto tree = std::make_unique<DrawableTreeTfrag>();
    tree->read_from_file(ref, dts);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-trans-tfrag") {
    auto tree = std::make_unique<DrawableTreeTransTfrag>();
    tree->read_from_file(ref, dts);
    return tree;
  }
  auto tree = std::make_unique<DrawableTreeUnknown>();
  tree->read_from_file(ref, dts);
  return tree;
}

void DrawableTreeArray::read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);

  auto trees_ref = get_field_ref(ref, "trees", dts);
  if ((trees_ref.byte_offset % 4) != 0) {
    throw Error("misaligned trees array");
  }

  for (int idx = 0; idx < length; idx++) {
    Ref array_slot_ref = trees_ref;
    array_slot_ref.byte_offset += idx * 4;

    Ref object_ref = deref_label(array_slot_ref);
    object_ref.byte_offset -= 4;

    trees.push_back(make_drawable_tree(typed_ref_from_basic(object_ref, dts), dts));
  }
}

std::string DrawableTreeArray::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  int next_indent = indent + 4;
  std::string result;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);

  for (size_t i = 0; i < trees.size(); i++) {
    result += fmt::format("{}trees [{}] ({}):\n", is, i, trees[i]->my_type());
    result += trees[i]->print(settings, next_indent);
  }

  return result;
}

void BspHeader::read_from_file(const decompiler::LinkedObjectFile& file,
                               const decompiler::DecompilerTypeSystem& dts) {
  TypedRef ref;
  ref.ref.byte_offset = 0;
  ref.ref.seg = 0;
  ref.ref.data = &file;
  ref.type = dts.ts.lookup_type("bsp-header");

  file_info.read_from_file(get_and_check_ref_to_basic(ref, "info", "file-info", dts), file, dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  visible_list_length = read_plain_data_field<s32>(ref, "visible-list-length", dts);

  drawable_tree_array.read_from_file(
      get_and_check_ref_to_basic(ref, "drawable-trees", "drawable-tree-array", dts), dts);
}

std::string BspHeader::print(const PrintSettings& settings) const {
  std::string result;
  int next_indent = 4;

  result += fmt::format("file-info:\n");
  result += file_info.print(next_indent);

  result += fmt::format("bsphere:\n");
  result += bsphere.print_meters(next_indent);

  result += fmt::format("visible-list-length: {}\n", visible_list_length);

  result += fmt::format("drawable-trees:\n");
  result += drawable_tree_array.print(settings, next_indent);
  return result;
}
}  // namespace level_tools