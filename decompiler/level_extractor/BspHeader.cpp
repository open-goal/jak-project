#include "BspHeader.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/goal_data_reader.h"
#include "decompiler/util/Error.h"
#include "common/dma/dma.h"

namespace level_tools {

std::string DrawStats::print() const {
  std::string result;
  result += fmt::format("tfrag tris: {}\n", total_tfrag_tris);
  result += fmt::format("tie prototype tris: {}\n", total_tie_prototype_tris);
  result += fmt::format("actors: {}\n", total_actors);
  result += fmt::format("instances: {}\n", total_tie_instances);
  result += fmt::format("total tfragments: {}\n", total_tfragments);
  return result;
}

void Vector::read_from_file(Ref ref) {
  if ((ref.byte_offset % 16) != 0) {
    throw Error("misaligned vector");
  }
  for (int i = 0; i < 4; i++) {
    const auto& word = ref.data->words_by_seg.at(ref.seg).at((ref.byte_offset / 4) + i);
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("vector didn't get plain data.");
    }
    memcpy(data + i, &word.data, 4);
  }
}

void Matrix4h::read_from_file(Ref ref) {
  if ((ref.byte_offset % 16) != 0) {
    throw Error("misaligned Matrix4h");
  }
  for (int i = 0; i < 8; i++) {
    const auto& word = ref.data->words_by_seg.at(ref.seg).at((ref.byte_offset / 4) + i);
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("Matrix4h didn't get plain data.");
    }
    memcpy(&data[i * 2], &word.data, 4);
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

void FileInfo::read_from_file(TypedRef ref, const decompiler::DecompilerTypeSystem& dts) {
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
                                         const decompiler::DecompilerTypeSystem& /*dts*/,
                                         DrawStats* /*stats*/) {
  type_name = ref.type->get_name();
}

std::string DrawableTreeUnknown::print(const PrintSettings& /*settings*/, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}???\n", is, type_name);
  return result;
}

std::string DrawableTreeUnknown::my_type() const {
  return type_name;
}

void DrawableInlineArrayUnknown::read_from_file(TypedRef ref,
                                                const decompiler::DecompilerTypeSystem& /*dts*/,
                                                DrawStats* /*stats*/) {
  type_name = ref.type->get_name();
}

std::string DrawableInlineArrayUnknown::print(const PrintSettings& /*settings*/, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}???\n", is, type_name);
  return result;
}

std::string DrawableInlineArrayUnknown::my_type() const {
  return type_name;
}

std::unique_ptr<Drawable> make_draw_node_child(TypedRef ref,
                                               const decompiler::DecompilerTypeSystem& dts,
                                               DrawStats* stats) {
  if (ref.type->get_name() == "draw-node") {
    auto result = std::make_unique<DrawNode>();
    result->read_from_file(ref, dts, stats);
    return result;
  } else if (ref.type->get_name() == "tfragment") {
    auto result = std::make_unique<TFragment>();
    result->read_from_file(ref, dts, stats);
    return result;
  } else if (ref.type->get_name() == "instance-tie") {
    auto result = std::make_unique<InstanceTie>();
    result->read_from_file(ref, dts, stats);
    return result;
  } else if (ref.type->get_name() == "drawable-actor") {
    auto result = std::make_unique<DrawableActor>();
    result->read_from_file(ref, dts, stats);
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
  } else if (type == "instance-tie") {
    return 64;
  } else if (type == "drawable-actor") {
    return 32;
  } else {
    throw Error("unknown child for stride: {}", type);
  }
}

void TFragmentDebugData::read_from_file(Ref ref,
                                        const decompiler::DecompilerTypeSystem& /*dts*/,
                                        DrawStats* stats) {
  u32 data[4];
  auto& words = ref.data->words_by_seg.at(ref.seg);
  for (int i = 0; i < 4; i++) {
    auto& word = words.at((ref.byte_offset / 4) + i);
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("debug data word type: {}\n", (int)word.kind());
    }
    data[i] = word.data;
  }

  memcpy(num_tris, data, 8);
  memcpy(num_dverts, data + 2, 8);

  u32 tris = 0;
  for (auto num_tri : num_tris) {
    tris = std::max(tris, (u32)num_tri);
  }
  stats->total_tfrag_tris += tris;

  auto& debug_word = words.at(4 + (ref.byte_offset / 4));
  if (debug_word.kind() != decompiler::LinkedWord::PLAIN_DATA || debug_word.data != 0) {
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

void tfrag_debug_print_unpack(Ref start, int qwc_total) {
  int word_offset = 0;

  while (word_offset < qwc_total * 4) {
    VifCode next(deref_u32(start, word_offset));
    fmt::print("{} at: {} bytes, {} qw\n", next.print(), word_offset * 4, word_offset / 4);
    word_offset++;
    switch (next.kind) {
      case VifCode::Kind::UNPACK_V4_16: {
        VifCodeUnpack up(next);
        assert(up.use_tops_flag == true);
        assert(up.is_unsigned == true);
        // assert(up.addr_qw == 0);
        int qw_to_write = next.num;
        assert(next.num);
        for (int qw = 0; qw < qw_to_write; qw++) {
          u32 words[2];
          words[0] = deref_u32(start, word_offset++);
          words[1] = deref_u32(start, word_offset++);
          u16 unpacked[4];
          memcpy(unpacked, words, 8);
          fmt::print("  [{:3d} {:3d} {:3d} {:3d}]\n", unpacked[0], unpacked[1], unpacked[2],
                     unpacked[3]);
        }

      } break;

      case VifCode::Kind::UNPACK_V4_32: {
        VifCodeUnpack up(next);
        assert(up.use_tops_flag == true);
        assert(up.is_unsigned == false);
        // assert(up.addr_qw == 9);
        assert(next.num);
        for (int qw = 0; qw < next.num; qw++) {
          u32 words[4];
          words[0] = deref_u32(start, word_offset++);
          words[1] = deref_u32(start, word_offset++);
          words[2] = deref_u32(start, word_offset++);
          words[3] = deref_u32(start, word_offset++);
          fmt::print("  [{:3d} {:3d} {:3d} {:3d}]\n", words[0], words[1], words[2], words[3]);
        }
      } break;

      case VifCode::Kind::UNPACK_V3_32: {
        VifCodeUnpack up(next);
        assert(up.use_tops_flag == true);
        assert(up.is_unsigned == false);
        // assert(up.addr_qw == 19);
        assert(next.num);
        for (int qw = 0; qw < next.num; qw++) {
          u32 words[3];
          words[0] = deref_u32(start, word_offset++);
          words[1] = deref_u32(start, word_offset++);
          words[2] = deref_u32(start, word_offset++);
          fmt::print("  [{:3d} {:3d} {:3d}]\n", words[0], words[1], words[2]);
        }
      } break;

      case VifCode::Kind::STROW: {
        u32 words[4];
        words[0] = deref_u32(start, word_offset++);
        words[1] = deref_u32(start, word_offset++);
        words[2] = deref_u32(start, word_offset++);
        words[3] = deref_u32(start, word_offset++);
        fmt::print(" row data [{:3d} {:3d} {:3d} {:3d}]\n", words[0], words[1], words[2], words[3]);
      } break;

      case VifCode::Kind::STMOD: {
      } break;

      case VifCode::Kind::STCYCL:
        break;

      case VifCode::Kind::UNPACK_V4_8: {
        VifCodeUnpack up(next);
        assert(up.use_tops_flag == true);
        //        assert(up.is_unsigned == false);
        //        assert(up.addr_qw == 129);
        assert(next.num);
        for (int qw = 0; qw < next.num; qw++) {
          s8 words[4];
          u32 all = deref_u32(start, word_offset++);
          memcpy(words, &all, 4);
          fmt::print("  [{:3d} {:3d} {:3d} {:3d}]\n", words[0], words[1], words[2], words[3]);
        }
      } break;
      case VifCode::Kind::NOP:
        break;
      default:
        fmt::print("unknown: {}\n", next.print());
        assert(false);
    }
  }
  fmt::print("-------------------------------------------\n");
}

std::vector<u8> read_dma_chain(Ref& start, u32 qwc) {
  std::vector<u8> result;
  result.resize(qwc * 16);
  for (u32 word = 0; word < qwc * 4; word++) {
    u32 x = deref_u32(start, word);
    memcpy(result.data() + (word * 4), &x, 4);
  }
  return result;
}

void TFragment::read_from_file(TypedRef ref,
                               const decompiler::DecompilerTypeSystem& dts,
                               DrawStats* stats) {
  stats->total_tfragments++;
  id = read_plain_data_field<s16>(ref, "id", dts);
  color_index = read_plain_data_field<s16>(ref, "color-index", dts);
  debug_data.read_from_file(deref_label(get_field_ref(ref, "debug-data", dts)), dts, stats);
  // todo color_indices
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto dma_field = get_field_ref(ref, "dma-qwc", dts);
  auto& dma_word = ref.ref.data->words_by_seg.at(dma_field.seg).at(dma_field.byte_offset / 4);
  if (dma_word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
    throw Error("bad dma qwc word");
  }
  memcpy(dma_qwc, &dma_word.data, 4);

  auto dma_slot = get_field_ref(ref, "dma-chain", dts);

  struct DmaInfo {
    Ref ref;
    std::string label_name;
  };
  DmaInfo dmas[3];
  for (int i = 0; i < 3; i++) {
    auto& word = dma_slot.data->words_by_seg.at(dma_slot.seg).at((dma_slot.byte_offset / 4));
    dmas[i].ref = deref_label(dma_slot);
    dmas[i].label_name = dma_slot.data->labels.at(word.label_id()).name;

    dma_slot.byte_offset += 4;
  }

  if (stats->debug_print_dma_data) {
    // first, common
    fmt::print("DMA COMMON {}, {} qwc:\n", dmas[0].label_name, dma_qwc[0]);
    tfrag_debug_print_unpack(dmas[0].ref, dma_qwc[0]);

    // next "base"
    fmt::print("DMA BASE {}, {} qwc:\n", dmas[1].label_name, dma_qwc[1]);
    tfrag_debug_print_unpack(dmas[1].ref, dma_qwc[1]);

    // next "level0"
    //  fmt::print("DMA LEVEL0 {}, {} qwc:\n", dmas[0].label_name, dma_qwc[3]);
    //  tfrag_debug_print_unpack(dmas[0].ref, dma_qwc[3]);

    // next "level1"
    fmt::print("DMA LEVEL1 {}, {} qwc:\n", dmas[2].label_name, dma_qwc[2]);
    tfrag_debug_print_unpack(dmas[2].ref, dma_qwc[2]);

    fmt::print("qwc's: {} {} {} {}\n", dma_qwc[0], dma_qwc[1], dma_qwc[2], dma_qwc[3]);
  }

  num_base_colors = read_plain_data_field<u8>(ref, "num-base-colors", dts);
  num_level0_colors = read_plain_data_field<u8>(ref, "num-level0-colors", dts);
  num_level1_colors = read_plain_data_field<u8>(ref, "num-level1-colors", dts);
  num_shaders = read_plain_data_field<u8>(ref, "num-shaders", dts);
  color_offset = read_plain_data_field<u8>(ref, "color-offset", dts);
  color_count = read_plain_data_field<u8>(ref, "color-count", dts);
  dma_base = read_dma_chain(dmas[1].ref, dma_qwc[1]);

  if (num_level0_colors > 0 || num_level1_colors > 0) {
    // if we're base only, this has junk in it, and it wouldn't be used anyway.
    dma_common_and_level0 = read_dma_chain(dmas[0].ref, std::max(dma_qwc[3], dma_qwc[0]));
  }

  dma_level1 = read_dma_chain(dmas[2].ref, dma_qwc[2]);

  // color indices
  int num_actual_colors = std::max(num_base_colors, std::max(num_level0_colors, num_level1_colors));
  int num_colors = ((num_actual_colors + 3) / 4) * 4;
  // each color is a u64 (4x u16 indices)
  // color_indices.resize(4 * num_colors);

  // 24 = 12 * u32
  auto color_idx_start = deref_label(get_field_ref(ref, "color-indices", dts));
  for (int i = 0; i < num_colors / 4; i++) {
    u32 low = deref_u32(color_idx_start, i * 2);
    u32 high = deref_u32(color_idx_start, i * 2 + 1);
    color_indices.push_back(low & 0xffff);
    color_indices.push_back(low >> 16);
    color_indices.push_back(high & 0xffff);
    color_indices.push_back(high >> 16);
  }
  assert((int)color_indices.size() == num_colors);

  // todo shader

  assert(num_colors / 4 == color_count);
  // fmt::print("colors: {} {} {}\n", num_base_colors, num_level0_colors, num_level1_colors);
  assert(read_plain_data_field<u8>(ref, "pad0", dts) == 0);
  assert(read_plain_data_field<u8>(ref, "pad1", dts) == 0);
  assert(read_plain_data_field<u32>(ref, "generic-u32", dts) == 0);
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

void TieFragment::read_from_file(TypedRef ref,
                                 const decompiler::DecompilerTypeSystem& dts,
                                 DrawStats* stats) {
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  num_tris = read_plain_data_field<u16>(ref, "num-tris", dts);
  num_dverts = read_plain_data_field<u16>(ref, "num-dverts", dts);
  tex_count = read_plain_data_field<u16>(ref, "tex-count", dts);
  gif_count = read_plain_data_field<u16>(ref, "gif-count", dts);
  vertex_count = read_plain_data_field<u16>(ref, "vertex-count", dts);

  auto gif_data_ref = deref_label(get_field_ref(ref, "gif-ref", dts));

  assert((tex_count % 5) == 0);
  u32 total_gif_qw = tex_count + gif_count;
  gif_data.resize(16 * total_gif_qw);
  for (u32 i = 0; i < total_gif_qw * 4; i++) {
    auto& word =
        gif_data_ref.data->words_by_seg.at(gif_data_ref.seg).at((gif_data_ref.byte_offset / 4) + i);
    assert(word.kind() == decompiler::LinkedWord::PLAIN_DATA);
    memcpy(gif_data.data() + (i * 4), &word.data, 4);
  }

  auto points_data_ref = deref_label(get_field_ref(ref, "point-ref", dts));
  point_ref.resize(16 * vertex_count);
  debug_label_name = inspect_ref(get_field_ref(ref, "point-ref", dts));
  for (u32 i = 0; i < vertex_count * 4; i++) {
    auto& word = points_data_ref.data->words_by_seg.at(points_data_ref.seg)
                     .at((points_data_ref.byte_offset / 4) + i);
    assert(word.kind() == decompiler::LinkedWord::PLAIN_DATA);
    memcpy(point_ref.data() + (i * 4), &word.data, 4);
  }

  stats->total_tie_prototype_tris += num_tris;
}

std::string TieFragment::print(const PrintSettings& /*settings*/, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());
  result += fmt::format("{}num-tris: {}\n", is, num_tris);
  result += fmt::format("{}num-dverts: {}\n", is, num_dverts);
  return result;
}

void DrawableActor::read_from_file(TypedRef ref,
                                   const decompiler::DecompilerTypeSystem& dts,
                                   DrawStats* stats) {
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  stats->total_actors++;
}

std::string DrawableActor::print(const PrintSettings& /*settings*/, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());
  return result;
}

void InstanceTie::read_from_file(TypedRef ref,
                                 const decompiler::DecompilerTypeSystem& dts,
                                 DrawStats* stats) {
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  bucket_index = read_plain_data_field<u16>(ref, "bucket-index", dts);
  id = read_plain_data_field<s16>(ref, "id", dts);
  flags = read_plain_data_field<u16>(ref, "flags", dts);
  //  assert(flags == 0); // TODO
  origin.read_from_file(get_field_ref(ref, "origin", dts));
  wind_index = read_plain_data_field<u16>(ref, "wind-index", dts);
  color_indices = deref_label(get_field_ref(ref, "color-indices", dts));
  stats->total_tie_instances++;
}

std::string InstanceTie::print(const PrintSettings& /*settings*/, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());
  result += fmt::format("{}bucket-index: {}\n", is, bucket_index);
  return result;
}

void DrawNode::read_from_file(TypedRef ref,
                              const decompiler::DecompilerTypeSystem& dts,
                              DrawStats* stats) {
  id = read_plain_data_field<s16>(ref, "id", dts);             // 4
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));  // 16
  child_count = read_plain_data_field<u8>(ref, "child-count", dts);
  flags = read_plain_data_field<u8>(ref, "flags", dts);
  // todo child
  auto first_child = get_field_ref(ref, "child", dts);
  auto first_child_obj = deref_label(first_child);
  first_child_obj.byte_offset -= 4;

  for (int i = 0; i < child_count; i++) {
    children.push_back(
        make_draw_node_child(typed_ref_from_basic(first_child_obj, dts), dts, stats));
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
                                             const decompiler::DecompilerTypeSystem& dts,
                                             DrawStats* stats) {
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
    draw_nodes.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats);
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
                                              const decompiler::DecompilerTypeSystem& dts,
                                              DrawStats* stats) {
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
    tfragments.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats);
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

void DrawableInlineArrayInstanceTie::read_from_file(TypedRef ref,
                                                    const decompiler::DecompilerTypeSystem& dts,
                                                    DrawStats* stats) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref obj_ref = data_ref;
    obj_ref.byte_offset += 64 * i;  // todo not a constant here
    auto type = get_type_of_basic(obj_ref);
    if (type != "instance-tie") {
      throw Error("bad draw node type: {}", type);
    }
    instances.emplace_back();
    instances.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats);
  }
}

std::string DrawableInlineArrayInstanceTie::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_draw_node) {
    for (size_t i = 0; i < instances.size(); i++) {
      result += fmt::format("{}draw-nodes [{}] ({}):\n", is, i, instances[i].my_type());
      result += instances[i].print(settings, next_indent);
    }
  }

  return result;
}

std::string DrawableInlineArrayInstanceTie::my_type() const {
  return "drawable-inline-array-instance-tie";
}

std::string PrototypeTie::my_type() const {
  return "prototype-tie";
}

void PrototypeTie::read_from_file(TypedRef ref,
                                  const decompiler::DecompilerTypeSystem& dts,
                                  DrawStats* stats) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref obj_ref = data_ref;
    obj_ref.byte_offset += 64 * i;  // todo not a constant here
    auto type = get_type_of_basic(obj_ref);
    if (type != "tie-fragment") {
      throw Error("bad draw node type: {}", type);
    }
    tie_fragments.emplace_back();
    tie_fragments.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats);
  }
}

std::string PrototypeTie::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_draw_node) {
    for (size_t i = 0; i < tie_fragments.size(); i++) {
      result += fmt::format("{}draw-nodes [{}] ({}):\n", is, i, tie_fragments[i].my_type());
      result += tie_fragments[i].print(settings, next_indent);
    }
  }

  return result;
}

std::unique_ptr<DrawableInlineArray> make_drawable_inline_array(
    TypedRef ref,
    const decompiler::DecompilerTypeSystem& dts,
    DrawStats* stats) {
  if (ref.type->get_name() == "drawable-inline-array-node") {
    auto result = std::make_unique<DrawableInlineArrayNode>();
    result->read_from_file(ref, dts, stats);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-tfrag") {
    auto result = std::make_unique<DrawableInlineArrayTFrag>();
    result->read_from_file(ref, dts, stats);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-trans-tfrag") {
    auto result = std::make_unique<DrawableInlineArrayTransTFrag>();
    result->read_from_file(ref, dts, stats);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-instance-tie") {
    auto result = std::make_unique<DrawableInlineArrayInstanceTie>();
    result->read_from_file(ref, dts, stats);
    return result;
  }
  auto result = std::make_unique<DrawableInlineArrayUnknown>();
  result->read_from_file(ref, dts, stats);
  return result;
}

void DrawableTreeTfrag::read_from_file(TypedRef ref,
                                       const decompiler::DecompilerTypeSystem& dts,
                                       DrawStats* stats) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  if ((data_ref.byte_offset % 4) != 0) {
    throw Error("misaligned data array");
  }

  auto palette = deref_label(get_field_ref(ref, "time-of-day-pal", dts));
  time_of_day.width = deref_u32(palette, 0);

  assert(time_of_day.width == 8);
  time_of_day.height = deref_u32(palette, 1);
  time_of_day.pad = deref_u32(palette, 2);
  assert(time_of_day.pad == 0);
  for (int i = 0; i < int(8 * time_of_day.height); i++) {
    time_of_day.colors.push_back(deref_u32(palette, 3 + i));
  }

  for (int idx = 0; idx < length; idx++) {
    Ref array_slot_ref = data_ref;
    array_slot_ref.byte_offset += idx * 4;

    Ref object_ref = deref_label(array_slot_ref);
    object_ref.byte_offset -= 4;

    arrays.push_back(make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats));
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

void DrawableTreeActor::read_from_file(TypedRef ref,
                                       const decompiler::DecompilerTypeSystem& dts,
                                       DrawStats* stats) {
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

    arrays.push_back(make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats));
  }
}

std::string DrawableTreeActor::print(const PrintSettings& settings, int indent) const {
  if (!settings.expand_drawable_tree_actor) {
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

std::string DrawableTreeActor::my_type() const {
  return "drawable-tree-actor";
}

void PrototypeBucketTie::read_from_file(TypedRef ref,
                                        const decompiler::DecompilerTypeSystem& dts,
                                        DrawStats* stats) {
  name = read_string_field(ref, "name", dts, true);
  flags = read_plain_data_field<u32>(ref, "flags", dts);
  assert(flags == 0 || flags == 2);
  in_level = read_plain_data_field<u16>(ref, "in-level", dts);
  utextures = read_plain_data_field<u16>(ref, "utextures", dts);
  // todo drawables
  dists.read_from_file(get_field_ref(ref, "dists", dts));
  rdists.read_from_file(get_field_ref(ref, "rdists", dts));
  stiffness = read_plain_data_field<float>(ref, "stiffness", dts);

  auto next_slot = get_field_ref(ref, "next", dts);
  for (int i = 0; i < 4; i++) {
    auto& word = ref.ref.data->words_by_seg.at(next_slot.seg).at(i + (next_slot.byte_offset / 4));
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("bad word type in PrototypeBucketTie next");
    }
    next[i] = word.data;
  }

  auto count_slot = get_field_ref(ref, "count", dts);
  for (int i = 0; i < 2; i++) {
    auto& word = ref.ref.data->words_by_seg.at(count_slot.seg).at(i + (count_slot.byte_offset / 4));
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("bad word type in PrototypeBucketTie count");
    }
    memcpy(count + 2 * i, &word.data, 4);
  }

  auto block_slot = get_field_ref(ref, "generic-count", dts);
  u8* block_start = (u8*)generic_count;
  for (int i = 0; i < 12; i++) {
    auto& word = ref.ref.data->words_by_seg.at(block_slot.seg).at(i + (block_slot.byte_offset / 4));
    if (word.kind() != decompiler::LinkedWord::PLAIN_DATA) {
      throw Error("bad word type in PrototypeBucketTie slot");
    }
    memcpy(block_start + 4 * i, &word.data, 4);
  }

  auto geom_start = get_field_ref(ref, "geometry", dts);
  for (int i = 0; i < 4; i++) {
    auto geom = deref_label(geom_start);
    geom.byte_offset -= 4;

    if (get_type_of_basic(geom) != "prototype-tie") {
      throw Error("bad type in prototype-bucket-tie: {}", get_type_of_basic(geom));
    }
    geometry[i].read_from_file(typed_ref_from_basic(geom, dts), dts, stats);
    geom_start.byte_offset += 4;
  }

  for (auto x : next) {
    assert(x == 0);
  }
  for (auto x : count) {
    assert(x == 0);
  }
  for (auto x : generic_count) {
    assert(x == 0);
  }
  for (auto x : generic_next) {
    assert(x == 0);
  }

  // get the color count data
  {
    u32 num_color_qwcs = 0;
    for (int i = 0; i < 4; i++) {
      u32 start = index_start[i];
      u32 end = start + frag_count[i];
      // fmt::print("i = {}: {} -> {}\n", i, start, end);
      assert(num_color_qwcs <= end);
      num_color_qwcs = std::max(end, num_color_qwcs);
    }

    auto data_array = get_field_ref(ref, "color-index-qwc", dts);
    for (u32 i = 0; i < num_color_qwcs; i++) {
      int byte_offset = data_array.byte_offset + i;
      auto word = data_array.data->words_by_seg.at(data_array.seg).at(byte_offset / 4);
      color_index_qwc.push_back(word.get_byte(byte_offset % 4));
    }
  }

  // get the colors
  auto palette = deref_label(get_field_ref(ref, "tie-colors", dts));
  time_of_day.width = deref_u32(palette, 0);

  assert(time_of_day.width == 8);
  time_of_day.height = deref_u32(palette, 1);
  time_of_day.pad = deref_u32(palette, 2);
  assert(time_of_day.pad == 0);
  for (int i = 0; i < int(8 * time_of_day.height); i++) {
    time_of_day.colors.push_back(deref_u32(palette, 3 + i));
  }
}

std::string PrototypeBucketTie::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  // int next_indent = indent + 4;
  result += fmt::format("{}name: {}\n", is, name);
  result += fmt::format("{}flags: {}\n", is, flags);
  result += fmt::format("{}in_level: {}\n", is, in_level);
  result += fmt::format("{}utextures: {}\n", is, utextures);
  if (settings.expand_drawable_tree_tie_proto_data) {
    for (int i = 0; i < 4; i++) {
      result += fmt::format("{}geometry[{}]:\n", is, i);
      result += geometry[i].print(settings, indent + 4);
    }
    result += fmt::format("{}dists: {}", is, dists.print_meters());
    result += fmt::format("{}rdists: {}", is, rdists.print());
    //  result += fmt::format("{}next: [{}, {}, {}, {}]\n", is, next[0], next[1], next[2], next[3]);
    //  result += fmt::format("{}count: [{}, {}, {}, {}]\n", is, count[0], count[1], count[2],
    //  count[3]); result += fmt::format("{}generic_count: [{}, {}, {}, {}]\n", is,
    //  generic_count[0],
    //                        generic_count[1], generic_count[2], generic_count[3]);
    //  result += fmt::format("{}generic_next: [{}, {}, {}, {}]\n", is, generic_next[0],
    //  generic_next[1],
    //                        generic_next[2], generic_next[3]);
    result += fmt::format("{}frag_count: [{}, {}, {}, {}]\n", is, frag_count[0], frag_count[1],
                          frag_count[2], frag_count[3]);
    result += fmt::format("{}index_start: [{}, {}, {}, {}]\n", is, index_start[0], index_start[1],
                          index_start[2], index_start[3]);
    result += fmt::format("{}base_qw: [{}, {}, {}, {}]\n", is, base_qw[0], base_qw[1], base_qw[2],
                          base_qw[3]);
    result += fmt::format("{}envmap_rfade: {}\n", is, envmap_rfade);
    result += fmt::format("{}envmap_fade_far: {} m\n", is, envmap_fade_far / 4096.f);
  }

  return result;
}

void PrototypeArrayTie::read_from_file(TypedRef ref,
                                       const decompiler::DecompilerTypeSystem& dts,
                                       DrawStats* stats) {
  length = read_plain_data_field<u32>(ref, "length", dts);
  allocated_length = read_plain_data_field<u32>(ref, "allocated-length", dts);
  content_type = read_type_field(ref, "content-type", dts, true);
  auto data_ref = get_field_ref(ref, "data", dts);

  for (u32 i = 0; i < length; i++) {
    Ref slot = data_ref;
    slot.byte_offset += 4 * i;
    Ref thing = deref_label(slot);
    thing.byte_offset -= 4;
    auto type = get_type_of_basic(thing);
    if (type != "prototype-bucket-tie") {
      throw Error("bad type in PrototypeArrayTie data: {}\n", type);
    }
    data.emplace_back();
    data.back().read_from_file(typed_ref_from_basic(thing, dts), dts, stats);
  }
}

std::string PrototypeArrayTie::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}allocated-length: {}\n", is, allocated_length);
  result += fmt::format("{}content-type: {}\n", is, content_type);

  for (u32 i = 0; i < data.size(); i++) {
    result += fmt::format("{}data [{}]:\n", is, i);
    result += data[i].print(settings, next_indent);
  }
  return result;
}

void ProxyPrototypeArrayTie::read_from_file(TypedRef ref,
                                            const decompiler::DecompilerTypeSystem& dts,
                                            DrawStats* stats) {
  prototype_array_tie.read_from_file(
      get_and_check_ref_to_basic(ref, "prototype-array-tie", "prototype-array-tie", dts), dts,
      stats);
  // TODO wind
}

std::string ProxyPrototypeArrayTie::print(const PrintSettings& settings, int indent) const {
  // just inline it for now
  return prototype_array_tie.print(settings, indent);
}

void DrawableTreeInstanceTie::read_from_file(TypedRef ref,
                                             const decompiler::DecompilerTypeSystem& dts,
                                             DrawStats* stats) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto pt = deref_label(get_field_ref(ref, "prototypes", dts));
  pt.byte_offset -= 4;

  prototypes.read_from_file(typed_ref_from_basic(pt, dts), dts, stats);

  auto data_ref = get_field_ref(ref, "data", dts);
  if ((data_ref.byte_offset % 4) != 0) {
    throw Error("misaligned data array");
  }
  for (int idx = 0; idx < length; idx++) {
    Ref array_slot_ref = data_ref;
    array_slot_ref.byte_offset += idx * 4;

    Ref object_ref = deref_label(array_slot_ref);
    object_ref.byte_offset -= 4;

    arrays.push_back(make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats));
  }
}

std::string DrawableTreeInstanceTie::print(const PrintSettings& settings, int indent) const {
  if (!settings.expand_drawable_tree_instance_tie && !settings.expand_drawable_tree_tie_proto) {
    return "";
  }
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_drawable_tree_tie_proto) {
    result += fmt::format("{}prototypes:\n", is);
    result += prototypes.print(settings, next_indent);
  }

  if (settings.expand_drawable_tree_instance_tie) {
    for (size_t i = 0; i < arrays.size(); i++) {
      result += fmt::format("{}arrays [{}] ({}):\n", is, i, arrays[i]->my_type());
      result += arrays[i]->print(settings, next_indent);
    }
  }

  return result;
}

std::string DrawableTreeInstanceTie::my_type() const {
  return "drawable-tree-instance-tie";
}

std::unique_ptr<DrawableTree> make_drawable_tree(TypedRef ref,
                                                 const decompiler::DecompilerTypeSystem& dts,
                                                 DrawStats* stats) {
  if (ref.type->get_name() == "drawable-tree-tfrag") {
    auto tree = std::make_unique<DrawableTreeTfrag>();
    tree->read_from_file(ref, dts, stats);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-trans-tfrag") {
    auto tree = std::make_unique<DrawableTreeTransTfrag>();
    tree->read_from_file(ref, dts, stats);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-lowres-tfrag") {
    auto tree = std::make_unique<DrawableTreeLowresTfrag>();
    tree->read_from_file(ref, dts, stats);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-dirt-tfrag") {
    auto tree = std::make_unique<DrawableTreeDirtTfrag>();
    tree->read_from_file(ref, dts, stats);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-ice-tfrag") {
    auto tree = std::make_unique<DrawableTreeIceTfrag>();
    tree->read_from_file(ref, dts, stats);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-instance-tie") {
    auto tree = std::make_unique<DrawableTreeInstanceTie>();
    tree->read_from_file(ref, dts, stats);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-actor") {
    auto tree = std::make_unique<DrawableTreeActor>();
    tree->read_from_file(ref, dts, stats);
    return tree;
  }
  auto tree = std::make_unique<DrawableTreeUnknown>();
  tree->read_from_file(ref, dts, stats);
  return tree;
}

void DrawableTreeArray::read_from_file(TypedRef ref,
                                       const decompiler::DecompilerTypeSystem& dts,
                                       DrawStats* stats) {
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

    trees.push_back(make_drawable_tree(typed_ref_from_basic(object_ref, dts), dts, stats));
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
                               const decompiler::DecompilerTypeSystem& dts,
                               DrawStats* stats) {
  TypedRef ref;
  ref.ref.byte_offset = 0;
  ref.ref.seg = 0;
  ref.ref.data = &file;
  ref.type = dts.ts.lookup_type("bsp-header");

  file_info.read_from_file(get_and_check_ref_to_basic(ref, "info", "file-info", dts), dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  visible_list_length = read_plain_data_field<s32>(ref, "visible-list-length", dts);

  drawable_tree_array.read_from_file(
      get_and_check_ref_to_basic(ref, "drawable-trees", "drawable-tree-array", dts), dts, stats);

  texture_remap_table.clear();

  s32 tex_remap_len = read_plain_data_field<s32>(ref, "texture-remap-table-len", dts);

  if (tex_remap_len > 0) {
    auto tex_remap_data = deref_label(get_field_ref(ref, "texture-remap-table", dts));
    for (int entry = 0; entry < tex_remap_len; entry++) {
      u64 low = deref_u32(tex_remap_data, 2 * entry);
      u64 high = deref_u32(tex_remap_data, 2 * entry + 1);
      TextureRemap remap;
      remap.original_texid = low;
      remap.new_texid = high;
      texture_remap_table.push_back(remap);
    }
  }
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