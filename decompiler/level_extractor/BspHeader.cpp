#include "BspHeader.h"

#include "common/dma/dma.h"
#include "common/log/log.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/Error.h"
#include "decompiler/util/goal_data_reader.h"

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

std::string Vector::print_decimal(int indent) const {
  s32 d[4];
  memcpy(d, data, 16);
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}<vector {:d} {:d} {:d} {:d}>\n", is, d[0], d[1], d[2], d[3]);
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
                                         DrawStats* /*stats*/,
                                         GameVersion /*version*/) {
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
                                                DrawStats* /*stats*/,
                                                GameVersion /*version*/) {
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
                                               DrawStats* stats,
                                               GameVersion version) {
  if (ref.type->get_name() == "draw-node") {
    auto result = std::make_unique<DrawNode>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  } else if (ref.type->get_name() == "tfragment") {
    auto result = std::make_unique<TFragment>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  } else if (ref.type->get_name() == "instance-tie") {
    auto result = std::make_unique<InstanceTie>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  } else if (ref.type->get_name() == "drawable-actor") {
    auto result = std::make_unique<DrawableActor>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  } else if (ref.type->get_name() == "instance-shrubbery") {
    auto result = std::make_unique<shrub_types::InstanceShrubbery>();
    result->read_from_file(ref, dts, stats, version);
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
  } else if (type == "instance-shrubbery") {
    return 80;
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

void tfrag_debug_print_unpack(Ref start, int qwc_total) {
  int word_offset = 0;

  while (word_offset < qwc_total * 4) {
    VifCode next(deref_u32(start, word_offset));
    lg::debug("{} at: {} bytes, {} qw", next.print(), word_offset * 4, word_offset / 4);
    word_offset++;
    switch (next.kind) {
      case VifCode::Kind::UNPACK_V4_16: {
        VifCodeUnpack up(next);
        ASSERT(up.use_tops_flag == true);
        ASSERT(up.is_unsigned == true);
        // ASSERT(up.addr_qw == 0);
        int qw_to_write = next.num;
        ASSERT(next.num);
        for (int qw = 0; qw < qw_to_write; qw++) {
          u32 words[2];
          words[0] = deref_u32(start, word_offset++);
          words[1] = deref_u32(start, word_offset++);
          u16 unpacked[4];
          memcpy(unpacked, words, 8);
          lg::debug("  [{:3d} {:3d} {:3d} {:3d}]", unpacked[0], unpacked[1], unpacked[2],
                    unpacked[3]);
        }

      } break;

      case VifCode::Kind::UNPACK_V4_32: {
        VifCodeUnpack up(next);
        ASSERT(up.use_tops_flag == true);
        ASSERT(up.is_unsigned == false);
        // ASSERT(up.addr_qw == 9);
        ASSERT(next.num);
        for (int qw = 0; qw < next.num; qw++) {
          u32 words[4];
          words[0] = deref_u32(start, word_offset++);
          words[1] = deref_u32(start, word_offset++);
          words[2] = deref_u32(start, word_offset++);
          words[3] = deref_u32(start, word_offset++);
          lg::debug("  [{:3d} {:3d} {:3d} {:3d}]", words[0], words[1], words[2], words[3]);
        }
      } break;

      case VifCode::Kind::UNPACK_V3_32: {
        VifCodeUnpack up(next);
        ASSERT(up.use_tops_flag == true);
        ASSERT(up.is_unsigned == false);
        // ASSERT(up.addr_qw == 19);
        ASSERT(next.num);
        for (int qw = 0; qw < next.num; qw++) {
          u32 words[3];
          words[0] = deref_u32(start, word_offset++);
          words[1] = deref_u32(start, word_offset++);
          words[2] = deref_u32(start, word_offset++);
          lg::debug("  [{:3d} {:3d} {:3d}]", words[0], words[1], words[2]);
        }
      } break;

      case VifCode::Kind::STROW: {
        u32 words[4];
        words[0] = deref_u32(start, word_offset++);
        words[1] = deref_u32(start, word_offset++);
        words[2] = deref_u32(start, word_offset++);
        words[3] = deref_u32(start, word_offset++);
        lg::debug(" row data [{:3d} {:3d} {:3d} {:3d}]", words[0], words[1], words[2], words[3]);
      } break;

      case VifCode::Kind::STMOD: {
      } break;

      case VifCode::Kind::STCYCL:
        break;

      case VifCode::Kind::UNPACK_V4_8: {
        VifCodeUnpack up(next);
        ASSERT(up.use_tops_flag == true);
        //        ASSERT(up.is_unsigned == false);
        //        ASSERT(up.addr_qw == 129);
        ASSERT(next.num);
        for (int qw = 0; qw < next.num; qw++) {
          s8 words[4];
          u32 all = deref_u32(start, word_offset++);
          memcpy(words, &all, 4);
          lg::debug("  [{:3d} {:3d} {:3d} {:3d}]", words[0], words[1], words[2], words[3]);
        }
      } break;
      case VifCode::Kind::NOP:
        break;
      default:
        ASSERT_MSG(false, fmt::format("unknown: {}", next.print()));
    }
  }
  lg::debug("-------------------------------------------");
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
                               DrawStats* stats,
                               GameVersion version) {
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
    lg::info("DMA COMMON {}, {} qwc:", dmas[0].label_name, dma_qwc[0]);
    tfrag_debug_print_unpack(dmas[0].ref, dma_qwc[0]);

    // next "base"
    lg::info("DMA BASE {}, {} qwc:", dmas[1].label_name, dma_qwc[1]);
    tfrag_debug_print_unpack(dmas[1].ref, dma_qwc[1]);

    // next "level0"
    //  lg::print("DMA LEVEL0 {}, {} qwc:\n", dmas[0].label_name, dma_qwc[3]);
    //  tfrag_debug_print_unpack(dmas[0].ref, dma_qwc[3]);

    // next "level1"
    lg::info("DMA LEVEL1 {}, {} qwc:", dmas[2].label_name, dma_qwc[2]);
    tfrag_debug_print_unpack(dmas[2].ref, dma_qwc[2]);

    lg::info("qwc's: {} {} {} {}", dma_qwc[0], dma_qwc[1], dma_qwc[2], dma_qwc[3]);
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
  ASSERT((int)color_indices.size() == num_colors);

  // todo shader

  ASSERT(num_colors / 4 == color_count);
  // lg::print("colors: {} {} {}\n", num_base_colors, num_level0_colors, num_level1_colors);
  if (version == GameVersion::Jak1) {
    ASSERT(read_plain_data_field<u8>(ref, "pad0", dts) == 0);
    ASSERT(read_plain_data_field<u8>(ref, "pad1", dts) == 0);
    ASSERT(read_plain_data_field<u32>(ref, "generic-u32", dts) == 0);
  } else {
    ASSERT(read_plain_data_field<u32>(ref, "generic-u32", dts) == 0);
  }
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

void memcpy_plain_data(u8* dst, const Ref& ref, size_t size_bytes) {
  const auto& words = ref.data->words_by_seg.at(ref.seg);
  for (size_t i = 0; i < size_bytes; i++) {
    size_t byte_offset = ref.byte_offset + i;
    u8 byte = words.at(byte_offset / 4).get_byte(byte_offset % 4);
    memcpy(dst + i, &byte, sizeof(u8));
  }
}

void TieFragment::read_from_file(TypedRef ref,
                                 const decompiler::DecompilerTypeSystem& dts,
                                 DrawStats* stats,
                                 GameVersion version) {
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  switch (version) {
    case GameVersion::Jak1:
      num_tris = read_plain_data_field<u16>(ref, "num-tris", dts);
      num_dverts = read_plain_data_field<u16>(ref, "num-dverts", dts);
      break;
    case GameVersion::Jak2: {
      auto debug_data_ref = TypedRef(deref_label(get_field_ref(ref, "debug", dts)),
                                     dts.ts.lookup_type("tie-fragment-debug"));
      num_tris = read_plain_data_field<u16>(debug_data_ref, "num-tris", dts);
      num_dverts = read_plain_data_field<u16>(debug_data_ref, "num-dverts", dts);
    } break;
    default:
      ASSERT(false);
  }

  tex_count = read_plain_data_field<u16>(ref, "tex-count", dts);
  gif_count = read_plain_data_field<u16>(ref, "gif-count", dts);
  vertex_count = read_plain_data_field<u16>(ref, "vertex-count", dts);

  auto gif_data_ref = deref_label(get_field_ref(ref, "gif-ref", dts));

  ASSERT((tex_count % 5) == 0);
  u32 total_gif_qw = tex_count + gif_count;
  gif_data.resize(16 * total_gif_qw);
  for (u32 i = 0; i < total_gif_qw * 4; i++) {
    auto& word =
        gif_data_ref.data->words_by_seg.at(gif_data_ref.seg).at((gif_data_ref.byte_offset / 4) + i);
    ASSERT(word.kind() == decompiler::LinkedWord::PLAIN_DATA);
    memcpy(gif_data.data() + (i * 4), &word.data, 4);
  }

  auto points_data_ref = deref_label(get_field_ref(ref, "point-ref", dts));
  point_ref.resize(16 * vertex_count);
  debug_label_name = inspect_ref(get_field_ref(ref, "point-ref", dts));
  for (u32 i = 0; i < vertex_count * 4; i++) {
    auto& word = points_data_ref.data->words_by_seg.at(points_data_ref.seg)
                     .at((points_data_ref.byte_offset / 4) + i);
    ASSERT(word.kind() == decompiler::LinkedWord::PLAIN_DATA);
    memcpy(point_ref.data() + (i * 4), &word.data, 4);
  }

  stats->total_tie_prototype_tris += num_tris;

  if (version > GameVersion::Jak1) {
    u16 normals_qwc = read_plain_data_field<u16>(ref, "normal-count", dts);
    if (normals_qwc) {
      normals.resize(16 * normals_qwc);
      auto normals_data_ref = deref_label(get_field_ref(ref, "normal-ref", dts));
      memcpy_plain_data((u8*)normals.data(), normals_data_ref, normals_qwc * 16);
    }
  } else {
    u16 generic_qwc = read_plain_data_field<u32>(ref, "generic-count", dts);
    if (generic_qwc) {
      generic_data.resize(16 * generic_qwc);
      auto generic_data_ref = deref_label(get_field_ref(ref, "generic-ref", dts));
      memcpy_plain_data((u8*)generic_data.data(), generic_data_ref, generic_qwc * 16);
    }
  }
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
                                   DrawStats* stats,
                                   GameVersion /*version*/) {
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
                                 DrawStats* stats,
                                 GameVersion /*version*/) {
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  bucket_index = read_plain_data_field<u16>(ref, "bucket-index", dts);
  id = read_plain_data_field<s16>(ref, "id", dts);
  flags = read_plain_data_field<u16>(ref, "flags", dts);
  //  ASSERT(flags == 0); // TODO
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
                              DrawStats* stats,
                              GameVersion version) {
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
        make_draw_node_child(typed_ref_from_basic(first_child_obj, dts), dts, stats, version));
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
                                             DrawStats* stats,
                                             GameVersion version) {
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
    draw_nodes.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats, version);
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
                                              DrawStats* stats,
                                              GameVersion version) {
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
    tfragments.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats, version);
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
                                                    DrawStats* stats,
                                                    GameVersion version) {
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
    instances.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats, version);
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
                                  DrawStats* stats,
                                  GameVersion version) {
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
    tie_fragments.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats, version);
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
    DrawStats* stats,
    GameVersion version) {
  if (ref.type->get_name() == "drawable-inline-array-node") {
    auto result = std::make_unique<DrawableInlineArrayNode>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-tfrag") {
    auto result = std::make_unique<DrawableInlineArrayTFrag>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-trans-tfrag") {
    auto result = std::make_unique<DrawableInlineArrayTransTFrag>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-tfrag-trans") {
    auto result = std::make_unique<DrawableInlineArrayTFragTrans>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-tfrag-water") {
    auto result = std::make_unique<DrawableInlineArrayTFragWater>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-instance-tie") {
    auto result = std::make_unique<DrawableInlineArrayInstanceTie>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  }

  if (ref.type->get_name() == "drawable-inline-array-instance-shrub") {
    auto result = std::make_unique<shrub_types::DrawableInlineArrayInstanceShrub>();
    result->read_from_file(ref, dts, stats, version);
    return result;
  }

  auto result = std::make_unique<DrawableInlineArrayUnknown>();
  result->read_from_file(ref, dts, stats, version);
  return result;
}

void DrawableTreeTfrag::read_from_file(TypedRef ref,
                                       const decompiler::DecompilerTypeSystem& dts,
                                       DrawStats* stats,
                                       GameVersion version) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  if ((data_ref.byte_offset % 4) != 0) {
    throw Error("misaligned data array");
  }

  auto palette = deref_label(get_field_ref(ref, "time-of-day-pal", dts));
  time_of_day.width = deref_u32(palette, 0);

  ASSERT(time_of_day.width == 8);
  time_of_day.height = deref_u32(palette, 1);
  time_of_day.pad = deref_u32(palette, 2);
  ASSERT(time_of_day.pad == 0);
  for (int i = 0; i < int(8 * time_of_day.height); i++) {
    time_of_day.colors.push_back(deref_u32(palette, 3 + i));
  }

  for (int idx = 0; idx < length; idx++) {
    Ref array_slot_ref = data_ref;
    array_slot_ref.byte_offset += idx * 4;

    Ref object_ref = deref_label(array_slot_ref);
    object_ref.byte_offset -= 4;

    arrays.push_back(
        make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats, version));
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
                                       DrawStats* stats,
                                       GameVersion version) {
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

    arrays.push_back(
        make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats, version));
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
                                        DrawStats* stats,
                                        GameVersion version) {
  name = read_string_field(ref, "name", dts, true);
  switch (version) {
    case GameVersion::Jak1:
      flags = read_plain_data_field<u32>(ref, "flags", dts);
      ASSERT(flags == 0 || flags == 2);
      break;
    case GameVersion::Jak2:
      flags = read_plain_data_field<u16>(ref, "flags", dts);
      break;
    default:
      ASSERT(false);
  }
  in_level = read_plain_data_field<u16>(ref, "in-level", dts);
  utextures = read_plain_data_field<u16>(ref, "utextures", dts);
  dists.read_from_file(get_field_ref(ref, "dists", dts));
  rdists.read_from_file(get_field_ref(ref, "rdists", dts));
  stiffness = read_plain_data_field<float>(ref, "stiffness", dts);

  if (version == GameVersion::Jak1) {
    auto fr = get_field_ref(ref, "collide-frag", dts);
    {
      const auto& word = fr.data->words_by_seg.at(fr.seg).at(fr.byte_offset / 4);
      if (word.kind() == decompiler::LinkedWord::PTR) {
        auto p = deref_label(fr);
        p.byte_offset -= 4;
        collide_frag.read_from_file(typed_ref_from_basic(p, dts), dts, stats, version);
      }
    }
  }

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

  auto block_slot = get_field_ref(ref, "frag-count", dts);
  u8* block_start = (u8*)frag_count;
  for (int i = 0; i < 6; i++) {
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
    geometry[i].read_from_file(typed_ref_from_basic(geom, dts), dts, stats, version);
    geom_start.byte_offset += 4;
  }

  for (auto x : next) {
    ASSERT(x == 0);
  }
  for (auto x : count) {
    ASSERT(x == 0);
  }
  //  for (auto x : generic_count) {
  //    ASSERT(x == 0);
  //  }
  //  for (auto x : generic_next) {
  //    ASSERT(x == 0);
  //  }

  // get the color count data
  {
    u32 num_color_qwcs = 0;
    for (int i = 0; i < 4; i++) {
      u32 start = index_start[i];
      u32 end = start + frag_count[i];
      ASSERT(num_color_qwcs <= end);
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

  ASSERT(time_of_day.width == 8);
  time_of_day.height = deref_u32(palette, 1);
  time_of_day.pad = deref_u32(palette, 2);
  ASSERT(time_of_day.pad == 0);
  for (int i = 0; i < int(8 * time_of_day.height); i++) {
    time_of_day.colors.push_back(deref_u32(palette, 3 + i));
  }

  auto fr = get_field_ref(ref, "envmap-shader", dts);
  const auto& word = fr.data->words_by_seg.at(fr.seg).at(fr.byte_offset / 4);
  if (word.kind() == decompiler::LinkedWord::PTR) {
    has_envmap_shader = true;
    Ref envmap_shader_ref(deref_label(fr));
    for (int i = 0; i < 5 * 16; i++) {
      int byte = envmap_shader_ref.byte_offset + i;
      u8 val = ref.ref.data->words_by_seg.at(envmap_shader_ref.seg).at(byte / 4).get_byte(byte % 4);
      envmap_shader[i] = val;
    }
  }

  if (version > GameVersion::Jak1) {
    u32 tint = read_plain_data_field<u32>(ref, "tint-color", dts);
    memcpy(jak2_tint_color.data(), &tint, 4);
  } else {
    jak2_tint_color.fill(0xff);
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
                                       DrawStats* stats,
                                       GameVersion version) {
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
    data.back().read_from_file(typed_ref_from_basic(thing, dts), dts, stats, version);
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
                                            DrawStats* stats,
                                            GameVersion version) {
  prototype_array_tie.read_from_file(
      get_and_check_ref_to_basic(ref, "prototype-array-tie", "prototype-array-tie", dts), dts,
      stats, version);
  wind_vectors = deref_label(get_field_ref(ref, "wind-vectors", dts));
}

std::string ProxyPrototypeArrayTie::print(const PrintSettings& settings, int indent) const {
  // just inline it for now
  return prototype_array_tie.print(settings, indent);
}

void DrawableTreeInstanceTie::read_from_file(TypedRef ref,
                                             const decompiler::DecompilerTypeSystem& dts,
                                             DrawStats* stats,
                                             GameVersion version) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto pt = deref_label(get_field_ref(ref, "prototypes", dts));
  pt.byte_offset -= 4;

  prototypes.read_from_file(typed_ref_from_basic(pt, dts), dts, stats, version);

  auto data_ref = get_field_ref(ref, "data", dts);
  if ((data_ref.byte_offset % 4) != 0) {
    throw Error("misaligned data array");
  }
  for (int idx = 0; idx < length; idx++) {
    Ref array_slot_ref = data_ref;
    array_slot_ref.byte_offset += idx * 4;

    Ref object_ref = deref_label(array_slot_ref);
    object_ref.byte_offset -= 4;

    arrays.push_back(
        make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats, version));
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

void DrawableTreeCollideFragment::read_from_file(TypedRef ref,
                                                 const decompiler::DecompilerTypeSystem& dts,
                                                 DrawStats* stats,
                                                 GameVersion version) {
  s16 length = read_plain_data_field<s16>(ref, "length", dts);
  auto data_ref = get_field_ref(ref, "data", dts);
  if ((data_ref.byte_offset % 4) != 0) {
    throw Error("misaligned data array");
  }

  Ref array_slot_ref = data_ref;
  array_slot_ref.byte_offset += (length - 1) * 4;

  Ref object_ref = deref_label(array_slot_ref);
  object_ref.byte_offset -= 4;
  last_array.read_from_file(typed_ref_from_basic(object_ref, dts), dts, stats, version);
}

std::string DrawableTreeCollideFragment::print(const PrintSettings& settings, int indent) const {
  return last_array.print(settings, indent);
}

std::string DrawableTreeCollideFragment::my_type() const {
  return "drawable-tree-collide-fragment";
}

void DrawableInlineArrayCollideFragment::read_from_file(TypedRef ref,
                                                        const decompiler::DecompilerTypeSystem& dts,
                                                        DrawStats* stats,
                                                        GameVersion /*version*/) {
  ASSERT(ref.type->get_name() == "drawable-inline-array-collide-fragment");
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref obj_ref = data_ref;
    obj_ref.byte_offset += 32 * i;  // todo not a constant here
    auto type = get_type_of_basic(obj_ref);
    if (type != "collide-fragment") {
      throw Error("bad collide fragment type: {}", type);
    }
    collide_fragments.emplace_back();
    collide_fragments.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats);
  }
}

std::string DrawableInlineArrayCollideFragment::print(const PrintSettings& settings,
                                                      int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_collide) {
    for (u32 i = 0; i < collide_fragments.size(); i++) {
      result += fmt::format("{}data [{}]:\n", is, i);
      result += collide_fragments[i].print(settings, next_indent);
    }
  }
  return result;
}

std::string DrawableInlineArrayCollideFragment::my_type() const {
  return "drawable-inline-array-collide-fragment";
}

void CollideFragment::read_from_file(TypedRef ref,
                                     const decompiler::DecompilerTypeSystem& dts,
                                     DrawStats* stats) {
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  auto r = deref_label(get_field_ref(ref, "mesh", dts));
  r.byte_offset -= 4;
  mesh.read_from_file(typed_ref_from_basic(r, dts), dts, stats);
}

std::string CollideFragment::print(const PrintSettings& settings, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());
  result += mesh.print(settings, indent);

  return result;
}

void CollideFragMesh::read_from_file(TypedRef ref,
                                     const decompiler::DecompilerTypeSystem& dts,
                                     DrawStats* /*stats*/) {
  strip_data_len = read_plain_data_field<u16>(ref, "strip-data-len", dts);
  poly_count = read_plain_data_field<u16>(ref, "poly-count", dts);
  vertex_count = read_plain_data_field<u8>(ref, "vertex-count", dts);
  vertex_data_qwc = read_plain_data_field<u8>(ref, "vertex-data-qwc", dts);
  total_qwc = read_plain_data_field<u8>(ref, "total-qwc", dts);
  base_trans.read_from_file(get_field_ref(ref, "base-trans", dts));
  base_trans.data[3] = 0;

  packed_data = deref_label(get_field_ref(ref, "packed-data", dts));
  pat_array = deref_label(get_field_ref(ref, "pat-array", dts));
}

std::string CollideFragMesh::print(const PrintSettings& /*settings*/, int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}strip-data-len: {}\n", is, strip_data_len);
  result += fmt::format("{}poly-count: {}\n", is, poly_count);
  result += fmt::format("{}vertex-count: {}\n", is, vertex_count);
  result += fmt::format("{}vertex-data-qwc: {}\n", is, vertex_data_qwc);
  result += fmt::format("{}total-qwc: {}\n", is, total_qwc);
  result += fmt::format("{}base-trans: {}", is, base_trans.print_decimal());

  return result;
}

//////////////////////////
// shrub
//////////////////////////

namespace shrub_types {

void DrawableTreeInstanceShrub::read_from_file(TypedRef ref,
                                               const decompiler::DecompilerTypeSystem& dts,
                                               level_tools::DrawStats* stats,
                                               GameVersion version) {
  // the usual drawable stuff
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  // unfortunately, shrub uses the arrays thing differently.
  // there's just one top level array, and the nodes are a bit scattered in memory below that.
  // it doesn't have the 8 child rule
  auto data_ref = get_field_ref(ref, "data", dts);
  if ((data_ref.byte_offset % 4) != 0) {
    throw Error("misaligned data array");
  }
  for (int idx = 0; idx < length; idx++) {
    Ref array_slot_ref = data_ref;
    array_slot_ref.byte_offset += idx * 4;

    Ref object_ref = deref_label(array_slot_ref);
    object_ref.byte_offset -= 4;

    arrays.push_back(
        make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats, version));
  }
  // confirm that we have the weird shrub pattern and only found one array.
  ASSERT(length == 1);

  // now, let's try to discover the remaining arrays (instances).
  // basically we just look after the top level array in memory.
  // once we find something else (the time of day palette) we know we're at the end.
  // the game finds these by traversing the tree, but this is a little easier, and gets us
  // the familiar arrays that we used in tie/tfrag.

  Ref object_ref = deref_label(data_ref);
  object_ref.byte_offset -= 4;
  discovered_arrays.push_back(
      make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats, version));

  bool done = false;
  object_ref.byte_offset += 16;
  while (!done) {
    auto& word = object_ref.data->words_by_seg.at(object_ref.seg).at(object_ref.byte_offset / 4);
    if (word.kind() == decompiler::LinkedWord::TYPE_PTR) {
      if (word.symbol_name() == "drawable-inline-array-node") {
        discovered_arrays.push_back(
            make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats, version));
      } else if (word.symbol_name() == "drawable-inline-array-instance-shrub") {
        discovered_arrays.push_back(
            make_drawable_inline_array(typed_ref_from_basic(object_ref, dts), dts, stats, version));
      } else if (word.symbol_name() == "time-of-day-palette" ||
                 word.symbol_name() == "light-hash" ||
                 word.symbol_name() == "collide-hash-fragment") {
        done = true;
      } else {
        ASSERT(word.symbol_name() == "draw-node" || word.symbol_name() == "instance-shrubbery");
      }
    }
    object_ref.byte_offset += 16;
  }

  // this "info" thing holds all the prototypes
  auto pt = deref_label(get_field_ref(ref, "info", dts));
  pt.byte_offset -= 4;
  info.read_from_file(typed_ref_from_basic(pt, dts), dts, stats, version);

  // time of day palette. we'll want these colors in the FR3 file.
  auto palette = deref_label(get_field_ref(ref, "colors-added", dts));
  time_of_day.width = deref_u32(palette, 0);
  ASSERT(time_of_day.width == 8);
  time_of_day.height = deref_u32(palette, 1);
  time_of_day.pad = deref_u32(palette, 2);
  ASSERT(time_of_day.pad == 0);
  for (int i = 0; i < int(8 * time_of_day.height); i++) {
    time_of_day.colors.push_back(deref_u32(palette, 3 + i));
  }
}

std::string DrawableTreeInstanceShrub::my_type() const {
  return "drawable-tree-instance-shrub";
}

std::string DrawableTreeInstanceShrub::print(const level_tools::PrintSettings& settings,
                                             int indent) const {
  if (!settings.expand_shrub) {
    return "";
  }
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_shrub) {
    for (size_t i = 0; i < discovered_arrays.size(); i++) {
      result += fmt::format("{}arrays [{}] ({}):\n", is, i, discovered_arrays[i]->my_type());
      result += discovered_arrays[i]->print(settings, next_indent);
    }

    result += fmt::format("{}prototypes:\n", is);
    result += info.print(settings, next_indent);
  }

  return result;
}

void InstanceShrubbery::read_from_file(TypedRef ref,
                                       const decompiler::DecompilerTypeSystem& dts,
                                       level_tools::DrawStats* /*stats*/,
                                       GameVersion /*version*/) {
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  bucket_index = read_plain_data_field<u16>(ref, "bucket-index", dts);
  id = read_plain_data_field<s16>(ref, "id", dts);
  origin.read_from_file(get_field_ref(ref, "origin", dts));
  wind_index = read_plain_data_field<u16>(ref, "wind-index", dts);
  color_indices = read_plain_data_field<u32>(ref, "color", dts);
  flat_normal.read_from_file(get_field_ref(ref, "flat-normal", dts));
}

std::string InstanceShrubbery::print(const level_tools::PrintSettings& /*settings*/,
                                     int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());
  result += fmt::format("{}bucket-index: {}\n", is, bucket_index);
  result += fmt::format("{}flat-normal: {}", is, flat_normal.print_meters());
  result += fmt::format("{}color-indices: {}\n", is, color_indices);
  result += fmt::format("{}wind-index: {}\n", is, wind_index);
  return result;
}

void DrawableInlineArrayInstanceShrub::read_from_file(TypedRef ref,
                                                      const decompiler::DecompilerTypeSystem& dts,
                                                      DrawStats* stats,
                                                      GameVersion version) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref obj_ref = data_ref;
    obj_ref.byte_offset += 80 * i;  // todo not a constant here
    auto type = get_type_of_basic(obj_ref);
    if (type != "instance-shrubbery") {
      throw Error("bad draw node type: {}", type);
    }
    instances.emplace_back();
    instances.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats, version);
  }
}

std::string DrawableInlineArrayInstanceShrub::print(const PrintSettings& settings,
                                                    int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  if (settings.expand_shrub) {
    for (size_t i = 0; i < instances.size(); i++) {
      result += fmt::format("{}draw-nodes [{}] ({}):\n", is, i, instances[i].my_type());
      result += instances[i].print(settings, next_indent);
    }
  }

  return result;
}

void PrototypeArrayShrubInfo::read_from_file(TypedRef ref,
                                             const decompiler::DecompilerTypeSystem& dts,
                                             level_tools::DrawStats* stats,
                                             GameVersion version) {
  prototype_inline_array_shrub.read_from_file(
      get_and_check_ref_to_basic(ref, "prototype-inline-array-shrub",
                                 "prototype-inline-array-shrub", dts),
      dts, stats, version);
  wind_vectors = deref_label(get_field_ref(ref, "wind-vectors", dts));
}

std::string PrototypeArrayShrubInfo::print(const level_tools::PrintSettings& settings,
                                           int indent) const {
  return prototype_inline_array_shrub.print(settings, indent);
}

void PrototypeInlineArrayShrub::read_from_file(TypedRef ref,
                                               const decompiler::DecompilerTypeSystem& dts,
                                               level_tools::DrawStats* stats,
                                               GameVersion version) {
  length = read_plain_data_field<s16>(ref, "length", dts);
  auto data_ref = get_field_ref(ref, "data", dts);

  for (int i = 0; i < length; i++) {
    Ref thing = data_ref;
    // note: unlike tie, these are stored in an inline array.
    thing.byte_offset += 112 * i;  // todo - not a constant here
    auto type = get_type_of_basic(thing);
    if (type != "prototype-bucket-shrub") {
      throw Error("bad type in PrototypeInlineArrayShrub data: {}\n", type);
    }
    data.emplace_back();
    data.back().read_from_file(typed_ref_from_basic(thing, dts), dts, stats, version);
  }
}

std::string PrototypeGenericShrub::print(const level_tools::PrintSettings& settings,
                                         int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}length: {}\n", is, length);

  for (u32 i = 0; i < shrubs.size(); i++) {
    result += fmt::format("{}data [{}]:\n", is, i);
    result += shrubs[i].print(settings, next_indent);
  }
  return result;
}

void PrototypeGenericShrub::read_from_file(TypedRef ref,
                                           const decompiler::DecompilerTypeSystem& dts,
                                           level_tools::DrawStats* stats,
                                           GameVersion version) {
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));
  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref thing = data_ref;
    // note: unlike tie, these are stored in an inline array.
    thing.byte_offset += 4 * i;  // 4 byte pointer
    thing = deref_label(thing);
    thing.byte_offset -= 4;  // basic offset
    auto type = get_type_of_basic(thing);
    if (type != "generic-shrub-fragment") {
      throw Error("bad type in PrototypeGenericShrub data: {}\n", type);
    }
    shrubs.emplace_back();
    shrubs.back().read_from_file(typed_ref_from_basic(thing, dts), dts, stats, version);
  }
}

std::string PrototypeInlineArrayShrub::print(const level_tools::PrintSettings& settings,
                                             int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}length: {}\n", is, length);

  for (u32 i = 0; i < data.size(); i++) {
    result += fmt::format("{}data [{}]:\n", is, i);
    result += data[i].print(settings, next_indent);
  }
  return result;
}

void PrototypeBucketShrub::read_from_file(TypedRef ref,
                                          const decompiler::DecompilerTypeSystem& dts,
                                          level_tools::DrawStats* stats,
                                          GameVersion version) {
  name = read_string_field(ref, "name", dts, true);
  switch (version) {
    case GameVersion::Jak1:
      flags = read_plain_data_field<u32>(ref, "flags", dts);
      ASSERT(flags == 0 || flags == 2);
      break;
    case GameVersion::Jak2:
      flags = read_plain_data_field<u16>(ref, "flags", dts);
      break;
    default:
      ASSERT(false);
  }
  if (flags) {
    // lid in misty has flag 2, not sure what it means yet.
    lg::info("proto: {} flags: {}", name, flags);
  }
  in_level = read_plain_data_field<u16>(ref, "in-level", dts);
  utextures = read_plain_data_field<u16>(ref, "utextures", dts);
  dists.read_from_file(get_field_ref(ref, "dists", dts));
  rdists.read_from_file(get_field_ref(ref, "rdists", dts));
  stiffness = read_plain_data_field<float>(ref, "stiffness", dts);
  // 64 to 112 should be zeros
  for (int i = 0; i < 12; i++) {
    ASSERT(deref_u32(ref.ref, 16 + i) == 0);
  }

  auto geom_start = get_field_ref(ref, "geometry", dts);

  // first geometry is generic and we should always have it.
  auto generic_geom_l = deref_label(geom_start);
  generic_geom_l.byte_offset -= 4;
  if (get_type_of_basic(generic_geom_l) != "prototype-generic-shrub") {
    throw Error("bad generic shrub type: {}", get_type_of_basic(generic_geom_l));
  }
  geom_start.byte_offset += 4;

  // second is same data, but in prototype-shrubbery form (for normal shrub renderer)
  auto normal_geom = deref_label(geom_start);
  normal_geom.byte_offset -= 4;
  if (get_type_of_basic(normal_geom) != "prototype-shrubbery") {
    throw Error("bad normal shrub type: {}", get_type_of_basic(normal_geom));
  }
  shrubbery_geom.read_from_file(typed_ref_from_basic(normal_geom, dts), dts, stats, version);
  geom_start.byte_offset += 4;

  generic_geom.read_from_file(typed_ref_from_basic(generic_geom_l, dts), dts, stats, version);

  // todo transparent version
  // todo billboard version.
}

std::string PrototypeBucketShrub::print(const level_tools::PrintSettings& settings,
                                        int indent) const {
  std::string is(indent, ' ');
  std::string result;
  result += fmt::format("{}name: {}\n", is, name);
  result += fmt::format("{}flags: {}\n", is, flags);

  result += fmt::format("{}generic-geometry [0]:\n", is);
  result += generic_geom.print(settings, indent + 4);

  result += fmt::format("{}normal-geometry [1]:\n", is);
  result += shrubbery_geom.print(settings, indent + 4);

  return result;
}

void PrototypeShrubbery::read_from_file(TypedRef ref,
                                        const decompiler::DecompilerTypeSystem& dts,
                                        level_tools::DrawStats* stats,
                                        GameVersion version) {
  id = read_plain_data_field<s16>(ref, "id", dts);
  length = read_plain_data_field<s16>(ref, "length", dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  auto data_ref = get_field_ref(ref, "data", dts);
  for (int i = 0; i < length; i++) {
    Ref obj_ref = data_ref;
    obj_ref.byte_offset += 32 * i;  // todo not a constant here
    auto type = get_type_of_basic(obj_ref);
    if (type != "shrubbery") {
      throw Error("bad draw node type: {}", type);
    }
    shrubs.emplace_back();
    shrubs.back().read_from_file(typed_ref_from_basic(obj_ref, dts), dts, stats, version);
  }
}

std::string PrototypeShrubbery::print(const level_tools::PrintSettings& settings,
                                      int indent) const {
  std::string is(indent, ' ');
  std::string result;
  int next_indent = indent + 4;
  result += fmt::format("{}id: {}\n", is, id);
  result += fmt::format("{}length: {}\n", is, length);
  result += fmt::format("{}bsphere: {}", is, bsphere.print_meters());

  for (size_t i = 0; i < shrubs.size(); i++) {
    result += fmt::format("{}draw-nodes [{}] ({}):\n", is, i, shrubs[i].my_type());
    result += shrubs[i].print(settings, next_indent);
  }

  return result;
}

void copy_dma_to_vector(std::vector<u8>* out, Ref data_start, int qwc) {
  out->resize(qwc * 16);
  for (int i = 0; i < qwc * 4; i++) {
    u32 val = deref_u32(data_start, i);
    memcpy(out->data() + i * 4, &val, 4);
  }
}

void Shrubbery::read_from_file(TypedRef ref,
                               const decompiler::DecompilerTypeSystem& dts,
                               level_tools::DrawStats* /*stats*/,
                               GameVersion /*version*/) {
  // read the easy ones.
  obj_qwc = read_plain_data_field<u8>(ref, "obj-qwc", dts);
  vtx_qwc = read_plain_data_field<u8>(ref, "vtx-qwc", dts);
  col_qwc = read_plain_data_field<u8>(ref, "col-qwc", dts);
  stq_qwc = read_plain_data_field<u8>(ref, "stq-qwc", dts);

  auto header_data = deref_label(get_field_ref(ref, "header", dts));
  // guess that the header is 24 * 4 = 96 bytes here.
  // not sure what it's used for yet.
  header.resize(24);
  for (int i = 0; i < 24; i++) {
    u32 val = deref_u32(header_data, i);
    memcpy(header.data() + i, &val, 4);
  }

  copy_dma_to_vector(&obj, deref_label(get_field_ref(ref, "obj", dts)), obj_qwc);
  copy_dma_to_vector(&vtx, deref_label(get_field_ref(ref, "vtx", dts)), vtx_qwc);
  copy_dma_to_vector(&col, deref_label(get_field_ref(ref, "col", dts)), col_qwc);
  copy_dma_to_vector(&stq, deref_label(get_field_ref(ref, "stq", dts)), stq_qwc);
  copy_dma_to_vector(&textures, deref_label(get_field_ref(ref, "textures", dts)), header[0] * 10);
}

std::string Shrubbery::print(const level_tools::PrintSettings& /*settings*/, int indent) const {
  std::string is(indent, ' ');

  return fmt::format("{} qwcs: {} {} {} {}, tex: {}\n", is, obj_qwc, vtx_qwc, col_qwc, stq_qwc,
                     header[0] * 2);
}

std::string GenericShrubFragment::print(const level_tools::PrintSettings& /*settings*/,
                                        int indent) const {
  std::string is(indent, ' ');
  return fmt::format("{} qwcs: {} {} {} {}: {}\n", is, cnt_qwc, vtx_qwc, col_qwc, stq_qwc, vtx_cnt);
}

void GenericShrubFragment::read_from_file(TypedRef ref,
                                          const decompiler::DecompilerTypeSystem& dts,
                                          level_tools::DrawStats* /*stats*/,
                                          GameVersion /*version*/) {
  cnt_qwc = read_plain_data_field<u8>(ref, "cnt-qwc", dts);
  vtx_qwc = read_plain_data_field<u8>(ref, "vtx-qwc", dts);
  col_qwc = read_plain_data_field<u8>(ref, "col-qwc", dts);
  stq_qwc = read_plain_data_field<u8>(ref, "stq-qwc", dts);
  vtx_cnt = read_plain_data_field<u32>(ref, "vtx-cnt", dts);

  copy_dma_to_vector(&textures, deref_label(get_field_ref(ref, "textures", dts)), cnt_qwc);
  copy_dma_to_vector(&vtx, deref_label(get_field_ref(ref, "vtx", dts)), vtx_qwc);
  copy_dma_to_vector(&col, deref_label(get_field_ref(ref, "col", dts)), col_qwc);
  copy_dma_to_vector(&stq, deref_label(get_field_ref(ref, "stq", dts)), stq_qwc);
}

}  // namespace shrub_types

std::unique_ptr<DrawableTree> make_drawable_tree(TypedRef ref,
                                                 const decompiler::DecompilerTypeSystem& dts,
                                                 DrawStats* stats,
                                                 GameVersion version) {
  if (ref.type->get_name() == "drawable-tree-tfrag") {
    auto tree = std::make_unique<DrawableTreeTfrag>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-trans-tfrag") {
    auto tree = std::make_unique<DrawableTreeTransTfrag>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-lowres-tfrag") {
    auto tree = std::make_unique<DrawableTreeLowresTfrag>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-dirt-tfrag") {
    auto tree = std::make_unique<DrawableTreeDirtTfrag>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-ice-tfrag") {
    auto tree = std::make_unique<DrawableTreeIceTfrag>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-instance-tie") {
    auto tree = std::make_unique<DrawableTreeInstanceTie>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-tfrag-trans") {
    auto tree = std::make_unique<DrawableTreeTfragTrans>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-tfrag-water") {
    auto tree = std::make_unique<DrawableTreeTfragWater>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-actor") {
    auto tree = std::make_unique<DrawableTreeActor>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-instance-shrub") {
    auto tree = std::make_unique<shrub_types::DrawableTreeInstanceShrub>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  if (ref.type->get_name() == "drawable-tree-collide-fragment") {
    auto tree = std::make_unique<DrawableTreeCollideFragment>();
    tree->read_from_file(ref, dts, stats, version);
    return tree;
  }

  auto tree = std::make_unique<DrawableTreeUnknown>();
  tree->read_from_file(ref, dts, stats, version);
  return tree;
}

void DrawableTreeArray::read_from_file(TypedRef ref,
                                       const decompiler::DecompilerTypeSystem& dts,
                                       DrawStats* stats,
                                       GameVersion version) {
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

    trees.push_back(make_drawable_tree(typed_ref_from_basic(object_ref, dts), dts, stats, version));
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
                               DrawStats* stats,
                               GameVersion version) {
  TypedRef ref;
  ref.ref.byte_offset = 0;
  ref.ref.seg = 0;
  ref.ref.data = &file;
  ref.type = dts.ts.lookup_type("bsp-header");

  file_info.read_from_file(get_and_check_ref_to_basic(ref, "info", "file-info", dts), dts);
  bsphere.read_from_file(get_field_ref(ref, "bsphere", dts));

  switch (version) {
    case GameVersion::Jak1:
      visible_list_length = read_plain_data_field<s32>(ref, "visible-list-length", dts);
      break;
    case GameVersion::Jak2:
      visible_list_length = read_plain_data_field<s16>(ref, "visible-list-length", dts);
      break;
    default:
      ASSERT(false);
  }

  drawable_tree_array.read_from_file(
      get_and_check_ref_to_basic(ref, "drawable-trees", "drawable-tree-array", dts), dts, stats,
      version);

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

  if (version > GameVersion::Jak1) {
    auto ff = get_field_ref(ref, "texture-flags", dts);
    memcpy_plain_data((u8*)texture_flags, ff, sizeof(u16) * kNumTextureFlags);
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
