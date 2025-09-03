#include "extract_shadow.h"

#include "common/log/log.h"
#include "common/util/BitUtils.h"

#include "decompiler/util/goal_data_reader.h"

namespace decompiler {

struct ShadowVertex {
  math::Vector3f pos;
  float weight;
};

struct ShadowRef {
  uint8_t joint_0 = 0;
  uint8_t joint_1 = 0;
};

struct ShadowTri {
  uint8_t verts[3];
  uint8_t faces;
};

struct ShadowEdge {
  uint8_t ind[2];
  uint8_t tri[2];
};

struct ShadowData {
  std::string name;
  uint32_t num_joints = 0;
  std::vector<ShadowVertex> one_bone_vertices;
  std::vector<ShadowVertex> two_bone_vertices;
  std::vector<ShadowRef> refs;
  std::vector<ShadowTri> single_tris, double_tris;
  std::vector<ShadowEdge> single_edges, double_edges;
};

std::string debug_dump_to_ply(const ShadowData& data) {
  int num_verts = data.one_bone_vertices.size() + data.two_bone_vertices.size();
  std::string result = fmt::format(
      "ply\nformat ascii 1.0\nelement vertex {}\nproperty float x\nproperty float y\nproperty "
      "float z\nproperty uchar red\nproperty uchar green\nproperty uchar blue\nelement face "
      "{}\nproperty list uchar int vertex_index\nend_header\n",
      2 * num_verts, data.single_tris.size() + data.double_tris.size());

  for (auto& vtx : data.one_bone_vertices) {
    result += fmt::format("{} {} {} {} {} {}\n", vtx.pos.x() / 1024.f, vtx.pos.y() / 1024.f,
                          vtx.pos.z() / 1024.f, 128, 128, 128);
  }
  for (auto& vtx : data.two_bone_vertices) {
    result += fmt::format("{} {} {} {} {} {}\n", vtx.pos.x() / 1024.f, vtx.pos.y() / 1024.f,
                          vtx.pos.z() / 1024.f, 128, 128, 128);
  }
  for (auto& vtx : data.one_bone_vertices) {
    result += fmt::format("{} {} {} {} {} {}\n", vtx.pos.x() / 1024.f, vtx.pos.y() / 1024.f,
                          vtx.pos.z() / 1024.f, 128, 256, 128);
  }
  for (auto& vtx : data.two_bone_vertices) {
    result += fmt::format("{} {} {} {} {} {}\n", vtx.pos.x() / 1024.f, vtx.pos.y() / 1024.f,
                          vtx.pos.z() / 1024.f, 128, 256, 128);
  }

  for (auto& face : data.single_tris) {
    result += fmt::format("3 {} {} {}\n", face.verts[0], face.verts[1], face.verts[2]);
  }

  for (auto& face : data.double_tris) {
    result += fmt::format("3 {} {} {}\n", face.verts[0] + num_verts, face.verts[1] + num_verts,
                          face.verts[2] + num_verts);
  }

  return result;
}

constexpr int kHeaderSize = 48;

ShadowData extract_shadow_data(const LinkedObjectFile& file,
                               const DecompilerTypeSystem& dts,
                               TypedRef header_ref,
                               const std::string& name,
                               int size_qwc,
                               int num_joints) {
  ShadowData shadow_data;

  ASSERT(size_qwc < 1024 * 1024);  // something reasonable
  std::vector<u8> data(size_qwc * 16);
  Ref shadow_ref = header_ref.ref;
  shadow_ref.byte_offset += kHeaderSize;
  memcpy_from_plain_data(data.data(), shadow_ref, size_qwc * 16 - kHeaderSize);

  // lg::info("name is {}, has {} joints, size {} bytes", name,
  // read_plain_data_field<s32>(header_ref, "num-joints", dts), data.size());

  shadow_data.name = name;
  shadow_data.num_joints = num_joints;

  const u32 num_verts = read_plain_data_field<u16>(header_ref, "num-verts", dts);
  const u32 num_twos = read_plain_data_field<u16>(header_ref, "num-twos", dts);
  ASSERT(num_verts >= num_twos);
  const u32 num_ones = num_verts - num_twos;
  // lg::info("  vert counts {} {}", num_ones, num_twos);

  const u32 ofs_verts = read_plain_data_field<u32>(header_ref, "ofs-verts", dts);
  const u32 ofs_refs = read_plain_data_field<u32>(header_ref, "ofs-refs", dts);
  const u32 ofs_single_tris = read_plain_data_field<u32>(header_ref, "ofs-single-tris", dts);
  const u32 ofs_single_edges = read_plain_data_field<u32>(header_ref, "ofs-single-edges", dts);
  const u32 ofs_double_tris = read_plain_data_field<u32>(header_ref, "ofs-double-tris", dts);
  const u32 ofs_double_edges = read_plain_data_field<u32>(header_ref, "ofs-double-edges", dts);

  const u32 num_single_tris = read_plain_data_field<u16>(header_ref, "num-single-tris", dts);
  const u32 num_single_edges = read_plain_data_field<u16>(header_ref, "num-single-edges", dts);
  const u32 num_double_tris = read_plain_data_field<u16>(header_ref, "num-double-tris", dts);
  const u32 num_double_edges = read_plain_data_field<u16>(header_ref, "num-double-edges", dts);

  ASSERT(ofs_verts == kHeaderSize);  // verts always right after the header

  // lg::info(" offsets {} {} {} {} {} {}", ofs_verts, ofs_refs, ofs_single_tris, ofs_single_edges,
  //          ofs_double_tris, ofs_double_edges);

  // vertices
  ASSERT(ofs_refs - ofs_verts == 16 * num_verts);
  shadow_data.one_bone_vertices.resize(num_ones);
  memcpy_from_plain_data(shadow_data.one_bone_vertices.data(), shadow_ref, num_ones * 16);
  shadow_ref.byte_offset += num_ones * 16;
  for (const auto& x : shadow_data.one_bone_vertices) {
    ASSERT(x.weight == 1);
  }

  shadow_data.two_bone_vertices.resize(num_twos);
  memcpy_from_plain_data(shadow_data.two_bone_vertices.data(), shadow_ref, num_twos * 16);
  shadow_ref.byte_offset += num_twos * 16;
  for (auto x : shadow_data.two_bone_vertices) {
    ASSERT(x.weight > 0 && x.weight < 1);
  }

  // refs
  ASSERT(ofs_single_tris - ofs_refs == align16(num_verts * 2));
  shadow_data.refs.resize(num_verts);
  memcpy_from_plain_data(shadow_data.refs.data(), shadow_ref, num_verts * 2);
  shadow_ref.byte_offset += ofs_single_tris - ofs_refs;
  for (size_t i = 0; i < num_verts; i++) {
    ASSERT(shadow_data.refs[i].joint_0 < shadow_data.num_joints);
    if (i < num_ones) {
      ASSERT(shadow_data.refs[i].joint_1 == 255);
    } else {
      ASSERT(shadow_data.refs[i].joint_1 < shadow_data.num_joints);
      ASSERT(shadow_data.refs[i].joint_1 != shadow_data.refs[i].joint_0);
    }
  }

  // single tris
  ASSERT(ofs_single_edges - ofs_single_tris == align16(num_single_tris * 4));
  shadow_data.single_tris.resize(num_single_tris);
  memcpy_from_plain_data(shadow_data.single_tris.data(), shadow_ref, num_single_tris * 4);
  shadow_ref.byte_offset += ofs_single_edges - ofs_single_tris;
  for (auto& tri : shadow_data.single_tris) {
    for (auto v : tri.verts) {
      ASSERT(v < num_verts);
    }
    ASSERT(tri.faces == 0);
  }

  // single edges
  ASSERT(ofs_double_tris - ofs_single_edges == align16(num_single_edges * 4));
  shadow_data.single_edges.resize(num_single_edges);
  memcpy_from_plain_data(shadow_data.single_edges.data(), shadow_ref, num_single_edges * 4);
  shadow_ref.byte_offset += ofs_double_tris - ofs_single_edges;
  for (auto& edge : shadow_data.single_edges) {
    for (auto x : edge.ind) {
      ASSERT(x < num_verts);
    }
    ASSERT(edge.tri[0] != 255);
    for (auto x : edge.tri) {
      ASSERT(x == 255 || x < shadow_data.single_tris.size());
    }
  }

  // double tris
  ASSERT(ofs_double_edges - ofs_double_tris == align16(num_double_tris * 4));
  shadow_data.double_tris.resize(num_double_tris);
  memcpy_from_plain_data(shadow_data.double_tris.data(), shadow_ref, num_double_tris * 4);
  shadow_ref.byte_offset += ofs_double_edges - ofs_double_tris;
  for (auto& tri : shadow_data.double_tris) {
    for (auto v : tri.verts) {
      ASSERT(v < num_verts);
    }
    ASSERT(tri.faces == 0);
  }

  // double edges
  ASSERT(size_qwc * 16 - ofs_double_edges == align16(num_double_edges * 4));
  shadow_data.double_edges.resize(num_double_edges);
  memcpy_from_plain_data(shadow_data.double_edges.data(), shadow_ref, num_double_edges * 4);
  for (auto& edge : shadow_data.double_edges) {
    for (auto x : edge.ind) {
      ASSERT(x < num_verts);
    }
    ASSERT(edge.tri[0] != 255);
    for (auto x : edge.tri) {
      ASSERT(x == 255 || x < shadow_data.double_tris.size());
    }
  }
  return shadow_data;
}

ShadowData extract_jak1_shadow_data(const LinkedObjectFile& file,
                                    const DecompilerTypeSystem& dts,
                                    int geo_word_idx) {
  Ref ref;
  ref.data = &file;
  ref.seg = 0;
  ref.byte_offset = geo_word_idx * 4;
  auto tr = typed_ref_from_basic(ref, dts);
  auto header_ref = TypedRef(get_field_ref(tr, "header", dts), dts.ts.lookup_type("shadow-header"));
  u32 size_qwc = read_plain_data_field<s32>(header_ref, "qwc-data", dts);
  const std::string name = read_string_field(tr, "name", dts, false);
  int num_joints = read_plain_data_field<s32>(header_ref, "num-joints", dts);
  return extract_shadow_data(file, dts, header_ref, name, size_qwc, num_joints);
}

std::vector<ShadowData> extract_jak2_shadow_data(const LinkedObjectFile& file,
                                                 const DecompilerTypeSystem& dts,
                                                 int geo_word_idx) {
  std::vector<ShadowData> shadow_datas;
  Ref ref;
  ref.data = &file;
  ref.seg = 0;
  ref.byte_offset = geo_word_idx * 4;
  auto tr = typed_ref_from_basic(ref, dts);
  uint32_t version = read_plain_data_field<u32>(tr, "version", dts);
  std::string name = read_string_field(tr, "name", dts, false);

  if (version == 0) {
    tr.type = dts.ts.lookup_type("shadow-geo-old");
    auto header_ref =
        TypedRef(get_field_ref(tr, "header", dts), dts.ts.lookup_type("shadow-frag-header"));
    u32 size_qwc = read_plain_data_field<s32>(header_ref, "qwc-data", dts);
    int num_joints = read_plain_data_field<s32>(header_ref, "num-joints", dts);
    shadow_datas.push_back(extract_shadow_data(file, dts, header_ref, name, size_qwc, num_joints));
  } else if (version == 1) {
    u32 num_joints = read_plain_data_field<u32>(tr, "num-joints", dts);
    uint32_t num_fragments = read_plain_data_field<u32>(tr, "num-fragments", dts);
    if (num_fragments == 0) {
      lg::error("Shadow geo {} with no fragments! Skipping\n", name);
      return {};
    }
    // lg::info("{} {} fragments", name, num_fragments);
    auto frags_ref =
        TypedRef(get_field_ref(tr, "frags", dts), dts.ts.lookup_type("shadow-frag-ref"));
    for (u32 i = 0; i < num_fragments; i++) {
      auto header_ref = TypedRef(deref_label(get_field_ref(frags_ref, "header", dts)),
                                 dts.ts.lookup_type("shadow-frag-header"));
      u32 size_qwc = read_plain_data_field<s32>(frags_ref, "qwc", dts);
      shadow_datas.push_back(
          extract_shadow_data(file, dts, header_ref, name, size_qwc, num_joints));
      frags_ref.ref.byte_offset += 8;
    }
  } else {
    lg::die("unknown version {}\n", version);
  }
  return shadow_datas;
}

std::vector<tfrag3::ShadowVertex> convert_vertices(const ShadowData& data) {
  std::vector<tfrag3::ShadowVertex> result;

  for (size_t i = 0; i < data.one_bone_vertices.size(); i++) {
    const auto& in = data.one_bone_vertices[i];
    auto& out = result.emplace_back();
    out.pos[0] = in.pos.x();
    out.pos[1] = in.pos.y();
    out.pos[2] = in.pos.z();
    out.weight = 1.f;
    out.mats[0] = data.refs.at(i).joint_0;
    out.mats[1] = data.refs.at(i).joint_1;
    ASSERT(out.mats[1] == 255);
    ASSERT(in.weight == 1.f);
    out.flags = 0;
  }

  for (size_t i = 0; i < data.two_bone_vertices.size(); i++) {
    const auto& in = data.two_bone_vertices[i];
    auto& out = result.emplace_back();
    out.pos[0] = in.pos.x();
    out.pos[1] = in.pos.y();
    out.pos[2] = in.pos.z();
    out.weight = in.weight;
    ASSERT(out.weight != 1.f && out.weight != 0.f);
    out.mats[0] = data.refs.at(data.one_bone_vertices.size() + i).joint_0;
    out.mats[1] = data.refs.at(data.one_bone_vertices.size() + i).joint_1;
    ASSERT(out.mats[0] != 255);
    ASSERT(out.mats[1] != 255);
    out.flags = 0;
  }

  return result;
}

tfrag3::ShadowTri convert_tri(const ShadowTri& tri) {
  tfrag3::ShadowTri result;
  for (int i = 0; i < 3; i++) {
    result.verts[i] = tri.verts[i];
  }
  return result;
}

tfrag3::ShadowEdge convert_edge(const ShadowEdge& edge) {
  tfrag3::ShadowEdge result;
  for (int i = 0; i < 2; i++) {
    result.ind[i] = edge.ind[i];
    result.tri[i] = edge.tri[i];
  }
  return result;
}

std::vector<tfrag3::ShadowTri> convert_tris(const std::vector<ShadowTri>& tris) {
  std::vector<tfrag3::ShadowTri> result;
  result.reserve(tris.size());
  for (auto& tri : tris) {
    result.push_back(convert_tri(tri));
  }
  return result;
}

std::vector<tfrag3::ShadowEdge> convert_edges(const std::vector<ShadowEdge>& edges) {
  std::vector<tfrag3::ShadowEdge> result;
  result.reserve(edges.size());
  for (auto& edge : edges) {
    result.push_back(convert_edge(edge));
  }
  return result;
}

void add_data_to_level(tfrag3::ShadowModelGroup& sd, const std::vector<ShadowData>& fragments) {
  if (fragments.empty()) {
    return;
  }
  auto& model = sd.models.emplace_back();
  model.name = fragments.front().name;
  model.max_bones = fragments.front().num_joints;

  for (auto& in_frag : fragments) {
    auto& out_frag = model.fragments.emplace_back();

    out_frag.single_tris = convert_tris(in_frag.single_tris);
    out_frag.double_tris = convert_tris(in_frag.double_tris);
    out_frag.single_edges = convert_edges(in_frag.single_edges);
    out_frag.double_edges = convert_edges(in_frag.double_edges);

    const u32 vertex_offset = sd.vertices.size();

    out_frag.first_vertex = vertex_offset;
    out_frag.num_one_bone_vertices = in_frag.one_bone_vertices.size();
    out_frag.num_two_bone_vertices = in_frag.two_bone_vertices.size();
    ASSERT(out_frag.num_one_bone_vertices + out_frag.num_two_bone_vertices <=
           tfrag3::ShadowModel::kMaxVertices);
    ASSERT(out_frag.single_tris.size() <= tfrag3::ShadowModel::kMaxTris);
    ASSERT(out_frag.double_tris.size() <= tfrag3::ShadowModel::kMaxTris);

    // insert top vertices
    auto vertices = convert_vertices(in_frag);
    sd.vertices.insert(sd.vertices.end(), vertices.begin(), vertices.end());

    // bottom vertices
    for (auto& v : vertices) {
      v.flags = 1;
    }
    sd.vertices.insert(sd.vertices.end(), vertices.begin(), vertices.end());
  }
  // if (dump_level) {
  //   auto file_path = file_util::get_file_path(
  //       {"debug_out/shadow", fmt::format("{}_{}.ply", ag_data.name_in_dgo, i)});
  //   file_util::create_dir_if_needed_for_file(file_path);
  //   file_util::write_text_file(file_path, debug_dump_to_ply(data));
  // }
  // i++;
}

void extract_shadow(const ObjectFileData& ag_data,
                    const DecompilerTypeSystem& dts,
                    tfrag3::Level& out,
                    bool dump_level,
                    GameVersion version) {
  // hack
  // dump_level = true;

  if (dump_level) {
    file_util::create_dir_if_needed(file_util::get_file_path({"debug_out/shadow"}));
  }
  auto& sd = out.shadow_data;

  if (version == GameVersion::Jak1) {
    auto geo_locations = find_objects_with_type(ag_data.linked_data, "shadow-geo");
    // if (!geo_locations.empty()) {
    //   lg::error("{} has {} shadows", ag_data.name_in_dgo, geo_locations.size());
    // }

    for (auto loc : geo_locations) {
      const ShadowData data = extract_jak1_shadow_data(ag_data.linked_data, dts, loc);
      add_data_to_level(sd, {data});
    }
  } else {
    // Jak2 has two versions of shadow. The "new" version has multiple fragments.
    // Although there is a shadow-geo-old type in GOAL code, it's not actually used in the game
    // data: both new and old types simply have shadow-geo type tags.
    ASSERT(find_objects_with_type(ag_data.linked_data, "shadow-geo-old").empty());

    auto geo_locations = find_objects_with_type(ag_data.linked_data, "shadow-geo");
    for (auto loc : geo_locations) {
      auto shadow_datas = extract_jak2_shadow_data(ag_data.linked_data, dts, loc);
      add_data_to_level(sd, shadow_datas);
    }
  }
}
}  // namespace decompiler