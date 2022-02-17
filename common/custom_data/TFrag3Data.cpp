#include "Tfrag3Data.h"
#include "common/util/Assert.h"

namespace tfrag3 {

void PackedTieVertices::serialize(Serializer& ser) {
  ser.from_pod_vector(&color_indices);
  ser.from_pod_vector(&matrices);
  ser.from_pod_vector(&matrix_groups);
  ser.from_pod_vector(&vertices);
}

void StripDraw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_pod_vector(&runs);
  ser.from_pod_vector(&vis_groups);
  ser.from_ptr(&num_triangles);
}

void StripDraw::unpack() {
  ASSERT(unpacked.vertex_index_stream.empty());
  for (auto& r : runs) {
    for (int i = 0; i < r.length; i++) {
      unpacked.vertex_index_stream.push_back(r.vertex0 + i);
    }
    unpacked.vertex_index_stream.push_back(UINT32_MAX);
  }
}

void InstancedStripDraw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_pod_vector(&vertex_index_stream);
  ser.from_pod_vector(&instance_groups);
  ser.from_ptr(&num_triangles);
}

void TieWindInstance::serialize(Serializer& ser) {
  ser.from_ptr(&matrix);
  ser.from_ptr(&wind_idx);
  ser.from_ptr(&stiffness);
}

void TfragTree::serialize(Serializer& ser) {
  ser.from_ptr(&kind);

  if (ser.is_saving()) {
    ser.save<size_t>(draws.size());
  } else {
    draws.resize(ser.load<size_t>());
  }
  for (auto& draw : draws) {
    draw.serialize(ser);
  }

  // ser.from_pod_vector(&vertices);
  ser.from_pod_vector(&packed_vertices.vertices);
  ser.from_pod_vector(&packed_vertices.cluster_origins);
  ser.from_pod_vector(&colors);
  bvh.serialize(ser);
}

void TieTree::unpack() {
  unpacked.vertices.resize(packed_vertices.color_indices.size());
  size_t i = 0;
  for (const auto& grp : packed_vertices.matrix_groups) {
    if (grp.matrix_idx == -1) {
      for (u32 src_idx = grp.start_vert; src_idx < grp.end_vert; src_idx++) {
        auto& vtx = unpacked.vertices[i];
        vtx.color_index = packed_vertices.color_indices[i];
        const auto& proto_vtx = packed_vertices.vertices[src_idx];
        vtx.x = proto_vtx.x;
        vtx.y = proto_vtx.y;
        vtx.z = proto_vtx.z;
        vtx.q = 1.f;
        vtx.s = proto_vtx.s;
        vtx.t = proto_vtx.t;
        i++;
      }
    } else {
      const auto& mat = packed_vertices.matrices[grp.matrix_idx];
      for (u32 src_idx = grp.start_vert; src_idx < grp.end_vert; src_idx++) {
        auto& vtx = unpacked.vertices[i];
        vtx.color_index = packed_vertices.color_indices[i];
        const auto& proto_vtx = packed_vertices.vertices[src_idx];
        auto temp = mat[0] * proto_vtx.x + mat[1] * proto_vtx.y + mat[2] * proto_vtx.z + mat[3];
        vtx.x = temp.x();
        vtx.y = temp.y();
        vtx.z = temp.z();
        vtx.q = 1.f;
        vtx.s = proto_vtx.s;
        vtx.t = proto_vtx.t;
        i++;
      }
    }
  }
}

void TfragTree::unpack() {
  unpacked.vertices.resize(packed_vertices.vertices.size());
  for (size_t i = 0; i < unpacked.vertices.size(); i++) {
    auto& o = unpacked.vertices[i];
    auto& in = packed_vertices.vertices[i];
    auto& cluster = packed_vertices.cluster_origins.at(in.cluster_idx);
    constexpr float kClusterSize = 4096 * 40;  // 100 in-game meters
    constexpr float kMasterOffset = 12000 * 4096;
    constexpr float rescale = kClusterSize / UINT16_MAX;
    float cx = -kMasterOffset + kClusterSize * cluster.x();
    float cy = -kMasterOffset + kClusterSize * cluster.y();
    float cz = -kMasterOffset + kClusterSize * cluster.z();
    o.x = cx + in.xoff * rescale;
    o.y = cy + in.yoff * rescale;
    o.z = cz + in.zoff * rescale;
    o.s = in.s / (1024.f);
    o.t = in.t / (1024.f);
    o.q = 1.f;
    o.color_index = in.color_index;
  }
}

void TieTree::serialize(Serializer& ser) {
  if (ser.is_saving()) {
    ser.save<size_t>(static_draws.size());
  } else {
    static_draws.resize(ser.load<size_t>());
  }
  for (auto& draw : static_draws) {
    draw.serialize(ser);
  }

  if (ser.is_saving()) {
    ser.save<size_t>(instanced_wind_draws.size());
  } else {
    instanced_wind_draws.resize(ser.load<size_t>());
  }
  for (auto& draw : instanced_wind_draws) {
    draw.serialize(ser);
  }

  if (ser.is_saving()) {
    ser.save<size_t>(wind_instance_info.size());
  } else {
    wind_instance_info.resize(ser.load<size_t>());
  }
  for (auto& inst : wind_instance_info) {
    inst.serialize(ser);
  }

  packed_vertices.serialize(ser);
  ser.from_pod_vector(&colors);
  bvh.serialize(ser);
}

void BVH::serialize(Serializer& ser) {
  ser.from_ptr(&first_leaf_node);
  ser.from_ptr(&last_leaf_node);
  ser.from_ptr(&first_root);
  ser.from_ptr(&num_roots);
  ser.from_ptr(&only_children);
  ser.from_pod_vector(&vis_nodes);
}

void Texture::serialize(Serializer& ser) {
  ser.from_ptr(&w);
  ser.from_ptr(&h);
  ser.from_pod_vector(&data);
  ser.from_str(&debug_name);
  ser.from_str(&debug_tpage_name);
}

void Level::serialize(Serializer& ser) {
  ser.from_ptr(&version);
  if (ser.is_loading() && version != TFRAG3_VERSION) {
    fmt::print("version mismatch when loading tfrag3 data. Got {}, expected {}\n", version,
               TFRAG3_VERSION);
    ASSERT(false);
  }

  ser.from_str(&level_name);

  if (ser.is_saving()) {
    ser.save<size_t>(textures.size());
  } else {
    textures.resize(ser.load<size_t>());
  }
  for (auto& tex : textures) {
    tex.serialize(ser);
  }

  for (int geom = 0; geom < 3; ++geom) {
    if (ser.is_saving()) {
      ser.save<size_t>(tfrag_trees[geom].size());
    } else {
      tfrag_trees[geom].resize(ser.load<size_t>());
    }
    for (auto& tree : tfrag_trees[geom]) {
      tree.serialize(ser);
    }
  }

  for (int geom = 0; geom < 4; ++geom) {
    if (ser.is_saving()) {
      ser.save<size_t>(tie_trees[geom].size());
    } else {
      tie_trees[geom].resize(ser.load<size_t>());
    }
    for (auto& tree : tie_trees[geom]) {
      tree.serialize(ser);
    }
  }

  ser.from_ptr(&version2);
  if (ser.is_loading() && version2 != TFRAG3_VERSION) {
    fmt::print("version mismatch when loading tfrag3 data (at end). Got {}, expected {}\n",
               version2, TFRAG3_VERSION);
    ASSERT(false);
  }
}

std::array<int, MemoryUsageCategory::NUM_CATEGORIES> Level::get_memory_usage() const {
  std::array<int, MemoryUsageCategory::NUM_CATEGORIES> result;
  result.fill(0);

  // textures
  for (const auto& tex : textures) {
    result[TEXTURE] += tex.data.size() * sizeof(u32);
  }

  // tfrag
  for (const auto& tfrag_tree_geoms : tfrag_trees) {
    for (const auto& tfrag_tree : tfrag_tree_geoms) {
      for (const auto& draw : tfrag_tree.draws) {
        result[TFRAG_INDEX] += draw.runs.size() * sizeof(StripDraw::VertexRun);
        result[TFRAG_VIS] += draw.vis_groups.size() * sizeof(StripDraw::VisGroup);
      }
      result[TFRAG_VERTS] +=
          tfrag_tree.packed_vertices.vertices.size() * sizeof(PackedTfragVertices::Vertex);
      result[TFRAG_CLUSTER] +=
          tfrag_tree.packed_vertices.cluster_origins.size() * sizeof(math::Vector<u16, 3>);
      result[TFRAG_TIME_OF_DAY] += tfrag_tree.colors.size() * sizeof(TimeOfDayColor);
      result[TFRAG_BVH] += tfrag_tree.bvh.vis_nodes.size() * sizeof(VisNode);
    }
  }

  // tie
  for (const auto& tie_tree_geoms : tie_trees) {
    for (const auto& tie_tree : tie_tree_geoms) {
      result[TIE_BVH] += tie_tree.bvh.vis_nodes.size();
      for (const auto& draw : tie_tree.static_draws) {
        result[TIE_DEINST_INDEX] += draw.runs.size() * sizeof(StripDraw::VertexRun);
        result[TIE_DEINST_VIS] += draw.vis_groups.size() * sizeof(StripDraw::VisGroup);
      }
      result[TIE_VERTS] +=
          tie_tree.packed_vertices.vertices.size() * sizeof(PackedTieVertices::Vertex);
      result[TIE_CIDX] += tie_tree.packed_vertices.color_indices.size() * sizeof(u16);
      result[TIE_MATRICES] += tie_tree.packed_vertices.matrices.size() * 4 * 4 * 4;
      result[TIE_GRPS] +=
          tie_tree.packed_vertices.matrix_groups.size() * sizeof(PackedTieVertices::MatrixGroup);
      result[TIE_TIME_OF_DAY] += tie_tree.colors.size() * sizeof(TimeOfDayColor);

      for (const auto& draw : tie_tree.instanced_wind_draws) {
        result[TIE_INST_INDEX] += draw.vertex_index_stream.size() * sizeof(u32);
        result[TIE_INST_VIS] +=
            draw.instance_groups.size() * sizeof(InstancedStripDraw::InstanceGroup);
      }
      result[TIE_WIND_INSTANCE_INFO] +=
          tie_tree.wind_instance_info.size() * sizeof(TieWindInstance);
    }
  }

  return result;
}

}  // namespace tfrag3
