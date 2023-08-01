#include "Tfrag3Data.h"

#include <algorithm>
#include <functional>

#ifndef __aarch64__
#include "xmmintrin.h"
#else
#include "third-party/sse2neon/sse2neon.h"
#endif

#include "common/util/Assert.h"

namespace tfrag3 {

void PackedTieVertices::serialize(Serializer& ser) {
  ser.from_pod_vector(&color_indices);
  ser.from_pod_vector(&matrices);
  ser.from_pod_vector(&matrix_groups);
  ser.from_pod_vector(&vertices);
}

void PackedShrubVertices::serialize(Serializer& ser) {
  ser.from_pod_vector(&matrices);
  ser.from_pod_vector(&instance_groups);
  ser.from_pod_vector(&vertices);
  ser.from_ptr(&total_vertex_count);
}

void StripDraw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_pod_vector(&runs);
  ser.from_pod_vector(&plain_indices);
  ser.from_pod_vector(&vis_groups);
  ser.from_ptr(&num_triangles);
}

void ShrubDraw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_ptr(&num_triangles);
  ser.from_ptr(&first_index_index);
  ser.from_ptr(&num_indices);
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
  ser.from_ptr(&use_strips);
}

math::Vector3f vopmula(math::Vector3f a, math::Vector3f b) {
  return math::Vector3f(a.y() * b.z(), a.z() * b.x(), a.x() * b.y());
}

math::Vector3f vopmsub(math::Vector3f acc, math::Vector3f a, math::Vector3f b) {
  return acc - vopmula(a, b);
}

/*!
 * Compute the normal transformation for a TIE from the TIE matrix.
 * Note that this isn't identical to the original game - we're missing the vf14 scaling factor
 * For now, I just set this to 1, then normalize in the shader. Though I think we could avoid
 * this by figuring out the value of vf14 here (I am just too lazy right now).
 */
std::array<math::Vector3f, 3> tie_normal_transform_v2(const std::array<math::Vector4f, 4>& m) {
  // let:
  // vf10, vf11, vf12, vf13 be the input matrix m
  std::array<math::Vector3f, 3> result;
  auto& vf10 = m[0];
  auto& vf11 = m[1];
  // auto& vf12 = m[2];

  //  lui t6, 16256
  //  mtc1 f1, t6 ;; 1.0
  //
  //  qmfc2.i s1, vf10
  //  mtc1 f12, s1
  float f12 = vf10.x();
  //  dsra32 s2, s1, 0
  //  mtc1 f13, s2
  float f13 = vf10.y();
  //  pextuw s2, r0, s2
  //  mtc1 f14, s2
  float f14 = vf10.z();
  //  mula.s f12, f12
  //  madda.s f13, f13
  //  madd.s f15, f14, f14
  float f15 = f12 * f12 + f13 * f13 + f14 * f14;
  float scale = 1.f / sqrtf(f15);
  //  rsqrt.s f15, f1, f15
  //  mfc1 s1, f15
  //  qmtc2.i vf14, s1
  //  vmulx.xyz vf16, vf10, vf14

  // vmulx.xyz vf16, vf10, vf14
  math::Vector3f vf16 = vf10.xyz() * scale;

  // vopmula.xyz acc, vf11, vf16
  math::Vector3f acc = vopmula(vf11.xyz(), vf16);

  // vopmsub.xyz vf17, vf16, vf11
  math::Vector3f vf17 = vopmsub(acc, vf16, vf11.xyz());

  // vopmula.xyz acc, vf16, vf17
  acc = vopmula(vf16, vf17);

  // vopmsub.xyz vf17, vf17, vf16
  vf17 = vopmsub(acc, vf17, vf16);

  // vmul.xyz vf14, vf17, vf17
  math::Vector3f vf14 = vf17.elementwise_multiply(vf17);

  // vmulax.w acc, vf0, vf14
  // vmadday.w acc, vf0, vf14
  // vmaddz.w vf14, vf0, vf14
  float sum = vf14.x() + vf14.y() + vf14.z();

  // vrsqrt Q, vf0.w, vf14.w
  float Q = 1.f / std::sqrt(sum);

  // vmulax.xyzw acc, vf24, vf16
  // vmadday.xyzw acc, vf25, vf16
  // vmaddz.xyzw vf10, vf26, vf16
  // vf10 = vf16; // assume cam is identity here.
  result[0] = vf16;

  // vwaitq
  // vmulq.xyz vf17, vf17, Q
  vf17 *= Q;

  // vopmula.xyz acc, vf16, vf17
  acc = vopmula(vf16, vf17);
  // vopmsub.xyz vf18, vf17, vf16
  math::Vector3f vf18 = vopmsub(acc, vf17, vf16);

  // vmulax.xyzw acc, vf24, vf17
  // vmadday.xyzw acc, vf25, vf17
  // vmaddz.xyzw vf11, vf26, vf17
  result[1] = vf17;

  // vmulax.xyzw acc, vf24, vf18
  // vmadday.xyzw acc, vf25, vf18
  // vmaddz.xyzw vf12, vf26, vf18
  result[2] = vf18;

  return result;
  //
  // sqc2 vf10, -112(t8)
  // sqc2 vf11, -96(t8)
  // sqc2 vf12, -80(t8)
}

u32 pack_to_gl_normal(s16 nx, s16 ny, s16 nz) {
  ASSERT(nx >= -512 && nx <= 511);
  ASSERT(ny >= -512 && ny <= 511);
  ASSERT(nz >= -512 && nz <= 511);
  return (nx & 0x3ff) | ((ny & 0x3ff) << 10) | ((nz & 0x3ff) << 20);
}

/*!
 * Unpack tie normal by transforming and converting to s16 for OpenGL.
 */
u32 unpack_tie_normal(const std::array<math::Vector3f, 3>& mat, s8 nx, s8 ny, s8 nz) {
  // rotate the normal
  math::Vector3f nrm = math::Vector3f::zero();
  nrm += mat[0] * nx;
  nrm += mat[1] * ny;
  nrm += mat[2] * nz;
  // convert to s16 for OpenGL renderer
  // nrm /= 0x100;  // number from EE asm
  // nrm *= 0x200;  // for normalized s10 -> float conversion by OpenGL.
  nrm *= 2;  // for normalized s10 -> float conversion by OpenGL.

  auto as_int = nrm.cast<s16>();

  return pack_to_gl_normal(as_int.x(), as_int.y(), as_int.z());
}

/*
void tie_normal_v3(__m128* out, const std::array<math::Vector4f, 4>& in) {
  math::Vector3f x_row = in[0].xyz();
  math::Vector3f y_row = in[1].xyz();
  math::Vector3f z_row = in[2].xyz();

  x_row.normalize();
  y_row = x_row.cross(y_row.cross(x_row)).normalized();
  z_row = x_row.cross(y_row);

  out[0] = _mm_setr_ps(x_row[0], x_row[1], x_row[2], 0);
  out[1] = _mm_setr_ps(y_row[0], y_row[1], y_row[2], 0);
  out[2] = _mm_setr_ps(z_row[0], z_row[1], z_row[2], 0);
}
 */

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
        vtx.s = proto_vtx.s;
        vtx.t = proto_vtx.t;
        vtx.nor = pack_to_gl_normal(proto_vtx.nx << 1, proto_vtx.ny << 1, proto_vtx.nz << 1);
        vtx.r = proto_vtx.r;
        vtx.g = proto_vtx.g;
        vtx.b = proto_vtx.b;
        vtx.a = proto_vtx.a;
        i++;
      }
    } else {
      const auto& mat = packed_vertices.matrices[grp.matrix_idx];

      __m128 mat0 = _mm_loadu_ps(mat[0].data());
      __m128 mat1 = _mm_loadu_ps(mat[1].data());
      __m128 mat2 = _mm_loadu_ps(mat[2].data());
      __m128 mat3 = _mm_loadu_ps(mat[3].data());

      if (grp.has_normals) {
        auto nmat = tie_normal_transform_v2(mat);
        for (u32 src_idx = grp.start_vert; src_idx < grp.end_vert; src_idx++) {
          auto& vtx = unpacked.vertices[i];
          vtx.color_index = packed_vertices.color_indices[i];
          const auto& proto_vtx = packed_vertices.vertices[src_idx];
          // auto temp = mat[0] * proto_vtx.x + mat[1] * proto_vtx.y + mat[2] * proto_vtx.z +
          // mat[3];
          __m128 transformed = mat3;
          transformed = _mm_add_ps(transformed, _mm_mul_ps(_mm_set1_ps(proto_vtx.x), mat0));
          transformed = _mm_add_ps(transformed, _mm_mul_ps(_mm_set1_ps(proto_vtx.y), mat1));
          transformed = _mm_add_ps(transformed, _mm_mul_ps(_mm_set1_ps(proto_vtx.z), mat2));
          _mm_storeu_ps(&vtx.x, transformed);
          vtx.s = proto_vtx.s;
          vtx.t = proto_vtx.t;
          vtx.nor = unpack_tie_normal(nmat, proto_vtx.nx, proto_vtx.ny, proto_vtx.nz);
          vtx.r = proto_vtx.r;
          vtx.g = proto_vtx.g;
          vtx.b = proto_vtx.b;
          vtx.a = proto_vtx.a;
          i++;
        }
      } else {
        for (u32 src_idx = grp.start_vert; src_idx < grp.end_vert; src_idx++) {
          auto& vtx = unpacked.vertices[i];
          vtx.color_index = packed_vertices.color_indices[i];
          const auto& proto_vtx = packed_vertices.vertices[src_idx];
          __m128 transformed = mat3;
          transformed = _mm_add_ps(transformed, _mm_mul_ps(_mm_set1_ps(proto_vtx.x), mat0));
          transformed = _mm_add_ps(transformed, _mm_mul_ps(_mm_set1_ps(proto_vtx.y), mat1));
          transformed = _mm_add_ps(transformed, _mm_mul_ps(_mm_set1_ps(proto_vtx.z), mat2));
          _mm_storeu_ps(&vtx.x, transformed);
          vtx.s = proto_vtx.s;
          vtx.t = proto_vtx.t;
          vtx.nor = 0;
          vtx.r = proto_vtx.r;
          vtx.g = proto_vtx.g;
          vtx.b = proto_vtx.b;
          vtx.a = proto_vtx.a;
          i++;
        }
      }
    }
  }

  for (auto& draw : static_draws) {
    draw.unpacked.idx_of_first_idx_in_full_buffer = unpacked.indices.size();
    ASSERT(draw.plain_indices.empty());
    for (auto& run : draw.runs) {
      for (u32 ri = 0; ri < run.length; ri++) {
        unpacked.indices.push_back(run.vertex0 + ri);
      }
      unpacked.indices.push_back(UINT32_MAX);
    }
  }
}

void ShrubTree::unpack() {
  unpacked.vertices.resize(packed_vertices.total_vertex_count);
  size_t i = 0;

  for (const auto& grp : packed_vertices.instance_groups) {
    const auto& mat = packed_vertices.matrices[grp.matrix_idx];
    for (u32 src_idx = grp.start_vert; src_idx < grp.end_vert; src_idx++) {
      auto& vtx = unpacked.vertices[i];
      vtx.color_index = grp.color_index;
      const auto& proto_vtx = packed_vertices.vertices[src_idx];
      auto temp = mat[0] * proto_vtx.x + mat[1] * proto_vtx.y + mat[2] * proto_vtx.z + mat[3];
      vtx.x = temp.x();
      vtx.y = temp.y();
      vtx.z = temp.z();
      vtx.s = proto_vtx.s;
      vtx.t = proto_vtx.t;
      memcpy(vtx.rgba_base, proto_vtx.rgba, 3);
      i++;
    }
  }
  ASSERT(i == unpacked.vertices.size());
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
    o.color_index = in.color_index;
  }

  for (auto& draw : draws) {
    draw.unpacked.idx_of_first_idx_in_full_buffer = unpacked.indices.size();
    for (auto& run : draw.runs) {
      for (u32 ri = 0; ri < run.length; ri++) {
        unpacked.indices.push_back(run.vertex0 + ri);
      }
      if (use_strips) {
        unpacked.indices.push_back(UINT32_MAX);
      }
    }
    unpacked.indices.insert(unpacked.indices.end(), draw.plain_indices.begin(),
                            draw.plain_indices.end());
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

  ser.from_ptr(&category_draw_indices);

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

  ser.from_ptr(&has_per_proto_visibility_toggle);
  ser.from_string_vector(&proto_names);
}

void ShrubTree::serialize(Serializer& ser) {
  ser.from_pod_vector(&time_of_day_colors);
  ser.from_pod_vector(&indices);
  packed_vertices.serialize(ser);
  if (ser.is_saving()) {
    ser.save<size_t>(static_draws.size());
  } else {
    static_draws.resize(ser.load<size_t>());
  }
  for (auto& draw : static_draws) {
    draw.serialize(ser);
  }
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
  ser.from_ptr(&combo_id);
  ser.from_pod_vector(&data);
  ser.from_str(&debug_name);
  ser.from_str(&debug_tpage_name);
  ser.from_ptr(&load_to_pool);
}

void IndexTexture::serialize(Serializer& ser) {
  ser.from_ptr(&w);
  ser.from_ptr(&h);
  ser.from_ptr(&combo_id);
  ser.from_pod_vector(&index_data);
  ser.from_ptr(&color_table);
  ser.from_str(&name);
  ser.from_str(&tpage_name);
  ser.from_string_vector(&level_names);
}

void CollisionMesh::serialize(Serializer& ser) {
  ser.from_pod_vector(&vertices);
}

void MercDraw::serialize(Serializer& ser) {
  ser.from_ptr(&mode);
  ser.from_ptr(&tree_tex_id);
  ser.from_ptr(&eye_id);
  ser.from_ptr(&first_index);
  ser.from_ptr(&index_count);
  ser.from_ptr(&num_triangles);
}

void Blerc::serialize(Serializer& ser) {
  ser.from_pod_vector(&float_data);
  ser.from_pod_vector(&int_data);
}

void MercModifiableDrawGroup::serialize(Serializer& ser) {
  if (ser.is_saving()) {
    ser.save<size_t>(mod_draw.size());
  } else {
    mod_draw.resize(ser.load<size_t>());
  }
  for (auto& draw : mod_draw) {
    draw.serialize(ser);
  }

  if (ser.is_saving()) {
    ser.save<size_t>(fix_draw.size());
  } else {
    fix_draw.resize(ser.load<size_t>());
  }
  for (auto& draw : fix_draw) {
    draw.serialize(ser);
  }
  ser.from_pod_vector(&vertices);
  ser.from_pod_vector(&vertex_lump4_addr);
  ser.from_pod_vector(&fragment_mask);
  ser.from_ptr(&expect_vidx_end);

  blerc.serialize(ser);
}

void MercEffect::serialize(Serializer& ser) {
  if (ser.is_saving()) {
    ser.save<size_t>(all_draws.size());
  } else {
    all_draws.resize(ser.load<size_t>());
  }
  for (auto& draw : all_draws) {
    draw.serialize(ser);
  }

  mod.serialize(ser);

  ser.from_ptr(&envmap_mode);
  ser.from_ptr(&envmap_texture);
  ser.from_ptr(&has_envmap);
  ser.from_ptr(&has_mod_draw);
}

void MercModel::serialize(Serializer& ser) {
  ser.from_str(&name);
  if (ser.is_saving()) {
    ser.save<size_t>(effects.size());
  } else {
    effects.resize(ser.load<size_t>());
  }
  for (auto& effect : effects) {
    effect.serialize(ser);
  }
  ser.from_ptr(&max_draws);
  ser.from_ptr(&max_bones);
  ser.from_ptr(&st_vif_add);
  ser.from_ptr(&xyz_scale);
  ser.from_ptr(&st_magic);
}

void MercModelGroup::serialize(Serializer& ser) {
  if (ser.is_saving()) {
    ser.save<size_t>(models.size());
  } else {
    models.resize(ser.load<size_t>());
  }
  for (auto& model : models) {
    model.serialize(ser);
  }

  ser.from_pod_vector(&indices);
  ser.from_pod_vector(&vertices);
}

void Level::serialize(Serializer& ser) {
  ser.from_ptr(&version);
  if (ser.is_loading() && version != TFRAG3_VERSION) {
    ASSERT_MSG(false, fmt::format("version mismatch when loading tfrag3 data. Got {}, expected {}, "
                                  "did you forget to re-decompile?",
                                  version, TFRAG3_VERSION));
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

  if (ser.is_saving()) {
    ser.save<size_t>(index_textures.size());
  } else {
    index_textures.resize(ser.load<size_t>());
  }
  for (auto& tex : index_textures) {
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

  if (ser.is_saving()) {
    ser.save<size_t>(shrub_trees.size());
  } else {
    shrub_trees.resize(ser.load<size_t>());
  }
  for (auto& tree : shrub_trees) {
    tree.serialize(ser);
  }

  collision.serialize(ser);
  merc_data.serialize(ser);

  ser.from_ptr(&version2);
  if (ser.is_loading() && version2 != TFRAG3_VERSION) {
    ASSERT_MSG(false, fmt::format(
                          "version mismatch when loading tfrag3 data (at end). Got {}, expected {}",
                          version2, TFRAG3_VERSION));
  }
}

void MercModifiableDrawGroup::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::MERC_MOD_VERT, sizeof(MercVertex) * vertices.size());
  tracker->add(MemoryUsageCategory::MERC_MOD_DRAW_1, sizeof(MercDraw) * fix_draw.size());
  tracker->add(MemoryUsageCategory::MERC_MOD_DRAW_2, sizeof(MercDraw) * mod_draw.size());
  tracker->add(MemoryUsageCategory::MERC_MOD_TABLE, sizeof(u16) * vertex_lump4_addr.size());
  tracker->add(MemoryUsageCategory::BLERC, sizeof(BlercFloatData) * blerc.float_data.size());
  tracker->add(MemoryUsageCategory::BLERC, sizeof(u32) * blerc.int_data.size());
}

void MercEffect::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::MERC_DRAW, sizeof(MercDraw) * all_draws.size());
  mod.memory_usage(tracker);
}

void MercModel::memory_usage(MemoryUsageTracker* tracker) const {
  for (auto& effect : effects) {
    effect.memory_usage(tracker);
  }
}

void MercModelGroup::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::MERC_VERT, sizeof(MercVertex) * vertices.size());
  tracker->add(MemoryUsageCategory::MERC_INDEX, sizeof(u32) * indices.size());
  for (auto& model : models) {
    model.memory_usage(tracker);
  }
}

void CollisionMesh::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::COLLISION, sizeof(Vertex) * vertices.size());
}

void PackedShrubVertices::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::SHRUB_VERT, 64 * matrices.size());
  tracker->add(MemoryUsageCategory::SHRUB_VERT, sizeof(InstanceGroup) * instance_groups.size());
  tracker->add(MemoryUsageCategory::SHRUB_VERT, sizeof(Vertex) * vertices.size());
}

void ShrubTree::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::SHRUB_TIME_OF_DAY,
               sizeof(TimeOfDayColor) * time_of_day_colors.size());
  packed_vertices.memory_usage(tracker);
  tracker->add(MemoryUsageCategory::SHRUB_DRAW, sizeof(ShrubDraw) * static_draws.size());
  tracker->add(MemoryUsageCategory::SHRUB_IND, sizeof(u32) * indices.size());
}

void InstancedStripDraw::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::TIE_INST_INDEX, sizeof(u32) * vertex_index_stream.size());
  tracker->add(MemoryUsageCategory::TIE_INST_VIS, sizeof(InstanceGroup) * instance_groups.size());
}

void PackedTieVertices::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::TIE_CIDX, sizeof(u16) * color_indices.size());
  tracker->add(MemoryUsageCategory::TIE_MATRICES, 64 * matrices.size());
  tracker->add(MemoryUsageCategory::TIE_GRPS, sizeof(MatrixGroup) * matrix_groups.size());
  tracker->add(MemoryUsageCategory::TIE_VERTS, sizeof(Vertex) * vertices.size());
}

void TieTree::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::TIE_BVH, sizeof(VisNode) * bvh.vis_nodes.size());
  for (auto& draw : static_draws) {
    tracker->add(MemoryUsageCategory::TIE_DEINST_INDEX,
                 draw.runs.size() * sizeof(StripDraw::VertexRun));
    tracker->add(MemoryUsageCategory::TIE_DEINST_INDEX, draw.plain_indices.size() * sizeof(u32));
    tracker->add(MemoryUsageCategory::TIE_DEINST_VIS,
                 draw.vis_groups.size() * sizeof(StripDraw::VisGroup));
  }
  packed_vertices.memory_usage(tracker);
  tracker->add(MemoryUsageCategory::TIE_TIME_OF_DAY, sizeof(TimeOfDayColor) * colors.size());

  for (auto& draw : instanced_wind_draws) {
    draw.memory_usage(tracker);
  }
  tracker->add(MemoryUsageCategory::TIE_WIND_INSTANCE_INFO,
               sizeof(TieWindInstance) * wind_instance_info.size());
}

void PackedTfragVertices::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::TFRAG_VERTS,
               sizeof(PackedTfragVertices::Vertex) * vertices.size());
  tracker->add(MemoryUsageCategory::TFRAG_CLUSTER,
               sizeof(math::Vector<u16, 3>) * cluster_origins.size());
}

void TfragTree::memory_usage(MemoryUsageTracker* tracker) const {
  for (auto& draw : draws) {
    tracker->add(MemoryUsageCategory::TFRAG_INDEX, draw.runs.size() * sizeof(StripDraw::VertexRun));
    tracker->add(MemoryUsageCategory::TFRAG_INDEX, draw.plain_indices.size() * sizeof(u32));
    tracker->add(MemoryUsageCategory::TFRAG_VIS,
                 draw.vis_groups.size() * sizeof(StripDraw::VisGroup));
  }
  packed_vertices.memory_usage(tracker);
  tracker->add(MemoryUsageCategory::TFRAG_TIME_OF_DAY, sizeof(TimeOfDayColor) * colors.size());
  tracker->add(MemoryUsageCategory::TFRAG_BVH, sizeof(VisNode) * bvh.vis_nodes.size());
}

void Texture::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::TEXTURE, data.size() * sizeof(u32));
}

void IndexTexture::memory_usage(MemoryUsageTracker* tracker) const {
  tracker->add(MemoryUsageCategory::SPECIAL_TEXTURE, index_data.size());
  tracker->add(MemoryUsageCategory::SPECIAL_TEXTURE, 256 * 4);  // clut
}

void Level::memory_usage(MemoryUsageTracker* tracker) const {
  for (const auto& texture : textures) {
    texture.memory_usage(tracker);
  }
  for (const auto& texture : index_textures) {
    texture.memory_usage(tracker);
  }
  for (const auto& tftk : tfrag_trees) {
    for (const auto& tree : tftk) {
      tree.memory_usage(tracker);
    }
  }
  for (const auto& ttk : tie_trees) {
    for (const auto& tree : ttk) {
      tree.memory_usage(tracker);
    }
  }
  for (const auto& tree : shrub_trees) {
    tree.memory_usage(tracker);
  }
  collision.memory_usage(tracker);
  merc_data.memory_usage(tracker);
}

void print_memory_usage(const tfrag3::Level& lev, int uncompressed_data_size) {
  int total_accounted = 0;
  MemoryUsageTracker mem_use;
  lev.memory_usage(&mem_use);

  std::vector<std::pair<std::string, int>> known_categories = {
      {"texture", mem_use.data[tfrag3::MemoryUsageCategory::TEXTURE]},
      {"special-texture", mem_use.data[tfrag3::MemoryUsageCategory::SPECIAL_TEXTURE]},
      {"tie-deinst-vis", mem_use.data[tfrag3::MemoryUsageCategory::TIE_DEINST_VIS]},
      {"tie-deinst-idx", mem_use.data[tfrag3::MemoryUsageCategory::TIE_DEINST_INDEX]},
      {"tie-inst-vis", mem_use.data[tfrag3::MemoryUsageCategory::TIE_INST_VIS]},
      {"tie-inst-idx", mem_use.data[tfrag3::MemoryUsageCategory::TIE_INST_INDEX]},
      {"tie-bvh", mem_use.data[tfrag3::MemoryUsageCategory::TIE_BVH]},
      {"tie-verts", mem_use.data[tfrag3::MemoryUsageCategory::TIE_VERTS]},
      {"tie-colors", mem_use.data[tfrag3::MemoryUsageCategory::TIE_TIME_OF_DAY]},
      {"tie-wind-inst-info", mem_use.data[tfrag3::MemoryUsageCategory::TIE_WIND_INSTANCE_INFO]},
      {"tie-cidx", mem_use.data[tfrag3::MemoryUsageCategory::TIE_CIDX]},
      {"tie-mats", mem_use.data[tfrag3::MemoryUsageCategory::TIE_MATRICES]},
      {"tie-grps", mem_use.data[tfrag3::MemoryUsageCategory::TIE_GRPS]},
      {"tfrag-vis", mem_use.data[tfrag3::MemoryUsageCategory::TFRAG_VIS]},
      {"tfrag-idx", mem_use.data[tfrag3::MemoryUsageCategory::TFRAG_INDEX]},
      {"tfrag-vert", mem_use.data[tfrag3::MemoryUsageCategory::TFRAG_VERTS]},
      {"tfrag-colors", mem_use.data[tfrag3::MemoryUsageCategory::TFRAG_TIME_OF_DAY]},
      {"tfrag-cluster", mem_use.data[tfrag3::MemoryUsageCategory::TFRAG_CLUSTER]},
      {"tfrag-bvh", mem_use.data[tfrag3::MemoryUsageCategory::TFRAG_BVH]},
      {"shrub-colors", mem_use.data[tfrag3::MemoryUsageCategory::SHRUB_TIME_OF_DAY]},
      {"shrub-vert", mem_use.data[tfrag3::MemoryUsageCategory::SHRUB_VERT]},
      {"shrub-ind", mem_use.data[tfrag3::MemoryUsageCategory::SHRUB_IND]},
      {"shrub-draw", mem_use.data[tfrag3::MemoryUsageCategory::SHRUB_DRAW]},
      {"collision", mem_use.data[tfrag3::MemoryUsageCategory::COLLISION]},
      {"merc-vert", mem_use.data[tfrag3::MemoryUsageCategory::MERC_VERT]},
      {"merc-idx", mem_use.data[tfrag3::MemoryUsageCategory::MERC_INDEX]},
      {"merc-draw", mem_use.data[tfrag3::MemoryUsageCategory::MERC_DRAW]},
      {"merc-mod-vert", mem_use.data[tfrag3::MemoryUsageCategory::MERC_MOD_VERT]},
      {"merc-mod-ind", mem_use.data[tfrag3::MemoryUsageCategory::MERC_MOD_IND]},
      {"merc-mod-table", mem_use.data[tfrag3::MemoryUsageCategory::MERC_MOD_TABLE]},
      {"merc-mod-draw-1", mem_use.data[tfrag3::MemoryUsageCategory::MERC_MOD_DRAW_1]},
      {"merc-mod-draw-2", mem_use.data[tfrag3::MemoryUsageCategory::MERC_MOD_DRAW_2]},
      {"blerc", mem_use.data[tfrag3::MemoryUsageCategory::BLERC]},
  };
  for (auto& known : known_categories) {
    total_accounted += known.second;
  }

  known_categories.push_back({"unknown", uncompressed_data_size - total_accounted});

  std::sort(known_categories.begin(), known_categories.end(),
            [](const auto& a, const auto& b) { return a.second > b.second; });

  for (const auto& x : known_categories) {
    if (x.second) {
      fmt::print("{:30s} : {:6d} kB {:3.1f}%\n", x.first, x.second / 1024,
                 100.f * (float)x.second / uncompressed_data_size);
    }
  }
}

std::size_t PreloadedVertex::hash::operator()(const PreloadedVertex& v) const {
  return std::hash<float>()(v.x) ^ std::hash<float>()(v.y) ^ std::hash<float>()(v.z) ^
         std::hash<float>()(v.s) ^ std::hash<float>()(v.t) ^ std::hash<u16>()(v.color_index);
}

}  // namespace tfrag3
