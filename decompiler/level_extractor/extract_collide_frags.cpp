#include "extract_collide_frags.h"

#include "common/util/FileUtil.h"

namespace decompiler {

struct CollideListItem {
  const level_tools::CollideFragMesh* mesh = nullptr;
  const level_tools::InstanceTie* inst = nullptr;

  struct {
    struct UnpackedFace {
      UnpackedFace(u8 a, u8 b, u8 c) : verts(a, b, c) {}

      math::Vector<u8, 3> verts;
      u32 pat;
    };

    std::vector<math::Vector4f> vu0_buffer;
    std::vector<UnpackedFace> faces;
  } unpacked;
};

/*!
 * Get all collide frags.
 */
std::vector<CollideListItem> build_all_frags_list(
    const level_tools::DrawableTreeCollideFragment* tree,
    const std::vector<const level_tools::DrawableTreeInstanceTie*>& ties) {
  std::vector<CollideListItem> list;
  for (auto& cf : tree->last_array.collide_fragments) {
    auto& elt = list.emplace_back();
    elt.mesh = &cf.mesh;
    elt.inst = nullptr;
  }

  for (auto tt : ties) {
    auto last_array = tt->arrays.back().get();
    auto as_instance_array = dynamic_cast<level_tools::DrawableInlineArrayInstanceTie*>(last_array);
    ASSERT(as_instance_array);
    for (auto& inst : as_instance_array->instances) {
      auto& frags = tt->prototypes.prototype_array_tie.data.at(inst.bucket_index)
                        .collide_frag.collide_fragments;
      for (auto& frag : frags) {
        auto& elt = list.emplace_back();
        elt.inst = &inst;
        elt.mesh = &frag.mesh;
      }
    }
  }
  return list;
}

float u32_to_float(u32 in) {
  float r;
  memcpy(&r, &in, sizeof(float));
  return r;
}

void unpack_part1_collide_list_item(CollideListItem& item) {
  int in_idx = 0;
  int out_idx = 0;
  int qw_to_write = item.mesh->vertex_count;

  while (out_idx < qw_to_write * 4) {
    auto& vert = item.unpacked.vu0_buffer.emplace_back();
    vert[0] = u32_to_float(0x4d000000 + deref_u16(item.mesh->packed_data, in_idx++));
    vert[1] = u32_to_float(0x4d000000 + deref_u16(item.mesh->packed_data, in_idx++));
    vert[2] = u32_to_float(0x4d000000 + deref_u16(item.mesh->packed_data, in_idx++));
    vert[3] = u32_to_float(0x3f800000);
    out_idx += 4;
  }
}

math::Vector<float, 4> transform_tie(const std::array<math::Vector4f, 4> mat,
                                     const math::Vector4f& pt) {
  auto temp = mat[0] * pt.x() + mat[1] * pt.y() + mat[2] * pt.z() + mat[3];
  math::Vector4f result;
  result.x() = temp.x();
  result.y() = temp.y();
  result.z() = temp.z();
  result.w() = 1.f;
  return result;
}

namespace {
// todo dedup
std::array<math::Vector4f, 4> extract_tie_matrix(const u16* data) {
  std::array<math::Vector4f, 4> result;
  for (int i = 0; i < 4; i++) {
    s32 x = data[12 + i];
    x <<= 16;
    x >>= 10;
    result[3][i] = x;
  }

  for (int vec = 0; vec < 3; vec++) {
    for (int i = 0; i < 4; i++) {
      s32 x = data[vec * 4 + i];
      x <<= 16;
      x >>= 16;
      result[vec][i] = (float)x / 4096.f;
    }
  }

  return result;
}
}  // namespace

void unpack_part2_collide_list_item(CollideListItem& item) {
  float base_offset = u32_to_float(0x4d000000);

  math::Vector4f vf13_combo_offset(base_offset, base_offset, base_offset, 0);
  s32 base_trans_int[4];
  memcpy(base_trans_int, item.mesh->base_trans.data, 16);
  math::Vector4f vf14_base_trans_float(base_trans_int[0], base_trans_int[1], base_trans_int[2],
                                       base_trans_int[3]);
  vf13_combo_offset -= vf14_base_trans_float;
  std::array<math::Vector4f, 4> mat;
  if (item.inst) {
    mat = extract_tie_matrix(item.inst->origin.data);
    mat[3][0] += item.inst->bsphere.data[0];
    mat[3][1] += item.inst->bsphere.data[1];
    mat[3][2] += item.inst->bsphere.data[2];
  }

  for (auto& ver : item.unpacked.vu0_buffer) {
    ver -= vf13_combo_offset;
    if (item.inst) {
      ver = transform_tie(mat, ver);
    }
  }
}

void find_faces(CollideListItem& item) {
  u32 byte_offset = 16 * item.mesh->vertex_data_qwc;

  while (true) {
    s8 t7_start_idx = deref_s8(item.mesh->packed_data, byte_offset++);
    if (t7_start_idx < 0) {
      return;
    }
    s8 t8_start_idx = deref_s8(item.mesh->packed_data, byte_offset++);
    s8 t6_start_idx = deref_s8(item.mesh->packed_data, byte_offset++);

    s8 t6_run_idx = t7_start_idx;
    s8 t7_run_idx = t8_start_idx;
    s8 ra_run_idx = t6_start_idx;

    item.unpacked.faces.emplace_back(t6_run_idx, t7_run_idx, ra_run_idx);
    s8 s5 = 1;
    while (true) {
      s8 next = deref_s8(item.mesh->packed_data, byte_offset++);
      if (!next) {
        break;
      }
      if (next < 0) {
        next = -((s32)next);
      } else {
        t6_run_idx = t7_run_idx;
        s5 = -s5;
      }
      next--;
      t7_run_idx = ra_run_idx;
      ra_run_idx = next;
      u8 result[3];
      result[1] = t7_run_idx;
      result[1 + s5] = ra_run_idx;
      result[1 - s5] = t6_run_idx;
      item.unpacked.faces.emplace_back(result[0], result[1], result[2]);
      ASSERT((u32)next < item.unpacked.vu0_buffer.size());
    }
  }
}

void extract_pats(CollideListItem& item) {
  u32 byte_offset = 16 * item.mesh->vertex_data_qwc + item.mesh->strip_data_len;
  for (auto& f : item.unpacked.faces) {
    auto pat_idx = deref_u8(item.mesh->packed_data, byte_offset++);

    u32 pat = deref_u32(item.mesh->pat_array, pat_idx);
    // lg::print("pat @ {} is 0x{:x}\n", pat_idx, pat);
    f.pat = pat;
  }
}

std::string debug_dump_to_obj(const std::vector<CollideListItem>& list) {
  std::vector<math::Vector4f> verts;
  std::vector<math::Vector<u32, 3>> faces;

  for (auto& item : list) {
    u32 f_off = verts.size() + 1;
    for (auto& f : item.unpacked.faces) {
      faces.emplace_back(f.verts[0] + f_off, f.verts[1] + f_off, f.verts[2] + f_off);
    }
    for (u32 t = 0; t < item.unpacked.vu0_buffer.size(); t++) {
      verts.push_back(item.unpacked.vu0_buffer[t] / 65536);
    }
  }

  std::string result;
  for (auto& vert : verts) {
    result += fmt::format("v {} {} {}\n", vert.x(), vert.y(), vert.z());
  }

  for (auto& face : faces) {
    result += fmt::format("f {}/{} {}/{} {}/{}\n", face.x(), face.x(), face.y(), face.y(), face.z(),
                          face.z());
  }

  return result;
}

void set_vertices_for_tri(tfrag3::CollisionMesh::Vertex* out, const math::Vector4f* in) {
  math::Vector3f v10 = in[1].xyz() - in[0].xyz();
  math::Vector3f v20 = in[2].xyz() - in[0].xyz();
  auto normal = (v10.cross(v20).normalized() * INT16_MAX).cast<s16>();

  for (int i = 0; i < 3; i++) {
    out[i].x = in[i].x();
    out[i].y = in[i].y();
    out[i].z = in[i].z();
    out[i].nx = normal.x();
    out[i].ny = normal.y();
    out[i].nz = normal.z();
    out[i].flags = 0;  // todo
    out[i].pad = 0;
  }
}

void extract_collide_frags(const level_tools::DrawableTreeCollideFragment* tree,
                           const std::vector<const level_tools::DrawableTreeInstanceTie*>& ties,
                           const std::string& debug_name,
                           tfrag3::Level& out,
                           bool dump_level) {
  // in-game, the broad-phase collision builds a list of fragments, then unpacks them with:
  /*
   *(dotimes (i (-> *collide-list* num-items))
      (let ((frag (-> *collide-list* items i)))
          ;; to VU0 memory
          (__pc-upload-collide-frag (-> frag mesh packed-data) (-> frag mesh vertex-data-qwc) (->
   frag mesh vertex-count))
          ;; from VU0 memory to scratchpad
          (unpack-background-collide-mesh obj (-> frag mesh) (-> frag inst) 0)
          ;; from scratchpad to collide-cache memory.
          (import-mesh-func obj (-> frag mesh))
        )
      )
   */

  auto all_frags = build_all_frags_list(tree, ties);
  [[maybe_unused]] u32 total_faces = 0;
  for (auto& frag : all_frags) {
    unpack_part1_collide_list_item(frag);
    unpack_part2_collide_list_item(frag);
    find_faces(frag);
    extract_pats(frag);
    total_faces += frag.unpacked.faces.size();
  }

  if (dump_level) {
    auto debug_out = debug_dump_to_obj(all_frags);
    auto file_path =
        file_util::get_file_path({"debug_out", fmt::format("collide-{}.obj", debug_name)});
    file_util::create_dir_if_needed_for_file(file_path);
    file_util::write_text_file(file_path, debug_out);
  }

  for (auto& item : all_frags) {
    for (auto& f : item.unpacked.faces) {
      math::Vector4f verts[3] = {item.unpacked.vu0_buffer[f.verts[0]],
                                 item.unpacked.vu0_buffer[f.verts[1]],
                                 item.unpacked.vu0_buffer[f.verts[2]]};
      tfrag3::CollisionMesh::Vertex out_verts[3];
      set_vertices_for_tri(out_verts, verts);
      for (int i = 0; i < 3; i++) {
        out_verts[i].pat = f.pat;
        out.collision.vertices.push_back(out_verts[i]);
      }
    }
  }
}

}  // namespace decompiler
