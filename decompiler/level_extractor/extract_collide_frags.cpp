#include "extract_collide_frags.h"

#include "common/log/log.h"
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
                           const Config& config,
                           const std::string& debug_name,
                           tfrag3::Level& out) {
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

  if (config.rip_collision) {
    auto debug_out = debug_dump_to_obj(all_frags);
    auto file_path = file_util::get_file_path(
        {fmt::format("debug_out/{}", config.game_name), fmt::format("collide-{}.obj", debug_name)});
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

////////////////////////
// Jak 2 Format
////////////////////////

void handle_collide_fragment(const TypedRef& collide_fragment,
                             const decompiler::DecompilerTypeSystem& dts,
                             const std::optional<std::array<math::Vector4f, 4>>& matrix,
                             std::vector<tfrag3::CollisionMesh::Vertex>* out) {
  struct Poly {
    u8 vert_index[3];
    u8 pat;
  };

  level_tools::Vector bbox_min;
  bbox_min.read_from_file(get_field_ref(collide_fragment, "bbox", dts));

  // There's a lot of indirection:
  // grid -> buckets -> index -> poly -> vertex.

  // First step: 3D coordinates are converted to a cell in a 3D grid.
  // The dimensions of this grid are stored as u8's in the dimension array.
  u32 dim_array = deref_u32(get_field_ref(collide_fragment, "dimension-array", dts), 0);
  u8 counts[4];
  memcpy(counts, &dim_array, 4);
  ASSERT(counts[3] == 0);  // unused

  // Each grid cell maps to a bucket (they go in order, no fancy hash function)
  u32 num_buckets = read_plain_data_field<uint16_t>(collide_fragment, "num-buckets", dts);
  // this should match the grid dims
  ASSERT(counts[0] * counts[1] * counts[2] == num_buckets);

  // read the buckets
  struct BucketEntry {
    s16 index, count;
  };
  std::vector<BucketEntry> buckets(num_buckets);
  memcpy_from_plain_data((u8*)buckets.data(),
                         deref_label(get_field_ref(collide_fragment, "bucket-array", dts)),
                         sizeof(BucketEntry) * num_buckets);

  // Each bucket references a series of entries in the index list. Check all buckets to see the
  // length of the index list.
  int max_from_buckets = 0;
  for (const auto& bucket : buckets) {
    int end = bucket.count + bucket.index;
    if (end > max_from_buckets) {
      max_from_buckets = end;
    }
  }
  // confirm the index list length matches
  u32 num_indices = read_plain_data_field<uint16_t>(collide_fragment, "num-indices", dts);
  ASSERT(max_from_buckets == (int)num_indices);

  // read the index list
  std::vector<u8> index_list(num_indices);
  memcpy_from_plain_data((u8*)index_list.data(),
                         deref_label(get_field_ref(collide_fragment, "index-array", dts)),
                         num_indices);
  u8 max_in_index_list = 0;
  for (auto x : index_list) {
    max_in_index_list = std::max(x, max_in_index_list);
  }

  u8 poly_count = read_plain_data_field<uint8_t>(collide_fragment, "poly-count", dts);
  u8 num_polys = read_plain_data_field<uint8_t>(collide_fragment, "num-polys", dts);
  ASSERT(poly_count == num_polys);
  if (poly_count == 0) {
    ASSERT(max_in_index_list == 255);
  } else {
    ASSERT(num_polys == max_in_index_list + 1);
  }

  // this value seems to be bogus
  // u16 vert_count = read_plain_data_field<uint16_t>(collide_fragment, "num-verts", dts);
  std::vector<Poly> polys(max_in_index_list + 1);
  memcpy_from_plain_data((u8*)polys.data(),
                         deref_label(get_field_ref(collide_fragment, "poly-array", dts)),
                         4 * (max_in_index_list + 1));
  int max_vi = 0;
  int max_pat = 0;
  for (const auto& p : polys) {
    for (int i = 0; i < 3; i++) {
      max_vi = std::max(max_vi, (int)p.vert_index[i]);
    }
    max_pat = std::max(max_pat, (int)p.pat);
  }
  std::vector<u16> verts((max_vi + 1) * 3);
  memcpy_from_plain_data((u8*)verts.data(),
                         deref_label(get_field_ref(collide_fragment, "vert-array", dts)),
                         6 * (max_vi + 1));

  std::vector<u32> pats(max_pat + 1);
  memcpy_from_plain_data((u8*)pats.data(),
                         deref_label(get_field_ref(collide_fragment, "pat-array", dts)),
                         4 * (max_pat + 1));

  for (const auto& p : polys) {
    math::Vector4f verts_in[3];
    for (int vi = 0; vi < 3; vi++) {
      int v = p.vert_index[vi];
      verts_in[vi] = {u32(verts.at(v * 3)) * 16 + bbox_min.data[0],
                      u32(verts.at(v * 3 + 1)) * 16 + bbox_min.data[1],
                      u32(verts.at(v * 3 + 2)) * 16 + bbox_min.data[2], 1.f};
      if (matrix) {
        verts_in[vi] = transform_tie(*matrix, verts_in[vi]);
      }
    }

    math::Vector3f v10 = verts_in[1].xyz() - verts_in[0].xyz();
    math::Vector3f v20 = verts_in[2].xyz() - verts_in[0].xyz();
    auto normal = (v10.cross(v20).normalized() * INT16_MAX).cast<s16>();
    for (int i = 0; i < 3; i++) {
      auto& vert_out = out->emplace_back();
      vert_out.x = verts_in[i].x();
      vert_out.y = verts_in[i].y();
      vert_out.z = verts_in[i].z();
      vert_out.nx = normal.x();
      vert_out.ny = normal.y();
      vert_out.nz = normal.z();
      vert_out.flags = 0;  // todo
      vert_out.pad = 0;
      vert_out.pad2 = 0;
      vert_out.pat = pats.at(p.pat);
    }
  }
}

std::string debug_dump_to_obj(const std::vector<tfrag3::CollisionMesh::Vertex>& verts_in) {
  std::vector<math::Vector4f> verts;
  std::vector<math::Vector<u32, 3>> faces;

  for (auto& v : verts_in) {
    verts.emplace_back(v.x / 4096.0, v.y / 4096.0, v.z / 4096.0, 1.0);

    u32 v_len = verts.size();
    if (v_len % 3 == 0) {
      // add face from last 3 vertices
      faces.emplace_back(v_len - 2, v_len - 1, v_len);
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

void extract_collide_frags(const level_tools::CollideHash& chash,
                           const std::vector<const level_tools::DrawableTreeInstanceTie*>& ties,
                           const Config& config,
                           const std::string& debug_name,
                           const decompiler::DecompilerTypeSystem& dts,
                           tfrag3::Level& out) {
  // We need to find all collide-hash-fragments, but we can't just scan through the entire file.
  // for collide-hash-fragments, we need to figure out which TIEs they belong to, to apply the
  // instance transformation matrix.

  // Each level has a bsp-header, which has a collide-hash, storing the collision data in a
  // hash-table based structure. Note that this system is separate from the "spatial-hash" system,
  // though they are both versions of spatial hashing.

  // A point in the level belongs to a cell in a uniform grid. (this grid is not stored anywhere)
  // Each cell's coordinates can be hashed to get a "bucket" index.
  // Each "bucket" points to a number of consecutive collide-hash-items stored in the item-array
  // of the collide-hash.

  // For just extracting the collision mesh, we can skip the bucket stuff and just iterate through
  // the item array directly. Note that items may appear multiple times if they belong to multiple
  // buckets. (this happens because some collision geometry is larger than a grid cell)

  std::unordered_set<u32> processed_offsets;
  auto data_ref = chash.item_array;
  for (int i = 0; i < chash.num_items; i++) {
    // skip over the ID field, which we don't care about.
    data_ref.byte_offset += 4;

    // get the object in the item array element
    auto obj = deref_label(data_ref);
    ASSERT(obj.seg == data_ref.seg);
    obj.byte_offset -= 4;  // basic offset

    if (processed_offsets.count(obj.byte_offset) == 0) {
      processed_offsets.insert(obj.byte_offset);

      auto type = get_type_of_basic(obj);
      if (type == "collide-hash-fragment") {
        // not instanced.
        handle_collide_fragment(typed_ref_from_basic(obj, dts), dts, {}, &out.collision.vertices);
      } else if (type == "instance-tie") {
      } else {
        ASSERT_NOT_REACHED();
      }
    }
    data_ref.byte_offset += 4;
  }

  // first, all ties. Store collide-hash-fragments that are associated by ties.
  for (auto tt : ties) {
    auto last_array = tt->arrays.back().get();
    auto as_instance_array = dynamic_cast<level_tools::DrawableInlineArrayInstanceTie*>(last_array);
    ASSERT(as_instance_array);
    for (auto& inst : as_instance_array->instances) {
      std::array<math::Vector4f, 4> mat;
      mat = extract_tie_matrix(inst.origin.data);
      mat[3][0] += inst.bsphere.data[0];
      mat[3][1] += inst.bsphere.data[1];
      mat[3][2] += inst.bsphere.data[2];
      auto& frags =
          tt->prototypes.prototype_array_tie.data.at(inst.bucket_index).collide_hash_frags;
      for (auto frag : frags) {
        frag.byte_offset -= 4;
        handle_collide_fragment(typed_ref_from_basic(frag, dts), dts, mat, &out.collision.vertices);
      }
    }
  }

  if (config.rip_collision) {
    // out.collision.vertices every 3 vertices make a face, so it duplicates vertices in many cases
    // for now debug_dump_to_obj isn't smart and doesn't hash these to save space or anything
    auto debug_out = debug_dump_to_obj(out.collision.vertices);
    auto file_path = file_util::get_file_path(
        {fmt::format("debug_out/{}", config.game_name), fmt::format("collide-{}.obj", debug_name)});
    file_util::create_dir_if_needed_for_file(file_path);
    file_util::write_text_file(file_path, debug_out);
  }
}
}  // namespace decompiler
