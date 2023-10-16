#include "collide_pack.h"

#include <functional>
#include <unordered_map>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/Timer.h"

struct PackedU16Verts {
  math::Vector<s32, 3> base;
  std::vector<math::Vector<u16, 3>> vertex;
};

/*!
 * Assert that the given float can be converted to a packed u16, and return that u16.
 */
u16 magic_float_to_u16(float in) {
  u16 u16s[2];
  memcpy(u16s, &in, 4);
  ASSERT_MSG(u16s[1] == 0x4d00,
             "Unable to pack collision vertex data to u16's. Try to make smaller triangles and "
             "avoid skinny triangles. (or there is a bug in the packer)");
  return u16s[0];
}

namespace {
float u32_to_float(u32 in) {
  float out;
  memcpy(&out, &in, 4);
  return out;
}
}  // namespace

/*!
 * Pack vertices to base + u16.
 * The format is quite strange to allow for fast unpacking.
 */
PackedU16Verts pack_verts_to_u16(const std::vector<math::Vector3f>& input) {
  PackedU16Verts result;
  if (input.empty()) {
    lg::warn("Empty collide fragment");
  }

  // this "magic" offset is a large float where a ulp is 16.f, or 1/256th of a meter.
  // this means that float -> int can be done as a single addition.
  // (or, in some cases, we can avoid float->int entirely)
  math::Vector3f magic_offset;
  magic_offset.fill(u32_to_float(0x4d000000));

  // we'll be treating everything as an offset from this minimum vertex:
  math::Vector3f min_vtx = input.empty() ? math::Vector3f::zero() : input[0];
  for (auto& vtx : input) {
    min_vtx.min_in_place(vtx);
  }
  //  give us a tiny bit of extra room to avoid rounding problems
  min_vtx -= 16.f;

  // to round down to nearest integer
  result.base = min_vtx.cast<s32>();
  auto base = result.base.cast<float>();

  // compute offset relative to base.
  for (auto& vtx : input) {
    // add the "magic offset" to make this a 0x4dXXXXXX float.
    // subtract the base to make this a 0x4d00XXXX float.
    auto vertex_magic = vtx + magic_offset - base;
    // and if we did it right, we should be able to pack to u16's here
    result.vertex.emplace_back(magic_float_to_u16(vertex_magic[0]),
                               magic_float_to_u16(vertex_magic[1]),
                               magic_float_to_u16(vertex_magic[2]));
  }

  // verify
  /*
  for (size_t i = 0; i < input.size(); i++) {
    math::Vector3f v;
    v[0] = u32_to_float(0x4d000000 + result.vertex[i][0]);
    v[1] = u32_to_float(0x4d000000 + result.vertex[i][1]);
    v[2] = u32_to_float(0x4d000000 + result.vertex[i][2]);
    float base_offset = u32_to_float(0x4d000000);
    math::Vector3f vf13_combo_offset(base_offset, base_offset, base_offset);
    math::Vector3f vf14_base_trans_float(result.base[0], result.base[1], result.base[2]);
    vf13_combo_offset -= vf14_base_trans_float;
    v -= vf13_combo_offset;
    lg::print("error {}\n", (v - input[i]).to_string_aligned());;
  }
   */

  return result;
}

struct PatSurfaceHash {
  size_t operator()(const jak1::PatSurface& in) const { return std::hash<u32>()(in.val); }
};

/*!
 * pat -> pat index mapping.
 * There's a pat "palette" with up 255 unique pats.
 */
struct PatMap {
  std::unordered_map<jak1::PatSurface, u32, PatSurfaceHash> map;
  std::vector<jak1::PatSurface> pats;

  u32 add_pat(jak1::PatSurface pat) {
    const auto& lookup = map.find(pat);
    if (lookup == map.end()) {
      u32 new_idx = pats.size();
      if (new_idx > UINT8_MAX) {
        lg::die("Too many pats. Use fewer. Or improve the pat code to use multiple pat arrays.");
      }
      map[pat] = new_idx;
      pats.push_back(pat);
      return new_idx;
    } else {
      return lookup->second;
    }
  }
};

/*!
 * A face, represented as indices
 */
struct IndexFace {
  math::Vector<u16, 3> vertex_indices;  // per vertex, winding order matters here
  u8 pat_idx;                           // pat for the whole face
};

/*!
 * All the faces in a frag
 */
struct IndexedFaces {
  std::vector<IndexFace> faces;  // index in to the two vertex array below:
  std::vector<math::Vector3f> vertices_float;
  PackedU16Verts vertices_u16;
};

struct Vector3fHash {
  size_t operator()(const math::Vector3f& in) const {
    return std::hash<float>()(in.x()) ^ std::hash<float>()(in.y()) ^ std::hash<float>()(in.z());
  }
};

/*!
 * Deduplicate vertices, converted to indexed, add to pat palette, pack to u16s.
 */
IndexedFaces dedup_frag_mesh(const collide::CollideFrag& frag, PatMap* pat_map) {
  IndexedFaces result;
  std::unordered_map<math::Vector3f, u32, Vector3fHash> vertex_map;

  for (auto& face_in : frag.faces) {
    auto& face_out = result.faces.emplace_back();
    // pat:
    face_out.pat_idx = pat_map->add_pat(face_in.pat);
    // vertices
    for (int i = 0; i < 3; i++) {
      const auto& lookup = vertex_map.find(face_in.v[i]);
      if (lookup == vertex_map.end()) {
        u32 idx = result.vertices_float.size();
        result.vertices_float.push_back(face_in.v[i]);
        face_out.vertex_indices[i] = idx;
        vertex_map[face_in.v[i]] = idx;
      } else {
        face_out.vertex_indices[i] = lookup->second;
      }
    }
  }
  result.vertices_u16 = pack_verts_to_u16(result.vertices_float);
  return result;
}

/*!
 * make strip table that doesn't do any stripping. It will be quite long, which might cause problem
 */
std::vector<u8> make_dumb_strip_table(const IndexedFaces& faces) {
  std::vector<u8> out;
  ASSERT_MSG(
      faces.vertices_float.size() < UINT8_MAX,
      "somehow have UINT8_MAX deduped vertices in a single fragment, likely a bug somewhere.");
  for (auto& face : faces.faces) {
    out.push_back(face.vertex_indices[0]);
    out.push_back(face.vertex_indices[1]);
    out.push_back(face.vertex_indices[2]);
    out.push_back(0);
  }
  out.push_back(-1);

  return out;
}

CollideFragMeshDataArray pack_collide_frags(const std::vector<collide::CollideFrag>& frag_data) {
  Timer pack_timer;
  CollideFragMeshDataArray result;
  PatMap pat_map;

  size_t total_pack_bytes = 0;
  lg::info("Packing {} fragments", frag_data.size());

  for (auto& frag_in : frag_data) {
    auto& frag_out = result.packed_frag_data.emplace_back();
    auto indexed = dedup_frag_mesh(frag_in, &pat_map);
    // first part of packed_data is the u16 vertex data:
    frag_out.vertex_count = indexed.vertices_u16.vertex.size();
    if (frag_out.vertex_count > 128) {
      lg::print("frag with too many vertices: {} had {} tris\n", frag_out.vertex_count,
                frag_in.faces.size());
      lg::error("SHOULD CRASH\n");
    }
    // the
    frag_out.packed_data.resize(sizeof(u16) * frag_out.vertex_count * 3);
    memcpy(frag_out.packed_data.data(), indexed.vertices_u16.vertex.data(),
           frag_out.packed_data.size());
    // align to 16-bytes
    while (frag_out.packed_data.size() & 0xf) {
      frag_out.packed_data.push_back(0);
    }
    // remember where
    frag_out.vertex_data_qwc = frag_out.packed_data.size() / 16;

    // up next, the strip table
    auto strip = make_dumb_strip_table(indexed);
    frag_out.packed_data.insert(frag_out.packed_data.end(), strip.begin(), strip.end());
    frag_out.strip_data_len = strip.size();
    ASSERT(frag_out.strip_data_len < UINT16_MAX);  // probably in big trouble in here.

    // pat table
    for (auto& face : indexed.faces) {
      frag_out.packed_data.push_back(face.pat_idx);
    }

    // align to 16-bytes so total_qwc works.
    while (frag_out.packed_data.size() & 0xf) {
      frag_out.packed_data.push_back(0);
    }
    // gonna guess here:
    frag_out.poly_count = indexed.faces.size();
    frag_out.total_qwc = frag_out.packed_data.size() / 16;
    ASSERT(frag_out.total_qwc <= 128);
    frag_out.base_trans_xyz_s32 = indexed.vertices_u16.base;
    frag_out.bsphere = frag_in.bsphere;
    total_pack_bytes += frag_out.packed_data.size();
  }

  result.pats = pat_map.pats;
  lg::info("Collide pack used {} unique pats", result.pats.size());
  lg::info("Total packed data size: {} kB, took {:.2f} ms", total_pack_bytes / 1024,
           pack_timer.getMs());
  return result;
}

/*
(deftype collide-frag-mesh (basic)
  ((packed-data     uint32         :offset-assert 4)  <- ptr
   (pat-array       uint32         :offset-assert 8)  <- ptr
   (strip-data-len  uint16         :offset-assert 12)
   (poly-count      uint16         :offset-assert 14)
   (base-trans      vector :inline :offset-assert 16)
   ;; these go in the w of the vector above.
   (vertex-count    uint8          :offset 28)        // done!
   (vertex-data-qwc uint8          :offset 29)        // done!
   (total-qwc       uint8          :offset 30)
   (unused          uint8          :offset 31)
   )
  :method-count-assert 9
  :size-assert         #x20
  :flag-assert         #x900000020
  )
 */

// packed_data:
//  (u16x3) per vertex, packed float vtx format.
//
