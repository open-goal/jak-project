#include "collide.h"

#include <algorithm>
#include <map>
#include <unordered_map>
#include <unordered_set>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "goalc/data_compiler/DataObjectGenerator.h"

namespace jak2 {
/*!
 * An axis-aligned bounding box
 */
struct BoundingBox {
  math::Vector3f min = math::Vector3f::zero();
  math::Vector3f max = math::Vector3f::zero();
};

/*!
 * See if "axis" is a separating axis for a bounding-box to triangle intersection test.
 * The bounding box is centered at the origin.
 * Return true if the axis is a separating axis.
 */
bool separating_axis_test(const math::Vector3f& bbox_half_side_length,
                          const math::Vector3f& axis,
                          const math::Vector3f& a,
                          const math::Vector3f& b,
                          const math::Vector3f& c) {
  // project triangle to axis
  const float pa = axis.dot(a);
  const float pb = axis.dot(b);
  const float pc = axis.dot(c);

  // project box to axis.
  const float pbox_plus = std::abs(axis[0] * bbox_half_side_length[0]) +
                          std::abs(axis[1] * bbox_half_side_length[1]) +
                          std::abs(axis[2] * bbox_half_side_length[2]);
  const float pbox_minus = -pbox_plus;

  const float ptri_max = std::max(std::max(pa, pb), pc);
  const float ptri_min = std::min(std::min(pa, pb), pc);

  if (ptri_max < pbox_minus) {
    return true;
  }

  if (ptri_min > pbox_plus) {
    return true;
  }

  // there must be overlap.
  return false;
}

/*!
 * Check to see if a triangle intersects an axis-aligned box.
 */
bool triangle_bounding_box(const BoundingBox& bbox_w,
                           const math::Vector3f& a_w,
                           const math::Vector3f& b_w,
                           const math::Vector3f& c_w) {
  // first, translate everything so the center of the bounding box is at the origin
  const math::Vector3f box_center = (bbox_w.max + bbox_w.min) / 2.f;

  const math::Vector3f half_side_length = bbox_w.max - box_center;
  const math::Vector3f a = a_w - box_center;
  const math::Vector3f b = b_w - box_center;
  const math::Vector3f c = c_w - box_center;

  // the separating axis says that if two convex shapes don't intersect, you can project them onto a
  // separating axis (line) and their projections don't overlap. This axis is either a face normal,
  // or a cross-product of edges from each shape.

  // To check intersection, we'll check each possible separating axis - if any are valid, then the
  // shapes don't intersect.

  // First, check the face normals of the box. This check is special-cased for speed - most
  // calls to this function will not have intersection, one of these will be a valid separating
  // axis.

  // find the elementwise min/max of triangle vertices
  const math::Vector3f tri_min = a.min(b.min(c));
  const math::Vector3f tri_max = a.max(b.max(c));

  // check face normals of the box
  for (int axis = 0; axis < 3; axis++) {
    if (tri_max[axis] < -half_side_length[axis]) {
      return false;
    }
    if (tri_min[axis] > half_side_length[axis]) {
      return false;
    }
  }

  // check the face normal of the tri
  const math::Vector3f tri_normal = (b - a).cross(c - a);
  if (separating_axis_test(half_side_length, tri_normal, a, b, c)) {
    return false;
  }

  // all three edges of the triangle
  const math::Vector3f tri_edges[3] = {
      a - b,
      a - c,
      c - b,
  };

  // check each triangle edge
  for (auto tri_edge : tri_edges) {
    // against each box edge
    for (int box_axis = 0; box_axis < 3; box_axis++) {
      const math::Vector3f axis = math::Vector3f::unit(box_axis).cross(tri_edge);
      if (separating_axis_test(half_side_length, axis, a, b, c)) {
        return false;
      }
    }
  }

  // all possible separating axes failed, there is intersection.
  return true;
}

bool bounding_box_bounding_box(const BoundingBox& a, const BoundingBox& b) {
  for (int i = 0; i < 3; i++) {
    if (a.min[i] > b.max[i]) {
      return false;
    }
    if (a.max[i] < b.min[i]) {
      return false;
    }
  }
  return true;
}

/*!
 * Convert jak1-format PatSurface to Jak 2.
 */
jak2::PatSurface jak2_pat(jak1::PatSurface jak1) {
  jak2::PatSurface result;

  switch (jak1.get_mode()) {
    case jak1::PatSurface::Mode::GROUND:
      result.set_mode(jak2::PatSurface::Mode::GROUND);
      break;
    case jak1::PatSurface::Mode::WALL:
      result.set_mode(jak2::PatSurface::Mode::WALL);
      break;
    case jak1::PatSurface::Mode::OBSTACLE:
      result.set_mode(jak2::PatSurface::Mode::OBSTACLE);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  switch (jak1.get_material()) {
    case jak1::PatSurface::Material::STONE:
      result.set_material(jak2::PatSurface::Material::STONE);
      break;
    case jak1::PatSurface::Material::ICE:
      result.set_material(jak2::PatSurface::Material::ICE);
      break;
    case jak1::PatSurface::Material::QUICKSAND:
      result.set_material(jak2::PatSurface::Material::QUICKSAND);
      break;
    case jak1::PatSurface::Material::WATERBOTTOM:
      result.set_material(jak2::PatSurface::Material::WATERBOTTOM);
      break;
    case jak1::PatSurface::Material::TAR:
      result.set_material(jak2::PatSurface::Material::TAR);
      break;
    case jak1::PatSurface::Material::SAND:
      result.set_material(jak2::PatSurface::Material::SAND);
      break;
    case jak1::PatSurface::Material::WOOD:
      result.set_material(jak2::PatSurface::Material::WOOD);
      break;
    case jak1::PatSurface::Material::GRASS:
      result.set_material(jak2::PatSurface::Material::GRASS);
      break;
    case jak1::PatSurface::Material::PCMETAL:
      result.set_material(jak2::PatSurface::Material::PCMETAL);
      break;
    case jak1::PatSurface::Material::SNOW:
      result.set_material(jak2::PatSurface::Material::SNOW);
      break;
    case jak1::PatSurface::Material::DEEPSNOW:
      result.set_material(jak2::PatSurface::Material::DEEPSNOW);
      break;
    case jak1::PatSurface::Material::HOTCOALS:
      result.set_material(jak2::PatSurface::Material::HOTCOALS);
      break;
    case jak1::PatSurface::Material::LAVA:
      result.set_material(jak2::PatSurface::Material::LAVA);
      break;
    case jak1::PatSurface::Material::CRWOOD:
      result.set_material(jak2::PatSurface::Material::CRWOOD);
      break;
    case jak1::PatSurface::Material::GRAVEL:
      result.set_material(jak2::PatSurface::Material::GRAVEL);
      break;
    case jak1::PatSurface::Material::DIRT:
      result.set_material(jak2::PatSurface::Material::DIRT);
      break;
    case jak1::PatSurface::Material::METAL:
      result.set_material(jak2::PatSurface::Material::METAL);
      break;
    case jak1::PatSurface::Material::STRAW:
      result.set_material(jak2::PatSurface::Material::STRAW);
      break;
    case jak1::PatSurface::Material::TUBE:
      result.set_material(jak2::PatSurface::Material::TUBE);
      break;
    case jak1::PatSurface::Material::SWAMP:
      result.set_material(jak2::PatSurface::Material::SWAMP);
      break;
    case jak1::PatSurface::Material::STOPPROJ:
      result.set_material(jak2::PatSurface::Material::STOPPROJ);
      break;
    case jak1::PatSurface::Material::ROTATE:
      result.set_material(jak2::PatSurface::Material::ROTATE);
      break;
    case jak1::PatSurface::Material::NEUTRAL:
      result.set_material(jak2::PatSurface::Material::NEUTRAL);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  switch (jak1.get_event()) {
    case jak1::PatSurface::Event::NONE:
      result.set_event(jak2::PatSurface::Event::NONE);
      break;
    case jak1::PatSurface::Event::DEADLY:
      result.set_event(jak2::PatSurface::Event::DEADLY);
      break;
    case jak1::PatSurface::Event::ENDLESSFALL:
      result.set_event(jak2::PatSurface::Event::ENDLESSFALL);
      break;
    case jak1::PatSurface::Event::BURN:
      result.set_event(jak2::PatSurface::Event::BURN);
      break;
    case jak1::PatSurface::Event::DEADLYUP:
      result.set_event(jak2::PatSurface::Event::DEADLYUP);
      break;
    case jak1::PatSurface::Event::BURNUP:
      result.set_event(jak2::PatSurface::Event::BURNUP);
      break;
    case jak1::PatSurface::Event::MELT:
      result.set_event(jak2::PatSurface::Event::MELT);
      break;
    default:
      ASSERT_NOT_REACHED();
  }

  result.set_noentity(jak1.get_noentity());
  result.set_nocamera(jak1.get_nocamera());

  result.set_noedge(jak1.get_noedge());
  result.set_nolineofsight(jak1.get_nolineofsight());

  return result;
}

/*!
 * Construct a collide hash from a jak1 format mesh by converting to jak 2.
 */
CollideHash construct_collide_hash(const std::vector<jak1::CollideFace>& tris) {
  std::vector<jak2::CollideFace> jak2_tris;
  jak2_tris.reserve(tris.size());

  for (const auto& tri : tris) {
    auto& new_tri = jak2_tris.emplace_back();
    for (int i = 0; i < 3; i++) {
      new_tri.v[i] = tri.v[i];
      new_tri.pat = jak2_pat(tri.pat);
    }
  }

  return construct_collide_hash(jak2_tris);
}

/*!
 * Utility to build a bounding box.
 * If no points are added, the box is set to 0.
 */
struct BBoxBuilder {
  bool added_one = false;
  BoundingBox box;

  // modify box to include this point.
  void add_pt(const math::Vector3f& pt) {
    if (added_one) {
      box.min.min_in_place(pt);
      box.max.max_in_place(pt);
    } else {
      box.min = pt;
      box.max = pt;
    }
    added_one = true;
  }

  // modify box to include this tri.
  void add_tri(const jak2::CollideFace& tri) {
    for (const auto& v : tri.v) {
      add_pt(v);
    }
  }

  void add_box(const BoundingBox& box) {
    add_pt(box.min);
    add_pt(box.max);
  }
};

/*!
 * Given two bounding boxes, compute the volume of their intersection.
 */
float overlap_volume(const BoundingBox& a, const BoundingBox& b) {
  BoundingBox intersection;
  for (int i = 0; i < 3; i++) {
    intersection.min[i] = std::max(a.min[i], b.min[i]);
    intersection.max[i] = std::min(a.max[i], b.max[i]);
  }
  const math::Vector3f size = intersection.max - intersection.min;
  float ret = 1.f;
  for (int i = 0; i < 3; i++) {
    if (size[i] <= 0) {
      return 0;
    }
    ret *= size[i];
  }
  return ret;
}

/*!
 * A portion of a mesh, used in the fragment_mesh function.
 */
struct Frag {
  std::vector<s32> tri_indices;
};

/*!
 * Statistics about a Frag, used for a few steps below.
 */
struct FragStats {
  BoundingBox bbox;
  math::Vector3f average_vertex_position;
  math::Vector3f median_vertex_position;
};

/*!
 * Find bounding box and average position for the triangles selected by indices.
 */
FragStats compute_frag_stats(const std::vector<jak2::CollideFace>& tris,
                             const std::vector<s32>& indices) {
  ASSERT(!tris.empty());
  ASSERT(!indices.empty());

  const float inv_vert_count = 1.f / (indices.size() * 3);

  FragStats ret;
  BBoxBuilder bbox;
  ret.average_vertex_position.set_zero();

  for (auto idx : indices) {
    for (const auto& vtx : tris[idx].v) {
      bbox.add_pt(vtx);
      ret.average_vertex_position += vtx * inv_vert_count;
    }
  }

  for (int i = 0; i < 3; i++) {
    std::vector<float> vx;
    vx.reserve(tris.size() * 3);
    for (auto idx : indices) {
      for (const auto& vtx : tris[idx].v) {
        vx.push_back(vtx[i]);
      }
    }
    std::sort(vx.begin(), vx.end());
    ret.median_vertex_position[i] = vx[vx.size() / 2];
  }

  ret.bbox = bbox.box;
  return ret;
}

struct VectorHash {
  size_t operator()(const math::Vector3f& in) const {
    return std::hash<float>()(in.x()) ^ std::hash<float>()(in.y()) ^ std::hash<float>()(in.z());
  }
};

struct CVertexHash {
  size_t operator()(const math::Vector<u16, 3>& in) const {
    return std::hash<u16>()(in.x()) ^ std::hash<u16>()(in.y()) ^ std::hash<u16>()(in.z());
  }
};

/*!
 * How many unique vertices are there in this frag?
 * (currently using float equality, however, a smarter version could look at quantized vertices)
 */
int unique_vertex_count(const Frag& frag, const std::vector<jak2::CollideFace>& tris) {
  std::unordered_set<math::Vector3f, VectorHash> vmap;
  for (auto i : frag.tri_indices) {
    for (const auto& v : tris[i].v) {
      vmap.insert(v);
    }
  }
  return (int)vmap.size();
}

/*!
 * Is this a frag that we can use in the game?
 */
bool frag_is_valid_for_packing(const Frag& frag,
                               const FragStats& stats,
                               const std::vector<jak2::CollideFace>& tris) {
  if (frag.tri_indices.size() >= UINT8_MAX) {
    // the fragment has too many triangles. I think this can actually be UINT8_MAX and we
    // just put 0 as the size. However, this is confusing so let's just make the max 1 less
    // for now.
    return false;
  }

  // there is a limit to the size of a fragment:
  // the -4096 removes 1 meter from the end, just to make sure that order-of-operations rounding
  // differences doesn't move a vertex outside the grid
  const float kMaxFragSize = UINT16_MAX * 16 - 4096;
  for (int i = 0; i < 3; i++) {
    if (stats.bbox.max[i] - stats.bbox.min[i] >= kMaxFragSize) {
      return false;
    }
  }

  // there is a limit to the number of unique vertices
  if (unique_vertex_count(frag, tris) >= UINT8_MAX) {
    return false;
  }

  return true;
}

/*!
 * A way to split the fragment along a plane
 */
struct FragSplit {
  // a plane that intersects the specified axis at the value. (and is normal to this axis)
  int axis = 0;
  float value = 0;
};

/*!
 * Info about a split
 */
struct SplitStats {
  // how many tris on each side
  int tri_count[2] = {0, 0};

  // the bounding box of those tris. only valid if nonzero tris.
  BoundingBox bboxes[2];

  float overlap_volume = 0;
  float imbalance = 0;
  bool had_zero = false;
};

SplitStats compute_split_stats(const Frag& frag,
                               const std::vector<jak2::CollideFace>& tris,
                               const FragSplit& split) {
  SplitStats stats;
  BBoxBuilder bbox[2];

  for (auto i : frag.tri_indices) {
    const auto& tri = tris[i];
    const math::Vector3f average_pt = (tri.v[0] + tri.v[1] + tri.v[2]) / 3.f;
    const int out_bin = (average_pt[split.axis] > split.value) ? 1 : 0;
    bbox[out_bin].add_tri(tri);
    stats.tri_count[out_bin]++;
  }
  stats.bboxes[0] = bbox[0].box;
  stats.bboxes[1] = bbox[1].box;

  if (stats.tri_count[0] && stats.tri_count[1]) {
    stats.overlap_volume = overlap_volume(stats.bboxes[0], stats.bboxes[1]);
    float max_count = std::max(stats.tri_count[1], stats.tri_count[0]);
    float min_count = std::min(stats.tri_count[1], stats.tri_count[0]);
    stats.imbalance = max_count / min_count;
    stats.had_zero = false;
  } else {
    stats.overlap_volume = 0;
    stats.imbalance = 0;
    stats.had_zero = true;
  }
  return stats;
}

int idx_of_max(float a, float b, float c) {
  if (a > b) {
    if (a > c) {
      return 0;
    } else {
      // a > b, c > a.
      return 2;
    }
  } else {
    if (b > c) {
      return 1;
    } else {
      return 2;
    }
  }
}

FragSplit pick_best_frag_split(const Frag& frag,
                               const FragStats& stats,
                               const std::vector<jak2::CollideFace>& tris) {
  // this is the tricky part.

  // I think the most important thing about splitting is that we should try to minimize overlapping
  // fragments in the final mesh. Overlapping fragments means that we'll need more space for
  // buckets, and the engine will need to check more fragments.

  // Based on what I learned with Jak 1, we also want to avoid:
  // - fragments with bad (large) aspect ratio. Although the Jak 2 code is likely _much_ better at
  //   this case because it uses a box instead of a sphere, I think that we'll struggle to split
  //   up these fragments at the later levels.

  math::Vector3f box_size = stats.bbox.max - stats.bbox.min;
  float min_box_size = box_size[0];
  float max_box_size = box_size[0];
  int max_idx = 0;
  for (int i = 0; i < 3; i++) {
    if (box_size[i] > max_box_size) {
      max_idx = i;
      max_box_size = box_size[i];
    }
    min_box_size = std::min(box_size[i], min_box_size);
  }

  const float aspect = max_box_size / min_box_size;

  FragSplit splits[3];
  SplitStats split_stats[3];

  for (int i = 0; i < 3; i++) {
    splits[i].axis = i;
    splits[i].value = stats.average_vertex_position[i];
    split_stats[i] = compute_split_stats(frag, tris, splits[i]);
  }

  if (aspect > 25) {
    if (split_stats[max_idx].imbalance < 4) {
      printf(
          "pick best frag split splitting a frag of size %d due to bad aspect (%f), with imbalance "
          "%f\n",
          (int)frag.tri_indices.size(), aspect, split_stats[max_idx].imbalance);
      return splits[max_idx];
    } else {
      printf(
          "weird: there's a bad aspect frag (%f, %f), but splitting along the worst axis causes "
          "imbalance %f.\n",
          max_box_size / 4096.f, min_box_size / 4096.f, split_stats[max_idx].imbalance);
    }
  }

  float scores[3];
  for (int i = 0; i < 3; i++) {
    if (split_stats[i].had_zero) {
      scores[i] = -std::numeric_limits<float>::max();
    } else {
      scores[i] = -split_stats[i].overlap_volume;
    }
  }

  return splits[idx_of_max(scores[0], scores[1], scores[2])];
}

Frag add_all_to_frag(const std::vector<jak2::CollideFace>& tris) {
  ASSERT(!tris.empty());

  Frag ret;
  ret.tri_indices.reserve(tris.size());
  for (size_t i = 0; i < tris.size(); i++) {
    ret.tri_indices.push_back(i);
  }
  return ret;
}

void split_frag(const Frag& in,
                const FragSplit& split,
                const std::vector<jak2::CollideFace>& tris,
                Frag* out_a,
                Frag* out_b) {
  for (auto i : in.tri_indices) {
    const auto& tri = tris[i];
    const math::Vector3f average_pt = (tri.v[0] + tri.v[1] + tri.v[2]) / 3.f;
    if (average_pt[split.axis] > split.value) {
      out_a->tri_indices.push_back(i);
    } else {
      out_b->tri_indices.push_back(i);
    }
  }
}

std::vector<Frag> fragment_mesh(const std::vector<jak2::CollideFace>& tris) {
  struct FragAndStats {
    Frag f;
    FragStats s;
  };

  auto initial_frag = add_all_to_frag(tris);
  auto initial_stats = compute_frag_stats(tris, initial_frag.tri_indices);
  if (frag_is_valid_for_packing(initial_frag, initial_stats, tris)) {
    printf("initial is good!\n");
    printf("%s\n%s\n\n", initial_stats.bbox.min.to_string_aligned().c_str(),
           initial_stats.bbox.max.to_string_aligned().c_str());
    return {initial_frag};
  }

  // split up all "too big" frags until they are good.
  std::vector<FragAndStats> too_big_frags = {{initial_frag, initial_stats}};
  std::vector<Frag> good_frags;

  while (!too_big_frags.empty()) {
    printf("sizes %zu %zu\n", too_big_frags.size(), good_frags.size());
    auto& back = too_big_frags.back();

    // split it!
    FragAndStats ab[2];
    auto split = pick_best_frag_split(back.f, back.s, tris);
    split_frag(back.f, split, tris, &ab[0].f, &ab[1].f);

    too_big_frags.pop_back();  // invalidate back.

    // check if split frags are good or not.
    for (auto& fs : ab) {
      fs.s = compute_frag_stats(tris, fs.f.tri_indices);
      if (frag_is_valid_for_packing(fs.f, fs.s, tris)) {
        good_frags.push_back(std::move(fs.f));
      } else {
        too_big_frags.push_back(std::move(fs));
      }
    }
  }
  return good_frags;
}

struct VectorIntHash {
  size_t operator()(const std::vector<int>& in) const {
    size_t ret = 0;
    for (size_t i = 0; i < in.size(); i++) {
      ret ^= std::hash<size_t>()(i ^ in[i]);
    }
    return ret;
  }
};

CollideHash build_grid_for_main_hash(std::vector<CollideFragment>&& frags) {
  lg::info("Creating main hash");
  CollideHash result;
  BBoxBuilder bbox;
  for (const auto& frag : frags) {
    bbox.add_pt(frag.bbox_min_corner);
    bbox.add_pt(frag.bbox_max_corner);
  }

  const math::Vector3f box_size = bbox.box.max - bbox.box.min;

  // grid the box. It _looks_ like the village1 level just picks dims that get you closest to 10000
  // for the cell size.
  constexpr float kTargetCellSize = 30000;

  int grid_dimension[3] = {(int)(box_size[0] / kTargetCellSize),
                           (int)(box_size[1] / kTargetCellSize),
                           (int)(box_size[2] / kTargetCellSize)};
  for (auto& x : grid_dimension) {
    if (x >= UINT8_MAX) {
      x = UINT8_MAX;
    }
  }
  lg::info("Size is {}x{}x{} (total {})\n", grid_dimension[0], grid_dimension[1], grid_dimension[2],
           grid_dimension[0] * grid_dimension[1] * grid_dimension[2]);
  const math::Vector3f grid_cell_size(box_size[0] / grid_dimension[0],
                                      box_size[1] / grid_dimension[1],
                                      box_size[2] / grid_dimension[2]);

  std::vector<std::vector<int>> frags_in_cells;

  // debug
  std::vector<bool> debug_found_flags(frags.size(), false);
  int debug_intersect_count = 0;

  // yzx order to match game
  for (int yi = 0; yi < grid_dimension[1]; yi++) {
    for (int zi = 0; zi < grid_dimension[2]; zi++) {
      for (int xi = 0; xi < grid_dimension[0]; xi++) {
        auto& cell_list = frags_in_cells.emplace_back();

        BoundingBox cell;
        cell.min =
            math::Vector3f(xi * grid_cell_size[0], yi * grid_cell_size[1], zi * grid_cell_size[2]) +
            bbox.box.min;
        cell.max = cell.min + grid_cell_size;

        for (size_t fi = 0; fi < frags.size(); fi++) {
          const auto& frag = frags[fi];
          if (bounding_box_bounding_box(cell, {frag.bbox_min_corner, frag.bbox_max_corner})) {
            debug_found_flags[fi] = true;
            debug_intersect_count++;
            cell_list.push_back(fi);
          }
        }

        std::sort(cell_list.begin(), cell_list.end());
      };
    }
  }

  lg::info("Index data size is {}, deduplicating", debug_intersect_count);

  std::unordered_map<std::vector<int>, size_t, VectorIntHash> index_map;
  for (const auto& cell_list : frags_in_cells) {
    auto& bucket = result.buckets.emplace_back();
    bucket.count = cell_list.size();

    const auto& it = index_map.find(cell_list);
    if (it == index_map.end()) {
      bucket.index = result.index_array.size();
      index_map[cell_list] = bucket.index;
      for (auto x : cell_list) {
        result.index_array.push_back(x);
      }
    } else {
      bucket.index = it->second;
    }
  }

  lg::info("Index array size is {} in the end", result.index_array.size());
  if (result.index_array.size() > UINT16_MAX) {
    printf("index array is too big: %d\n", (int)result.index_array.size());
    ASSERT_NOT_REACHED();
  }

  int unique_found = 0;
  for (auto x : debug_found_flags) {
    if (x) {
      unique_found++;
    }
  }

  printf("frag find counts: %d %d %d\n", unique_found, (int)debug_found_flags.size(),
         debug_intersect_count);
  if (unique_found != (int)debug_found_flags.size()) {
    printf(" --- !!! %d frags disappeared\n", (int)debug_found_flags.size() - unique_found);
  }

  //  for (auto& list : frags_in_cells) {
  //    auto& bucket = result.buckets.emplace_back();
  //    bucket.index = result.index_array.size();
  //    bucket.count = list.size();
  //    for (auto x : list) {
  //      result.index_array.push_back(x);
  //    }
  //  }

  result.grid_step = grid_cell_size;
  result.axis_scale =
      math::Vector3f(1.f / grid_cell_size[0], 1.f / grid_cell_size[1], 1.f / grid_cell_size[2]);
  result.bbox_min_corner = bbox.box.min;
  result.bbox_min_corner_i = bbox.box.min.cast<s32>();
  result.bbox_max_corner_i = bbox.box.max.cast<s32>();
  result.qwc_id_bits = (frags.size() + 127) / 128;
  result.fragments = std::move(frags);

  for (int i = 0; i < 3; i++) {
    result.dimension_array[i] = grid_dimension[i];
  }
  return result;
}

/*!
 * Build a CollideFragment by "hashing" a list of triangles
 */
CollideFragment build_grid_for_frag(const std::vector<jak2::CollideFace>& tris, const Frag& frag) {
  CollideFragment result;

  // find the bounding box
  BBoxBuilder bbox;
  for (auto i : frag.tri_indices) {
    bbox.add_tri(tris[i]);
  }

  // build vertex, poly, pat tables:
  std::vector<math::Vector<u16, 3>> vertices;
  std::vector<CollideFragmentPoly> polys;
  std::vector<jak2::PatSurface> pats;

  std::unordered_map<math::Vector<u16, 3>, size_t, CVertexHash> vertex_to_vertex_array_index;
  std::unordered_map<u32, size_t> pat_to_pat_array_index;

  for (auto ti : frag.tri_indices) {
    const auto& input_tri = tris[ti];
    auto& poly = polys.emplace_back();

    // add pat:
    auto pat_it = pat_to_pat_array_index.find(input_tri.pat.val);
    if (pat_it == pat_to_pat_array_index.end()) {
      pat_to_pat_array_index[input_tri.pat.val] = pats.size();
      ASSERT(pats.size() < UINT8_MAX);
      poly.pat_index = pats.size();
      pats.push_back(input_tri.pat);
    } else {
      poly.pat_index = pat_it->second;
    }

    // add vertices
    for (int i = 0; i < 3; i++) {
      const math::Vector3f vert_f = (input_tri.v[i] - bbox.box.min) / 16.f;
      for (int j = 0; j < 3; j++) {
        ASSERT(vert_f[j] >= 0 && vert_f[j] < UINT16_MAX);
      }
      const auto vert_i = vert_f.cast<u16>();
      const auto& it = vertex_to_vertex_array_index.find(vert_i);
      if (it == vertex_to_vertex_array_index.end()) {
        vertex_to_vertex_array_index[vert_i] = vertex_to_vertex_array_index.size();
        ASSERT(vertex_to_vertex_array_index.size() < UINT8_MAX);
        poly.vertex_index[i] = vertices.size();
        vertices.push_back(vert_i);
      } else {
        poly.vertex_index[i] = it->second;
      }
    }
  }

  // grid the box. We can have only 256 cells, so we take a 1x1 grid and split it in half 8 times.
  // TODO: there are probably smarter ways to do this.
  math::Vector3f grid_cell_size = bbox.box.max - bbox.box.min;
  int grid_dimension[3] = {1, 1, 1};
  for (int i = 0; i < 8; i++) {
    int split_axis = idx_of_max(grid_cell_size[0], grid_cell_size[1], grid_cell_size[2]);
    grid_dimension[split_axis] *= 2;
    grid_cell_size[split_axis] /= 2;
  }
  ASSERT(grid_dimension[0] * grid_dimension[1] * grid_dimension[2] == 256);

  // per-cell, a list of polys that intersect it.
  std::vector<std::vector<int>> polys_in_cells;

  // debug
  std::vector<bool> debug_found_flags(frag.tri_indices.size(), false);
  int debug_intersect_count = 0;

  // yzx order to match game
  for (int yi = 0; yi < grid_dimension[1]; yi++) {
    for (int zi = 0; zi < grid_dimension[2]; zi++) {
      for (int xi = 0; xi < grid_dimension[0]; xi++) {
        auto& cell_list = polys_in_cells.emplace_back();

        BoundingBox cell;
        cell.min =
            math::Vector3f(xi * grid_cell_size[0], yi * grid_cell_size[1], zi * grid_cell_size[2]) +
            bbox.box.min;
        cell.max = cell.min + grid_cell_size;

        for (size_t ti = 0; ti < frag.tri_indices.size(); ti++) {
          const auto& tri = tris[frag.tri_indices[ti]];
          if (triangle_bounding_box(cell, tri.v[0], tri.v[1], tri.v[2])) {
            debug_found_flags[ti] = true;
            debug_intersect_count++;
            cell_list.push_back(ti);
          }
        }

        std::sort(cell_list.begin(), cell_list.end());
      };
    }
  }

  // TODO: could dedup buckets here.
  int unique_found = 0;
  for (auto x : debug_found_flags) {
    if (x) {
      unique_found++;
    }
  }

  // printf("find counts: %d %d %d\n", unique_found, (int)debug_found_flags.size(),
  // debug_intersect_count);
  ASSERT(debug_intersect_count < INT16_MAX);  // not really sure what to do if this happens...
  if (unique_found != (int)debug_found_flags.size()) {
    printf(" --- !!! %d triangles disappeared\n", (int)debug_found_flags.size() - unique_found);
  }

  result.pat_array = std::move(pats);
  for (auto& list : polys_in_cells) {
    auto& bucket = result.buckets.emplace_back();
    bucket.index = result.index_array.size();
    bucket.count = list.size();
    for (auto x : list) {
      result.index_array.push_back(x);
    }
  }
  result.poly_array = std::move(polys);
  for (auto x : vertices) {
    auto& v = result.vert_array.emplace_back();
    v.position[0] = x.x();
    v.position[1] = x.y();
    v.position[2] = x.z();
  }

  result.grid_step = grid_cell_size;
  result.axis_scale =
      math::Vector3f(1.f / grid_cell_size[0], 1.f / grid_cell_size[1], 1.f / grid_cell_size[2]);
  result.bbox_min_corner = bbox.box.min;
  result.bbox_max_corner = bbox.box.max;
  result.bbox_min_corner_i = bbox.box.min.cast<s32>();
  result.bbox_max_corner_i = bbox.box.max.cast<s32>();

  // bsphere:
  math::Vector3f mid = (result.bbox_max_corner + result.bbox_min_corner) / 2.f;
  math::Vector3f size = (result.bbox_max_corner - result.bbox_min_corner) / 2.f;
  const float radius = size.length();
  result.bsphere = math::Vector4f(mid.x(), mid.y(), mid.z(), radius);

  for (int i = 0; i < 3; i++) {
    result.dimension_array[i] = grid_dimension[i];
  }
  return result;
}

CollideHash construct_collide_hash(const std::vector<jak2::CollideFace>& tris) {
  CollideHash collide_hash;

  std::vector<Frag> frags = fragment_mesh(tris);
  std::vector<CollideFragment> hashed_frags;
  for (auto& frag : frags) {
    hashed_frags.push_back(build_grid_for_frag(tris, frag));
  }

  // hash tris in frags
  // hash frags
  // ??
  return build_grid_for_main_hash(std::move(hashed_frags));
}

size_t add_pod_to_object_file(DataObjectGenerator& gen,
                              const u8* in,
                              size_t size_bytes,
                              size_t align_bytes) {
  const size_t align_words = (align_bytes + 3) / 4;
  gen.align(align_words);
  const size_t ret = gen.current_offset_bytes();

  const size_t full_words = size_bytes / 4;
  size_t bytes = 0;
  for (size_t word = 0; word < full_words; word++) {
    u32 data = 0;
    memcpy(&data, in + 4 * word, 4);
    gen.add_word(data);
    bytes += 4;
  }

  u8 remainder[4] = {0, 0, 0, 0};
  int i = 0;
  while (bytes < size_bytes) {
    remainder[i++] = in[bytes++];
  }
  u32 last;
  memcpy(&last, remainder, 4);
  gen.add_word(last);
  return ret;
}

template <typename T>
size_t add_pod_vector_to_object_file(DataObjectGenerator& gen, const std::vector<T>& data) {
  return add_pod_to_object_file(gen, (const u8*)data.data(), data.size() * sizeof(T), sizeof(T));
}

size_t add_to_object_file(const CollideFragment& frag, DataObjectGenerator& gen) {
  // PAT ARRAY
  static_assert(sizeof(jak2::PatSurface) == sizeof(u32));
  auto pat_array = add_pod_vector_to_object_file(gen, frag.pat_array);

  // Bucket ARRAY
  static_assert(sizeof(CollideBucket) == sizeof(u32));
  auto bucket_array = add_pod_vector_to_object_file(gen, frag.buckets);

  // Poly ARRAY
  static_assert(sizeof(CollideFragmentPoly) == sizeof(u32));
  auto poly_array = add_pod_vector_to_object_file(gen, frag.poly_array);

  // Vert ARRAY
  static_assert(sizeof(CollideFragmentVertex) == 6);
  gen.align(4);
  auto vert_array = add_pod_vector_to_object_file(gen, frag.vert_array);

  // Index ARRAY
  auto index_array = add_pod_vector_to_object_file(gen, frag.index_array);

  // collide-hash-fragment
  gen.align_to_basic();
  gen.add_type_tag("collide-hash-fragment");  // 0
  size_t result = gen.current_offset_bytes();

  //  ((num-buckets     uint16                              :offset 4)
  //   (num-indices     uint16                              :offset 6)
  const u32 bucket_index_word = (((u32)frag.index_array.size()) << 16) | ((u32)frag.buckets.size());
  gen.add_word(bucket_index_word);  // 4

  //   (pat-array       uint32 :offset 8)
  gen.link_word_to_byte(gen.add_word(0), pat_array);  // 8

  //   (bucket-array    uint32                              :offset 12)
  gen.link_word_to_byte(gen.add_word(0), bucket_array);  // 12

  // bsphere of drawable
  gen.add_word_float(frag.bsphere.x());  // 16
  gen.add_word_float(frag.bsphere.y());  // 20
  gen.add_word_float(frag.bsphere.z());  // 24
  gen.add_word_float(frag.bsphere.w());  // 28

  //   (grid-step       vector                      :inline :offset-assert 32)
  gen.add_word_float(frag.grid_step.x());  // 32
  gen.add_word_float(frag.grid_step.y());  // 36
  gen.add_word_float(frag.grid_step.z());  // 40

  //   (dimension-array uint32                      4       :offset 44)
  u32 dim_array = 0;
  dim_array |= (frag.dimension_array[0]);
  dim_array |= ((frag.dimension_array[1]) << 8);
  dim_array |= ((frag.dimension_array[2]) << 16);
  gen.add_word(dim_array);  // 44

  //   (bbox            bounding-box                :inline :offset-assert 48)
  gen.add_word_float(frag.bbox_min_corner.x());  // 48
  gen.add_word_float(frag.bbox_min_corner.y());  // 52
  gen.add_word_float(frag.bbox_min_corner.z());  // 56

  //   (num-verts       uint16                              :offset 60)
  //   (num-polys       uint8                               :offset 62)
  //   (poly-count      uint8                               :offset 63)
  u32 counts_word = 0;
  ASSERT(frag.vert_array.size() < UINT16_MAX);
  counts_word |= (frag.vert_array.size());
  ASSERT(frag.poly_array.size() < UINT8_MAX);
  counts_word |= (frag.poly_array.size() << 16);
  counts_word |= (frag.poly_array.size() << 24);
  gen.add_word(counts_word);  // 60

  //   (axis-scale      vector                      :inline :offset 64)
  gen.add_word_float(frag.axis_scale.x());  // 64
  gen.add_word_float(frag.axis_scale.y());  // 68
  gen.add_word_float(frag.axis_scale.z());  // 72

  //   (poly-array      uint32                              :offset 76)
  gen.link_word_to_byte(gen.add_word(0), poly_array);  // 76

  //   (bbox4w          bounding-box4w              :inline :offset-assert 80)
  gen.add_word(frag.bbox_min_corner_i.x());  // 80
  gen.add_word(frag.bbox_min_corner_i.y());  // 84
  gen.add_word(frag.bbox_min_corner_i.z());  // 88

  //   (vert-array      uint32                              :offset 92)
  gen.link_word_to_byte(gen.add_word(0), vert_array);  // 92

  gen.add_word(frag.bbox_max_corner_i.x());  // 96
  gen.add_word(frag.bbox_max_corner_i.y());  // 100
  gen.add_word(frag.bbox_max_corner_i.z());  // 104

  //   (index-array     uint32                              :offset 108)
  gen.link_word_to_byte(gen.add_word(0), index_array);

  //   (avg-extents     vector                      :inline :offset 80)
  //   (stats           collide-hash-fragment-stats :inline :offset 60)
  return result;
}

size_t add_to_object_file(const CollideHash& hash, DataObjectGenerator& gen) {
  std::vector<size_t> frags;
  for (auto& frag : hash.fragments) {
    frags.push_back(add_to_object_file(frag, gen));
  }

  auto buckets = add_pod_vector_to_object_file(gen, hash.buckets);

  // create the item array.
  auto item_array = gen.current_offset_bytes();
  for (auto& x : hash.index_array) {
    gen.add_word(x);
    gen.link_word_to_byte(gen.add_word(0), frags.at(x));
  }

  gen.align_to_basic();
  gen.add_type_tag("collide-hash");  // 0
  size_t result = gen.current_offset_bytes();

  //((num-ids         uint16                 :offset 4)
  // (id-count        uint16                 :offset 6)
  u32 ids_word = 0;
  ids_word |= hash.fragments.size();
  ids_word |= (hash.fragments.size() << 16);
  gen.add_word(ids_word);  // 4

  // (num-buckets     uint32                 :offset 8)
  gen.add_word(hash.buckets.size());  // 8

  // (qwc-id-bits     uint32                 :offset 12)
  gen.add_word(hash.qwc_id_bits);  // 12

  // (grid-step       vector         :inline :offset 16)
  gen.add_word_float(hash.grid_step.x());  // 16
  gen.add_word_float(hash.grid_step.y());  // 20
  gen.add_word_float(hash.grid_step.z());  // 24
  gen.add_word_float(1.f);                 // 28

  // (bbox            bounding-box   :inline :offset-assert 32)
  gen.add_word_float(hash.bbox_min_corner.x());  // 32
  gen.add_word_float(hash.bbox_min_corner.y());  // 36
  gen.add_word_float(hash.bbox_min_corner.z());  // 40

  // (bucket-array    uint32                 :offset 44)
  gen.link_word_to_byte(gen.add_word(0), buckets);  // 44

  // (axis-scale      vector         :inline :offset 48)
  gen.add_word_float(hash.axis_scale.x());  // 48
  gen.add_word_float(hash.axis_scale.y());  // 52
  gen.add_word_float(hash.axis_scale.z());  // 56

  // (item-array      (inline-array collide-hash-item)                 :offset 60 :score 1)
  gen.link_word_to_byte(gen.add_word(0), item_array);  // 60

  // (bbox4w          bounding-box4w :inline :offset-assert 64)
  gen.add_word(hash.bbox_min_corner_i.x());  // 64
  gen.add_word(hash.bbox_min_corner_i.y());  // 68
  gen.add_word(hash.bbox_min_corner_i.z());  // 72

  // (dimension-array uint32         3       :offset 76) ;; ?
  u32 dim_array = 0;
  dim_array |= (hash.dimension_array[0]);
  dim_array |= ((hash.dimension_array[1]) << 8);
  dim_array |= ((hash.dimension_array[2]) << 16);
  gen.add_word(dim_array);  // 76

  gen.add_word(hash.bbox_max_corner_i.x());  // 80
  gen.add_word(hash.bbox_max_corner_i.y());  // 84
  gen.add_word(hash.bbox_max_corner_i.z());  // 88

  // (num-items       uint32                 :offset 92)
  gen.add_word(hash.index_array.size());

  // (avg-extents     vector         :inline :offset 64)

  return result;
}
}  // namespace jak2