#pragma once

#include <vector>

#include "common/common_types.h"
#include "common/math/Vector.h"

#include "goalc/build_level/collide/common/collide_common.h"

class DataObjectGenerator;
namespace jak3 {
// High-level collision system idea:
// Each level has a single collide-hash object storing all collision data.
// The mesh is divided into "fragments". Each fragment is made up of triangles.
// There's a two-level lookup: if you want to find all triangles in a box, you must first find all
// the fragments that intersect the box, then find all the triangles in those fragments that
// intersect the box.
// Each fragment has a bounding box. All triangles inside that fragment fit inside the bounding box.

/*!
 * Vertex in the collide mesh. This is stored as an offset from the bottom corner of the bounding
 * box. This is scaled by 16. (a "1" stored here means a distance of 16.f, or 16/4096 of in-game
 * meter.)
 */
struct CollideFragmentVertex {
  u16 position[3];
};

/*!
 * Polygon in the collide mesh. This is a reference to three vertices in the vertex array, and a
 * "pat" (polygon attributes?) in the pat array.
 */
struct CollideFragmentPoly {
  u8 vertex_index[3];
  u8 pat_index;
};

/*!
 * The Collide Fragment is divided into a 3D grid. Each cell in the grid has a "bucket" which
 * collects a list of all polygons that intersect the cell. The bucket stores a reference to values
 * in the index list, which are polygon indices.
 */
struct CollideBucket {
  s16 index;
  s16 count;
};

struct CollideFragment {
  std::vector<jak3::PatSurface> pat_array;

  // per-cell references to the index list
  std::vector<CollideBucket> buckets;

  // references to polygons
  std::vector<u8> index_array;

  // references to vertices/pats
  std::vector<CollideFragmentPoly> poly_array;

  std::vector<CollideFragmentVertex> vert_array;

  // others

  // the x/y/z sizes of a grid cell
  math::Vector3f grid_step;

  // inverse of grid step
  math::Vector3f axis_scale;

  // the corners of our bounding box
  math::Vector3f bbox_min_corner;
  math::Vector3f bbox_max_corner;
  math::Vector4f bsphere;
  math::Vector<s32, 3> bbox_min_corner_i;
  math::Vector<s32, 3> bbox_max_corner_i;

  // the number of cells in the grid along the x/y/z axis
  u32 dimension_array[3] = {0, 0, 0};
};

/*
 ((num-ids         uint16                 :offset 4)
(id-count        uint16                 :offset 6)
(num-buckets     uint32                 :offset 8)
(qwc-id-bits     uint32                 :offset 12)
(grid-step       vector         :inline :offset 16)
(bbox            bounding-box   :inline :offset-assert 32)
(bbox4w          bounding-box4w :inline :offset-assert 64)
(axis-scale      vector         :inline :offset 48)
(avg-extents     vector         :inline :offset 64)
(bucket-array    uint32                 :offset 44)
(item-array      (inline-array collide-hash-item)                 :offset 60 :score 1)
(dimension-array uint32         3       :offset 76) ;; ?
(num-items       uint32                 :offset 92)
 */

struct CollideHash {
  // if you have a bit for each ID in the item list, how many quadwords (128-byte word) is it?
  u32 qwc_id_bits = 0;

  // this is similar to the use in CollideHashFragment, but this points to entries in the .
  std::vector<CollideBucket> buckets;

  // buckets point to this array, which points to the fragments below
  std::vector<u32> index_array;

  // the actual fragments
  std::vector<CollideFragment> fragments;

  // all these have the same meaning as in CollideFragment and define the grid.
  math::Vector3f grid_step;
  math::Vector3f axis_scale;
  math::Vector3f bbox_min_corner;
  math::Vector<s32, 3> bbox_min_corner_i;
  math::Vector<s32, 3> bbox_max_corner_i;
  u32 dimension_array[3] = {0, 0, 0};
};

CollideHash construct_collide_hash(const std::vector<jak1::CollideFace>& tris);
CollideHash construct_collide_hash(const std::vector<jak3::CollideFace>& tris);

size_t add_to_object_file(const CollideHash& hash, DataObjectGenerator& gen);
}  // namespace jak3