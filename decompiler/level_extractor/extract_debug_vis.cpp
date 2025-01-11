#include "extract_debug_vis.h"

#include "common/log/log.h"
#include "common/math/Vector.h"

namespace {

using Point = math::Vector3d;
using Plane = math::Vector4d;  // Plane(a, b, c, d) -> ax + by + cz = d

Plane flip_plane_normal(const Plane& in) {
  return in * -1;
}

math::Vector3d plane_normal(const Plane& in) {
  return in.xyz();
}

math::Vector4d convert_vector(const level_tools::Vector& in) {
  return math::Vector4d(in.data[0], in.data[1], in.data[2], in.data[3]);
}

double point_plane_check(const Point& pt, const Plane& plane) {
  return pt.x() * plane.x() + pt.y() * plane.y() + pt.z() * plane.z() - plane.w();
}

/*!
 * Find the largest leaf index.
 */
int max_leaf_idx(const std::vector<level_tools::Jak1BspNode>& nodes) {
  int ret = -1;
  for (auto& node : nodes) {
    if (node.back.is_leaf)
      ret = std::max(ret, node.back.index);
    if (node.front.is_leaf)
      ret = std::max(ret, node.front.index);
  }
  return ret;
}

/*!
 * Description of the tree structure of BSP nodes.
 */
struct ParentData {
  std::vector<int> node_parents;  // parent_node = node_parents[child_node]
  std::vector<int> leaf_parents;  // parent_node = leaf_parents[leaf]

  int num_leaves() const { return leaf_parents.size(); }
};

/*!
 * Find the tree structure of BSP nodes from the array, that the BSP nodes are a tree, and there
 * are no unparented/unused slots in either the leaf or node arrays. Things would still work if
 * there were unused slots, but it just wastes memory and makes no sense.
 */
ParentData find_parents(const std::vector<level_tools::Jak1BspNode>& nodes) {
  ParentData result;
  result.node_parents.resize(nodes.size(), -1);
  result.leaf_parents.resize(max_leaf_idx(nodes) + 1, -1);

  // start at the root:
  std::vector<int> to_explore = {0};
  while (!to_explore.empty()) {
    int node_idx = to_explore.back();
    to_explore.pop_back();

    auto& n = nodes.at(node_idx);

    // add parents, and assert it's the first time we've seen the child (since it's a tree)
    // you could imagine mapping two volumes to the same leaf, but it looks like they don't
    if (n.front.is_leaf) {
      ASSERT(result.leaf_parents.at(n.front.index) == -1);
      result.leaf_parents.at(n.front.index) = node_idx;
    } else {
      to_explore.push_back(n.front.index);
      ASSERT(result.node_parents.at(n.front.index) == -1);
      result.node_parents.at(n.front.index) = node_idx;
    }

    if (n.back.is_leaf) {
      ASSERT(result.leaf_parents.at(n.back.index) == -1);
      result.leaf_parents.at(n.back.index) = node_idx;
    } else {
      to_explore.push_back(n.back.index);
      ASSERT(result.node_parents.at(n.back.index) == -1);
      result.node_parents.at(n.back.index) = node_idx;
    }
  }

  // check for unused slots in leaf/node arrays.
  int unparented_leaves = 0;
  int unparented_nodes = 0;
  for (auto np : result.node_parents) {
    if (np == -1)
      unparented_nodes++;
  }
  for (auto lp : result.leaf_parents) {
    if (lp == -1)
      unparented_leaves++;
  }
  ASSERT(unparented_nodes == 1);   // the root is unparented
  ASSERT(unparented_leaves == 0);  // every leaf should be the child of a

  return result;
}

/*!
 * For a given leaf, find the bounding planes. A point (x, y, z) is inside the leaf, if for all
 * planes (a, b, c, d):
 *
 * ax + by + cz - d > 0
 *
 */
std::vector<Plane> planes_for_leaf(int leaf_idx,
                                   const ParentData& tree,
                                   const std::vector<level_tools::Jak1BspNode>& nodes) {
  std::vector<Plane> planes;

  int parent_idx = tree.leaf_parents.at(leaf_idx);
  int child_idx = leaf_idx;

  // each iteration adds the plane from parent_idx, then goes up the tree.
  while (parent_idx != -1) {
    // printf(" p %d\n", parent_idx);
    const auto& node = nodes.at(parent_idx);
    if (node.back.index == child_idx) {
      // if this is the "back" child of the parent, flip the sign of the plane - this child is
      // if the unflipped plane check fails.
      planes.push_back(flip_plane_normal(convert_vector(node.plane)));
    } else if (node.front.index == child_idx) {
      planes.push_back(convert_vector(node.plane));
    } else {
      ASSERT_NOT_REACHED();
    }

    int new_parent = tree.node_parents.at(parent_idx);
    child_idx = parent_idx;
    parent_idx = new_parent;
  }

  return planes;
}

/*!
 * Representation of a convex face as collection of points on a plane. The winding of the points
 * should match the normal. (do we really care about this?)
 */
struct Face {
  std::vector<Point> points;
  Plane plane;
  void verify() const;
  double area() const;
  Point avg_vertex_pos() const;
  void flip_if_needed();
};

void Face::flip_if_needed() {
  ASSERT(points.size() > 1);
  const Point c = avg_vertex_pos();
  const Point ab = points.at(1) - points.at(0);
  const Point ac = c - points.at(0);
  const math::Vector3d abac = ab.cross(ac);
  const double winding_check = abac.dot(plane_normal(plane));

  if (winding_check < 0) {
    std::reverse(points.begin(), points.end());
  }
}
/*!
 * Get the average of all vertices. Because the face is convex, this returns a point in the area.
 */
Point Face::avg_vertex_pos() const {
  Point ret = Point::zero();
  for (auto& p : points) {
    ret += p;
  }
  ret /= double(points.size());
  return ret;
}

/*!
 * Get the area of the face, also checking the winding order.
 */
double Face::area() const {
  // let c be any point inside the face:
  const Point c = avg_vertex_pos();

  double total = 0;

  for (int ai = 0; ai < points.size(); ai++) {
    const int bi = (ai + 1) % points.size();
    const Point ab = points.at(bi) - points.at(ai);
    const Point ac = c - points.at(ai);
    const math::Vector3d abac = ab.cross(ac);
    const double winding_check = abac.dot(plane_normal(plane));
    if (winding_check < 0) {
      lg::die("winding check failed");
    }
    // printf("abac: %f\n", abac.length());
    total += abac.length();
  }
  return total * 0.5;
}

/*!
 * Verify that a face is valid: all points lie on the plane and the winding order is correct.
 */
void Face::verify() const {
  // verify that all points lie on the plane.
  for (const auto& pt : points) {
    const double dist_from_plane = point_plane_check(pt, plane);
    if (std::abs(dist_from_plane) > 200) {
      lg::die("Point not on plane error {} {}", dist_from_plane, plane.to_string_aligned());
    }
  }

  // checks winding:
  const double a = area();
  ASSERT(a > 0);

  // TODO: could check for tangled stuff here?
}

/*!
 * Representation of a volume as a collection of bounding faces.
 */
struct Volume {
  std::vector<Face> faces;
  void verify() const;
  bool check_point_in_volume(const Point& pt) const;
  double surface_area() const;
  double volume() const;
  Point some_internal_point() const;
};

/*!
 * Is pt inside this volume?
 */
bool Volume::check_point_in_volume(const Point& pt) const {
  for (auto& face : faces) {
    if (point_plane_check(pt, face.plane) < 0) {
      return false;
    }
  }
  return true;
}

/*!
 * Get a point inside the volume. Average of face centers now.
 */
Point Volume::some_internal_point() const {
  Point ret = Point::zero();
  for (auto& face : faces) {
    ret += face.avg_vertex_pos();
  }
  return ret /= double(faces.size());
}

/*!
 * Verify all faces in the volume, then verify face orientation.
 */
void Volume::verify() const {
  for (const auto& face : faces) {
    face.verify();
  }

  if (!check_point_in_volume(some_internal_point())) {
    lg::die("pt in volume verify failed");
  }
}

/*!
 * Compute the surface area.
 */
double Volume::surface_area() const {
  double ret = 0;
  for (const auto& face : faces) {
    ret += face.area();
  }
  return ret;
}

/*!
 * Find the volume enclosed.
 */
double Volume::volume() const {
  double ret = 0;
  Point d = some_internal_point();
  for (const auto& face : faces) {
    Point c = face.avg_vertex_pos();
    for (size_t ai = 0; ai < face.points.size(); ai++) {
      size_t bi = (ai + 1) % face.points.size();
      Point a = face.points.at(ai);
      Point b = face.points.at(bi);
      ret += std::abs((a - d).dot((b - d).cross(c - d))) / 6.;
    }
  }
  return ret;
}

enum class FacePlaneResult { ALL_IN, ALL_OUT, SPLIT };

/*!
 * Check a face against a plane to determine if the face is all on one side, all on the other, or in
 * the middle.
 */
FacePlaneResult check_face_plane(const Face& face, const Plane& plane) {
  bool found_in = false;
  bool found_out = false;

  for (auto& pt : face.points) {
    if (point_plane_check(pt, plane) > 0) {
      found_in = true;
    } else {
      found_out = true;
    }
  }

  if (found_in && found_out) {
    return FacePlaneResult::SPLIT;
  } else if (found_in && !found_out) {
    return FacePlaneResult::ALL_IN;
  } else if (found_out && !found_in) {
    return FacePlaneResult::ALL_OUT;
  } else {
    ASSERT_NOT_REACHED();
  }
}

struct ClipFaceResult {
  Face clipped_face;
  Point a, b;  // the new points added to this face.
};

/*!
 * Compute the intersection between a line segment and plane.
 */
std::optional<Point> plane_line_segment_isect(const Plane& plane,
                                              const Point& p0,
                                              const Point& p1,
                                              double* u) {
  // let p = p0 + u * (p1 - p0)
  // if p is on the plane, then dot(p, n) = d
  // dot(p0 + u * (p1 - p0), n) = d
  // dot(p0, n) + u * dot(p1 - p0, n) = d
  // u = (d - dot(p0, n)) / dot(p1 - p0, n)
  const math::Vector3d n = plane.xyz();
  *u = (plane.w() - p0.dot(n)) / (p1 - p0).dot(n);
  if (*u >= 0 && *u <= 1) {
    Point ret = p0 + (p1 - p0) * *u;
    // fmt::print("ret dist: {}, {} {}\n", point_plane_check(ret, plane), point_plane_check(p0,
    // plane),
    //            point_plane_check(p1, plane));
    return ret;
  } else {
    return std::nullopt;
  }
}

/*!
 * Clip a face, returning the new face, and the two vertices of the new face that intersect the
 * clipping plane.
 */
ClipFaceResult clip_face(const Face& face, const Plane& plane) {
  ClipFaceResult result;
  result.clipped_face.plane = face.plane;

  // determine if each point is in the new face.
  std::vector<bool> pt_in;
  for (auto& pt : face.points) {
    pt_in.push_back(point_plane_check(pt, plane) > 0);
  }

  // loop around points on this face, including them in the new face only if they are inside the
  // plane. When the permiter enters and exits the clipping plane, generate new vertices, and store
  // these in the a/b outputs.
  int saw_exit = 0;
  int saw_enter = 0;
  for (size_t i = 0; i < face.points.size(); i++) {
    const auto p0 = face.points.at(i);
    const auto p1 = face.points.at((i + 1) % face.points.size());
    if (pt_in[i]) {
      // if the point is in, just add it.
      result.clipped_face.points.push_back(p0);
      // exit point - need to insert a new vertex here!
      if (!pt_in[(i + 1) % face.points.size()]) {
        saw_exit++;
        double u;
        auto new_pt = plane_line_segment_isect(plane, p0, p1, &u);
        ASSERT(new_pt.has_value());
        // fmt::print("CLIP A: {} {} -> {} (u = {})\n", p0.to_string_aligned(),
        // p1.to_string_aligned(),
        //            new_pt->to_string_aligned(), u);
        result.clipped_face.points.push_back(*new_pt);
        result.a = *new_pt;
      }
    } else {
      if (pt_in[(i + 1) % face.points.size()]) {
        // enter
        saw_enter++;
        double u;
        auto new_pt = plane_line_segment_isect(plane, p0, p1, &u);
        ASSERT(new_pt.has_value());
        // fmt::print("CLIP B: {} {} -> {} (u = {})\n", p0.to_string_aligned(),
        // p1.to_string_aligned(),
        //            new_pt->to_string_aligned(), u);
        result.clipped_face.points.push_back(*new_pt);
        result.b = *new_pt;
      }
    }
  }

  ASSERT(saw_enter == 1);
  ASSERT(saw_exit == 1);
  result.clipped_face.verify();
  return result;
}

std::vector<Point> extract_face_ring(const std::vector<ClipFaceResult>& cfr) {
  std::vector<Point> result;
  ASSERT(cfr.size() > 1);
  std::vector<bool> used(cfr.size(), false);

  // add the first one
  int num_used = 1;
  used[0] = true;
  // result.push_back(cfr[0].a);
  result.push_back(cfr[0].b);

  // for (auto& cf : cfr) {
  //   fmt::print("{} {}\n", cf.a.to_string_aligned(), cf.b.to_string_aligned());
  // }

  // gross N^2 loop to guess at the order of the edges in the new face beacuse I didn't track face
  // connectivity... :(
  // printf("efr: %d %ld\n", num_used, used.size());
  while (num_used < used.size()) {
    const auto& tgt = result.back();
    double best_dist = std::numeric_limits<double>::max();
    size_t best_idx = -1;

    for (size_t i = 0; i < cfr.size(); i++) {
      // printf("checking %ld (%d)\n", i, int(used[i]));
      if (used[i]) {
        continue;
      }
      const double dist = (cfr[i].a - tgt).squared_length();
      if (dist < best_dist) {
        best_dist = dist;
        best_idx = i;
      }
    }
    num_used++;
    used.at(best_idx) = true;
    // printf("PICKED %ld\n", best_idx);
    // ASSERT(best_dist < 4096 * 4096 * 10);  // hmm
    result.push_back(cfr[best_idx].b);
  }

  // 0 b = [-16777216.000 16777216.000 -2211840.000]
  // 3 a = [-16777216.000 16777216.000 -2211840.000] -> [-16777216.000 -16777216.000 -2211840.000]
  // 1 a = [-16777216.000 -16777216.000 -2211840.000] -> [16777216.000 -16777216.000 -2211840.000]
  // 2 a = [16777216.000 -16777216.000 -2211840.000] -> 16777216.000 16777216.000 -2211840.000]

  // fmt::print("last in the result: {}\n", result.back().to_string_aligned());
  // fmt::print("cfr[0]a           : {}\n", cfr[0].a.to_string_aligned());
  // fmt::print("diff              : {}\n", (result.back() - cfr[0].a).to_string_aligned());
  // fmt::print("diff              : {}\n", (result.back() - cfr[0].a).squared_length());

  // ASSERT(4096 * 4096 * 10 > (result.back() - cfr[0].a).squared_length());
  return result;
}

// TODO: paranoid clipping that checks a = b, b = a, area sums.

Volume clip_volume(const Volume& vol, const Plane& plane) {
  Volume new_volume;
  std::vector<ClipFaceResult> split_faces;

  // categorize faces
  for (const auto& face : vol.faces) {
    // printf("running on face\n");
    switch (check_face_plane(face, plane)) {
      case FacePlaneResult::ALL_IN:
        new_volume.faces.push_back(face);
        break;
      case FacePlaneResult::SPLIT:
        split_faces.push_back(clip_face(face, plane));
        break;
      case FacePlaneResult::ALL_OUT:
        break;
    }
  }
  size_t all_in_count = new_volume.faces.size();
  // printf("face counts: %ld -> %ld in, %ld split\n", vol.faces.size(), all_in_count,
  //        split_faces.size());
  // ASSERT(!new_volume.faces.empty());

  if (!split_faces.empty()) {
    // add clipped faces
    for (auto& cf : split_faces) {
      new_volume.faces.push_back(cf.clipped_face);
    }

    // build the new face
    Face new_face;
    new_face.plane = plane;
    new_face.points = extract_face_ring(split_faces);
    new_face.flip_if_needed();
    new_face.verify();
    new_volume.faces.push_back(new_face);
  } else {
  }

  return new_volume;
}

Volume paranoid_clip_volume(const Volume& vol, const Plane& plane) {
  // printf("CLIP STARTING!\n");

  Volume clipped = clip_volume(vol, plane);
  clipped.verify();
  Volume other = clip_volume(vol, flip_plane_normal(plane));
  other.verify();

  // TODO check areas
  const double a1 = vol.surface_area();
  const double a2 = clipped.surface_area() + other.surface_area();
  ASSERT(a2 >= a1);  // todo: could be better.
  const double v1 = vol.volume();
  const double v2 = clipped.volume() + other.volume();
  if (std::abs(v1 - v2) > 0.001 * v1) {
    lg::die("Bad volumes: {} != {}\n", v1 / 1e6, v2 / 1e6);
  }

  // printf("CLIP Completed!\n");
  return clipped;
}

/*!
 * Given a sphere (x, y, z, radius), build a Volume for the axis-aligned bounding box of this
 * sphere.
 */
Volume make_aabb_for_sphere(const level_tools::Vector& sphere) {
  Volume volume;

  Point origin(sphere.data[0], sphere.data[1], sphere.data[2]);
  const double ox = origin.x();
  const double oy = origin.y();
  const double oz = origin.z();
  const double r = sphere.data[3];

  Face top_face;
  top_face.plane = Plane(0, -1, 0, -oy - r);
  top_face.points.emplace_back(ox - r, oy + r, oz - r);
  top_face.points.emplace_back(ox + r, oy + r, oz - r);
  top_face.points.emplace_back(ox + r, oy + r, oz + r);
  top_face.points.emplace_back(ox - r, oy + r, oz + r);
  volume.faces.push_back(top_face);

  Face bot_face;
  bot_face.plane = Plane(0, 1, 0, -oy - r);
  bot_face.points.emplace_back(ox - r, oy - r, oz - r);
  bot_face.points.emplace_back(ox - r, oy - r, oz + r);
  bot_face.points.emplace_back(ox + r, oy - r, oz + r);
  bot_face.points.emplace_back(ox + r, oy - r, oz - r);
  volume.faces.push_back(bot_face);

  Face s1_face;
  s1_face.plane = Plane(-1, 0, 0, -ox - r);
  s1_face.points.emplace_back(ox + r, oy - r, oz - r);
  s1_face.points.emplace_back(ox + r, oy - r, oz + r);
  s1_face.points.emplace_back(ox + r, oy + r, oz + r);
  s1_face.points.emplace_back(ox + r, oy + r, oz - r);
  volume.faces.push_back(s1_face);

  Face s2_face;
  s2_face.plane = Plane(1, 0, 0, -ox - r);
  s2_face.points.emplace_back(ox - r, oy - r, oz - r);
  s2_face.points.emplace_back(ox - r, oy + r, oz - r);
  s2_face.points.emplace_back(ox - r, oy + r, oz + r);
  s2_face.points.emplace_back(ox - r, oy - r, oz + r);
  volume.faces.push_back(s2_face);

  Face front_face;
  front_face.plane = Plane(0, 0, -1, -oz - r);
  front_face.points.emplace_back(ox - r, oy - r, oz + r);
  front_face.points.emplace_back(ox - r, oy + r, oz + r);
  front_face.points.emplace_back(ox + r, oy + r, oz + r);
  front_face.points.emplace_back(ox + r, oy - r, oz + r);
  volume.faces.push_back(front_face);

  Face back_face;
  back_face.plane = Plane(0, 0, 1, -oz - r);
  back_face.points.emplace_back(ox - r, oy - r, oz - r);
  back_face.points.emplace_back(ox + r, oy - r, oz - r);
  back_face.points.emplace_back(ox + r, oy + r, oz - r);
  back_face.points.emplace_back(ox - r, oy + r, oz - r);
  volume.faces.push_back(back_face);

  volume.verify();

  const double expected_vol = 8 * r * r * r;
  const double expected_sa = 24 * r * r;

  const double vol = volume.volume();
  const double sa = volume.surface_area();

  if (std::abs(vol - expected_vol) > expected_vol * 0.001) {
    lg::die("volume bad: {} {}\n", vol, expected_vol);
  }

  if (std::abs(sa - expected_sa) > expected_sa * 0.001) {
    lg::die("sa bad: {} {}\n", vol, expected_vol);
  }

  // lg::print("got {} {}, {} {}\n", vol, expected_vol, sa, expected_sa);

  return volume;
}

tfrag3::BspVisVertex make_vtx(const Point& pt, int leaf) {
  tfrag3::BspVisVertex v;
  v.bsp_cell = leaf;
  v.x = pt.x();
  v.y = pt.y();
  v.z = pt.z();
  return v;
}

void generate_volume_verts(std::vector<tfrag3::BspVisVertex>* verts,
                           std::vector<u32>* indices,
                           int leaf,
                           const Volume& volume) {
  // TODO: we could reuse vertices between faces...

  for (const auto& face : volume.faces) {
    // center
    indices->push_back(verts->size());
    verts->push_back(make_vtx(face.avg_vertex_pos(), leaf));

    // points
    for (const auto& pt : face.points) {
      indices->push_back(verts->size());
      verts->push_back(make_vtx(pt, leaf));
    }

    // next fan
    indices->push_back(UINT32_MAX);
  }
}

}  // namespace

void extract_bsp_cells(const level_tools::BspHeader& bsp, tfrag3::Level* out) {
  printf("bsp cells for %s %f %f %f %f\n", bsp.name.c_str(), bsp.bsphere.data[0],
         bsp.bsphere.data[1], bsp.bsphere.data[2], bsp.bsphere.data[3]);

  // ugh - bsphere seems like it's not set.
  level_tools::Vector derp;
  derp.data[0] = 0;
  derp.data[1] = 0;
  derp.data[2] = 0;
  derp.data[3] = 4096. * 4096. * 10.;

  auto parents = find_parents(bsp.jak1_bsp_nodes);
  for (int leaf_idx = 0; leaf_idx < parents.num_leaves(); leaf_idx++) {
    // printf("-----------------------------LEAF %d/%d\n", leaf_idx, parents.num_leaves());
    auto planes = planes_for_leaf(leaf_idx, parents, bsp.jak1_bsp_nodes);
    std::reverse(planes.begin(), planes.end());
    Volume vol = make_aabb_for_sphere(derp);

    for (auto& clip : planes) {
      vol = paranoid_clip_volume(vol, clip);
    }

    bool skip = false;
    for (auto& face : vol.faces) {
      for (auto& pt : face.points) {
        for (int i = 0; i < 3; i++) {
          if (std::abs(pt[i]) > derp.data[3] * 0.8) {
            skip = true;
          }
        }
      }
    }

    if (!skip) {
      generate_volume_verts(&out->debug_data.bsp_cell_vertices, &out->debug_data.bsp_cell_indices,
                      leaf_idx, vol);
    }



    // ASSERT_NOT_REACHED();
  }
}

namespace decompiler {
void extract_debug_vis(const level_tools::BspHeader& bsp, tfrag3::Level* out) {
  extract_bsp_cells(bsp, out);
}
}  // namespace decompiler
