#include "Shadow3CPU.h"

#include <set>

/*
*- `xform-verts` transform mesh vertices into camera space (no perspective)
- `init-vars` transform settings to camera space
- `calc-dual-verts` project vertices to plane
- `scissor-top` (only executed if shdf03 is set), clip vertices to top plane, if above
- `scissor-edges`, clip vertices to near plane
- `find-facing-single-tris`, set face bit to indicate orientation, cull backward ones
- `find-single-edges`, find edges that, when extruded, should be drawn
- `find-facing-double-tris`, set face bit indicate orientation. double sided tris, so no culling
- `find-double-edges`, find edges to extrude from the double-sided tris
- `add-verts`
- `add-facing-single-tris`
- `add-single-edges`
- `add-double-tris`
- `add-double-edges`
*/

void transform_vertices(const ShadowCPUInput& input, ShadowCPUWorkspace* work) {
  struct Bone {
    math::Vector4f mat[4];
    u8 pad[16 * 4];
  };
  static_assert(sizeof(Bone) == 128);
  const tfrag3::ShadowVertex* vertex_ptr = &input.vertices->operator[](input.model->first_vertex);
  math::Vector4f* out_ptr = work->vertices;
  const Bone* first_bone_ptr = (const Bone*)(3 * 8 * 4 * sizeof(float) + input.bones);

  for (int i = 0; i < input.model->num_one_bone_vertices; i++) {
    const Bone& bone = first_bone_ptr[vertex_ptr->mats[0]];
    *out_ptr = bone.mat[3] +                       //
               bone.mat[0] * vertex_ptr->pos[0] +  //
               bone.mat[1] * vertex_ptr->pos[1] +  //
               bone.mat[2] * vertex_ptr->pos[2];
    vertex_ptr++;
    out_ptr++;
  }

  for (int i = 0; i < input.model->num_two_bone_vertices; i++) {
    const Bone& bone0 = first_bone_ptr[vertex_ptr->mats[0]];
    math::Vector4f p0 = bone0.mat[3] +                       //
                        bone0.mat[0] * vertex_ptr->pos[0] +  //
                        bone0.mat[1] * vertex_ptr->pos[1] +  //
                        bone0.mat[2] * vertex_ptr->pos[2];
    p0 *= vertex_ptr->weight;
    const Bone& bone1 = first_bone_ptr[vertex_ptr->mats[1]];
    math::Vector4f p1 = bone1.mat[3] +                       //
                        bone1.mat[0] * vertex_ptr->pos[0] +  //
                        bone1.mat[1] * vertex_ptr->pos[1] +  //
                        bone1.mat[2] * vertex_ptr->pos[2];
    p1 *= (1.f - vertex_ptr->weight);
    *out_ptr = p0 + p1;

    out_ptr++;
    vertex_ptr++;
  }
}

void calc_dual_verts(const ShadowCPUInput& input, ShadowCPUWorkspace* work) {
  int num_verts = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  for (int i = 0; i < num_verts; i++) {
    math::Vector4f origin(input.origin.x(), input.origin.y(), input.origin.z(), 1.f);
    math::Vector4f p = work->vertices[i];
    math::Vector4f offset = origin - p;
    math::Vector4f plane = input.bottom_plane;
    work->dual_vertices[i] = p - offset * p.dot(plane) / offset.xyz().dot(plane.xyz());
  }
}

void scissor_top(const ShadowCPUInput& input, ShadowCPUWorkspace* work) {
  // TODO
}

void scissor_edges(const ShadowCPUInput& input, ShadowCPUWorkspace* work) {
  // TODO
}

void find_facing_single_tris(const ShadowCPUInput& input,
                             ShadowCPUWorkspace* work,
                             ShadowCPUOutput* output,
                             const std::vector<tfrag3::ShadowTri>& tris) {
  int edge_offset = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  int num_0 = 0;
  int num_1 = 0;
  for (size_t i = 0; i < tris.size(); i++) {
    const auto& tri = tris[i];
    math::Vector3f v0 = work->vertices[tri.verts[0]].xyz();
    math::Vector3f v1 = work->vertices[tri.verts[1]].xyz();
    math::Vector3f v2 = work->vertices[tri.verts[2]].xyz();
    math::Vector3f n = (v1 - v0).cross(v2 - v0);
    bool highlight = i == input.debug_highlight_tri;
    if (n.dot(input.light_dir) < 0.f) {
      num_0++;
      work->tri_flags[i] = 1;
      output->push_index(tri.verts[0], !highlight);
      output->push_index(tri.verts[1], !highlight);
      output->push_index(tri.verts[2], !highlight);
    } else {
      num_1++;
      work->tri_flags[i] = 0;
      output->push_index(static_cast<int>(tri.verts[0]) + edge_offset, !highlight);
      output->push_index(static_cast<int>(tri.verts[1]) + edge_offset, !highlight);
      output->push_index(static_cast<int>(tri.verts[2]) + edge_offset, !highlight);
    }
  }
}

// void find_facing_double_tris(const ShadowCPUInput& input,
//                              ShadowCPUWorkspace* work,
//                              ShadowCPUOutput* output,
//                              const std::vector<tfrag3::ShadowTri>& tris) {
//   int edge_offset = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
//   const int flag_offset = input.model->double_tris.size();
//   int num_0 = 0;
//   int num_1 = 0;
//   for (size_t i = 0; i < tris.size(); i++) {
//     const auto& tri = tris[i];
//     math::Vector3f v0 = work->vertices[tri.verts[0]].xyz();
//     math::Vector3f v1 = work->vertices[tri.verts[1]].xyz();
//     math::Vector3f v2 = work->vertices[tri.verts[2]].xyz();
//     math::Vector3f n = (v1 - v0).cross(v2 - v0);
//     if (n.dot(input.light_dir) < 0.f) {
//       num_0++;
//       work->tri_flags[i + flag_offset] = 1;
//
//     } else {
//       num_1++;
//       work->tri_flags[i + flag_offset] = 0;
//     }
//
//     output->push_index(tri.verts[0], false);
//     output->push_index(tri.verts[1], false);
//     output->push_index(tri.verts[2], false);
//     output->push_index(tri.verts[1], false);
//     output->push_index(tri.verts[0], false);
//     output->push_index(tri.verts[2], false);
//     output->push_index(static_cast<int>(tri.verts[0]) + edge_offset, false);
//     output->push_index(static_cast<int>(tri.verts[1]) + edge_offset, false);
//     output->push_index(static_cast<int>(tri.verts[2]) + edge_offset, false);
//     output->push_index(static_cast<int>(tri.verts[1]) + edge_offset, false);
//     output->push_index(static_cast<int>(tri.verts[0]) + edge_offset, false);
//     output->push_index(static_cast<int>(tri.verts[2]) + edge_offset, false);
//   }
// }

void find_single_edges(const ShadowCPUInput& input,
                       ShadowCPUWorkspace* work,
                       ShadowCPUOutput* output) {
  int num_weird = 0;
  int num_0 = 0;
  int num_1 = 0;
  int edge_offset = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  for (size_t i = 0; i < input.model->single_edges.size(); i++) {
    const auto& e = input.model->single_edges[i];
    bool skip = false;
    bool out_back = false;

    if (e.tri[1] == 255) {
      out_back = true;
      skip = work->tri_flags[e.tri[0]] == 0;
      num_weird++;
    } else {
      u8 f0 = work->tri_flags[e.tri[0]];
      u8 f1 = work->tri_flags[e.tri[1]];

      if (f0 == f1) {
        skip = true;
      } else {
        if (f0 == 1) {
          out_back = true;
          num_0++;
        } else {
          num_1++;
        }
      }
    }

    if (!skip) {
      if (out_back) {
        output->push_index(e.ind[0], true);
        output->push_index(static_cast<int>(e.ind[0]) + edge_offset, true);
        output->push_index(static_cast<int>(e.ind[1]) + edge_offset, true);

        output->push_index(e.ind[0], true);
        output->push_index(static_cast<int>(e.ind[1]) + edge_offset, true);
        output->push_index(e.ind[1], true);
      } else {
        output->push_index(e.ind[0], true);
        output->push_index(static_cast<int>(e.ind[1]) + edge_offset, true);
        output->push_index(static_cast<int>(e.ind[0]) + edge_offset, true);

        output->push_index(e.ind[0], true);
        output->push_index(e.ind[1], true);
        output->push_index(static_cast<int>(e.ind[1]) + edge_offset, true);
      }
    }
  }
}

void find_facing_double_tris() {}

void find_double_edges() {}

void calc_shadow_indices(const ShadowCPUInput& input,
                         ShadowCPUWorkspace* work,
                         ShadowCPUOutput* output) {
  output->num_indices = 0;
  output->num_f0_indices = 0;
  output->num_f1_indices = 0;

  // HACK
  for (auto& f : work->tri_flags) {
    f = 77;
  }

  transform_vertices(input, work);
  calc_dual_verts(input, work);
  scissor_top(input, work);
  scissor_edges(input, work);
  find_facing_single_tris(input, work, output, input.model->single_tris);
  find_single_edges(input, work, output);
  // find_facing_double_tris(input, work, output, input.model->double_tris);

  for (int i = 0; i < output->num_indices; i++) {
    output->indices[i] += input.model->first_vertex;
  }
}
