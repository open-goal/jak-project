#include "Shadow3CPU.h"

// This file generates indices with correct face orientations for a shadow volume.
// As usual, Naughty Dog has a few tricks.
// You can have double-sided triangles - basically infinitely thin geometry that casts a shadow.
// It is also okay to have holes in your mesh of single-sided triangles as long as no light ray
// would enter or exit your "volume" through the gap first or last.
// (so you should only have gaps for weird internal areas that would never have an effect on the
//  final shadow) It is always safe to eliminate triangles that are fully enclosed within another
// closed volume.

/**
 * Since the shadow mesh has skeletal animation, we must compute the vertex positions on the CPU to
 * determine the shadow volume. There's no way around this other than geometry shaders.
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

  // vertices influenced by one bone
  for (int i = 0; i < input.model->num_one_bone_vertices; i++) {
    const Bone& bone = first_bone_ptr[vertex_ptr->mats[0]];
    *out_ptr = bone.mat[3] +                       //
               bone.mat[0] * vertex_ptr->pos[0] +  //
               bone.mat[1] * vertex_ptr->pos[1] +  //
               bone.mat[2] * vertex_ptr->pos[2];
    vertex_ptr++;
    out_ptr++;
  }

  // vertices influenced by two bones.
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

/**
 * Compute the projection of each vertex onto the "bottom plane". This is another ND trick.
 * Instead of doing the traditional thing of making an infinite volume, they effectively clip
 * the mesh against some plane that sits (ideally) slightly below the ground. This avoids the issue
 * of casting shadows on the "wrong side of the ground", and reduces fill.
 */
void calc_dual_verts(const ShadowCPUInput& input, ShadowCPUWorkspace* work) {
  const int num_verts = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  for (int i = 0; i < num_verts; i++) {
    math::Vector4f origin(input.origin.x(), input.origin.y(), input.origin.z(), 1.f);
    math::Vector4f p = work->vertices[i];
    math::Vector4f offset = origin - p;
    math::Vector4f plane = input.bottom_plane;
    work->dual_vertices[i] = p - offset * p.dot(plane) / offset.xyz().dot(plane.xyz());
  }
}

/**
 * Another ND trick: clip the mesh against some plane slightly above the ground. I think this just
 * reduces fill.
 */
void scissor_top(const ShadowCPUInput& input, ShadowCPUWorkspace* work) {
  const int num_verts = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  for (int i = 0; i < num_verts; i++) {
    auto& original_vertex = work->vertices[i];
    const auto& dual_vertex = work->dual_vertices[i];
    const float above = original_vertex.dot(input.top_plane);
    if (above > 0) {
      const math::Vector4f offset = dual_vertex - original_vertex;
      float scale = above / offset.xyz().dot(input.top_plane.xyz());
      original_vertex -= offset * scale;
    }
  }
}

/**
 * Clip against the near plane. I'm not sure why this is needed, but it may be another fill-reducing
 * trick, or maybe just allows them to avoid scissoring against the near plane in the VU program?
 * Either way, it seems like this isn't really needed.
 */
void scissor_edges(const ShadowCPUInput& input, ShadowCPUWorkspace* work) {
  // TODO
}

/**
 * Add "cap" triangles. These are either triangles in the original mesh that point toward the light,
 * and their projection to the bottom plane.
 */
void find_facing_single_tris(const ShadowCPUInput& input,
                             ShadowCPUWorkspace* work,
                             ShadowCPUOutput* output,
                             const std::vector<tfrag3::ShadowTri>& tris) {
  const int num_verts = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  for (size_t i = 0; i < tris.size(); i++) {
    const auto& tri = tris[i];
    // recompute normal after transformation:
    math::Vector3f v0 = work->vertices[tri.verts[0]].xyz();
    math::Vector3f v1 = work->vertices[tri.verts[1]].xyz();
    math::Vector3f v2 = work->vertices[tri.verts[2]].xyz();
    math::Vector3f n = (v1 - v0).cross(v2 - v0);

    bool highlight = i == input.debug_highlight_tri;
    if (n.dot(input.light_dir) < 0.f) {
      work->tri_flags[i] = 1;
      // facing toward the light, add the triangle as it appears in the original mesh
      output->push_index(tri.verts[0], !highlight);
      output->push_index(tri.verts[1], !highlight);
      output->push_index(tri.verts[2], !highlight);
      // and the projection. This triangle has the normal pointing the other way, since it closes
      // the volume, so the indices are flipping.
      output->push_index(static_cast<int>(tri.verts[0]) + num_verts, !highlight);
      output->push_index(static_cast<int>(tri.verts[2]) + num_verts, !highlight);
      output->push_index(static_cast<int>(tri.verts[1]) + num_verts, !highlight);
    } else {
      // facing away from the light.
      work->tri_flags[i] = 0;
    }
  }
}

/**
 * Build walls. A wall will happen on an edge between a facing and non-facing tri.
 * Or, if there is no second tri, there will be a wall whenever the first tri is facing.
 */
void find_single_edges(const ShadowCPUInput& input,
                       ShadowCPUWorkspace* work,
                       ShadowCPUOutput* output) {
  int edge_offset = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  for (size_t i = 0; i < input.model->single_edges.size(); i++) {
    const auto& e = input.model->single_edges[i];
    const u8 f0 = work->tri_flags[e.tri[0]];
    const auto t1 = e.tri[1];

    bool flip = false;  // set if the edge orientation is backward.
    if (t1 == 255) {
      if (f0 == 0) {  // only one tri, skip if not facing
        continue;
      }
      // if facing, then the edge is already oriented the right way!
    } else {
      const u8 f1 = work->tri_flags[e.tri[1]];
      if (f0 == f1) {  // both tris face the same way - no wall needed.
        continue;
      }
      flip = f0 == 0;  // ND convention here for edge direction.
      // this is somewhat of an odd convention because it seems like edges on singles
      // are backward. oh well.
    }

    if (!flip) {
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
/**
 * Add cap triangles for double triangles. One side is always facing!
 */
void find_facing_double_tris(const ShadowCPUInput& input,
                             ShadowCPUWorkspace* work,
                             ShadowCPUOutput* output,
                             const std::vector<tfrag3::ShadowTri>& tris) {
  const int num_verts = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;

  for (size_t i = 0; i < tris.size(); i++) {
    const auto& tri = tris[i];
    math::Vector3f v0 = work->vertices[tri.verts[0]].xyz();
    math::Vector3f v1 = work->vertices[tri.verts[1]].xyz();
    math::Vector3f v2 = work->vertices[tri.verts[2]].xyz();
    math::Vector3f n = (v1 - v0).cross(v2 - v0);
    if (n.dot(input.light_dir) < 0.f) {
      work->tri_flags[i] = 1;
      // treat this as a normal single-sided triangle that is facing.
      output->push_index(tri.verts[0], false);
      output->push_index(tri.verts[1], false);
      output->push_index(tri.verts[2], false);
      output->push_index(static_cast<int>(tri.verts[0]) + num_verts, false);
      output->push_index(static_cast<int>(tri.verts[2]) + num_verts, false);
      output->push_index(static_cast<int>(tri.verts[1]) + num_verts, false);
    } else {
      work->tri_flags[i] = 0;
      // we need to flip vertices to face the light.
      output->push_index(tri.verts[0], false);
      output->push_index(tri.verts[2], false);
      output->push_index(tri.verts[1], false);
      output->push_index(static_cast<int>(tri.verts[0]) + num_verts, false);
      output->push_index(static_cast<int>(tri.verts[1]) + num_verts, false);
      output->push_index(static_cast<int>(tri.verts[2]) + num_verts, false);
    }
  }
}

void find_double_edges(const ShadowCPUInput& input,
                       ShadowCPUWorkspace* work,
                       ShadowCPUOutput* output) {
  int edge_offset = input.model->num_one_bone_vertices + input.model->num_two_bone_vertices;
  for (size_t i = 0; i < input.model->double_edges.size(); i++) {
    const auto& e = input.model->double_edges[i];
    const u8 f0 = work->tri_flags[e.tri[0]];
    const auto t1 = e.tri[1];

    auto add = [&](bool flip) {
      if (flip) {
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
    };

    if (t1 == 255) {
      add(f0 == 1);
    } else {
      const u8 f1 = work->tri_flags[e.tri[1]];
      ASSERT(f0 != 77);
      ASSERT(f1 != 77);
      add(f0 == 1);
      add(f1 != 1);
    }
  }
}

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
  if (input.scissor_top) {
    scissor_top(input, work);
  }
  scissor_edges(input, work);
  find_facing_single_tris(input, work, output, input.model->single_tris);
  find_single_edges(input, work, output);
  find_facing_double_tris(input, work, output, input.model->double_tris);
  find_double_edges(input, work, output);

  for (int i = 0; i < output->num_indices; i++) {
    output->indices[i] += input.model->first_vertex;
  }
}
