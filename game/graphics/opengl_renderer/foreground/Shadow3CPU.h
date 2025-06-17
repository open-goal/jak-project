#pragma once
#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"

struct ShadowCPUInput {
  math::Vector3f origin;
  math::Vector4f top_plane, bottom_plane;
  math::Vector3f light_dir;
  const u8* bones = nullptr;
  const tfrag3::ShadowModel* model = nullptr;
  std::vector<tfrag3::ShadowVertex>* vertices = nullptr;
  u32 flags = 0;
  int debug_highlight_tri = 0;
};

struct ShadowCPUOutput {
  static constexpr int kMaxIndices = (256 * 3) + (256 * 3 * 2);

  void push_index(u32 i, bool facing) {
    indices[num_indices++] = i;
    if (!facing) {
      f0_indices[num_f0_indices++] = i;
    } else {
      f1_indices[num_f1_indices++] = i;
    }
  }

  int num_indices = 0;
  u32 indices[kMaxIndices];

  int num_f0_indices = 0;
  u32 f0_indices[kMaxIndices];
  int num_f1_indices = 0;
  int f1_indices[kMaxIndices];
};

struct ShadowCPUWorkspace {
  math::Vector4f vertices[tfrag3::ShadowModel::kMaxVertices];
  math::Vector4f dual_vertices[tfrag3::ShadowModel::kMaxVertices];
  u8 tri_flags[tfrag3::ShadowModel::kMaxTris];
};

void calc_shadow_indices(const ShadowCPUInput& input,
                         ShadowCPUWorkspace* work,
                         ShadowCPUOutput* output);