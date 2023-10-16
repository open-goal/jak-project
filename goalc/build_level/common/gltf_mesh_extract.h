#pragma once

#include <string>

#include "common/custom_data/Tfrag3Data.h"

#include "goalc/build_level/collide/common/collide_common.h"
#include "goalc/build_level/common/TexturePool.h"

namespace gltf_mesh_extract {

struct Input {
  std::string filename;
  TexturePool* tex_pool = nullptr;
  bool get_colors = true;
  bool auto_wall_enable = true;
  float auto_wall_angle = 30.f;
  bool double_sided_collide = false;
};

struct TfragOutput {
  std::vector<tfrag3::StripDraw> strip_draws;
  std::vector<tfrag3::PreloadedVertex> vertices;
  std::vector<math::Vector<u8, 4>> color_palette;
};

struct CollideOutput {
  std::vector<jak1::CollideFace> faces;
};

struct Output {
  TfragOutput tfrag;
  CollideOutput collide;
};

void extract(const Input& in, Output& out);

}  // namespace gltf_mesh_extract