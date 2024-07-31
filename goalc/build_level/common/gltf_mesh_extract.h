#pragma once

#include <string>

#include "common/custom_data/Tfrag3Data.h"

#include "goalc/build_level/collide/common/collide_common.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

namespace gltf_util {
struct TexturePool;
}

namespace gltf_mesh_extract {

struct Input {
  std::string filename;
  gltf_util::TexturePool* tex_pool = nullptr;
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

struct PatResult {
  bool set = false;
  bool ignore = false;
  jak1::PatSurface pat;
};

PatResult custom_props_to_pat(const tinygltf::Value& val, const std::string& /*debug_name*/);
void extract(const Input& in, Output& out);

}  // namespace gltf_mesh_extract