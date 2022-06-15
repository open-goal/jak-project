#pragma once

#include <string>

#include "common/custom_data/Tfrag3Data.h"
#include "tools/build_level/TexturePool.h"

namespace gltf_mesh_extract {


struct Input {
  std::string filename;
  TexturePool* tex_pool = nullptr;
};

struct Output {
  std::vector<tfrag3::StripDraw> strip_draws;
  std::vector<tfrag3::PreloadedVertex> vertices;
  std::vector<math::Vector<u8, 4>> color_palette;
};

void extract(const Input& in, Output& out);


}