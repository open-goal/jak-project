#pragma once

#include "common/log/log.h"
#include "common/util/gltf_util.h"

namespace decompiler {
struct MercExtractData {
  gltf_util::TexturePool tex_pool;
  std::vector<u32> new_indices;
  std::vector<tfrag3::PreloadedVertex> new_vertices;
  std::vector<math::Vector<u8, 4>> new_colors;
  std::vector<math::Vector3f> normals;
  std::vector<gltf_util::JointsAndWeights> joints_and_weights;
  tfrag3::MercModel new_model;
};

// Data produced by loading a replacement model
struct MercSwapData {
  std::vector<u32> new_indices;
  std::vector<tfrag3::MercVertex> new_vertices;
  std::vector<tfrag3::Texture> new_textures;
  tfrag3::MercModel new_model;
};

MercSwapData load_replacement_merc_model(const std::string& name,
                                         u32 current_idx_count,
                                         u32 current_vtx_count,
                                         u32 current_tex_count,
                                         const std::string& path,
                                         const std::vector<tfrag3::MercVertex>& old_verts,
                                         bool custom_mdl);
}  // namespace decompiler