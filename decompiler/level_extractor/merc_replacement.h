#pragma once

#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/data/TextureDB.h"

// Data produced by loading a replacement model
struct MercSwapData {
  std::vector<u32> new_indices;
  std::vector<tfrag3::MercVertex> new_vertices;
  std::vector<tfrag3::Texture> new_textures;
  tfrag3::MercModel new_model;
};

MercSwapData load_replacement_merc_model(u32 current_idx_count,
                                         u32 current_vtx_count,
                                         u32 current_tex_count,
                                         const std::string& path,
                                         const std::vector<tfrag3::MercVertex>& old_verts);
