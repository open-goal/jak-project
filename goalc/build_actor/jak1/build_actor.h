#pragma once

#include "goalc/build_actor/common/art_types.h"

namespace jak1 {
struct ArtGroup {
  GameVersion version;
  FileInfo info;
  std::vector<ArtElement*> data;

  ArtGroup(const std::string& file_name, GameVersion version) {
    this->version = version;
    info.file_type = "art-group";
    info.file_name = file_name;
    info.major_version = versions::jak1::ART_FILE_VERSION;
    info.minor_version = 0;
    info.tool_debug = "Created by OpenGOAL buildactor";
  }
  size_t generate(DataObjectGenerator& gen);
};

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

void run_build_actor(const std::string& input_model,
                     const std::string& output_file,
                     const std::string& output_prefix);
}  // namespace jak1
