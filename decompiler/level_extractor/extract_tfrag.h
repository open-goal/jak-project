#pragma once

#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"

#include "decompiler/data/TextureDB.h"
#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {

// the different "kinds" of tfrag. The actual renderers are almost identical and the only different
// is in GS setup (alpha blending) and in how the closest object is used.

// This is the actual tree data, minus the tfrags themselves.
struct VisNodeTree {
  std::vector<tfrag3::VisNode> vis_nodes;
  u16 first_child_node = 0;
  u16 last_child_node = 0;
  u16 first_root = 0;
  u16 num_roots = 0;
  bool only_children = false;
};

// will pool textures with others already in out.
void extract_tfrag(const level_tools::DrawableTreeTfrag* tree,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   const std::vector<std::pair<int, int>>& expected_missing_textures,
                   tfrag3::Level& out,
                   bool dump_level,
                   const std::string& level_name,
                   bool disable_atest_in_normal);

}  // namespace decompiler
