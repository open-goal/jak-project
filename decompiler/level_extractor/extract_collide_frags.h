#pragma once

#include "BspHeader.h"

#include "common/custom_data/Tfrag3Data.h"

namespace decompiler {

void extract_collide_frags(const level_tools::DrawableTreeCollideFragment* tree,
                           const std::vector<const level_tools::DrawableTreeInstanceTie*>& ties,
                           const std::string& debug_name,
                           tfrag3::Level& out,
                           bool dump_level);

}  // namespace decompiler