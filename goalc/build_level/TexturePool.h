#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "common/custom_data/Tfrag3Data.h"

struct TexturePool {
  std::unordered_map<std::string, int> textures_by_name;
  std::vector<tfrag3::Texture> textures_by_idx;
};