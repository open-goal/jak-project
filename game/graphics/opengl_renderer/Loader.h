#pragma once

#include "common/custom_data/Tfrag3Data.h"

class Loader {
 public:
  tfrag3::Level* get_tfrag3_level(const std::string& level_name);

 private:
  std::unordered_map<std::string, tfrag3::Level> m_tfrag3_levels;
};
