#pragma once

#include "goalc/build_level/common/build_level.h"

namespace jak1 {
bool run_build_level(const std::string& input_file,
                     const std::string& bsp_output_file,
                     const std::string& output_prefix);
}  // namespace jak1