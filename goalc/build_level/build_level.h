#pragma once

#include <string>
#include <vector>

bool run_build_level(const std::string& input_file,
                     const std::string& bsp_output_file,
                     const std::string& output_prefix);
std::vector<std::string> get_build_level_deps(const std::string& input_file);
