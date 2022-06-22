#pragma once

#include <vector>
#include <string>

bool run_build_level(const std::string& input_file, const std::string& output_file);
std::vector<std::string> get_build_level_deps(const std::string& input_file);