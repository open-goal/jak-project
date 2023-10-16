#pragma once

#include <string>
#include <vector>

#include "common/log/log.h"
#include "common/util/compress.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"

#include "decompiler/level_extractor/extract_level.h"

void save_pc_data(const std::string& nickname, tfrag3::Level& data, const fs::path& fr3_output_dir);
std::vector<std::string> get_build_level_deps(const std::string& input_file);
std::vector<decompiler::ObjectFileRecord> find_art_groups(
    std::vector<std::string>& processed_ags,
    const std::vector<std::string>& custom_level_ag,
    const std::vector<decompiler::ObjectFileRecord>& dgo_files);
