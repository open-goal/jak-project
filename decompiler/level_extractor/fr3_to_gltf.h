#pragma once

#include <map>

#include "common/custom_data/Tfrag3Data.h"
#include "common/util/FileUtil.h"

#include "decompiler/level_extractor/common_formats.h"

/*!
 * Export the background geometry (tie, tfrag, shrub) to a GLTF binary format (.glb) file.
 */
void save_level_background_as_gltf(const tfrag3::Level& level, const fs::path& glb_file);
void save_level_foreground_as_gltf(const tfrag3::Level& level,
                                   const std::map<std::string, level_tools::ArtData>& art_data,
                                   const fs::path& glb_path);