#pragma once

#include "common/custom_data/Tfrag3Data.h"
#include "common/util/FileUtil.h"

/*!
 * Export the background geometry (tie, tfrag, shrub) to a GLTF binary format (.glb) file.
 */
void save_level_background_as_gltf(const tfrag3::Level& level, const fs::path& glb_file);
void save_level_foreground_as_gltf(const tfrag3::Level& level, const fs::path& glb_file);