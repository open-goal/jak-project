#pragma once

/*!
 * @file LinkedObjectFileCreation.h
 * Create a LinkedObjectFile from raw object file data.
 * This implements a decoder for the GOAL linking format.
 */

#include "LinkedObjectFile.h"

namespace decompiler {
class DecompilerTypeSystem;
LinkedObjectFile to_linked_object_file(const std::vector<uint8_t>& data,
                                       const std::string& name,
                                       DecompilerTypeSystem& dts,
                                       GameVersion game_version);
}  // namespace decompiler
