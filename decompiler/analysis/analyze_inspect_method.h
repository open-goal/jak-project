#pragma once

#include <string>
#include <unordered_map>

#include "decompiler/Function/Function.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

std::string inspect_inspect_method(Function& inspect_method,
                                   const std::string& type_name,
                                   DecompilerTypeSystem& dts,
                                   LinkedObjectFile& file,
                                   TypeSystem& scratch_ts,
                                   TypeSystem& previous_game_ts);

}