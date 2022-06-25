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
                                   TypeSystem& previous_game_ts);

std::string inspect_top_level_symbol_defines(std::unordered_set<std::string>& already_seen,
                                             Function& top_level,
                                             LinkedObjectFile& file,
                                             DecompilerTypeSystem& dts,
                                             DecompilerTypeSystem& previous_game_ts);

}  // namespace decompiler