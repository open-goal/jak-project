#pragma once

#include "decompiler/Function/Function.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
void run_defstate(
    Function& top_level_func,
    const std::unordered_map<std::string, std::unordered_set<std::string>>& skip_states = {});
}
