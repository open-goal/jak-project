#pragma once

#include <string>
#include <vector>

#include "decompiler/Function/Function.h"

namespace decompiler {
void run_sound_info(Function& top_level_func, std::vector<std::string>& sound_info_names);
}
