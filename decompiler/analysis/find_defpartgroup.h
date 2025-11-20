#pragma once

#include "decompiler/Function/Function.h"

namespace decompiler {
void run_defpartgroup(Function& top_level_func,
                      std::unordered_map<u32, std::string>& part_group_table);
}
