#pragma once

#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/Function/Function.h"

namespace decompiler {
void run_defstate(Function& top_level_func);
}