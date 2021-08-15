#pragma once

#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/Function/Function.h"

namespace decompiler {
void run_defstate(DecompilerTypeSystem& dts, Function& top_level_func);
}