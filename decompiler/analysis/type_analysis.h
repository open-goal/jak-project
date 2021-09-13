#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "common/type_system/TypeSpec.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/config.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
bool run_type_analysis_ir2(const TypeSpec& my_type, DecompilerTypeSystem& dts, Function& func);
}