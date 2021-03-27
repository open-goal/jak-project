#pragma once

#include <vector>
#include <unordered_map>
#include <string>
#include "common/type_system/TypeSpec.h"
#include "decompiler/config.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {
bool run_type_analysis_ir2(const TypeSpec& my_type, DecompilerTypeSystem& dts, Function& func);
}