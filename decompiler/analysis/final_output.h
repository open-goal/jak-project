#pragma once
#include <string>
#include "decompiler/Function/Function.h"

namespace decompiler {
std::string final_defun_out(const Function& func, const Env& env, const DecompilerTypeSystem& dts);
}
