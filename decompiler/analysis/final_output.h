#pragma once
#include <string>
#include "decompiler/Function/Function.h"

namespace decompiler {

enum class FunctionDefSpecials { NONE, DEFUN_DEBUG };

std::string final_defun_out(const Function& func,
                            const Env& env,
                            const DecompilerTypeSystem& dts,
                            FunctionDefSpecials special_mode = FunctionDefSpecials::NONE);
std::string write_from_top_level(const Function& top_level,
                                 const DecompilerTypeSystem& dts,
                                 const LinkedObjectFile& file);
}  // namespace decompiler
