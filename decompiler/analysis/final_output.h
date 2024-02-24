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
                                 const LinkedObjectFile& file,
                                 const std::vector<std::string>& imports,
                                 const std::unordered_set<std::string>& skip_functions);

goos::Object get_arg_list_for_function(const Function& func, const Env& env);
goos::Object final_output_lambda(const Function& function, GameVersion version);
goos::Object final_output_defstate_anonymous_behavior(const Function& func,
                                                      const DecompilerTypeSystem& dts);
}  // namespace decompiler
