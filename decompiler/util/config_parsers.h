#pragma once
#include <vector>
#include "decompiler/config.h"
#include "third-party/json.hpp"

namespace decompiler {
std::vector<StackVariableHint> parse_stack_var_hints(const nlohmann::json& json);
std::unordered_map<int, std::vector<decompiler::TypeCast>> parse_cast_hints(
    const nlohmann::json& casts);
}  // namespace decompiler
