#pragma once
#include <vector>

#include "decompiler/config.h"

#include "third-party/json.hpp"

namespace decompiler {
std::vector<StackStructureHint> parse_stack_structure_hints(const nlohmann::json& json);
std::unordered_map<int, std::vector<decompiler::RegisterTypeCast>> parse_cast_hints(
    const nlohmann::json& casts);
}  // namespace decompiler
