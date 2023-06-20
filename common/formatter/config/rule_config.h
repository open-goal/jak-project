#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/formatter/formatting_rules.h"

namespace formatter {
extern const std::unordered_map<std::string, std::vector<std::shared_ptr<IndentationRule>>>
    opengoal_indentation_rules;
}
