#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/formatter/formatting_rules.h"

namespace formatter_rules {
namespace config {
struct FormConfiguration {
  bool force_hang = false;
  int start_hang_at_index = 0;
};

extern std::unordered_map<std::string, FormConfiguration> opengoal_form_config;
}  // namespace config
}  // namespace formatter_rules
