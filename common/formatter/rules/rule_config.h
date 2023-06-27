#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/formatter/rules/formatting_rules.h"

namespace formatter_rules {
namespace config {
struct FormFormattingConfig {
  bool force_hang = false;
  bool force_flow = false;
  int start_hang_at_index = 0;
  int start_flow_at_index = 0;
};

extern std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config;
}  // namespace config
}  // namespace formatter_rules
