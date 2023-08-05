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
  std::optional<int> allow_inlining_if_size_less_than = {};
  int start_hang_at_index = 0;
  int start_flow_at_index = 0;
  int inline_until_index = -1;
  std::optional<int> bindings_at_index = {};
  std::optional<int> skip_newlines_until_index = {};
  std::vector<int> force_newline_at_indices = {};
  bool bindings_force_newlines = false;
  std::unordered_map<int, std::shared_ptr<FormFormattingConfig>> index_configs = {};
};

extern const std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config;
}  // namespace config
}  // namespace formatter_rules
