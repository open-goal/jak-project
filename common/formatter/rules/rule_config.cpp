#include "rule_config.h"

namespace formatter_rules {
namespace config {

// TODO - populate these more

// TODO - this could be greatly simplified with C++20's designated initialization
FormFormattingConfig new_flow_rule(int start_index) {
  FormFormattingConfig cfg;
  cfg.force_flow = true;
  cfg.start_flow_at_index = start_index;
  cfg.inline_until_index = start_index;
  return cfg;
}

FormFormattingConfig new_binding_rule() {
  FormFormattingConfig cfg;
  cfg.has_bindings = true;
  cfg.force_flow = true;
  cfg.inline_until_index = 2;
  return cfg;
}

const std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config = {
    {"defun", new_flow_rule(3)},
    {"let", new_binding_rule()}};
}  // namespace config
}  // namespace formatter_rules
