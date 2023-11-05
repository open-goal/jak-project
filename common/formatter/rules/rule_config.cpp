#include "rule_config.h"

#include "common/formatter/formatter_tree.h"

namespace formatter_rules {
namespace config {

// TODO - populate these more

// TODO - this could be greatly simplified with C++20's designated initialization
FormFormattingConfig new_flow_rule(int start_index) {
  FormFormattingConfig cfg;
  cfg.hang_forms = false;
  cfg.inline_until_index = start_index;
  return cfg;
}

FormFormattingConfig new_binding_rule() {
  FormFormattingConfig cfg;
  cfg.hang_forms = false;
  cfg.combine_first_two_lines = true;
  auto binding_list_config = std::make_shared<FormFormattingConfig>();
  binding_list_config->hang_forms = false;
  binding_list_config->indentation_width = 1;
  binding_list_config->indentation_width_for_index = [](FormFormattingConfig /*cfg*/, int index) {
    if (index == 0) {
      return 0;
    }
    return 4;
  };
  binding_list_config->should_prevent_inlining = [](FormFormattingConfig /*config*/, int num_refs) {
    // Only prevent inlining a binding list, if there are more than 1 bindings
    if (num_refs > 1) {
      return true;
    }
    return false;
  };
  binding_list_config->prevent_inlining =
      true;  // TODO - we only want to prevent inlining if there are more than 2 elements
  cfg.index_configs.emplace(1, binding_list_config);
  return cfg;
}

const std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config = {
    {"defun", new_flow_rule(3)},
    {"defmethod", new_flow_rule(4)},
    {"let", new_binding_rule()}};
}  // namespace config
}  // namespace formatter_rules
