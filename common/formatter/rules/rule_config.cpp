#include "rule_config.h"

namespace formatter_rules {
namespace config {

static FormFormattingConfig new_permissive_flow_rule() {
  return {.config_set = true, .hang_forms = false, .combine_first_two_lines = true};
}

static FormFormattingConfig new_flow_rule(int start_index) {
  return {.config_set = true,
          .hang_forms = false,
          .inline_until_index = [start_index](const std::vector<std::string>& /*curr_lines*/) {
            return start_index;
          }};
}

static FormFormattingConfig new_deftype_rule(
    int start_index,
    const std::vector<int>& inlining_preventation_indices) {
  FormFormattingConfig cfg;
  cfg.config_set = true;
  cfg.hang_forms = false;
  cfg.inline_until_index = [start_index](std::vector<std::string> curr_lines) {
    if (curr_lines.size() >= 4 && curr_lines.at(3) == "()") {
      return 4;
    }
    return start_index;
  };
  for (const auto& index : inlining_preventation_indices) {
    auto temp_config = std::make_shared<FormFormattingConfig>();
    temp_config->config_set = true;
    temp_config->prevent_inlining = true;
    temp_config->hang_forms = false;
    temp_config->indentation_width = 1;
    auto temp_list_config = std::make_shared<FormFormattingConfig>();
    temp_list_config->force_inline = true;
    temp_list_config->hang_forms = false;
    temp_config->default_index_config = temp_list_config;
    if (index == 3) {
      temp_config->determine_column_widths_for_list_elements = true;
    }
    cfg.index_configs.emplace(index, temp_config);
  }
  return cfg;
}

static FormFormattingConfig new_binding_rule(int form_head_width) {
  FormFormattingConfig cfg;
  cfg.config_set = true;
  cfg.hang_forms = false;
  cfg.combine_first_two_lines = true;
  auto binding_list_config = std::make_shared<FormFormattingConfig>();
  binding_list_config->config_set = true;
  binding_list_config->hang_forms = false;
  binding_list_config->indentation_width = 1;
  binding_list_config->indentation_width_for_index = [form_head_width](FormFormattingConfig /*cfg*/,
                                                                       int index) {
    if (index == 0) {
      return 0;
    }
    return form_head_width;
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

static FormFormattingConfig new_pair_rule(bool combine_first_two_expr) {
  FormFormattingConfig cfg;
  cfg.config_set = true;
  cfg.hang_forms = false;
  cfg.prevent_inlining = true;
  cfg.combine_first_two_lines = combine_first_two_expr;
  auto pair_config = std::make_shared<FormFormattingConfig>();
  pair_config->config_set = true;
  pair_config->hang_forms = false;
  pair_config->indentation_width = 1;
  cfg.default_index_config = pair_config;
  return cfg;
}

const std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config = {
    {"case", new_pair_rule(true)},
    {"cond", new_pair_rule(false)},
    {"defmethod", new_flow_rule(3)},
    {"deftype", new_deftype_rule(3, {3, 4, 5, 6})},
    {"defun", new_flow_rule(3)},
    {"defun-debug", new_flow_rule(3)},
    {"defbehavior", new_flow_rule(4)},
    {"if", new_permissive_flow_rule()},
    {"define", new_permissive_flow_rule()},
    {"define-extern", new_permissive_flow_rule()},
    {"defmacro", new_flow_rule(3)},
    {"dotimes", new_flow_rule(2)},
    {"let", new_binding_rule(4)},
    {"rlet", new_binding_rule(5)},
    {"when", new_flow_rule(2)},
    {"begin", new_flow_rule(0)},
    {"with-dma-buffer-add-bucket", new_flow_rule(2)}};
}  // namespace config
}  // namespace formatter_rules
