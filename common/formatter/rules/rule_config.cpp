#include "rule_config.h"

#include "common/util/string_util.h"

namespace formatter_rules {
namespace config {

static FormFormattingConfig new_inlinable_simple_flow_rule() {
  return {.config_set = true, .hang_forms = false};
}

static FormFormattingConfig new_permissive_flow_rule() {
  return {.config_set = true, .hang_forms = false, .combine_first_two_lines = true};
}

static FormFormattingConfig new_flow_rule(int start_index, bool has_constant_pairs = false) {
  return {.config_set = true,
          .hang_forms = false,
          .inline_until_index =
              [start_index](const std::vector<std::string>& /*curr_lines*/) { return start_index; },
          .has_constant_pairs = has_constant_pairs};
}

static FormFormattingConfig new_function_rule(int start_index, bool has_constant_pairs = false) {
  FormFormattingConfig cfg = {
      .config_set = true,
      .hang_forms = false,
      .inline_until_index =
          [start_index](const std::vector<std::string>& /*curr_lines*/) { return start_index; },
      .has_constant_pairs = has_constant_pairs};
  auto arg_list_config = std::make_shared<FormFormattingConfig>();
  arg_list_config->force_inline = true;
  arg_list_config->hang_forms = false;
  cfg.index_configs.emplace(2, arg_list_config);
  return cfg;
}

static FormFormattingConfig new_inlineable_flow_rule(int start_index,
                                                     bool has_constant_pairs = false) {
  return {.config_set = true,
          .hang_forms = false,
          .inline_until_index =
              [start_index](const std::vector<std::string>& curr_lines) {
                int total_width = 0;
                for (const auto& line : curr_lines) {
                  // Check for comments
                  // TODO - this shows how this isn't really the best strategy but it holds up
                  if (str_util::contains(line, ";")) {
                    // Can't inline, there's a comment!
                    return start_index;
                  }
                  total_width += line.length();
                  // an empty line implies a new-line was forced, this is bleeding implementation
                  // details, but fine for now
                  if (line.empty()) {
                    return start_index;
                  }
                }
                if (total_width <= 120) {
                  return (int)curr_lines.size();
                }
                return start_index;
              },
          .has_constant_pairs = has_constant_pairs};
}

static FormFormattingConfig new_defstate_rule(int start_index, bool has_constant_pairs = false) {
  FormFormattingConfig cfg = {
      .config_set = true,
      .hang_forms = false,
      .inline_until_index =
          [start_index](const std::vector<std::string>& /*curr_lines*/) { return start_index; },
      .has_constant_pairs = has_constant_pairs};
  std::vector<int> state_handler_indexes = {4,  6, 8, 10,
                                            12, 14};  // NOTE - not all of these have to be defined
  for (const auto& index : state_handler_indexes) {
    auto temp_config = std::make_shared<FormFormattingConfig>();
    temp_config->prevent_inlining = true;
    temp_config->parent_mutable_extra_indent = 2;
    cfg.index_config_override.emplace(index, temp_config);
  }
  return cfg;
}

static FormFormattingConfig new_defmethod_rule(int start_index, bool has_constant_pairs = false) {
  // TODO - might be nice to have a function that returns a config based on a given index, instead
  // of hardcoding them!
  // Right now this only works for non-`new` methods (else we may bleed into the body of a normal
  // method)
  auto arg_list_config = std::make_shared<FormFormattingConfig>();
  arg_list_config->force_inline = true;
  arg_list_config->hang_forms = false;
  FormFormattingConfig cfg = {.config_set = true,
                              .hang_forms = false,
                              .inline_until_index =
                                  [start_index](const std::vector<std::string>& curr_lines) {
                                    if (curr_lines.size() >= 2 && curr_lines.at(1) == "new") {
                                      // defmethod was changed to omit the type name for everything
                                      // except the `new` method, so special case.
                                      return start_index + 1;
                                    }
                                    return start_index;
                                  },
                              .has_constant_pairs = has_constant_pairs};
  cfg.index_configs.emplace(2, arg_list_config);
  return cfg;
}

static FormFormattingConfig new_lambda_rule(int start_index, bool has_constant_pairs = false) {
  FormFormattingConfig cfg = {.config_set = true,
                              .hang_forms = false,
                              .inline_until_index =
                                  [start_index](const std::vector<std::string>& curr_lines) {
                                    if (curr_lines.size() >= 2 && curr_lines.at(1) == ":behavior") {
                                      // defmethod was changed to omit the type name for everything
                                      // except the `new` method, so special case.
                                      return start_index + 2;
                                    }
                                    return start_index;
                                  },
                              .has_constant_pairs = has_constant_pairs};
  return cfg;
}

static FormFormattingConfig new_defenum_rule() {
  auto temp_list_config = std::make_shared<FormFormattingConfig>();
  temp_list_config->force_inline = true;
  temp_list_config->hang_forms = false;
  return {
      .config_set = true,
      .hang_forms = false,
      .inline_until_index = [](const std::vector<std::string>& /*curr_lines*/) { return 2; },
      .has_constant_pairs = true,
      .default_index_config = temp_list_config,
  };
}

static FormFormattingConfig new_deftype_rule(
    int start_index,
    int num_columns_to_compute_widths,
    const std::vector<int>& inlining_preventation_indices) {
  FormFormattingConfig cfg;
  cfg.has_constant_pairs = true;
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
      temp_config->num_columns_to_compute_widths = num_columns_to_compute_widths;
    }
    cfg.index_configs.emplace(index, temp_config);
  }
  return cfg;
}

static FormFormattingConfig new_defproc_rule(
    int start_index,
    int num_columns_to_compute_widths,
    const std::vector<int>& inlining_preventation_indices) {
  FormFormattingConfig cfg;
  cfg.has_constant_pairs = true;
  cfg.config_set = true;
  cfg.hang_forms = false;
  cfg.inline_until_index = [start_index](std::vector<std::string> curr_lines) {
    // if (curr_lines.size() >= 4 && curr_lines.at(3) == "()") {
    //   return 4;
    // }
    return start_index;
  };
  for (const auto& index : inlining_preventation_indices) {
    auto temp_config = std::make_shared<FormFormattingConfig>();
    temp_config->config_set = true;
    temp_config->prevent_inlining = true;
    temp_config->hang_forms = false;
    temp_config->indentation_width = 1;
    auto temp_list_config = std::make_shared<FormFormattingConfig>();
    temp_list_config->force_inline = false;
    temp_list_config->hang_forms = false;
    temp_config->default_index_config = temp_list_config;
    if (index == 3) {
      temp_config->determine_column_widths_for_list_elements = true;
      temp_config->num_columns_to_compute_widths = num_columns_to_compute_widths;
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

static FormFormattingConfig new_inline_binding_rule(int form_head_width) {
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
    return false;
  };
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

static FormFormattingConfig new_top_level_inline_form(bool elide_new_line) {
  return {.force_inline = true, .elide_top_level_newline = elide_new_line};
}

const std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config = {
    {"case", new_pair_rule(true)},
    {"case-str", new_pair_rule(true)},
    {"cond", new_pair_rule(false)},
    {"#cond", new_pair_rule(false)},
    {"in-package", new_top_level_inline_form(true)},
    {"bundles", new_top_level_inline_form(true)},
    {"require", new_top_level_inline_form(true)},
    {"def-art-elt", new_top_level_inline_form(true)},
    {"def-joint-node", new_top_level_inline_form(true)},
    {"declare-file", new_top_level_inline_form(false)},
    {"defenum", new_defenum_rule()},
    {"defmethod", new_defmethod_rule(3)},
    {"lambda", new_lambda_rule(2)},
    {"deftype", new_deftype_rule(3, 1, {3, 4, 5, 6})},
    {"defproc", new_defproc_rule(3, 1, {3, 4, 5, 6})},
    {"suspend-for", new_flow_rule(2)},
    {"spawn-proc", new_flow_rule(2)},
    {"defun", new_flow_rule(3)},
    {"defun-recursive", new_flow_rule(4)},
    {"defun-debug-recursive", new_flow_rule(4)},
    {"defun-debug", new_flow_rule(3)},
    {"defbehavior", new_flow_rule(4)},
    {"if", new_inlineable_flow_rule(2)},
    {"#if", new_inlineable_flow_rule(2)},
    {"define", new_permissive_flow_rule()},
    {"def-mips2c", new_permissive_flow_rule()},
    {"defconstant", new_permissive_flow_rule()},
    {"defglobalconstant", new_permissive_flow_rule()},
    {"defmethod-mips2c", new_permissive_flow_rule()},
    {"define-extern", new_permissive_flow_rule()},
    {"declare-type", new_permissive_flow_rule()},
    {"defmacro", new_function_rule(3)},
    {"desfun", new_function_rule(3)},
    {"defskelgroup", new_flow_rule(2, true)},
    {"defpartgroup", new_flow_rule(2, true)},
    {"defpart", new_flow_rule(2, true)},
    {"defstate", new_defstate_rule(3, true)},
    {"behavior", new_flow_rule(2)},
    {"dotimes", new_flow_rule(2)},
    {"dolist", new_flow_rule(2)},
    {"process-spawn-function", new_flow_rule(2)},
    {"let", new_binding_rule(4)},
    {"protect", new_binding_rule(4)},
    {"let*", new_binding_rule(5)},
    {"rlet", new_binding_rule(5)},
    {"mlet", new_binding_rule(5)},
    {"when", new_flow_rule(2)},
    {"unless", new_flow_rule(2)},
    {"with-profiler", new_flow_rule(2)},
    {"with-pc", new_flow_rule(0)},
    {"#unless", new_flow_rule(2)},
    {"#when", new_flow_rule(2)},
    {"#when-game", new_flow_rule(2)},
    {"countdown", new_flow_rule(2)},
    {"until", new_flow_rule(2)},
    {"loop", new_flow_rule(0)},
    {"while", new_flow_rule(2)},
    {"begin", new_flow_rule(0)},
    {"seval", new_flow_rule(0)},
    {"with-pp", new_flow_rule(0)},
    {"with-gensyms", new_flow_rule(2)},
    {"with-vf0", new_flow_rule(0)},
    {"with-vf", new_inline_binding_rule(8)},
    {"with-cnt-vif-block", new_inline_binding_rule(19)},
    {"local-vars", new_inlinable_simple_flow_rule()},
    {"with-dma-buffer-add-bucket", new_flow_rule(2)}};
}  // namespace config
}  // namespace formatter_rules
