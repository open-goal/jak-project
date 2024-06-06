#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace formatter_rules {
namespace config {
struct FormFormattingConfig {
  bool config_set = false;
  bool force_inline = false;
  bool hang_forms =
      true;  // TODO - remove this eventually, it's only involved in setting the
             // indentation width, which we can do via the new indentation_width function
  int indentation_width =
      2;  // 2 for a flow // TODO - also remove this, prefer storing the first node's width in the
          // metadata on the first pass, that's basically all this does
  std::function<int(FormFormattingConfig, int)> indentation_width_for_index =
      [](FormFormattingConfig config, int /*index*/) { return config.indentation_width; };
  bool combine_first_two_lines =
      false;  // NOTE - basically hang, but will probably stick around after hang is gone, may be
              // redundant (inline_until_index!)
  std::function<std::optional<int>(const std::vector<std::string>& /*curr_lines*/)>
      inline_until_index = [](std::vector<std::string> /*curr_lines*/) { return std::nullopt; };
  bool has_constant_pairs = false;
  bool prevent_inlining = false;  // TODO - duplicate of below
  std::function<bool(FormFormattingConfig, int num_refs)> should_prevent_inlining =
      [](FormFormattingConfig config, int /*num_refs*/) { return config.prevent_inlining; };
  int parent_mutable_extra_indent = 0;
  std::optional<std::shared_ptr<FormFormattingConfig>> default_index_config;
  std::unordered_map<int, std::shared_ptr<FormFormattingConfig>> index_configs = {};
  // TODO / NOTe - not fully implemented, only doing `parent_mutable_extra_indent` right now
  std::unordered_map<int, std::shared_ptr<FormFormattingConfig>> index_config_override = {};

  bool determine_column_widths_for_list_elements = false;
  int num_columns_to_compute_widths = 0;
  std::vector<int> list_element_column_widths = {};

  bool elide_top_level_newline = false;
};

extern const std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config;
}  // namespace config
}  // namespace formatter_rules
