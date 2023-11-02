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
  // new
  bool hang_forms = true;  // TODO - remove this eventually, it's only involved in setting the
                           // indentation width, which we can do via the indentation_width function
  int indentation_width =
      2;  // 2 for a flow // TODO - also remove this, prefer storing the first node's width in the
          // metadata on the first pass, that's basically all this does
  std::function<int(FormFormattingConfig, int)> indentation_width_for_index =
      [](FormFormattingConfig config, int /*index*/) { return config.indentation_width; };
  bool combine_first_two_lines =
      false;  // NOTE - basically hang, but will probably stick around after hang is gone
  int inline_until_index = -1;
  bool has_constant_pairs = false;
  bool prevent_inlining = false;
  std::function<bool(FormFormattingConfig, int num_refs)> should_prevent_inlining =
      [](FormFormattingConfig config, int /*num_refs*/) { return config.prevent_inlining; };
  int parent_mutable_extra_indent = 0;
  std::unordered_map<int, std::shared_ptr<FormFormattingConfig>> index_configs = {};
};

extern const std::unordered_map<std::string, FormFormattingConfig> opengoal_form_config;
}  // namespace config
}  // namespace formatter_rules
