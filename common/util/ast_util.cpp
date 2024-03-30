#include "ast_util.h"

namespace ast_util {
std::string get_source_code(const std::string& source, const TSNode& node) {
  uint32_t start = ts_node_start_byte(node);
  uint32_t end = ts_node_end_byte(node);
  return source.substr(start, end - start);
}

void search_for_forms_that_begin_with(const std::string& source,
                                      const TSNode curr_node,
                                      const std::vector<std::string>& prefix,
                                      std::vector<TSNode>& results) {
  if (ts_node_child_count(curr_node) == 0) {
    return;
  }
  std::vector<std::string> node_elements;
  bool added = false;
  for (size_t i = 0; i < ts_node_child_count(curr_node); i++) {
    const auto child_node = ts_node_child(curr_node, i);
    const auto contents = get_source_code(source, child_node);
    node_elements.push_back(contents);
    // Check for a match
    if (node_elements == prefix && !added) {
      results.push_back(curr_node);
      added = true;
    }
    search_for_forms_that_begin_with(source, child_node, prefix, results);
  }
}
}  // namespace ast_util
