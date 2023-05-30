#include "formatter_tree.h"

// Check if the original source only has whitespace up to a new-line after it's token
bool node_followed_by_only_whitespace(const std::string& source, const TSNode& node) {
  uint32_t pos = ts_node_end_byte(node);
  while (pos < source.length()) {
    const auto& c = source.at(pos);
    if (c == '\n') {
      return true;
    } else if (c == ' ' || c == '\t') {
      pos++;
      continue;
    }
    return false;
  }
  return true;
}

std::string get_source_code(const std::string& source, const TSNode& node) {
  uint32_t start = ts_node_start_byte(node);
  uint32_t end = ts_node_end_byte(node);
  return source.substr(start, end - start);
}

FormatterTree::FormatterTree(const std::string& source, const TSNode& root_node) {
  root = FormatterTree::Node();
  root.metadata.is_root = true;
  construct_formatter_tree_recursive(source, root_node, root);
}

// TODO make an imperative version eventually
void FormatterTree::construct_formatter_tree_recursive(const std::string& source,
                                                       TSNode curr_node,
                                                       Node& tree_node) {
  if (ts_node_child_count(curr_node) == 0) {
    tree_node.refs.push_back(FormatterTree::Node(get_source_code(source, curr_node)));
    return;
  }
  const std::string curr_node_type = ts_node_type(curr_node);
  FormatterTree::Node list_node;
  if (curr_node_type == "list_lit") {
    list_node = FormatterTree::Node();
  }
  for (size_t i = 0; i < ts_node_child_count(curr_node); i++) {
    const auto child_node = ts_node_child(curr_node, i);
    // We skip parens
    const auto contents = get_source_code(source, child_node);
    if (contents == "(" || contents == ")") {
      continue;
    }
    if (curr_node_type == "list_lit") {
      // Check to see if the first line of the form has more than 1 element
      if (i == 1) {
        list_node.metadata.multiple_elements_first_line =
            !node_followed_by_only_whitespace(source, child_node);
      }
      construct_formatter_tree_recursive(source, child_node, list_node);
    } else {
      construct_formatter_tree_recursive(source, child_node, tree_node);
    }
  }
  if (curr_node_type == "list_lit") {
    tree_node.refs.push_back(list_node);
  }
}
