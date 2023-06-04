#include "formatter_tree.h"

#include "common/util/string_util.h"

#include "config/rule_config.h"

namespace formatter {
const std::shared_ptr<FormattingRule> default_rule = std::make_shared<FormattingRule>();
}

std::shared_ptr<FormattingRule> FormatterTreeNode::get_formatting_rule(const int depth,
                                                                       const int index) const {
  // TODO - really lazy for now
  if (!rules.empty()) {
    return rules.at(0);
  }
  return formatter::default_rule;
}

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

bool nodes_on_same_line(const std::string& source, const TSNode& n1, const TSNode& n2) {
  // Get the source between the two lines, if there are any new-lines, the answer is NO
  uint32_t start = ts_node_start_byte(n1);
  uint32_t end = ts_node_end_byte(n2);
  const auto code_between = source.substr(start, end - start);
  return !str_util::contains(code_between, "\n");
}

std::string get_source_code(const std::string& source, const TSNode& node) {
  uint32_t start = ts_node_start_byte(node);
  uint32_t end = ts_node_end_byte(node);
  // TODO - comments end with a \n, this is likely a tree-sitter grammar problem
  return str_util::rtrim(source.substr(start, end - start));
}

FormatterTree::FormatterTree(const std::string& source, const TSNode& root_node) {
  root = FormatterTreeNode();
  root.metadata.is_root = true;
  construct_formatter_tree_recursive(source, root_node, root);
}

// TODO make an imperative version eventually
void FormatterTree::construct_formatter_tree_recursive(const std::string& source,
                                                       TSNode curr_node,
                                                       FormatterTreeNode& tree_node) {
  if (ts_node_child_count(curr_node) == 0) {
    tree_node.refs.push_back(FormatterTreeNode(get_source_code(source, curr_node)));
    return;
  }
  const std::string curr_node_type = ts_node_type(curr_node);
  FormatterTreeNode list_node;
  if (curr_node_type == "list_lit") {
    list_node = FormatterTreeNode();
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
        // Peek at the first element of the list to determine formatting rules
        if (formatter::opengoal_rules.find(contents) != formatter::opengoal_rules.end()) {
          list_node.rules = formatter::opengoal_rules.at(contents);
        }
      }
      construct_formatter_tree_recursive(source, child_node, list_node);
      // Check if the node that was recursively added to the list was on the same line
      auto& new_node = list_node.refs.at(list_node.refs.size() - 1);
      new_node.metadata.was_on_first_line_of_form =
          nodes_on_same_line(source, curr_node, child_node);
    } else {
      construct_formatter_tree_recursive(source, child_node, tree_node);
    }
  }
  if (curr_node_type == "list_lit") {
    tree_node.refs.push_back(list_node);
  }
}
