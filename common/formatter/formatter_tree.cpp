#include "formatter_tree.h"

#include "common/util/string_util.h"

#include "third-party/fmt/core.h"

std::string get_source_code(const std::string& source, const TSNode& node) {
  uint32_t start = ts_node_start_byte(node);
  uint32_t end = ts_node_end_byte(node);
  return source.substr(start, end - start);
}

int num_blank_lines_following_node(const std::string& source, const TSNode& node) {
  int num_lines = -1;  // The first new-line encountered is not a blank line
  uint32_t cursor = ts_node_end_byte(node);
  // TODO - this breaks on lines with whitespace as well, should probably seek past that!
  while (cursor < source.length() && source.at(cursor) == '\n') {
    num_lines++;
    cursor++;
  }
  return num_lines;
}

// Check if the original source only has whitespace up to a new-line before it's token
bool node_preceeded_by_only_whitespace(const std::string& source, const TSNode& node) {
  // NOTE - this returns incorrectly because we skip brackets in lists, we'll see if that matters
  int32_t pos = ts_node_start_byte(node) - 1;
  while (pos > 0) {
    const auto& c = source.at(pos);
    if (c == '\n') {
      return true;
    } else if (c == ' ' || c == '\t') {
      pos--;
      continue;
    }
    return false;
  }
  return true;
}

FormatterTreeNode::FormatterTreeNode(const std::string& source, const TSNode& node)
    : token(get_source_code(source, node)) {
  metadata.node_type = ts_node_type(node);
  metadata.is_comment = str_util::starts_with(str_util::ltrim(token.value()), ";") ||
                        str_util::starts_with(str_util::ltrim(token.value()), "#|");
  // Set any metadata based on the value of the token
  metadata.num_blank_lines_following = num_blank_lines_following_node(source, node);
  metadata.is_inline = !node_preceeded_by_only_whitespace(source, node);
};

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

FormatterTree::FormatterTree(const std::string& source, const TSNode& root_node) {
  root = FormatterTreeNode();
  root.metadata.is_top_level = true;
  construct_formatter_tree_recursive(source, root_node, root);
}

// TODO make an imperative version eventually
void FormatterTree::construct_formatter_tree_recursive(const std::string& source,
                                                       TSNode curr_node,
                                                       FormatterTreeNode& tree_node) {
  if (ts_node_child_count(curr_node) == 0) {
    tree_node.refs.push_back(FormatterTreeNode(source, curr_node));
    return;
  }
  const std::string curr_node_type = ts_node_type(curr_node);
  FormatterTreeNode list_node;
  if (curr_node_type == "list_lit") {
    list_node = FormatterTreeNode();
  } else if (curr_node_type == "str_lit") {
    // Strings are a special case, they are literals and essentially tokens but the grammar can
    // detect formatter identifiers, this is useful for semantic highlighting but doesn't matter for
    // formatting So for strings, we treat them as if they should be a single token
    tree_node.refs.push_back(FormatterTreeNode(source, curr_node));
    return;
  }
  for (size_t i = 0; i < ts_node_child_count(curr_node); i++) {
    const auto child_node = ts_node_child(curr_node, i);
    // We skip parens
    const auto contents = get_source_code(source, child_node);
    if (contents == "(" || contents == ")") {
      continue;
    }
    if (curr_node_type == "list_lit") {
      construct_formatter_tree_recursive(source, child_node, list_node);
    } else {
      construct_formatter_tree_recursive(source, child_node, tree_node);
    }
  }
  if (curr_node_type == "list_lit") {
    tree_node.refs.push_back(list_node);
  }
}
