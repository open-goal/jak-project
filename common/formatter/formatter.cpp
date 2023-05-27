#include "formatter.h"

#include "formatter_tree.h"

#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "tree_sitter/api.h"

#include "third-party/fmt/core.h"

// Declare the `tree_sitter_opengoal` function, which is
// implemented by the `tree-sitter-opengoal` library.
extern "C" {
extern const TSLanguage* tree_sitter_opengoal();
}

std::string get_source_code(const std::string& source, const TSNode& node) {
  uint32_t start = ts_node_start_byte(node);
  uint32_t end = ts_node_end_byte(node);
  return source.substr(start, end - start);
}

// TODO - use this instead eventually, no recursion it will be faster
FormatterTree construct_formatter_tree_iterative(TSTreeCursor* cursor, const std::string& source) {
  FormatterTree tree = FormatterTree();
  // an imperative breadth-first-search
  while (true) {
    if (ts_tree_cursor_goto_first_child(cursor)) {
      continue;
    } else {
      const auto curr_node = ts_tree_cursor_current_node(cursor);
      const std::string curr_node_type = ts_node_type(curr_node);
      uint32_t start = ts_node_start_byte(curr_node);
      uint32_t end = ts_node_end_byte(curr_node);
      const auto contents = source.substr(start, end - start);
    }

    if (ts_tree_cursor_goto_next_sibling(cursor)) {
      continue;
    }

    while (true) {
      if (!ts_tree_cursor_goto_parent(cursor)) {
        return tree;
      }
      if (ts_tree_cursor_goto_next_sibling(cursor)) {
        break;
      }
    }
  }
  return tree;
}

void construct_formatter_tree_recursive(const std::string& source,
                                        TSNode curr_node,
                                        FormatterTree::Node& tree_node) {
  if (ts_node_child_count(curr_node) == 0) {
    tree_node.refs.push_back(FormatterTree::Node(get_source_code(source, curr_node)));
    return;
  }
  // TODO - populate metadata
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
      construct_formatter_tree_recursive(source, child_node, list_node);
    } else {
      construct_formatter_tree_recursive(source, child_node, tree_node);
    }
  }
  if (curr_node_type == "list_lit") {
    tree_node.refs.push_back(list_node);
  }
}

// TODO - move this to str_util
std::string repeat(size_t n, const std::string& str) {
  if (n == 0 || str.empty())
    return {};
  if (n == 1)
    return str;
  const auto period = str.size();
  if (period == 1)
    return std::string(n, str.front());

  std::string ret(str);
  ret.reserve(period * n);
  std::size_t m{2};
  for (; m < n; m *= 2)
    ret += ret;
  ret.append(ret.c_str(), (n - (m / 2)) * period);
  return ret;
}

std::string formatter::format_code(const std::string& source) {
  // Create a parser.
  std::shared_ptr<TSParser> parser(ts_parser_new(), TreeSitterParserDeleter());

  // Set the parser's language (JSON in this case).
  ts_parser_set_language(parser.get(), tree_sitter_opengoal());

  // Build a syntax tree based on source code stored in a string.
  // TODO - if it fails to parse, don't format it!
  std::shared_ptr<TSTree> tree(
      ts_parser_parse_string(parser.get(), NULL, source.c_str(), source.length()),
      TreeSitterTreeDeleter());

  // Get the root node of the syntax tree.
  TSNode root_node = ts_tree_root_node(tree.get());
  auto root = FormatterTree::Node();

  construct_formatter_tree_recursive(source, root_node, root);

  return str_util::trim("");
}
