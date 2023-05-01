#include "formatter.h"

#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "tree_sitter/api.h"

#include "third-party/fmt/core.h"

// Declare the `tree_sitter_opengoal` function, which is
// implemented by the `tree-sitter-opengoal` library.
extern "C" {
extern const TSLanguage* tree_sitter_opengoal();
}

void walk_tree(TSTreeCursor* cursor, std::string& output, const std::string& source_code) {
  // an imperative breadth-first-search
  while (true) {
    // Process the node
    const auto curr_node = ts_tree_cursor_current_node(cursor);
    const std::string curr_node_type = ts_node_type(curr_node);
    std::string curr_node_field_name;
    if (ts_tree_cursor_current_field_name(cursor)) {
      curr_node_field_name = ts_tree_cursor_current_field_name(cursor);
    }
    if (curr_node_field_name == "open") {
      output += "(";
    } else if (curr_node_field_name == "close") {
      output.pop_back();
      output += ") ";
    }
    if (curr_node_type == "sym_name" || curr_node_type == "num_lit" ||
        curr_node_type == "str_lit") {
      uint32_t start = ts_node_start_byte(curr_node);
      uint32_t end = ts_node_end_byte(curr_node);
      const char* type = ts_node_type(curr_node);
      (void)type;
      // TODO - if it's a string literal, take out any newlines and reflow the string to the
      // line-length
      const auto contents = source_code.substr(start, end - start);
      output += contents + " ";
    }

    if (ts_tree_cursor_goto_first_child(cursor)) {
      continue;
    }

    if (ts_tree_cursor_goto_next_sibling(cursor)) {
      continue;
    }

    while (true) {
      if (!ts_tree_cursor_goto_parent(cursor)) {
        if (output.at(output.length() - 1) == ' ') {
          output.pop_back();
        }
        return;
      }
      if (ts_tree_cursor_goto_next_sibling(cursor)) {
        break;
      }
    }
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

// It's possible to walk a tree-sitter tree imperatively with a cursor
// but the code for that is more verbose and less intuitive and I'm not sure how much
// of a benefit I'd get out of it since for formatting i basically have to convert every
// cursor to it's fat node
//
// But in any case, do it the easy way first and refactor later
void format_code(const std::string& source,
                 TSNode curr_node,
                 std::string& output,
                 std::string curr_form_head = "",
                 int indent = 0) {
  if (ts_node_child_count(curr_node) == 0) {
    uint32_t start = ts_node_start_byte(curr_node);
    uint32_t end = ts_node_end_byte(curr_node);
    // TODO - if it's a string literal, take out any newlines and reflow the string to the
    // line-length
    const auto contents = source.substr(start, end - start);
    if (contents == ")") {
      output.pop_back();
      output += ") ";
    } else if (contents == "(") {
      output += "(";
    } else {
      output += contents + " ";
    }
    return;
  }
  const std::string curr_node_type = ts_node_type(curr_node);
  for (size_t i = 0; i < ts_node_child_count(curr_node); i++) {
    auto child_node = ts_node_child(curr_node, i);
    // If we are opening a list, peek at the first element in the list
    // this is so we can properly handle indentation based on different forms
    if (curr_node_type == "list_lit" && i == 1) {
      uint32_t start = ts_node_start_byte(child_node);
      uint32_t end = ts_node_end_byte(child_node);
      // TODO - if it's a string literal, take out any newlines and reflow the string to the
      // line-length
      curr_form_head = source.substr(start, end - start);
    }
    std::string curr_node_field_name;
    auto curr_field_name_raw = ts_node_field_name_for_child(
        curr_node, i);  // TODO - why is this always returning `close` for the opening paren..
    if (curr_field_name_raw) {
      curr_node_field_name = curr_field_name_raw;
    }
    if (curr_form_head == "defun" && i == 4) {
      indent += 2;
      output += "\n" + repeat(indent, " ");
    } else if (curr_form_head == "defun" && i == 5) {
      output += "\n" + repeat(indent, " ");
    }
    format_code(source, child_node, output, curr_form_head, indent);
    if (curr_node_type == "source") {
      output += "\n\n";
    }
  }
}

std::string formatter::format_code(const std::string& source) {
  // Create a parser.
  std::shared_ptr<TSParser> parser(ts_parser_new(), TreeSitterParserDeleter());

  // Set the parser's language (JSON in this case).
  ts_parser_set_language(parser.get(), tree_sitter_opengoal());

  // Build a syntax tree based on source code stored in a string.
  std::shared_ptr<TSTree> tree(
      ts_parser_parse_string(parser.get(), NULL, source.c_str(), source.length()),
      TreeSitterTreeDeleter());

  // Get the root node of the syntax tree.
  TSNode root_node = ts_tree_root_node(tree.get());

  std::string output = "";
  format_code(source, root_node, output, "", 0);

  return str_util::trim(output);
}
