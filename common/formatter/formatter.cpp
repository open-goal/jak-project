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

std::string apply_formatting(const FormatterTreeNode& curr_node,
                             std::string output,
                             int tree_depth = 0) {
  if (!curr_node.token && curr_node.refs.empty()) {
    return output;
  }
  std::string curr_form = "";
  // Print the token
  if (curr_node.token) {
    curr_form += curr_node.token.value();
    return curr_form;
  }
  if (!curr_node.metadata.is_top_level) {
    curr_form += "(";
  }
  // Iterate the form

  bool inline_form = false;
  // Also check if the form should be constant-paired
  const bool constant_pair_form =
      formatter_rules::constant_pairs::form_should_be_constant_paired(curr_node);
  if (!constant_pair_form) {
    // Determine if the form should be inlined or hung/flowed
    // TODO - this isn't entirely accurate, needs current cursor positioning (which is tricky
    // because recursion!)
    inline_form = formatter_rules::indent::form_can_be_inlined(curr_form, curr_node);
  }
  for (int i = 0; i < curr_node.refs.size(); i++) {
    const auto& ref = curr_node.refs.at(i);
    // Append a newline if needed
    if (!inline_form) {
      formatter_rules::indent::append_newline(curr_form, ref, curr_node, tree_depth, i,
                                              constant_pair_form);
    }
    // Either print the element's token, or recursively format it as well
    if (ref.token) {
      // TODO depth hard-coded to 1, i think this can be removed, since
      // forms are always done bottom-top recursively, they always act
      // independently as if it was the shallowest depth
      if (!inline_form) {
        formatter_rules::indent::flow_line(curr_form, ref, curr_node, 1, i);
      }
      if (ref.metadata.node_type == "comment" && ref.metadata.is_inline) {
        curr_form += " " + ref.token.value();
      } else if (ref.metadata.node_type == "block_comment") {
        curr_form += formatter_rules::comments::format_block_comment(ref.token.value());
      } else {
        curr_form += ref.token.value();
      }
      if (!curr_node.metadata.is_top_level) {
        curr_form += " ";
      }
    } else {
      auto formatted_form = apply_formatting(ref, "", tree_depth + 1);
      if (!curr_node.metadata.is_top_level && !inline_form) {
        formatter_rules::indent::hang_lines(formatted_form, ref, curr_node, constant_pair_form);
      }
      curr_form += formatted_form;
      if (!curr_node.metadata.is_top_level) {
        curr_form += " ";
      }
    }
    // Handle blank lines at the top level, skip if it's the final element
    formatter_rules::blank_lines::separate_by_newline(curr_form, curr_node, ref, i);
  }
  if (!curr_node.metadata.is_top_level) {
    curr_form = str_util::rtrim(curr_form) + ")";
  }
  return curr_form;
}

std::optional<std::string> formatter::format_code(const std::string& source) {
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
  if (ts_node_is_null(root_node) || ts_node_has_error(root_node)) {
    return std::nullopt;
  }

  const auto formatting_tree = FormatterTree(source, root_node);
  std::string formatted_code = apply_formatting(formatting_tree.root, "");

  return formatted_code;
}
