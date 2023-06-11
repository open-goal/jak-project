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

// TODO - incoporate some rules from zprint
// https://github.com/kkinnear/zprint/blob/main/doc/types/classic.md
// as well as maybe adjust the default rules to incorporate line length
// https://github.com/kkinnear/zprint/blob/main/doc/options/indent.md
// TODO - block comments seem to have an issue being parsed properly, also it basically needs the
// code for flexibly wrapping a block of code in configurable symbols (parens, block comment braces,
// etc)

std::string apply_formatting(const FormatterTreeNode& curr_node,
                             std::string output,
                             int tree_depth = 0) {
  if (!curr_node.token && curr_node.refs.empty()) {
    return output;
  }
  std::string curr_form = "";
  // Print the token
  if (curr_node.token) {
    // TODO - perhaps unneeded
    curr_node.get_formatting_rule(tree_depth, -1)
        ->indent_token(curr_form, curr_node, curr_node, tree_depth, -1);
    curr_form += curr_node.token.value();
    return curr_form;
  }
  // TODO - this might have some issues for non-list top level elements (ie. comments)
  if (!curr_node.metadata.is_top_level) {
    curr_form += "(";
  }
  // Iterate the form
  for (int i = 0; i < curr_node.refs.size(); i++) {
    const auto& ref = curr_node.refs.at(i);
    // Append a newline if needed
    curr_node.get_formatting_rule(tree_depth, i)
        ->append_newline(curr_form, ref, curr_node, tree_depth, i);
    // Either print the element's token, or recursively format it as well
    if (ref.token) {
      curr_node.get_formatting_rule(tree_depth, i)
          ->indent_token(curr_form, ref, curr_node, 1,
                         i);  // TODO depth hard-coded to 1, i think this can be removed, since
                              // forms are always done bottom-top recursively, they always act
                              // independently as if it was the shallowest depth
      curr_form += ref.token.value();
      if (!curr_node.metadata.is_top_level) {
        curr_form += " ";
      }
    } else {
      auto formatted_form = apply_formatting(ref, "", tree_depth + 1);
      if (!curr_node.metadata.is_top_level) {
        curr_node.get_formatting_rule(tree_depth, i)
            ->align_form_lines(formatted_form, ref, curr_node);
      }
      curr_form += formatted_form;
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
