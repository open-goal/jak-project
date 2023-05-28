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

std::string align_form(const std::string& form, int alignment_width) {
  const auto lines = str_util::split(form);
  std::string aligned_form = "";
  for (int i = 0; i < lines.size(); i++) {
    aligned_form += str_util::repeat(alignment_width, " ") + lines.at(i);
    if (i != lines.size() - 1) {
      aligned_form += "\n";
    }
  }
  return aligned_form;
}

std::string apply_formatting(const FormatterTree::Node& curr_node,
                             std::string output,
                             int tree_depth = 0) {
  if (!curr_node.token && curr_node.refs.empty()) {
    return output;
  }
  std::string curr_form = "";
  if (curr_node.token) {
    curr_form += curr_node.token.value();
    return curr_form;
  }
  if (!curr_node.metadata.is_root) {
    curr_form += "(";
  }
  for (int i = 0; i < curr_node.refs.size(); i++) {
    const auto& ref = curr_node.refs.at(i);
    // TODO - abstract these into formatting rules
    if (!curr_node.metadata.is_root && curr_node.metadata.multiple_elements_first_line) {
      if (i > 1) {
        // TODO - kinda unsafe
        // Trim the current form before applying a new-line
        curr_form = str_util::rtrim(curr_form) + "\n";
        if (ref.token) {
          curr_form += str_util::repeat(curr_node.refs.at(0).token.value().length() + 2, " ");
        }
      }
    } else if (!curr_node.metadata.is_root) {
      if (i > 0) {
        // Trim the current form before applying a new-line
        curr_form = str_util::rtrim(curr_form) + "\n";
        curr_form += str_util::repeat(tree_depth, " ");
      }
    }
    if (ref.token) {
      curr_form += ref.token.value() + " ";
    } else {
      if (!curr_node.metadata.is_root && curr_node.metadata.multiple_elements_first_line) {
        // align returned form's lines with this forms lines
        // TODO - kinda unsafe
        curr_form += align_form(apply_formatting(ref, "", tree_depth + 1),
                                curr_node.refs.at(0).token.value().length() + 2);
      } else {
        curr_form += apply_formatting(ref, "", tree_depth + 1);
      }
    }
    if (curr_node.metadata.is_root && i < curr_node.refs.size() - 1) {
      curr_form += "\n\n";
    }
  }
  if (!curr_node.metadata.is_root) {
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
