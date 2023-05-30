#include "formatting_rules.h"

#include "common/util/string_util.h"

void FormattingRule::newline_and_indent_element(std::string& curr_text,
                                                const FormatterTreeNode& containing_node,
                                                const int depth,
                                                const int index,
                                                const bool for_token) {
  if (containing_node.metadata.multiple_elements_first_line) {
    if (index > 1) {
      // TODO - kinda unsafe
      // Trim the current form before applying a new-line
      curr_text = str_util::rtrim(curr_text) + "\n";
      // Only apply indentation if we are about to print a normal text token
      if (for_token) {
        curr_text += str_util::repeat(containing_node.refs.at(0).token.value().length() + 2, " ");
      }
    }
  } else {
    if (index > 0) {
      // Trim the current form before applying a new-line
      curr_text = str_util::rtrim(curr_text) + "\n";
      curr_text += str_util::repeat(depth, " ");
    }
  }
}

void FormattingRule::align_form_lines(std::string& text, const FormatterTreeNode& containing_node) {
  const auto lines = str_util::split(text);
  // TODO - sketchy
  const int alignment_width = containing_node.refs.at(0).token.value().length() + 2;
  std::string aligned_form = "";
  for (int i = 0; i < lines.size(); i++) {
    aligned_form += str_util::repeat(alignment_width, " ") + lines.at(i);
    if (i != lines.size() - 1) {
      aligned_form += "\n";
    }
  }
  text = aligned_form;
}
