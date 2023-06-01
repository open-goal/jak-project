#include "formatting_rules.h"

#include "common/util/string_util.h"

void FormattingRule::newline_and_indent_element(std::string& curr_text,
                                                const FormatterTreeNode& node,
                                                const FormatterTreeNode& containing_node,
                                                const int depth,
                                                const int index) {
  if (containing_node.metadata.multiple_elements_first_line) {
    if (index > 1) {
      // TODO - unsafe (breaks on a list of lists)
      // Trim the current form before applying a new-line
      curr_text = str_util::rtrim(curr_text) + "\n";
      // Only apply indentation if we are about to print a normal text token
      if (node.token.has_value()) {
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
  // TODO - unsafe (breaks on a list of lists)
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

void InnerFormattingRule::newline_and_indent_element(std::string& curr_text,
                                                     const FormatterTreeNode& node,
                                                     const FormatterTreeNode& containing_node,
                                                     const int depth,
                                                     const int index) {
  // TODO - not sure if this will always remain, but the first index has no indentation (next to the
  // form)
  if (index < 1) {
    return;
  }
  if (m_depth != depth || m_index && m_index.value() != index) {
    // do nothing, the rule doesn't apply at this depth or index
    return;
  }
  // We only new-line elements if they were not originally on the first line
  if (!node.metadata.was_on_first_line_of_form) {
    curr_text = str_util::rtrim(curr_text) + "\n" + str_util::repeat(depth * 2, " ");
  }
}

void InnerFormattingRule::align_form_lines(std::string& text,
                                           const FormatterTreeNode& containing_node) {
  // do nothing, this rule does not care about alignment at all
  return;
}
