#include "formatting_rules.h"

#include "common/util/string_util.h"

void FormattingRule::append_newline(std::string& curr_text,
                                    const FormatterTreeNode& node,
                                    const FormatterTreeNode& containing_node,
                                    const int depth,
                                    const int index) {
  if (index <= 0 && !containing_node.metadata.multiple_elements_first_line ||
      index <= 1 && containing_node.metadata.multiple_elements_first_line) {
    return;
  }
  curr_text = str_util::rtrim(curr_text) + "\n";
}

void FormattingRule::indent_token(std::string& curr_text,
                                  const FormatterTreeNode& node,
                                  const FormatterTreeNode& containing_node,
                                  const int depth,
                                  const int index) {
  if (node.metadata.is_root) {
    return;
  }
  if (containing_node.metadata.multiple_elements_first_line) {
    if (index > 1) {
      // Only apply indentation if we are about to print a normal text token
      // TODO - unsafe
      if (node.token.has_value()) {
        curr_text += str_util::repeat(containing_node.refs.at(0).token.value().length() + 2, " ");
      }
    }
  } else {
    if (index > 0) {
      curr_text += str_util::repeat(depth, " ");
    }
  }
}

void FormattingRule::align_form_lines(std::string& text,
                                      const FormatterTreeNode& node,
                                      const FormatterTreeNode& containing_node) {
  const auto lines = str_util::split(text);
  // TODO - unsafe (breaks on a list of lists)
  int alignment_width = 1;
  if (containing_node.metadata.multiple_elements_first_line) {
    alignment_width = containing_node.refs.at(0).token.value().length() + 2;
  }
  std::string aligned_form = "";
  for (int i = 0; i < lines.size(); i++) {
    aligned_form += str_util::repeat(alignment_width, " ") + lines.at(i);
    if (i != lines.size() - 1) {
      aligned_form += "\n";
    }
  }
  text = aligned_form;
}

void InnerFormattingRule::append_newline(std::string& curr_text,
                                         const FormatterTreeNode& node,
                                         const FormatterTreeNode& containing_node,
                                         const int depth,
                                         const int index) {
  if (index < 1 || (m_depth != depth || m_index && m_index.value() != index)) {
    return;
  }
  if (!node.metadata.was_on_first_line_of_form) {
    curr_text = str_util::rtrim(curr_text) + "\n";
  }
}

void InnerFormattingRule::indent_token(std::string& curr_text,
                                       const FormatterTreeNode& node,
                                       const FormatterTreeNode& containing_node,
                                       const int depth,
                                       const int index) {
  if (index < 1 || (m_depth != depth || m_index && m_index.value() != index)) {
    return;
  }
  // We only new-line elements if they were not originally on the first line
  if (!node.metadata.was_on_first_line_of_form) {
    curr_text += str_util::repeat(depth * 2, " ");
  }
}

void InnerFormattingRule::align_form_lines(std::string& text,
                                           const FormatterTreeNode& node,
                                           const FormatterTreeNode& containing_node) {
  if (node.metadata.was_on_first_line_of_form) {
    return;
  }
  const auto lines = str_util::split(text);
  // TODO - unsafe (breaks on a list of lists)
  int alignment_width = 2;
  std::string aligned_form = "";
  for (int i = 0; i < lines.size(); i++) {
    aligned_form += str_util::repeat(alignment_width, " ") + lines.at(i);
    if (i != lines.size() - 1) {
      aligned_form += "\n";
    }
  }
  text = aligned_form;
  return;
}
