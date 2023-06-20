#include "formatting_rules.h"

#include <set>

#include "common/util/string_util.h"

#include "third-party/fmt/core.h"

namespace formatter_rules {
namespace blank_lines {
void separate_by_newline(std::string& curr_text,
                         const FormatterTreeNode& containing_node,
                         const FormatterTreeNode& node,
                         const int index) {
  // We only are concerned with top level forms or elements
  // Skip the last element, no trailing new-lines (let the editors handle this!)
  // Also peek ahead to see if there was a comment on this line, if so don't separate things!
  if (!containing_node.metadata.is_top_level || index >= containing_node.refs.size() - 1 ||
      (containing_node.refs.at(index + 1).metadata.is_comment &&
       containing_node.refs.at(index + 1).metadata.is_inline)) {
    return;
  }
  curr_text += "\n";
  // If it's a comment, but has no following blank lines, dont insert a blank line
  if (node.metadata.is_comment && node.metadata.num_blank_lines_following == 0) {
    return;
  }
  // Otherwise, add only 1 blank line
  curr_text += "\n";
}
}  // namespace blank_lines

namespace comments {
std::string format_block_comment(const std::string& comment) {
  // Normalize block comments, remove any trailing or leading whitespace
  // Only allow annotations on the first line, like #|@file
  // Don't mess with internal indentation as the user might intend it to be a certain way.
  std::string new_comment = "";
  std::string comment_contents = "";
  bool seek_until_whitespace = str_util::starts_with(comment, "#|@");
  int chars_seeked = 0;
  for (const auto& c : comment) {
    if (c == '\n' || (seek_until_whitespace && (c == ' ' || c == '\t')) ||
        (!seek_until_whitespace && (c != '#' && c != '|'))) {
      break;
    }
    chars_seeked++;
    new_comment += c;
  }
  // Remove the first line content and any leading whitespace
  comment_contents = str_util::ltrim_newlines(comment.substr(chars_seeked));
  // Remove trailing whitespace
  comment_contents = str_util::rtrim(comment_contents);
  // remove |#
  // TODO - check suffix
  comment_contents.pop_back();
  comment_contents.pop_back();
  comment_contents = str_util::rtrim(comment_contents);
  new_comment += fmt::format("\n{}\n|#", comment_contents);
  return new_comment;
}
}  // namespace comments

namespace constant_pairs {
// TODO - probably need to include quoted literals as well, though the grammar currently does not
// differentiate between a quoted symbol and a quoted form
const std::set<std::string> constant_pair_types = {"kwd_lit",  "num_lit",  "str_lit", "char_lit",
                                                   "null_lit", "bool_lit", "sym_lit"};

bool is_element_second_in_constant_pair(const FormatterTreeNode& containing_node,
                                        const FormatterTreeNode& node,
                                        const int index) {
  if (containing_node.refs.empty() || index == 0) {
    return false;
  }
  // Ensure that a keyword came before hand
  if (containing_node.refs.at(index - 1).metadata.node_type != "kwd_lit") {
    return false;
  }
  // Check the type of the element
  if (constant_pair_types.find(node.metadata.node_type) != constant_pair_types.end()) {
    return true;
  }
  return false;
}
}  // namespace constant_pairs

namespace indent {
void append_newline(std::string& curr_text,
                    const FormatterTreeNode& node,
                    const FormatterTreeNode& containing_node,
                    const int depth,
                    const int index) {
  if (index <= 0 && !containing_node.metadata.multiple_elements_first_line ||
      index <= 1 && containing_node.metadata.multiple_elements_first_line ||
      containing_node.metadata.is_top_level ||
      (node.metadata.is_comment && node.metadata.is_inline)) {
    return;
  }
  // Check if it's a constant pair
  if (constant_pairs::is_element_second_in_constant_pair(containing_node, node, index)) {
    return;
  }
  curr_text = str_util::rtrim(curr_text) + "\n";
}

void flow_line(std::string& curr_text,
               const FormatterTreeNode& node,
               const FormatterTreeNode& containing_node,
               const int depth,
               const int index) {
  if (node.metadata.is_top_level) {
    return;
  }
  // If the element is the second element in a constant pair, that means we did not append a
  // new-line before hand so we require no indentation (it's inline with the previous element)
  if (constant_pairs::is_element_second_in_constant_pair(containing_node, node, index)) {
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

void hang_lines(std::string& text,
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
}  // namespace indent

}  // namespace formatter_rules
