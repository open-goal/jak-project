#include "formatting_rules.h"

#include <set>

#include "common/util/string_util.h"

#include "third-party/fmt/core.h"

namespace formatter_rules {

// TODO - probably need to include quoted literals as well, though the grammar currently does not
// differentiate between a quoted symbol and a quoted form
const std::set<std::string> constant_types = {"kwd_lit",  "num_lit",  "str_lit",
                                              "char_lit", "null_lit", "bool_lit"};
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

bool is_element_second_in_constant_pair(const FormatterTreeNode& containing_node,
                                        const FormatterTreeNode& node,
                                        const int index) {
  if (containing_node.refs.empty() || index == 0) {
    return false;
  }
  // Ensure that a keyword came before hand
  if (containing_node.refs.at(index - 1).metadata.node_type != "kwd_lit") {
    return false;
  } else if (node.metadata.node_type == "kwd_lit") {
    // NOTE - there is ambiugity here which cannot be totally solved (i think?)
    // if the element itself is also a keyword, assume this is two adjacent keywords and they should
    // not be paired
    return false;
  }
  // Check the type of the element
  if (constant_types.find(node.metadata.node_type) != constant_types.end()) {
    return true;
  }
  return false;
}

bool form_should_be_constant_paired(const FormatterTreeNode& node) {
  // Criteria for a list to be constant paired:
  // - needs to start with a non-symbol
  // - needs atleast the minimum amount of pairs, so 2 pairs can still be inlined
  if (node.refs.empty()) {
    return false;
  }
  int num_pairs = 0;
  for (int i = 0; i < node.refs.size() - 1; i++) {
    const auto& ref = node.refs.at(i);
    const auto& next_ref = node.refs.at(i + 1);
    if (ref.token && next_ref.token) {
      // If the first element a keyword and the following item is a constant, it's a pair
      // move forward one extra index
      if (ref.metadata.node_type == "kwd_lit" &&
          constant_types.find(next_ref.metadata.node_type) != constant_types.end()) {
        num_pairs++;
        i++;
      }
    }
  }
  return num_pairs >= min_pair_amount;
}

}  // namespace constant_pairs

namespace indent {

int cursor_pos(const std::string& curr_text) {
  if (curr_text.empty()) {
    return 0;
  }
  // Get the last line of the text (which is also the line we are on!)
  int pos = 0;
  for (int i = curr_text.size() - 1; i >= 0; i--) {
    const auto& c = curr_text.at(i);
    if (c == '\n') {
      break;
    }
    pos++;
  }
  return pos;
}

int compute_form_width_after_index(const FormatterTreeNode& node,
                                   const int index,
                                   const int depth = 0) {
  if (node.refs.empty()) {
    if (node.token) {
      return node.token->size();
    } else {
      return 0;
    }
  }
  int form_width = 0;
  for (int i = 0; i < node.refs.size(); i++) {
    const auto& ref = node.refs.at(i);
    if (depth == 0 && i < index) {
      continue;
    }
    if (ref.token) {
      form_width += ref.token->size() + 1;
    } else {
      form_width += compute_form_width_after_index(ref, index, depth + 1) + 1;
    }
  }
  return form_width;
}

bool form_exceed_line_width(const std::string& curr_text,
                            const FormatterTreeNode& containing_node,
                            const int index) {
  // Compute length from the current cursor position on the line as this check is done for every
  // element of the form and not in advance
  //
  // This is for a good reason, intermediate nodes may override this styling and force to be
  // formatted inline
  //
  // We early out as soon as we exceed the width
  int curr_line_pos = cursor_pos(curr_text);
  if (curr_line_pos >= line_width_target) {
    return true;
  }
  int remaining_width_required = compute_form_width_after_index(containing_node, index);
  if (curr_line_pos + remaining_width_required >= line_width_target) {
    return true;
  }
  return false;
}

bool form_contains_comment(const FormatterTreeNode& node) {
  if (node.metadata.is_comment) {
    return true;
  }
  for (const auto& ref : node.refs) {
    if (ref.metadata.is_comment) {
      return true;
    } else if (!node.refs.empty()) {
      if (form_contains_comment(ref)) {
        return true;
      }
    }
  }
  return false;
}

bool form_can_be_inlined(std::string& curr_text, const FormatterTreeNode& node) {
  // Two main checks:
  // - first, is the form too long to fit on a line TODO - increase accuracy here
  if (form_exceed_line_width(curr_text, node, 0)) {
    return false;
  }
  // - second, are there any comments? (inlined or not, doesn't matter)
  if (form_contains_comment(node)) {
    return false;
  }
  return true;
}

void append_newline(std::string& curr_text,
                    const FormatterTreeNode& node,
                    const FormatterTreeNode& containing_node,
                    const int depth,
                    const int index,
                    const bool constant_pair_form) {
  if (index <= 0 || containing_node.metadata.is_top_level ||
      (node.metadata.is_comment && node.metadata.is_inline)) {
    return;
  }
  // Check if it's a constant pair
  if (constant_pair_form &&
      constant_pairs::is_element_second_in_constant_pair(containing_node, node, index)) {
    return;
  }
  curr_text = str_util::rtrim(curr_text) + "\n";
}

void flow_line(std::string& curr_text,
               const FormatterTreeNode& node,
               const FormatterTreeNode& containing_node,
               const int depth,
               const int index) {
  if (node.metadata.is_top_level || (node.metadata.is_inline && node.metadata.is_comment)) {
    return;
  }
  // If the element is the second element in a constant pair, that means we did not append a
  // new-line before hand so we require no indentation (it's inline with the previous element)
  if (constant_pairs::is_element_second_in_constant_pair(containing_node, node, index)) {
    return;
  }
  if (index > 0) {
    // If the first element in the list is a constant, we only indent with 1 space instead
    if (constant_types.find(containing_node.refs.at(0).metadata.node_type) !=
        constant_types.end()) {
      curr_text += str_util::repeat(depth, " ");
    } else {
      curr_text += str_util::repeat(depth, "  ");
    }
  }
}

void hang_lines(std::string& text,
                const FormatterTreeNode& node,
                const FormatterTreeNode& containing_node,
                const bool constant_pair_form) {
  const auto lines = str_util::split(text);
  // TODO - unsafe (breaks on a list of lists)
  int alignment_width = 2;
  if (constant_pair_form &&
      constant_types.find(containing_node.refs.at(0).metadata.node_type) != constant_types.end()) {
    alignment_width = 3;
  }
  // TODO - implement hanging
  // always hang unless flowing is "better" (this is the hard part)
  /*else if (containing_node.metadata.multiple_elements_first_line) {
    alignment_width = containing_node.refs.at(0).token.value().length() + 2;
  }*/
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
