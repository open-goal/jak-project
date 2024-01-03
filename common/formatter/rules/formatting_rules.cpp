#include "formatting_rules.h"

#include <set>

#include "rule_config.h"

#include "common/util/string_util.h"

#include "third-party/fmt/core.h"

namespace formatter_rules {

// TODO - probably need to include quoted literals as well, though the grammar currently does not
// differentiate between a quoted symbol and a quoted form
const std::set<std::string> constant_types = {"kwd_lit",  "num_lit",  "str_lit",
                                              "char_lit", "null_lit", "bool_lit"};

namespace constant_list {
bool is_constant_list(const FormatterTreeNode& node) {
  if (!node.is_list() || node.refs.empty()) {
    return false;
  }
  const auto& type = node.refs.at(0).metadata.node_type;
  return constant_types.find(type) != constant_types.end();
}
}  // namespace constant_list

namespace blank_lines {

bool should_insert_blank_line(const FormatterTreeNode& containing_node,
                              const FormatterTreeNode& node,
                              const int index) {
  // We only do this at the top level and don't leave a trailing new-line
  if (!containing_node.metadata.is_top_level || index >= (int)containing_node.refs.size() - 1) {
    return false;
  }
  // If it's a comment, but has no following blank lines, dont insert a blank line
  if (node.metadata.is_comment && node.metadata.num_blank_lines_following == 0) {
    return false;
  }
  // If the next form is a comment and is inline, don't insert a comment
  if ((index + 1) < (int)containing_node.refs.size() &&
      containing_node.refs.at(index + 1).metadata.is_comment &&
      containing_node.refs.at(index + 1).metadata.is_inline) {
    return false;
  }

  // TODO - only if the form doesn't fit on a single line
  return true;
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

// TODO - remove index, not needed, could just pass in the previous node
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
  return true;
}

bool form_should_be_constant_paired(const FormatterTreeNode& node) {
  // Criteria for a list to be constant paired:
  // - needs to start with a non-symbol
  // - needs atleast the minimum amount of pairs, so 2 pairs can still be inlined
  if (node.refs.empty()) {
    return false;
  }
  int num_pairs = 0;
  for (int i = 0; i < (int)node.refs.size() - 1; i++) {
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

}  // namespace formatter_rules
