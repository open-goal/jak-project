#pragma once

#include <string>

#include "common/formatter/formatter_tree.h"

namespace formatter_rules {
// The formatter will try to collapse as much space as possible in the top-level, this means
// separating forms by a single empty  blank line
//
// The exception are comments, top level comments will retain their following blank lines from the
// original source
// - this could be none, in the case where a comment is directly next to a form (like this one!)
//   - you don't want them to be separated!
// - or this could be top level comments / comment blocks documenting things but not being
// associated with a form
//   - in this case, you want them to remain separated
//
// Reference - https://github.com/kkinnear/zprint/blob/main/doc/options/blank.md
namespace blank_lines {
void separate_by_newline(std::string& curr_text,
                         const FormatterTreeNode& containing_node,
                         const FormatterTreeNode& node,
                         const int index);
}

// TODO:
// - align consecutive comment lines
// - if/when the formatter is concerned with line length, there are implications here
//
// Reference - https://github.com/kkinnear/zprint/blob/main/doc/options/comments.md
namespace comments {
std::string format_block_comment(const std::string& comment);
}

// Paired elements in a list will be kept in-line rather than the default new-line indentation
// For example:
// (:msg "hello world" :delay 100 :fn (lambda () (+ 1 1)))
// Would typically become:
// (:msg
//  "hello world"
//  :delay
//  100
//  :fn
//    (lambda ()
//      (+ 1 1)))
// But with constant pairs:
// (:msg "hello world"
//  :delay 100
//  :fn
//    (lambda ()
//      (+ 1 1)))
//
// Reference - https://github.com/kkinnear/zprint/blob/main/doc/options/constantpairs.md
namespace constant_pairs {
const static int min_pair_amount = 4;

// Determines if the given element is the second element in a constant pair, if it is then we would
// usually want to elide the new-line in whatever code that applies it
//
// This is true if:
// - the element is in a list
// - the element is preceeded by a keyword
// - the element is a:
//   - keyword, symbol, string, number, or boolean
bool is_element_second_in_constant_pair(const FormatterTreeNode& containing_node,
                                        const FormatterTreeNode& node,
                                        const int index);
bool form_should_be_constant_paired(const FormatterTreeNode& node);
}  // namespace constant_pairs

// There are two main types of indentations "flow"s and "hang"s
// (this is
//       a
//       hang)
// (this
//   is
//   a
//   flow)
//
// `flow` is the default indentation mode and right now the determination between the two is down to
// manual configuration based on the head of the form. In general, we only `flow` or `hang` if the
// form cannot fit on the current line.
//
// Additionally, if the head of the form is a constant we `flow` with an indent of `1` instead of
// `2`
//
// By default, we always hang unless:
// - the head-form overrides the configuration
// - TODO it will use more lines than the flow approach
//
// Reference - https://github.com/kkinnear/zprint/blob/main/doc/options/indent.md
namespace indent {
const static int line_width_target = 120;

bool form_can_be_inlined(const std::string& curr_text, const FormatterTreeNode& curr_node);

// TODO - right now this is very primitive in that it only checks against our hard-coded config
// eventually make this explore both routes and determine which is best
// Also factor in distance from the gutter (theres some zprint rationale somewhere on this)
bool should_form_flow(const FormatterTreeNode& list_node, const bool inlining_form);
std::optional<bool> inline_form_element(const FormatterTreeNode& list_node, const int index);

void append_newline(std::string& curr_text,
                    const FormatterTreeNode& node,
                    const FormatterTreeNode& containing_node,
                    const int index,
                    const bool flowing,
                    const bool constant_pair_form,
                    const bool force_newline);
void indent_line(std::string& curr_text,
                 const FormatterTreeNode& node,
                 const FormatterTreeNode& containing_node,
                 const int depth,
                 const int index,
                 const bool flowing);
void align_lines(std::string& text,
                 const FormatterTreeNode& node,
                 const FormatterTreeNode& containing_node,
                 const bool constant_pair_form,
                 const bool flowing,
                 const bool force_flow,
                 const bool inline_element);

}  // namespace indent

// Let forms fall into two main categories
// - Ones that can be entirely inlined
// - Ones that cannot
//
// Single line let forms:
// - If there is a single binding, let it continue to be one line
// - otherwise, flow it just as a multi-line let would be done
//
// For multi-line let forms, the difficulty is how to format the binding:
// - align the bindings to within the binding list (single indent space)
// - always format the binding value on the same line as the binding, no new lines
// - forms inside the let binding are flowed
//
// Reference - https://github.com/kkinnear/zprint/blob/main/doc/options/let.md
namespace let {
// TODO - like above, factor in current cursor position
bool can_be_inlined(const FormatterTreeNode& form);
}  // namespace let
}  // namespace formatter_rules
