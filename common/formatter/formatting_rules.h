#pragma once

#include <string>

#include "formatter_tree.h"

class FormatterTreeNode;

namespace formatter_rules {
// The formatter will try to collapse as much space as possible in the top-level, this means
// separating forms by a single empty blank line
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

// TODO - nothing here yet, in the future:
// - align consecutive comment lines
// - if/when the formatter is concerned with line length, there are implications here
//
// Reference - https://github.com/kkinnear/zprint/blob/main/doc/options/comments.md
namespace comments {}

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
}  // namespace constant_pairs
}  // namespace formatter_rules

// Indentation rules are heavily inspired by the descriptions here
// https://github.com/weavejester/cljfmt/blob/master/docs/INDENTS.md
//
// This style of formatting assumes the code coming in is already reasonably formatted, aka was
// written by an actual human and isn't just a single line of minified code
//
// If it _is_ though, the formatting rules will still be able to do a somewhat decent job, as
// certain forms are configured to format a specific way, but it probably won't be broadly
// consistent with code written normally
//
// cljfmt observations:
// - a form that starts on the first line but spans multiple lines (it doesn't really handle this
// well) ex. (println (hello
//           world) ye) ;; you'd expect the 'ye' to be aligned with `(h...`
// - vector lists are treated differently from paren lists (seems to leave them inline or default
// indent them if they span multiple lines) ex. [hello world
//  what]

// The default rule that is used if no other rule applies to the given form
//
// For lists this will format like so:
// - If the only element on the first line is the head of the form, every subsequent element is
// indented with 1 space on a new line
// (println   ; <= one or fewer elements on first line
//  "hello"
//  "world")
// - otherwise, every element after the 2nd is on a new line and aligned to that 1st list arg.
// (println "hello"   ; <= more than one element on first line
//          "world")
class IndentationRule {
 public:
  virtual ~IndentationRule() = default;
  virtual void append_newline(std::string& curr_text,
                              const FormatterTreeNode& node,
                              const FormatterTreeNode& containing_node,
                              const int depth,
                              const int index);
  virtual void indent_token(std::string& curr_text,
                            const FormatterTreeNode& node,
                            const FormatterTreeNode& containing_node,
                            const int depth,
                            const int index);
  virtual void align_form_lines(std::string& text,
                                const FormatterTreeNode& node,
                                const FormatterTreeNode& containing_node);
};

// Inner indentation always indents by 2 spaces for every line after the first regardless of the
// number of elements `depth` defines at what depth the rule should be applied, and optionally
// `index` narrows this down further to a given index at that depth
//
// Some simple examples:
// (defn greet [name]
//   (println "Hello" name))
//
// (defn dismiss
//   [name]
//   (println "Goodbye" name))
class InnerIndentationRule : public IndentationRule {
 private:
  int m_depth;
  std::optional<int> m_index;

 public:
  InnerIndentationRule(int depth) : m_depth(depth){};
  InnerIndentationRule(int depth, int index) : m_depth(depth), m_index(index){};
  virtual void append_newline(std::string& curr_text,
                              const FormatterTreeNode& node,
                              const FormatterTreeNode& containing_node,
                              const int depth,
                              const int index) override;
  virtual void indent_token(std::string& curr_text,
                            const FormatterTreeNode& node,
                            const FormatterTreeNode& containing_node,
                            const int depth,
                            const int index) override;
  void align_form_lines(std::string& text,
                        const FormatterTreeNode& node,
                        const FormatterTreeNode& containing_node) override;
};
