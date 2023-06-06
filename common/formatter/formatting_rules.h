#pragma once

#include <string>

#include "formatter_tree.h"

class FormatterTreeNode;

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
class FormattingRule {
 public:
  virtual ~FormattingRule() = default;
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
class InnerFormattingRule : public FormattingRule {
 private:
  int m_depth;
  std::optional<int> m_index;

 public:
  InnerFormattingRule(int depth) : m_depth(depth){};
  InnerFormattingRule(int depth, int index) : m_depth(depth), m_index(index){};
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
