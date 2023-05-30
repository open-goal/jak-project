#pragma once

#include <string>

#include "formatter_tree.h"

class FormatterTreeNode;

// Indentation rules are heavily inspired by the descriptions here
// https://github.com/weavejester/cljfmt/blob/master/docs/INDENTS.md This style of formatting
// assumes the code coming in is already reasonably formatted, aka was written by an actual human
// and isn't just a single line of minified code
//
// If it _is_ though, the formatting rules will still be able to do a somewhat decent job, as
// certain forms are configured to format a specific way, but it probably won't be broadly
// consistent with code written normally

// Used if no other formatting rule applies to the given form
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
  virtual void newline_and_indent_element(std::string& curr_text,
                                          const FormatterTreeNode& containing_node,
                                          const int depth,
                                          const int index,
                                          const bool for_token);
  virtual void align_form_lines(std::string& text, const FormatterTreeNode& containing_node);
};
