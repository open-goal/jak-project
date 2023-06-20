#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "formatting_rules.h"

#include "tree_sitter/api.h"

class IndentationRule;

// Treesitter is fantastic for validating and parsing our code into a structured tree format without
// whitespace so we can do that ourselves (formatting) However, the treesitter AST is a bit too
// detailed for purposes of formatting.
//
// When formatting there is no need to know things like are we
// in a function, or a symbol, etc. This extra information is fantastic for parsing or manipulating
// the language, but becomes burdensome when just trying to write elegant formatting code when all
// we really care about is:
// - getting all the text tokens for the source code
// - having them in a proper, nested format
// The treesitter format is complicated and highly nested, leading to some very hard to understand
// code. So my solution is a 2-pass format.
//
// Pass 1 - convert the AST into a simplified FormatterTree
// Pass 2 - use the simplified tree to output the final code

namespace formatter {
extern const std::shared_ptr<IndentationRule> default_indentation_rule;
}

class FormatterTreeNode {
 public:
  struct Metadata {
    std::string node_type;
    bool is_top_level = false;
    bool is_comment = false;
    bool is_inline = false;
    int num_blank_lines_following = 0;
    // Whether the form had more than 1 element on the first line
    // (println
    //  "test")
    // vs
    // (println "test")
    bool multiple_elements_first_line = false;
    bool was_on_first_line_of_form = false;
  };
  std::vector<FormatterTreeNode> refs;
  Metadata metadata;
  // The token is optional because list nodes do not contain a token, they just contain a bunch of
  // eventually token node refs
  std::optional<std::string> token;
  std::vector<std::shared_ptr<IndentationRule>> rules = {};

  FormatterTreeNode() = default;
  FormatterTreeNode(const std::string& source, const TSNode& node);
  FormatterTreeNode(const Metadata& _metadata) : metadata(_metadata){};

  // Considers the input to determine the most relevant formatting rule for the given node
  // if none are applicable, returns `formatter::default_rule`
  std::shared_ptr<IndentationRule> get_formatting_rule(const int depth, const int index) const;
};

// A FormatterTree has a very simple and crude tree structure where:
// - Nodes are essentially forms, which contain in-order tokens or references to nested forms
// - Nodes can have associated metadata, often related to their context in the original code
// - Nodes can also have multiple formatting rules associated with them.  Often this is the default
// rule or based on pre-configured overrides due to the head of the form, ex. 'defun'
class FormatterTree {
 public:
  FormatterTree(const std::string& source, const TSNode& root_node);
  FormatterTreeNode root;

 private:
  void construct_formatter_tree_recursive(const std::string& source,
                                          TSNode curr_node,
                                          FormatterTreeNode& tree_node);
};
