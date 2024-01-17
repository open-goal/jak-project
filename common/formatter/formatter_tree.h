#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "rules/rule_config.h"
#include "tree_sitter/api.h"

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
//
// TLDR - The treesitter format is complicated and highly nested, leading to some very hard to
// understand code. So my solution is atleast a 2-pass format.
//
// Pass 1 - convert the AST into a simplified FormatterTree
// Pass 2 - use the simplified tree to output the final code

class FormatterTreeNode {
 public:
  struct Metadata {
    std::string node_type;
    bool is_top_level = false;
    bool is_comment = false;
    bool is_inline = false;
    int num_blank_lines_following = 0;
    bool is_binding_list = false;  // TODO set this
  };
  std::vector<FormatterTreeNode> refs;
  Metadata metadata;
  // The token is optional because list nodes do not contain a token, they just contain a bunch of
  // eventually-containing token node refs
  std::optional<std::string> token;
  std::optional<std::string> node_prefix;

  formatter_rules::config::FormFormattingConfig formatting_config;

  FormatterTreeNode() = default;
  FormatterTreeNode(const std::string& source, const TSNode& node);
  FormatterTreeNode(const Metadata& _metadata) : metadata(_metadata){};

  bool is_list() const { return !token.has_value(); }
  std::string token_str() const {
    if (node_prefix && token) {
      return node_prefix.value() + token.value();
    }
    if (token) {
      return token.value();
    }
    return "";
  }
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
                                          FormatterTreeNode& tree_node,
                                          std::optional<std::string> node_prefix = {});
};
