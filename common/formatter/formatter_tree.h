#pragma once

#include <optional>
#include <string>
#include <vector>

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
// The treesitter format is complicated and highly nested, leading to some very hard to understand
// code. So my solution is a 2-pass format.
//
// Pass 1 - convert the AST into a simplified FormatterTree
// Pass 2 - use the simplified tree to output the final code

// A FormatterTree has a very simple and crude tree structure where:
// Nodes are essentially forms, which contain in-order tokens or references to nested forms
// Nodes can have associated metadata, often related to their context in the original code
class FormatterTree {
 public:
  struct NodeMetadata {
    bool is_root = false;
    // Whether the form had more than 1 element on the first line
    // (println
    //  "test")
    // vs
    // (println "test")
    bool multiple_elements_first_line;
  };

  class Node {
   public:
    std::vector<Node> refs;
    NodeMetadata metadata;
    // The token is optional because list nodes do not contain a token, they just contain a bunch of
    // eventually token node refs
    std::optional<std::string> token;

    Node() = default;
    Node(const std::string& _token) : token(_token){};
    Node(const NodeMetadata& _metadata) : metadata(_metadata){};
  };

  FormatterTree(const std::string& source, const TSNode& root_node);
  Node root;

 private:
  void construct_formatter_tree_recursive(const std::string& source,
                                          TSNode curr_node,
                                          Node& tree_node);
};
