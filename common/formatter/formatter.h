#pragma once

#include <optional>
#include <string>

#include "formatting_rules.h"

#include "tree_sitter/api.h"

// TODO:
// - Considering _eventually_ adding line-length heuristics
namespace formatter {

struct TreeSitterParserDeleter {
  void operator()(TSParser* ptr) const { ts_parser_delete(ptr); }
};

struct TreeSitterTreeDeleter {
  void operator()(TSTree* ptr) const { ts_tree_delete(ptr); }
};

std::optional<std::string> format_code(const std::string& source);
}  // namespace formatter
