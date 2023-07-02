#pragma once

#include <optional>
#include <string>

#include "common/formatter/rules/formatting_rules.h"
#include "common/formatter/rules/rule_config.h"

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
