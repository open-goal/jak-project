#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "common/util/FileUtil.h"

namespace demacro {

struct Rule {
  std::string name;
  std::vector<std::string> match;
  std::vector<std::string> rewrite;
  std::unordered_map<std::string, std::string> capture_types;
};

struct RuleSet {
  std::vector<Rule> rules;
};

struct RuleStat {
  std::string name;
  int rewrites = 0;
};

struct RewriteResult {
  std::string source;
  std::vector<RuleStat> stats;

  int rewrite_count() const;
};

RuleSet parse_rules(const std::string& contents, const std::string& source_name = "<rules>");
RuleSet load_rules(const fs::path& path);

RewriteResult rewrite(const std::string& source, const RuleSet& rules);
RewriteResult rewrite(const std::string& source, const fs::path& rule_path);

}  // namespace demacro
