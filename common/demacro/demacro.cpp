#include "demacro.h"

#include <algorithm>
#include <memory>
#include <mutex>
#include <optional>
#include <stdexcept>
#include <unordered_map>

#include "common/formatter/formatter.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"
#include "common/util/string_util.h"

#include "fmt/format.h"
#include "third-party/json.hpp"
#include "tree_sitter/api.h"

extern "C" {
extern const TSLanguage* tree_sitter_opengoal();
}

namespace demacro {
namespace {

struct Node {
  bool is_list = false;
  uint32_t start = 0;
  uint32_t end = 0;
  std::string atom;
  std::vector<Node> children;
};

struct Comment {
  uint32_t start = 0;
  uint32_t end = 0;
};

struct ParsedSource {
  Node root;
  std::vector<Comment> comments;
};

struct Captures {
  std::unordered_map<std::string, const Node*> single;
  std::unordered_map<std::string, std::vector<const Node*>> sequence;
};

struct CompiledRule {
  std::string name;
  std::vector<Node> match;
  std::vector<Node> rewrite;
  size_t index = 0;
};

struct Candidate {
  uint32_t start = 0;
  uint32_t end = 0;
  size_t rule_index = 0;
  Captures captures;
};

struct Edit {
  uint32_t start = 0;
  uint32_t end = 0;
  size_t rule_index = 0;
  std::string replacement;
};

bool is_comment_node(const TSNode& node) {
  const std::string_view type = ts_node_type(node);
  return type == "comment" || type == "block_comment";
}

bool is_gap_node(const TSNode& node) {
  const std::string_view type = ts_node_type(node);
  return type == "(" || type == ")" || type == "_ws" || type == "ERROR" ||
         is_comment_node(node);
}

std::string node_text(const std::string& source, const TSNode& node) {
  const auto start = ts_node_start_byte(node);
  const auto end = ts_node_end_byte(node);
  return source.substr(start, end - start);
}

void collect_comments(const TSNode& node, std::vector<Comment>* comments) {
  if (is_comment_node(node)) {
    comments->push_back({ts_node_start_byte(node), ts_node_end_byte(node)});
    return;
  }
  for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
    collect_comments(ts_node_child(node, i), comments);
  }
}

std::optional<Node> convert_node(const std::string& source, const TSNode& node) {
  if (is_gap_node(node)) {
    return {};
  }

  Node result;
  result.start = ts_node_start_byte(node);
  result.end = ts_node_end_byte(node);

  const std::string_view type = ts_node_type(node);
  if (type == "list_lit" || type == "source") {
    result.is_list = true;
    for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
      auto child = convert_node(source, ts_node_child(node, i));
      if (child) {
        result.children.push_back(std::move(*child));
      }
    }
  } else {
    // Reader forms and strings are deliberately atomic. Demacro is interested in the shape of
    // ordinary lists, and retaining their exact spelling avoids damaging quoted data or strings.
    result.atom = node_text(source, node);
  }
  return result;
}

ParsedSource parse_source(const std::string& source, const std::string& source_name) {
  std::shared_ptr<TSParser> parser(ts_parser_new(), formatter::TreeSitterParserDeleter());
  ts_parser_set_language(parser.get(), tree_sitter_opengoal());
  std::shared_ptr<TSTree> tree(
      ts_parser_parse_string(parser.get(), nullptr, source.c_str(), source.size()),
      formatter::TreeSitterTreeDeleter());

  auto root = ts_tree_root_node(tree.get());
  if (ts_node_is_null(root) || ts_node_has_error(root)) {
    throw std::runtime_error(fmt::format("Unable to parse OpenGOAL forms in {}", source_name));
  }
  ParsedSource result;
  auto converted = convert_node(source, root);
  if (!converted) {
    throw std::runtime_error(
        fmt::format("OpenGOAL parser returned an empty tree for {}", source_name));
  }
  result.root = std::move(*converted);
  collect_comments(root, &result.comments);
  return result;
}

std::vector<std::string> json_string_or_array(const nlohmann::json& value,
                                              const std::string& rule_name,
                                              const std::string& field_name) {
  if (value.is_string()) {
    return {value.get<std::string>()};
  }
  if (value.is_array()) {
    std::vector<std::string> result;
    for (const auto& entry : value) {
      if (!entry.is_string()) {
        throw std::runtime_error(
            fmt::format("Demacro rule '{}' field '{}' must contain only strings", rule_name,
                        field_name));
      }
      result.push_back(entry.get<std::string>());
    }
    return result;
  }
  throw std::runtime_error(
      fmt::format("Demacro rule '{}' field '{}' must be a string or array", rule_name, field_name));
}

std::string substitute_row_values(
    std::string text,
    const std::unordered_map<std::string, std::string>& substitutions,
    const std::string& rule_name) {
  for (const auto& [key, value] : substitutions) {
    const auto placeholder = "{{" + key + "}}";
    size_t cursor = 0;
    while ((cursor = text.find(placeholder, cursor)) != std::string::npos) {
      text.replace(cursor, placeholder.size(), value);
      cursor += value.size();
    }
  }
  if (text.find("{{") != std::string::npos) {
    throw std::runtime_error(
        fmt::format("Demacro rule '{}' contains an unresolved table placeholder", rule_name));
  }
  return text;
}

Rule parse_rule(const nlohmann::json& entry,
                const std::unordered_map<std::string, std::string>& substitutions,
                const std::string& name_suffix) {
  Rule rule;
  const auto base_name = entry.at("name").get<std::string>();
  rule.name = substitute_row_values(base_name, substitutions, base_name) + name_suffix;
  rule.match = json_string_or_array(entry.at("match"), rule.name, "match");
  rule.rewrite = json_string_or_array(entry.at("rewrite"), rule.name, "rewrite");
  for (auto& form : rule.match) {
    form = substitute_row_values(std::move(form), substitutions, rule.name);
  }
  for (auto& form : rule.rewrite) {
    form = substitute_row_values(std::move(form), substitutions, rule.name);
  }
  return rule;
}

std::vector<Node> parse_form_sequence(const std::vector<std::string>& forms,
                                      const std::string& description) {
  std::vector<Node> result;
  for (const auto& form : forms) {
    auto parsed = parse_source(form, description);
    if (parsed.root.children.size() != 1) {
      throw std::runtime_error(
          fmt::format("{} must contain exactly one OpenGOAL form", description));
    }
    result.push_back(std::move(parsed.root.children.front()));
  }
  return result;
}

bool is_capture(const Node& node, std::string* name) {
  if (node.is_list || node.atom.size() < 2 || node.atom.front() != '$' || node.atom[1] == '*') {
    return false;
  }
  *name = node.atom.substr(1);
  return true;
}

bool is_sequence_capture(const Node& node, std::string* name) {
  if (node.is_list || node.atom.size() < 3 || !str_util::starts_with(node.atom, "$*")) {
    return false;
  }
  *name = node.atom.substr(2);
  return true;
}

bool structurally_equal(const Node& a, const Node& b) {
  if (a.is_list != b.is_list) {
    return false;
  }
  if (!a.is_list) {
    return a.atom == b.atom;
  }
  if (a.children.size() != b.children.size()) {
    return false;
  }
  for (size_t i = 0; i < a.children.size(); ++i) {
    if (!structurally_equal(a.children.at(i), b.children.at(i))) {
      return false;
    }
  }
  return true;
}

std::vector<Captures> match_node_all(const Node& pattern,
                                     const Node& input,
                                     const Captures& captures);

bool sequence_equal(const std::vector<const Node*>& a, const std::vector<const Node*>& b) {
  if (a.size() != b.size()) {
    return false;
  }
  for (size_t i = 0; i < a.size(); ++i) {
    if (!structurally_equal(*a.at(i), *b.at(i))) {
      return false;
    }
  }
  return true;
}

std::vector<Captures> match_children_all(const std::vector<Node>& patterns,
                                         size_t pattern_idx,
                                         const std::vector<Node>& inputs,
                                         size_t input_idx,
                                         const Captures& captures) {
  if (pattern_idx == patterns.size()) {
    return input_idx == inputs.size() ? std::vector<Captures>{captures}
                                      : std::vector<Captures>{};
  }

  std::string sequence_name;
  if (is_sequence_capture(patterns.at(pattern_idx), &sequence_name)) {
    std::vector<Captures> results;
    for (size_t count = 0; input_idx + count <= inputs.size(); ++count) {
      std::vector<const Node*> captured;
      captured.reserve(count);
      for (size_t i = 0; i < count; ++i) {
        captured.push_back(&inputs.at(input_idx + i));
      }

      auto next = captures;
      const auto previous = next.sequence.find(sequence_name);
      if (previous != next.sequence.end() && !sequence_equal(previous->second, captured)) {
        continue;
      }
      next.sequence[sequence_name] = std::move(captured);
      auto matches =
          match_children_all(patterns, pattern_idx + 1, inputs, input_idx + count, next);
      results.insert(results.end(), std::make_move_iterator(matches.begin()),
                     std::make_move_iterator(matches.end()));
    }
    return results;
  }

  if (input_idx == inputs.size()) {
    return {};
  }
  std::vector<Captures> results;
  for (auto& node_match :
       match_node_all(patterns.at(pattern_idx), inputs.at(input_idx), captures)) {
    auto matches =
        match_children_all(patterns, pattern_idx + 1, inputs, input_idx + 1, node_match);
    results.insert(results.end(), std::make_move_iterator(matches.begin()),
                   std::make_move_iterator(matches.end()));
  }
  return results;
}

std::vector<Captures> match_node_all(const Node& pattern,
                                     const Node& input,
                                     const Captures& captures) {
  std::string capture_name;
  if (is_capture(pattern, &capture_name)) {
    const auto previous = captures.single.find(capture_name);
    if (previous != captures.single.end()) {
      return structurally_equal(*previous->second, input) ? std::vector<Captures>{captures}
                                                         : std::vector<Captures>{};
    }
    auto result = captures;
    result.single[capture_name] = &input;
    return {std::move(result)};
  }

  if (pattern.is_list != input.is_list) {
    return {};
  }
  if (!pattern.is_list) {
    return pattern.atom == input.atom ? std::vector<Captures>{captures}
                                      : std::vector<Captures>{};
  }
  return match_children_all(pattern.children, 0, input.children, 0, captures);
}

bool match_sibling_sequence(const std::vector<Node>& patterns,
                            const std::vector<Node>& inputs,
                            size_t start,
                            Captures* captures,
                            size_t* count) {
  if (start > inputs.size()) {
    return false;
  }

  // A top-level rule sequence intentionally has a fixed number of forms. Variable-length captures
  // belong inside a form, where their surrounding list gives them an unambiguous boundary.
  if (start + patterns.size() > inputs.size()) {
    return false;
  }
  std::vector<Captures> matches{*captures};
  for (size_t i = 0; i < patterns.size(); ++i) {
    std::vector<Captures> next;
    for (const auto& current : matches) {
      auto node_matches = match_node_all(patterns.at(i), inputs.at(start + i), current);
      next.insert(next.end(), std::make_move_iterator(node_matches.begin()),
                  std::make_move_iterator(node_matches.end()));
    }
    if (next.empty()) {
      return false;
    }
    matches = std::move(next);
  }
  *captures = std::move(matches.front());
  *count = patterns.size();
  return true;
}

std::string original_text(const Node& node, const std::string& source) {
  return source.substr(node.start, node.end - node.start);
}

std::string render_template(const Node& node,
                            const Captures& captures,
                            const std::string& source);

std::string render_template_children(const std::vector<Node>& nodes,
                                     const Captures& captures,
                                     const std::string& source) {
  std::string result;
  for (const auto& child : nodes) {
    std::string sequence_name;
    if (is_sequence_capture(child, &sequence_name)) {
      const auto found = captures.sequence.find(sequence_name);
      if (found == captures.sequence.end()) {
        throw std::runtime_error(
            fmt::format("Unknown demacro sequence capture '$*{}'", sequence_name));
      }
      for (const auto* captured : found->second) {
        if (!result.empty()) {
          result += " ";
        }
        result += original_text(*captured, source);
      }
    } else {
      if (!result.empty()) {
        result += " ";
      }
      result += render_template(child, captures, source);
    }
  }
  return result;
}

std::string render_template(const Node& node,
                            const Captures& captures,
                            const std::string& source) {
  std::string capture_name;
  if (is_capture(node, &capture_name)) {
    const auto found = captures.single.find(capture_name);
    if (found == captures.single.end()) {
      throw std::runtime_error(fmt::format("Unknown demacro capture '${}'", capture_name));
    }
    return original_text(*found->second, source);
  }
  if (!node.is_list) {
    return node.atom;
  }
  return "(" + render_template_children(node.children, captures, source) + ")";
}

std::string indentation_at(const std::string& source, uint32_t offset) {
  const auto line_start = source.rfind('\n', offset == 0 ? 0 : offset - 1);
  const auto start = line_start == std::string::npos ? 0 : line_start + 1;
  size_t cursor = start;
  while (cursor < offset && (source.at(cursor) == ' ' || source.at(cursor) == '\t')) {
    ++cursor;
  }
  return source.substr(start, cursor - start);
}

std::string indent_after_newlines(std::string text, const std::string& indentation) {
  size_t cursor = 0;
  while ((cursor = text.find('\n', cursor)) != std::string::npos) {
    ++cursor;
    if (cursor < text.size()) {
      text.insert(cursor, indentation);
      cursor += indentation.size();
    }
  }
  return text;
}

std::string render_rewrite(const CompiledRule& rule,
                           const Candidate& candidate,
                           const ParsedSource& parsed,
                           const std::string& source) {
  const auto indentation = indentation_at(source, candidate.start);
  std::string replacement;
  for (const auto& form : rule.rewrite) {
    if (!replacement.empty()) {
      replacement += "\n" + indentation;
    }
    replacement += indent_after_newlines(
        render_template(form, candidate.captures, source), indentation);
  }

  // Comments are not part of semantic matching. Retain any comment which would otherwise be
  // swallowed by replacing a multi-form expansion. Comments already inside a raw captured form are
  // present in replacement and are not duplicated.
  std::string saved_comments;
  for (const auto& comment : parsed.comments) {
    if (comment.start < candidate.start || comment.end > candidate.end) {
      continue;
    }
    const auto text = source.substr(comment.start, comment.end - comment.start);
    if (replacement.find(text) == std::string::npos) {
      if (!saved_comments.empty()) {
        saved_comments += "\n" + indentation;
      }
      saved_comments += text;
    }
  }
  if (!saved_comments.empty()) {
    replacement = saved_comments + "\n" + indentation + replacement;
  }
  return replacement;
}

void find_candidates(const Node& parent,
                     const std::vector<CompiledRule>& rules,
                     std::vector<Candidate>* result) {
  if (!parent.is_list) {
    return;
  }

  for (size_t child_idx = 0; child_idx < parent.children.size(); ++child_idx) {
    for (const auto& rule : rules) {
      Captures captures;
      size_t match_count = 0;
      if (match_sibling_sequence(rule.match, parent.children, child_idx, &captures, &match_count)) {
        result->push_back({parent.children.at(child_idx).start,
                           parent.children.at(child_idx + match_count - 1).end, rule.index,
                           std::move(captures)});
      }
    }
  }

  for (const auto& child : parent.children) {
    find_candidates(child, rules, result);
  }
}

std::vector<Candidate> select_non_overlapping(std::vector<Candidate> candidates) {
  std::sort(candidates.begin(), candidates.end(), [](const Candidate& a, const Candidate& b) {
    if (a.start != b.start) {
      return a.start < b.start;
    }
    if (a.rule_index != b.rule_index) {
      return a.rule_index < b.rule_index;
    }
    return a.end < b.end;
  });

  std::vector<Candidate> result;
  uint32_t last_end = 0;
  bool have_last = false;
  for (auto& candidate : candidates) {
    if (have_last && candidate.start < last_end) {
      continue;
    }
    last_end = candidate.end;
    have_last = true;
    result.push_back(std::move(candidate));
  }
  return result;
}

std::vector<CompiledRule> compile_rules(const RuleSet& rules) {
  std::vector<CompiledRule> result;
  for (size_t i = 0; i < rules.rules.size(); ++i) {
    const auto& rule = rules.rules.at(i);
    if (rule.match.empty()) {
      throw std::runtime_error(fmt::format("Demacro rule '{}' has an empty match", rule.name));
    }
    if (rule.rewrite.empty()) {
      throw std::runtime_error(fmt::format("Demacro rule '{}' has an empty rewrite", rule.name));
    }
    result.push_back({rule.name,
                      parse_form_sequence(rule.match, "match for demacro rule " + rule.name),
                      parse_form_sequence(rule.rewrite, "rewrite for demacro rule " + rule.name),
                      i});
  }
  return result;
}

RewriteResult rewrite_compiled(const std::string& source,
                               const RuleSet& rules,
                               const std::vector<CompiledRule>& compiled) {
  RewriteResult result;
  result.source = source;
  for (const auto& rule : rules.rules) {
    result.stats.push_back({rule.name, 0});
  }

  // Reparse after each batch so patterns created by another rewrite can be recognized, while a
  // malformed or cyclic rule file cannot loop forever.
  constexpr int kMaxPasses = 100;
  for (int pass = 0; pass < kMaxPasses; ++pass) {
    const auto parsed = parse_source(result.source, "<OpenGOAL source>");
    std::vector<Candidate> candidates;
    find_candidates(parsed.root, compiled, &candidates);
    auto selected = select_non_overlapping(std::move(candidates));
    if (selected.empty()) {
      return result;
    }

    std::vector<Edit> edits;
    edits.reserve(selected.size());
    for (const auto& candidate : selected) {
      edits.push_back({candidate.start, candidate.end, candidate.rule_index,
                       render_rewrite(compiled.at(candidate.rule_index), candidate, parsed,
                                      result.source)});
      result.stats.at(candidate.rule_index).rewrites++;
    }
    for (auto edit = edits.rbegin(); edit != edits.rend(); ++edit) {
      result.source.replace(edit->start, edit->end - edit->start, edit->replacement);
    }
  }

  throw std::runtime_error("Demacro exceeded 100 rewrite passes; the rule set is probably cyclic");
}

struct CachedRuleSet {
  RuleSet rules;
  std::vector<CompiledRule> compiled;
};

const CachedRuleSet& load_cached_rules(const fs::path& path) {
  static std::mutex cache_mutex;
  static std::unordered_map<std::string, std::unique_ptr<CachedRuleSet>> cache;

  const auto key = fs::absolute(path).lexically_normal().string();
  std::lock_guard<std::mutex> lock(cache_mutex);
  const auto existing = cache.find(key);
  if (existing != cache.end()) {
    return *existing->second;
  }

  auto loaded = std::make_unique<CachedRuleSet>();
  loaded->rules = load_rules(path);
  loaded->compiled = compile_rules(loaded->rules);
  return *cache.emplace(key, std::move(loaded)).first->second;
}

}  // namespace

int RewriteResult::rewrite_count() const {
  int result = 0;
  for (const auto& stat : stats) {
    result += stat.rewrites;
  }
  return result;
}

RuleSet parse_rules(const std::string& contents, const std::string& source_name) {
  const auto json = parse_commented_json(contents, source_name);
  if (!json.is_object() || !json.contains("rules") || !json.at("rules").is_array()) {
    throw std::runtime_error(fmt::format("{} must contain a 'rules' array", source_name));
  }

  std::unordered_map<std::string, std::vector<std::unordered_map<std::string, std::string>>> tables;
  if (json.contains("tables")) {
    if (!json.at("tables").is_object()) {
      throw std::runtime_error(fmt::format("{} field 'tables' must be an object", source_name));
    }
    for (const auto& [table_name, rows] : json.at("tables").items()) {
      if (!rows.is_array()) {
        throw std::runtime_error(
            fmt::format("{} table '{}' must be an array", source_name, table_name));
      }
      for (const auto& row : rows) {
        if (!row.is_object()) {
          throw std::runtime_error(
              fmt::format("{} table '{}' contains a non-object row", source_name, table_name));
        }
        std::unordered_map<std::string, std::string> values;
        for (const auto& [key, value] : row.items()) {
          if (!value.is_string()) {
            throw std::runtime_error(fmt::format(
                "{} table '{}' row values must be strings", source_name, table_name));
          }
          values[key] = value.get<std::string>();
        }
        tables[table_name].push_back(std::move(values));
      }
    }
  }

  RuleSet result;
  for (const auto& entry : json.at("rules")) {
    if (!entry.is_object()) {
      throw std::runtime_error(fmt::format("{} contains a non-object rule", source_name));
    }
    if (entry.contains("for_each")) {
      const auto table_name = entry.at("for_each").get<std::string>();
      const auto table = tables.find(table_name);
      if (table == tables.end()) {
        throw std::runtime_error(fmt::format("Demacro rule '{}' refers to unknown table '{}'",
                                             entry.at("name").get<std::string>(), table_name));
      }
      for (size_t i = 0; i < table->second.size(); ++i) {
        result.rules.push_back(parse_rule(entry, table->second.at(i),
                                          fmt::format("[{}:{}]", table_name, i)));
      }
    } else {
      result.rules.push_back(parse_rule(entry, {}, ""));
    }
  }
  return result;
}

RuleSet load_rules(const fs::path& path) {
  return parse_rules(file_util::read_text_file(path), path.string());
}

RewriteResult rewrite(const std::string& source, const RuleSet& rules) {
  return rewrite_compiled(source, rules, compile_rules(rules));
}

RewriteResult rewrite(const std::string& source, const fs::path& rule_path) {
  const auto& cached = load_cached_rules(rule_path);
  return rewrite_compiled(source, cached.rules, cached.compiled);
}

}  // namespace demacro
