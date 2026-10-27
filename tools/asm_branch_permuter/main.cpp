#include <algorithm>
#include <cstdint>
#include <cstring>
#include <limits>
#include <map>
#include <set>
#include <thread>
#include <unordered_map>

#ifndef _WIN32
#include <unistd.h>
#endif

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/string_util.h"
#include "common/util/unicode_util.h"

#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Function/CfgVtx.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/analysis/final_output.h"
#include "decompiler/config.h"
#include "tools/shared/decompiler_setup.h"
#include "tools/shared/isolated_map.h"

#include "fmt/format.h"
#include "fmt/ranges.h"
#include "third-party/CLI11.hpp"
#include "third-party/json.hpp"

using namespace decompiler;

namespace {

using BlockSet = std::vector<int>;

constexpr int CRASH_SCORE = std::numeric_limits<int>::max();

enum class Level : int {
  CRASHED,
  CFG_FAILED,
  ATOMIC_OPS_FAILED,
  STRUCTURING_FAILED,
  TYPES_FAILED,
  EXPRESSIONS_FAILED,
  HAS_ERRORS,
  CLEAN,
};

constexpr Level ACCEPTABLE_LEVEL = Level::TYPES_FAILED;

const char* level_description(Level level) {
  switch (level) {
    case Level::CRASHED:
      return "decompiler crashed";
    case Level::CFG_FAILED:
      return "cfg failed to resolve";
    case Level::ATOMIC_OPS_FAILED:
      return "cfg resolved, atomic op conversion failed";
    case Level::STRUCTURING_FAILED:
      return "cfg resolved, structuring failed";
    case Level::TYPES_FAILED:
      return "structured, type analysis failed";
    case Level::EXPRESSIONS_FAILED:
      return "structured, expression building failed";
    case Level::HAS_ERRORS:
      return "decompiled, with errors";
    case Level::CLEAN:
      return "decompiled cleanly";
  }
  return "?";
}

struct Target {
  ObjectFileData* data = nullptr;
  Function* func = nullptr;
  int seg = -1;
  // the name used for hacks.jsonc lookups. state handlers are renamed after the basic block pass
  // so this can differ from func->name().
  std::string hack_key;
};

struct Verified {
  BlockSet set;
  std::optional<Level> level;  // nullopt when verification was skipped
  std::string warnings;
  std::string output;

  bool acceptable() const { return !level || *level >= ACCEPTABLE_LEVEL; }
};

struct Options {
  int jobs = 1;
  size_t exhaustive_budget = 250000;
  int beam_width = 16;
  int max_size = 0;
  int max_verify = 1000;
  int max_results = 5;
  bool verify = true;
  bool print_output = false;
  double time_limit = 0;  // seconds per search, 0 for no limit
};

BlockSet merge(const BlockSet& a, const BlockSet& b) {
  BlockSet result;
  std::set_union(a.begin(), a.end(), b.begin(), b.end(), std::back_inserter(result));
  return result;
}

double n_choose_k(size_t n, size_t k) {
  if (k > n) {
    return 0;
  }
  double result = 1;
  for (size_t i = 1; i <= k; i++) {
    result = result * (n - k + i) / i;
  }
  return result;
}

std::vector<BlockSet> combinations(const std::vector<int>& items, size_t k) {
  std::vector<BlockSet> result;
  if (k > items.size()) {
    return result;
  }
  std::vector<size_t> idx(k);
  for (size_t i = 0; i < k; i++) {
    idx[i] = i;
  }
  while (true) {
    BlockSet set;
    for (auto i : idx) {
      set.push_back(items[i]);
    }
    result.push_back(std::move(set));

    size_t i = k;
    while (i > 0 && idx[i - 1] == items.size() - k + i - 1) {
      i--;
    }
    if (i == 0) {
      break;
    }
    idx[i - 1]++;
    for (size_t j = i; j < k; j++) {
      idx[j] = idx[j - 1] + 1;
    }
  }
  return result;
}

std::string encode_int(int32_t value) {
  return std::string(reinterpret_cast<const char*>(&value), sizeof(value));
}

int32_t decode_int(const std::string& str) {
  int32_t value;
  ASSERT(str.size() == sizeof(value));
  memcpy(&value, str.data(), sizeof(value));
  return value;
}

class Permuter {
 public:
  Permuter(ObjectFileDB& db, Config& config, Target target, Options options)
      : m_db(db), m_config(config), m_target(std::move(target)), m_options(options) {
    auto& func = *m_target.func;
    auto& hacks = m_config.hacks;

    auto cond_it = hacks.cond_with_else_len_by_func_name.find(m_target.hack_key);
    if (cond_it != hacks.cond_with_else_len_by_func_name.end()) {
      m_cond_hack = cond_it->second;
    }

    // the verification passes look hacks up by the current name, so alias the ones the basic
    // block and atomic op passes would have found under the original name.
    if (func.name() != m_target.hack_key) {
      if (cond_it != hacks.cond_with_else_len_by_func_name.end()) {
        hacks.cond_with_else_len_by_func_name[func.name()] = m_cond_hack;
      }
      if (hacks.hint_inline_assembly_functions.count(m_target.hack_key)) {
        hacks.hint_inline_assembly_functions.insert(func.name());
      }
    }

    m_pristine_blocks = func.basic_blocks;
    for (auto& block : m_pristine_blocks) {
      block.pred.clear();
      block.succ_ft = -1;
      block.succ_branch = -1;
    }
    m_pristine_warnings = func.warnings;
  }

  std::vector<int> branch_blocks() const {
    std::vector<int> result;
    const auto& func = *m_target.func;
    for (int i = 0; i < (int)m_pristine_blocks.size(); i++) {
      const auto& block = m_pristine_blocks[i];
      if (block.end_word == block.start_word) {
        continue;
      }
      bool likely = is_branch(func.instructions.at(block.end_word - 1), true);
      bool normal = block.end_word - block.start_word >= 2 &&
                    is_branch(func.instructions.at(block.end_word - 2), false);
      if (likely || normal) {
        result.push_back(i);
      }
    }
    return result;
  }

  std::string describe_block(int block_id) const {
    const auto& func = *m_target.func;
    const auto& block = m_pristine_blocks.at(block_id);
    const auto& labels = m_target.data->linked_data.labels;
    if (block.end_word == block.start_word) {
      return "(empty)";
    }
    if (is_branch(func.instructions.at(block.end_word - 1), true)) {
      return func.instructions.at(block.end_word - 1).to_string(labels);
    }
    if (block.end_word - block.start_word >= 2 &&
        is_branch(func.instructions.at(block.end_word - 2), false)) {
      return fmt::format("{} ; delay: {}",
                         func.instructions.at(block.end_word - 2).to_string(labels),
                         func.instructions.at(block.end_word - 1).to_string(labels));
    }
    return "(no branch)";
  }

  // 0 if the cfg resolves, otherwise the number of extra unresolved top level vertices.
  std::vector<int> cfg_scores(const std::vector<BlockSet>& sets) {
    size_t chunk = std::clamp<size_t>(sets.size() / (m_options.jobs * 4), 1, 256);
    auto results = isolated_map(sets.size(), m_options.jobs, chunk,
                                [&](size_t i) { return encode_int(cfg_score(sets[i])); });
    m_evaluations += sets.size();
    std::vector<int> scores;
    for (auto& result : results) {
      scores.push_back(result ? decode_int(*result) : CRASH_SCORE);
    }
    return scores;
  }

  std::vector<Verified> verify(const std::vector<BlockSet>& sets, bool want_output) {
    std::vector<Verified> verified;
    if (!m_options.verify) {
      for (auto& set : sets) {
        verified.push_back({set, std::nullopt, "", ""});
      }
      return verified;
    }

    auto results = isolated_map(sets.size(), m_options.jobs, 1,
                                [&](size_t i) { return run_full_decompile(sets[i], want_output); });
    m_verifications += sets.size();

    for (size_t i = 0; i < sets.size(); i++) {
      Verified v;
      v.set = sets[i];
      if (!results[i]) {
        v.level = Level::CRASHED;
      } else {
        const auto& str = *results[i];
        v.level = (Level)str.at(0);
        uint32_t warnings_size;
        memcpy(&warnings_size, str.data() + 1, sizeof(warnings_size));
        v.warnings = str.substr(1 + sizeof(warnings_size), warnings_size);
        v.output = str.substr(1 + sizeof(warnings_size) + warnings_size);
      }
      verified.push_back(std::move(v));
    }
    return verified;
  }

  // verifies the sets that resolved the cfg, best first. works in batches and stops after the first
  // batch with an acceptable result, so a long list of resolving sets doesn't cost much.
  std::vector<Verified> verify_resolved(const std::vector<BlockSet>& sets,
                                        const std::vector<int>& scores) {
    std::vector<BlockSet> resolved;
    for (size_t i = 0; i < sets.size(); i++) {
      if (scores[i] == 0) {
        resolved.push_back(sets[i]);
      }
    }
    m_last_capped = resolved.size() > (size_t)m_options.max_verify;
    if (m_last_capped) {
      resolved.resize(m_options.max_verify);
    }

    std::vector<Verified> result;
    size_t batch = std::max(m_options.jobs, m_options.max_results);
    for (size_t begin = 0; begin < resolved.size(); begin += batch) {
      std::vector<BlockSet> part(resolved.begin() + begin,
                                 resolved.begin() + std::min(resolved.size(), begin + batch));
      for (auto& v : verify(part, false)) {
        note_rejected(v);
        result.push_back(std::move(v));
      }
      if (std::any_of(result.begin(), result.end(), [](auto& v) { return v.acceptable(); })) {
        break;
      }
    }

    // fewer warnings breaks ties, e.g. a set that avoids "very strange gotos" is the nicer one.
    std::stable_sort(result.begin(), result.end(), [](const Verified& a, const Verified& b) {
      auto a_level = a.level.value_or(Level::CLEAN);
      auto b_level = b.level.value_or(Level::CLEAN);
      if (a_level != b_level) {
        return a_level > b_level;
      }
      return std::count(a.warnings.begin(), a.warnings.end(), '\n') <
             std::count(b.warnings.begin(), b.warnings.end(), '\n');
    });
    return result;
  }

  Verified minimize(Verified best, const BlockSet& required) {
    while (!out_of_time()) {
      std::vector<BlockSet> smaller;
      for (int block : best.set) {
        if (std::binary_search(required.begin(), required.end(), block)) {
          continue;
        }
        BlockSet set;
        std::copy_if(best.set.begin(), best.set.end(), std::back_inserter(set),
                     [&](int b) { return b != block; });
        smaller.push_back(std::move(set));
      }
      auto verified = verify_resolved(smaller, cfg_scores(smaller));
      auto it = std::find_if(verified.begin(), verified.end(), [&](const Verified& v) {
        return v.acceptable() &&
               v.level.value_or(Level::CLEAN) >= best.level.value_or(Level::CLEAN);
      });
      if (it == verified.end()) {
        return best;
      }
      best = *it;
    }
    return best;
  }

  // drops blocks from `start` for as long as the function decompiles exactly the same (level,
  // warnings and output). returns nullopt if `start` doesn't get the function structured, then
  // there is no output to compare against.
  std::optional<BlockSet> trim(const BlockSet& start) {
    auto reference = verify({start}, true).front();
    m_trim_reference_level = *reference.level;
    if (*reference.level <= Level::STRUCTURING_FAILED) {
      return std::nullopt;
    }
    auto same_as_reference = [&](const Verified& v) {
      return v.level == reference.level && v.warnings == reference.warnings &&
             v.output == reference.output;
    };

    BlockSet current = start;
    while (!current.empty()) {
      std::vector<BlockSet> smaller;
      for (int block : current) {
        BlockSet set;
        std::copy_if(current.begin(), current.end(), std::back_inserter(set),
                     [&](int b) { return b != block; });
        smaller.push_back(std::move(set));
      }
      auto verified = verify(smaller, true);

      std::vector<size_t> removable;
      for (size_t i = 0; i < verified.size(); i++) {
        if (same_as_reference(verified[i])) {
          removable.push_back(i);
        }
      }
      if (removable.empty()) {
        break;
      }

      if (removable.size() > 1) {
        BlockSet without_all;
        for (size_t i = 0; i < current.size(); i++) {
          if (std::find(removable.begin(), removable.end(), i) == removable.end()) {
            without_all.push_back(current[i]);
          }
        }
        if (same_as_reference(verify({without_all}, true).front())) {
          current = without_all;
          continue;
        }
      }
      current = smaller[removable.front()];
    }
    return current;
  }

  bool out_of_time() const {
    return m_options.time_limit > 0 && m_timer.getSeconds() > m_options.time_limit;
  }
  Level trim_reference_level() const { return m_trim_reference_level; }
  std::optional<Verified> best_rejected() const { return m_best_rejected; }
  bool last_capped() const { return m_last_capped; }
  size_t evaluations() const { return m_evaluations; }
  size_t verifications() const { return m_verifications; }
  const Target& target() const { return m_target; }

 private:
  int cfg_score(const BlockSet& set) {
    auto& func = *m_target.func;
    func.basic_blocks = m_pristine_blocks;
    func.warnings = m_pristine_warnings;
    std::unordered_set<int> asm_blocks(set.begin(), set.end());
    auto cfg = build_cfg(m_target.data->linked_data, m_target.seg, func, m_cond_hack, asm_blocks,
                         m_config.game_version);
    if (cfg->is_fully_resolved()) {
      return 0;
    }
    return std::max(1, cfg->get_top_level_vertices_count() - 1);
  }

  std::string run_full_decompile(const BlockSet& set, bool want_output) {
    auto& func = *m_target.func;
    std::unordered_set<int> asm_blocks(set.begin(), set.end());
    m_config.hacks.blocks_ending_in_asm_branch_by_func_name[func.name()] = asm_blocks;

    func.warnings = DecompWarnings();
    func.cfg = nullptr;
    func.cfg_ok = false;
    func.contains_asm_ops = false;
    func.ir2 = {};
    func.ir2.form_pool = std::make_shared<FormPool>();
    func.ir2.env.version = m_config.game_version;

    m_db.ir2_function_filter = &func;
    m_db.ir2_do_segment_analysis_phase1(m_target.seg, m_config, *m_target.data);
    m_db.ir2_do_segment_analysis_phase2(m_target.seg, m_config, *m_target.data);
    m_db.ir2_function_filter = nullptr;

    Level level;
    if (!func.cfg_ok) {
      level = Level::CFG_FAILED;
    } else if (!func.ir2.atomic_ops_succeeded) {
      level = Level::ATOMIC_OPS_FAILED;
    } else if (!func.ir2.top_form) {
      level = Level::STRUCTURING_FAILED;
    } else if (!func.ir2.env.types_succeeded) {
      level = Level::TYPES_FAILED;
    } else if (!func.ir2.expressions_succeeded) {
      level = Level::EXPRESSIONS_FAILED;
    } else if (func.warnings.has_errors()) {
      level = Level::HAS_ERRORS;
    } else {
      level = Level::CLEAN;
    }

    std::string output;
    if (want_output && func.ir2.expressions_succeeded) {
      output = final_defun_out(func, func.ir2.env, m_db.dts);
    } else if (want_output && func.ir2.top_form) {
      output = func.ir2.top_form->to_string(func.ir2.env);
    }

    auto warnings = func.warnings.get_warning_text(false);
    uint32_t warnings_size = warnings.size();
    std::string result(1, (char)level);
    result.append(reinterpret_cast<const char*>(&warnings_size), sizeof(warnings_size));
    result += warnings;
    result += output;
    return result;
  }

  void note_rejected(const Verified& v) {
    if (v.acceptable()) {
      return;
    }
    if (!m_best_rejected || *v.level > *m_best_rejected->level ||
        (*v.level == *m_best_rejected->level && v.set.size() < m_best_rejected->set.size())) {
      m_best_rejected = v;
    }
  }

  ObjectFileDB& m_db;
  Config& m_config;
  Target m_target;
  Options m_options;
  CondWithElseLengthHack m_cond_hack;
  std::vector<BasicBlock> m_pristine_blocks;
  DecompWarnings m_pristine_warnings;
  std::optional<Verified> m_best_rejected;
  bool m_last_capped = false;
  Level m_trim_reference_level = Level::CRASHED;
  Timer m_timer;
  size_t m_evaluations = 0;
  size_t m_verifications = 0;
};

std::string format_set(const BlockSet& set) {
  return fmt::format("[{}]", fmt::join(set, ", "));
}

struct SearchResult {
  std::optional<Verified> best;        // the smallest acceptable set found
  std::vector<Verified> alternatives;  // other acceptable sets of the same size
  std::optional<Verified> closest;     // the best set that got verified but wasn't acceptable
  std::optional<std::pair<BlockSet, int>> closest_cfg;  // set with the fewest unresolved vertices
  bool timed_out = false;
};

std::vector<Verified> acceptable_only(std::vector<Verified> verified) {
  verified.erase(std::remove_if(verified.begin(), verified.end(),
                                [](const Verified& v) { return !v.acceptable(); }),
                 verified.end());
  return verified;
}

void report_progress(const Permuter& permuter,
                     const Options& options,
                     const std::string& stage,
                     size_t k,
                     const std::vector<int>& scores,
                     const std::vector<Verified>& verified,
                     const Timer& step_timer) {
  auto resolved = std::count(scores.begin(), scores.end(), 0);
  auto crashed = std::count(scores.begin(), scores.end(), CRASH_SCORE);
  fmt::print("{} size {:>2}: {:>7} sets", stage, k, scores.size());
  if (crashed) {
    fmt::print(", {} crashed", crashed);
  }
  if (resolved) {
    fmt::print(", {} resolve the cfg", resolved);
  } else {
    auto best = *std::min_element(scores.begin(), scores.end());
    if (best != CRASH_SCORE) {
      fmt::print(", best leaves {} vertices unresolved", best);
    }
  }
  if (options.verify && resolved) {
    fmt::print(", {} of {} decompiled", acceptable_only(verified).size(), verified.size());
    std::map<Level, int> failures;
    for (auto& v : verified) {
      if (!v.acceptable()) {
        failures[*v.level]++;
      }
    }
    std::vector<std::string> parts;
    for (auto& [level, count] : failures) {
      parts.push_back(fmt::format("{} {}", count, level_description(level)));
    }
    if (!parts.empty()) {
      fmt::print(" ({})", fmt::join(parts, ", "));
    }
    if (permuter.last_capped()) {
      fmt::print(", verification capped by --max-verify");
    }
  }
  fmt::print(" ({:.2f}s)\n", step_timer.getSeconds());
}

SearchResult search(Permuter& permuter,
                    const std::vector<int>& candidates,
                    const BlockSet& required,
                    const Options& options,
                    bool print_progress) {
  SearchResult result;
  size_t max_size = options.max_size > 0 ? std::min<size_t>(options.max_size, candidates.size())
                                         : candidates.size();

  auto finish = [&](std::vector<Verified> verified, bool minimize) {
    result.best = verified.front();
    if (minimize) {
      if (print_progress) {
        fmt::print("Minimizing {}...\n", format_set(result.best->set));
      }
      result.best = permuter.minimize(*result.best, required);
      return;
    }
    for (auto& v : verified) {
      if (v.set != result.best->set && v.set.size() == result.best->set.size() &&
          (int)result.alternatives.size() < options.max_results) {
        result.alternatives.push_back(v);
      }
    }
  };

  struct Scored {
    BlockSet set;
    int score;
  };
  std::vector<Scored> layer;
  auto finish_without_result = [&]() {
    result.closest = permuter.best_rejected();
    auto best = std::min_element(layer.begin(), layer.end(), [](const Scored& a, const Scored& b) {
      return a.score < b.score;
    });
    if (best != layer.end() && best->score != CRASH_SCORE) {
      result.closest_cfg = std::make_pair(best->set, best->score);
    }
    return result;
  };

  size_t next_size = 0;
  for (size_t k = 0; k <= max_size; k++) {
    double count = n_choose_k(candidates.size(), k);
    if (k > 0 && permuter.evaluations() + count > options.exhaustive_budget) {
      break;
    }
    if (permuter.out_of_time()) {
      result.timed_out = true;
      return finish_without_result();
    }
    Timer step_timer;
    std::vector<BlockSet> sets;
    for (auto& combo : combinations(candidates, k)) {
      sets.push_back(merge(required, combo));
    }
    auto scores = permuter.cfg_scores(sets);
    auto verified = permuter.verify_resolved(sets, scores);
    if (print_progress) {
      report_progress(permuter, options, "exhaustive", k, scores, verified, step_timer);
    }

    auto ok = acceptable_only(verified);
    if (!ok.empty()) {
      finish(ok, false);
      return result;
    }

    layer.clear();
    for (size_t i = 0; i < sets.size(); i++) {
      layer.push_back({sets[i], scores[i]});
    }
    next_size = k + 1;
  }

  if (next_size <= max_size && print_progress) {
    fmt::print("Exhaustive budget used up, switching to beam search (width {})\n",
               options.beam_width);
  }
  for (size_t k = next_size; k <= max_size; k++) {
    if (permuter.out_of_time()) {
      result.timed_out = true;
      break;
    }
    std::stable_sort(layer.begin(), layer.end(),
                     [](const Scored& a, const Scored& b) { return a.score < b.score; });
    std::vector<BlockSet> beam;
    for (auto& scored : layer) {
      if ((int)beam.size() >= options.beam_width || scored.score == CRASH_SCORE) {
        break;
      }
      beam.push_back(scored.set);
    }
    if (beam.empty()) {
      break;
    }

    std::set<BlockSet> expansions;
    for (auto& set : beam) {
      for (int block : candidates) {
        if (!std::binary_search(set.begin(), set.end(), block)) {
          expansions.insert(merge(set, {block}));
        }
      }
    }
    if (expansions.empty()) {
      break;
    }

    Timer step_timer;
    std::vector<BlockSet> sets(expansions.begin(), expansions.end());
    auto scores = permuter.cfg_scores(sets);
    auto verified = permuter.verify_resolved(sets, scores);
    if (print_progress) {
      report_progress(permuter, options, "beam      ", k, scores, verified, step_timer);
    }

    auto ok = acceptable_only(verified);
    if (!ok.empty()) {
      finish(ok, true);
      return result;
    }

    layer.clear();
    for (size_t i = 0; i < sets.size(); i++) {
      layer.push_back({sets[i], scores[i]});
    }
  }
  return finish_without_result();
}

void print_result(Permuter& permuter,
                  const Verified& best,
                  const std::vector<Verified>& alternatives) {
  const auto& target = permuter.target();
  fmt::print("\nFound a working set of {} block(s): {}\n", best.set.size(), format_set(best.set));
  if (best.level) {
    fmt::print("  result: {}\n", level_description(*best.level));
  } else {
    fmt::print("  result: cfg resolves (not verified further, --no-verify)\n");
  }
  for (int block : best.set) {
    fmt::print("  B{:<4} {}\n", block, permuter.describe_block(block));
  }
  if (!best.warnings.empty()) {
    fmt::print("  warnings:\n");
    for (auto& line : str_util::split(best.warnings)) {
      if (!line.empty()) {
        fmt::print("    {}\n", line);
      }
    }
  }

  if (!alternatives.empty()) {
    fmt::print("\nOther sets of the same size:\n");
    for (auto& alt : alternatives) {
      fmt::print("  {:<24} {}\n", format_set(alt.set),
                 alt.level ? level_description(*alt.level) : "cfg resolves");
    }
  }

  fmt::print("\nhacks.jsonc entry for blocks_ending_in_asm_branch:\n");
  fmt::print("    \"{}\": {},\n", target.hack_key, format_set(best.set));

  if (!best.output.empty()) {
    fmt::print("\n{}\n", best.output);
  }
}

std::vector<int> parse_block_list(const std::string& str) {
  std::vector<int> result;
  for (auto& part : str_util::split(str, ',')) {
    if (!part.empty()) {
      result.push_back(std::stoi(part));
    }
  }
  std::sort(result.begin(), result.end());
  result.erase(std::unique(result.begin(), result.end()), result.end());
  return result;
}

// finds functions by the name they have during the basic block pass
std::unordered_map<std::string, std::vector<Target>>
find_targets(ObjectFileDB& db, Config& config, const std::unordered_set<std::string>& names) {
  std::unordered_map<std::string, std::vector<Target>> result;
  std::vector<ObjectFileData*> to_decompile;
  db.for_each_function([&](Function& func, int seg, ObjectFileData& data) {
    auto name = func.name();
    if (names.count(name)) {
      result[name].push_back({&data, &func, seg, name});
      if (std::find(to_decompile.begin(), to_decompile.end(), &data) == to_decompile.end()) {
        to_decompile.push_back(&data);
      }
    }
  });

  for (size_t i = 0; i < to_decompile.size(); i++) {
    fmt::print("\rDecompiling {}/{} object(s)...", i + 1, to_decompile.size());
    fflush(stdout);
    db.process_object_file_data(*to_decompile[i], fs::path(), config, {}, {});
  }
  fmt::print("\n");
  return result;
}

std::optional<Target> find_target(ObjectFileDB& db,
                                  Config& config,
                                  const std::string& function_name) {
  auto matches = find_targets(db, config, {function_name})[function_name];

  if (matches.empty()) {
    fmt::print(
        "'{}' not found by its original name, decompiling every loaded object to look for it as a "
        "state handler. Use --obj to make this faster.\n",
        function_name);
    std::unordered_map<const Function*, std::string> original_names;
    db.for_each_function(
        [&](Function& func, int, ObjectFileData&) { original_names[&func] = func.name(); });
    db.for_each_obj([&](ObjectFileData& data) {
      db.process_object_file_data(data, fs::path(), config, {}, {});
    });
    db.for_each_function([&](Function& func, int seg, ObjectFileData& data) {
      if (func.name() == function_name) {
        matches.push_back({&data, &func, seg, original_names.at(&func)});
      }
    });
  }

  if (matches.empty()) {
    fmt::print("Could not find a function named '{}'.\n", function_name);
    return std::nullopt;
  }
  if (matches.size() > 1) {
    fmt::print("'{}' is ambiguous, found in:\n", function_name);
    for (auto& match : matches) {
      fmt::print("  {}\n", match.data->to_unique_name());
    }
    fmt::print("Use --obj to pick one.\n");
    return std::nullopt;
  }
  return matches.front();
}

BlockSet existing_entry(const Config& config, const std::string& hack_key) {
  BlockSet result;
  auto it = config.hacks.blocks_ending_in_asm_branch_by_func_name.find(hack_key);
  if (it != config.hacks.blocks_ending_in_asm_branch_by_func_name.end()) {
    result.assign(it->second.begin(), it->second.end());
    std::sort(result.begin(), result.end());
  }
  return result;
}

// trims every entry in blocks_ending_in_asm_branch, see Permuter::trim.
int trim_all(ObjectFileDB& db,
             Config& config,
             const Options& options,
             const fs::path& output_path,
             const Timer& timer) {
  // copied, the permuters add aliases for renamed state handlers to the config.
  auto entries = config.hacks.blocks_ending_in_asm_branch_by_func_name;
  std::unordered_set<std::string> names;
  for (auto& [name, blocks] : entries) {
    names.insert(name);
  }
  std::vector<std::string> sorted_names(names.begin(), names.end());
  std::sort(sorted_names.begin(), sorted_names.end());

  auto targets = find_targets(db, config, names);

  nlohmann::json results = nlohmann::json::object();
  int trimmed = 0;
  size_t total_before = 0, total_after = 0;
  for (auto& name : sorted_names) {
    BlockSet before = existing_entry(config, name);
    BlockSet after = before;
    nlohmann::json entry;
    std::string status;
    const auto& matches = targets[name];
    if (matches.empty()) {
      status = "not found";
    } else if (matches.size() > 1) {
      status = "ambiguous, skipped";
    } else if (matches.front().func->suspected_asm) {
      status = "asm function, skipped";
    } else {
      entry["object"] = matches.front().data->to_unique_name();
      Permuter permuter(db, config, matches.front(), options);
      auto result = permuter.trim(before);
      entry["level"] = level_description(permuter.trim_reference_level());
      if (!result) {
        status = fmt::format("skipped, existing entry: {}",
                             level_description(permuter.trim_reference_level()));
      } else {
        after = *result;
        status = after.size() < before.size() ? "trimmed" : "unchanged";
      }
    }
    if (after.size() < before.size()) {
      trimmed++;
    }
    total_before += before.size();
    total_after += after.size();
    fmt::print("{:<60} {:>4} -> {:<4} {}\n", name, before.size(), after.size(), status);

    entry["before"] = before;
    entry["after"] = after;
    entry["status"] = status;
    results[name] = entry;
  }

  fmt::print("\nTrimmed {} of {} entries, {} -> {} blocks in {:.2f}s\n", trimmed,
             sorted_names.size(), total_before, total_after, timer.getSeconds());
  if (!output_path.empty()) {
    file_util::write_text_file(output_path, results.dump(2));
    fmt::print("Wrote results to {}\n", output_path.string());
  }
  return 0;
}

// lets the decompiler attempt a function flagged as asm or mips2c
bool unflag_asm(Config& config, const Target& target) {
  auto& func = *target.func;
  config.hacks.asm_functions_by_name.erase(target.hack_key);
  config.hacks.mips2c_functions_by_name.erase(target.hack_key);
  func.suspected_asm = false;
  func.basic_blocks = find_blocks_in_function(target.data->linked_data, target.seg, func);
  func.analyze_prologue(target.data->linked_data);
  return !func.suspected_asm;
}

// for every function in asm_functions_by_name / mips2c_functions_by_name, searches for a
// blocks_ending_in_asm_branch entry that lets it decompile without the flag.
int try_asm_functions(ObjectFileDB& db,
                      Config& config,
                      const Options& options,
                      const fs::path& output_path,
                      const Timer& timer) {
  const auto& hacks = config.hacks;
  std::unordered_set<std::string> names(hacks.asm_functions_by_name.begin(),
                                        hacks.asm_functions_by_name.end());
  names.insert(hacks.mips2c_functions_by_name.begin(), hacks.mips2c_functions_by_name.end());
  for (auto& [name, table] : hacks.mips2c_jump_table_functions) {
    names.erase(name);
  }
  std::vector<std::string> sorted_names(names.begin(), names.end());
  std::sort(sorted_names.begin(), sorted_names.end());

  auto targets = find_targets(db, config, names);

  nlohmann::json results = nlohmann::json::object();
  std::map<std::string, int> summary;
  for (auto& name : sorted_names) {
    nlohmann::json entry;
    bool is_asm = hacks.asm_functions_by_name.count(name);
    bool is_mips2c = hacks.mips2c_functions_by_name.count(name);
    entry["lists"] = is_asm && is_mips2c ? "asm, mips2c" : (is_asm ? "asm" : "mips2c");

    const auto& matches = targets[name];
    if (matches.size() != 1) {
      entry["status"] = matches.empty() ? "not found" : "ambiguous";
    } else {
      const auto& target = matches.front();
      entry["object"] = target.data->to_unique_name();
      auto out = isolated_map(1, 1, 1, [&](size_t) {
#ifndef _WIN32
        // backstop for a single search step that runs far past the time limit
        alarm((unsigned)(options.time_limit * 2) + 60);
#endif
        nlohmann::json r;
        if (!unflag_asm(config, target)) {
          r["status"] = "still flagged as asm by the prologue analysis";
          return r.dump();
        }
        Permuter permuter(db, config, target, options);
        auto result = search(permuter, permuter.branch_blocks(), {}, options, false);
        if (result.best) {
          r["status"] = "found";
          r["set"] = result.best->set;
          r["level"] = level_description(*result.best->level);
          r["warnings"] = result.best->warnings;
        } else {
          r["status"] = result.timed_out ? "timed out" : "no set found";
          if (result.closest) {
            r["closest"] = result.closest->set;
            r["level"] = level_description(*result.closest->level);
          }
        }
        return r.dump();
      });
      if (out.front()) {
        entry.update(nlohmann::json::parse(*out.front()));
      } else {
        entry["status"] = "crashed or timed out";
      }
    }

    auto status = entry["status"].get<std::string>();
    auto level = entry.contains("level") ? entry["level"].get<std::string>() : "";
    summary[status == "found" ? level : status]++;
    fmt::print("{:<55} {:<12} {}{}\n", name, entry["lists"].get<std::string>(), status,
               status == "found"
                   ? fmt::format(": {} {}", format_set(entry["set"].get<BlockSet>()), level)
                   : "");
    fflush(stdout);
    results[name] = entry;
  }

  fmt::print("\n");
  for (auto& [what, count] : summary) {
    fmt::print("{:>5}  {}\n", count, what);
  }
  fmt::print("{} functions in {:.2f}s\n", sorted_names.size(), timer.getSeconds());
  if (!output_path.empty()) {
    file_util::write_text_file(output_path, results.dump(2));
    fmt::print("Wrote results to {}\n", output_path.string());
  }
  return 0;
}

}  // namespace

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  fs::path config_path;
  fs::path in_folder;
  std::string config_game_version;
  std::string config_override = "{}";
  std::string function_name;
  std::string object_name;
  std::string required_str;
  std::string candidates_str;
  bool verbose = false;
  bool no_verify = false;
  bool trim = false;
  bool try_asm = false;
  fs::path output_path;
  Options options;
  options.jobs = std::max(1u, std::thread::hardware_concurrency());

  CLI::App app{"OpenGOAL asm branch permuter"};
  app.add_option("config-path", config_path, "Path to the decompiler config .jsonc file")
      ->required();
  app.add_option("in-folder", in_folder,
                 "The path containing the iso_data folders. ie. ./iso_data/. Assumes the "
                 "'gameName' from the config as a sub-directory")
      ->required();
  app.add_option("--version", config_game_version, "e.g. ntsc_v1")->required();
  app.add_option("-f,--function", function_name,
                 "Function to search for, as it is named in hacks.jsonc");
  app.add_flag("--trim", trim,
               "Instead of searching from scratch, drop blocks from the existing entry for as long "
               "as the decompiled output stays exactly the same. Without -f, does this for every "
               "entry in blocks_ending_in_asm_branch");
  app.add_flag("--try-asm", try_asm,
               "For every function in asm_functions_by_name or mips2c_functions_by_name, search "
               "for a blocks_ending_in_asm_branch entry that lets it decompile without the flag");
  app.add_option("--time-limit", options.time_limit,
                 "Seconds to spend searching per function, 0 for no limit (default 60 with "
                 "--try-asm)");
  app.add_option("-o,--output-path", output_path,
                 "With --trim (and no -f) or --try-asm, write the results to this json file");
  app.add_option("--obj", object_name,
                 "Only load this object file (much faster). Defaults to the config's "
                 "allowed_objects");
  app.add_option("--config-override", config_override,
                 "JSON provided will be merged with the specified config");
  app.add_option("--require", required_str,
                 "Comma separated blocks that must always be in the set");
  app.add_option("--candidates", candidates_str,
                 "Comma separated blocks to restrict the search to. Defaults to every block that "
                 "ends in a branch");
  app.add_option("-j,--jobs", options.jobs, "Number of worker processes");
  app.add_option("--exhaustive-budget", options.exhaustive_budget,
                 "Max number of sets to try in the exhaustive search before switching to a beam "
                 "search");
  app.add_option("--beam-width", options.beam_width,
                 "Number of partial sets kept per step of the beam search");
  app.add_option("--max-size", options.max_size, "Max number of blocks in a set, 0 for no limit");
  app.add_option("--max-verify", options.max_verify,
                 "Max number of cfg-resolving sets to fully decompile per step");
  app.add_option("--max-results", options.max_results,
                 "Max number of alternative sets of the same size to list");
  app.add_flag("--no-verify", no_verify,
               "Only require the cfg to resolve, skip decompiling the result");
  app.add_flag("-p,--print", options.print_output, "Print the decompiled function for the result");
  app.add_flag("-v,--verbose", verbose, "Show the decompiler's own log output");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);
  options.verify = !no_verify || trim || try_asm;
  if (try_asm && options.time_limit == 0) {
    options.time_limit = 60;
  }
  if (function_name.empty() && !trim && !try_asm) {
    fmt::print("Give a --function, or use --trim or --try-asm.\n");
    return 1;
  }

  if (!file_util::setup_project_path(std::nullopt)) {
    fmt::print("Unable to setup project path\n");
    return 1;
  }

  lg::initialize();
  lg::set_stdout_level(verbose ? lg::level::info : lg::level::off_unless_die);

  Config config;
  try {
    config = read_config_file(config_path, config_game_version, config_override);
  } catch (const std::exception& e) {
    fmt::print("Failed to parse config: {}\n", e.what());
    return 1;
  }
  if (!object_name.empty()) {
    config.allowed_objects = {object_name};
  }
  if (config.allowed_objects.empty()) {
    fmt::print("No --obj or allowed_objects given, loading every object. This will be slow.\n");
  }
  config.generate_all_types = false;
  config.generate_symbol_definition_map = false;

  in_folder = in_folder / config.game_name;
  if (!fs::exists(in_folder)) {
    fmt::print("in-folder '{}' does not exist\n", in_folder.string());
    return 1;
  }

  Timer timer;
  fmt::print("Loading object files...\n");
  auto db_ptr = load_object_file_db(config, in_folder);
  auto& db = *db_ptr;

  if (try_asm) {
    return try_asm_functions(db, config, options, output_path, timer);
  }
  if (trim && function_name.empty()) {
    return trim_all(db, config, options, output_path, timer);
  }

  auto target = find_target(db, config, function_name);
  if (!target) {
    return 1;
  }
  auto& func = *target->func;

  if (func.suspected_asm) {
    fmt::print("'{}' is treated as an assembly function, there is no cfg to resolve.\n",
               function_name);
    return 1;
  }

  fmt::print("\n{} in {}\n", target->hack_key, target->data->to_unique_name());
  fmt::print("  {} basic blocks, cfg {} with the current config\n", func.basic_blocks.size(),
             func.cfg_ok ? "resolves" : "does NOT resolve");
  auto existing_it = config.hacks.blocks_ending_in_asm_branch_by_func_name.find(target->hack_key);
  if (existing_it != config.hacks.blocks_ending_in_asm_branch_by_func_name.end()) {
    BlockSet existing(existing_it->second.begin(), existing_it->second.end());
    std::sort(existing.begin(), existing.end());
    fmt::print("  existing entry: {} (ignored by the search)\n", format_set(existing));
  }

  Permuter permuter(db, config, *target, options);

  if (trim) {
    auto before = existing_entry(config, target->hack_key);
    if (before.empty()) {
      fmt::print("'{}' has no existing entry to trim.\n", target->hack_key);
      return 1;
    }
    auto after = permuter.trim(before);
    if (!after) {
      fmt::print("With the existing entry: {}. There is no output to compare against.\n",
                 level_description(permuter.trim_reference_level()));
      return 1;
    }
    fmt::print("\nTrimmed {} -> {} block(s), the decompiled output is unchanged.\n", before.size(),
               after->size());
    fmt::print("    \"{}\": {},\n", target->hack_key, format_set(*after));
    fmt::print("\n{} full decompiles in {:.2f}s\n", permuter.verifications(), timer.getSeconds());
    return 0;
  }

  auto required = parse_block_list(required_str);
  std::vector<int> candidates =
      candidates_str.empty() ? permuter.branch_blocks() : parse_block_list(candidates_str);
  candidates.erase(std::remove_if(candidates.begin(), candidates.end(),
                                  [&](int b) {
                                    return std::binary_search(required.begin(), required.end(), b);
                                  }),
                   candidates.end());
  for (int block : merge(required, candidates)) {
    if (block < 0 || block >= (int)func.basic_blocks.size()) {
      fmt::print("Block {} is out of range\n", block);
      return 1;
    }
  }

  fmt::print("  {} candidate block(s) ending in a branch", candidates.size());
  if (!required.empty()) {
    fmt::print(", always including {}", format_set(required));
  }
  fmt::print("\n  using {} worker process(es)\n\n", options.jobs);

  auto result = search(permuter, candidates, required, options, true);
  if (result.best) {
    auto best = *result.best;
    if (options.print_output && options.verify) {
      best = permuter.verify({best.set}, true).front();
    }
    print_result(permuter, best, result.alternatives);
    fmt::print("\n{} cfg evaluations, {} full decompiles in {:.2f}s\n", permuter.evaluations(),
               permuter.verifications(), timer.getSeconds());
    return 0;
  }

  fmt::print("\nNo working set found{}.\n", result.timed_out ? " before the time limit" : "");
  if (result.closest) {
    fmt::print("Closest: {} ({})\n", format_set(result.closest->set),
               level_description(*result.closest->level));
    for (auto& line : str_util::split(result.closest->warnings)) {
      if (!line.empty()) {
        fmt::print("    {}\n", line);
      }
    }
  } else if (result.closest_cfg) {
    fmt::print("Closest cfg: {} with {} unresolved vertices left\n",
               format_set(result.closest_cfg->first), result.closest_cfg->second);
  }
  fmt::print("{} cfg evaluations, {} full decompiles in {:.2f}s\n", permuter.evaluations(),
             permuter.verifications(), timer.getSeconds());
  return 1;
}
