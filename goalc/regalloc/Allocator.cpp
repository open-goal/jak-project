#include <algorithm>
#include "Allocator.h"
#include "LiveInfo.h"

void find_basic_blocks(RegAllocCache* cache, const AllocationInput& in) {
  std::vector<int> dividers;

  dividers.push_back(0);
  dividers.push_back(in.instructions.size());

  // loop over instructions, finding jump targets
  for (uint32_t i = 0; i < in.instructions.size(); i++) {
    const auto& instr = in.instructions[i];
    if (!instr.jumps.empty()) {
      dividers.push_back(i + 1);
      for (auto dest : instr.jumps) {
        dividers.push_back(dest);
      }
    }
  }

  // sort dividers, and make blocks
  std::sort(dividers.begin(), dividers.end(), [](int a, int b) { return a < b; });

  for (uint32_t i = 0; i < dividers.size() - 1; i++) {
    if (dividers[i] != dividers[i + 1]) {
      // new basic block!
      RegAllocBasicBlock block;
      for (int j = dividers[i]; j < dividers[i + 1]; j++) {
        block.instr_idx.push_back(j);
      }

      block.idx = cache->basic_blocks.size();
      cache->basic_blocks.push_back(block);
    }
  }

  if (!cache->basic_blocks.empty()) {
    cache->basic_blocks.front().is_entry = true;
    cache->basic_blocks.back().is_exit = true;
  }

  auto find_basic_block_to_target = [&](int instr) {
    bool found = false;
    uint32_t result = -1;
    for (uint32_t i = 0; i < cache->basic_blocks.size(); i++) {
      if (!cache->basic_blocks[i].instr_idx.empty() &&
          cache->basic_blocks[i].instr_idx.front() == instr) {
        assert(!found);
        found = true;
        result = i;
      }
    }
    if (!found) {
      printf("[RegAlloc Error] couldn't find basic block beginning with instr %d of %ld\n", instr,
             in.instructions.size());
    }
    assert(found);
    return result;
  };

  // link blocks
  for (auto& block : cache->basic_blocks) {
    assert(!block.instr_idx.empty());
    auto& last_instr = in.instructions.at(block.instr_idx.back());
    if (last_instr.fallthrough) {
      // try to link to next block:
      int next_idx = block.idx + 1;
      if (next_idx < (int)cache->basic_blocks.size()) {
        cache->basic_blocks.at(next_idx).pred.push_back(block.idx);
        block.succ.push_back(next_idx);
      }
    }
    for (auto target : last_instr.jumps) {
      cache->basic_blocks.at(find_basic_block_to_target(target)).pred.push_back(block.idx);
      block.succ.push_back(find_basic_block_to_target(target));
    }
  }
}

namespace {
void compute_live_ranges(RegAllocCache* cache, const AllocationInput& in) {
  // then resize live ranges to the correct size
  cache->live_ranges.resize(cache->max_var, LiveInfo(in.instructions.size(), 0));

  // now compute the ranges
  for (auto& block : cache->basic_blocks) {
    // from var use
    for (auto instr_id : block.instr_idx) {
      auto& inst = in.instructions.at(instr_id);

      for (auto& lst : {inst.read, inst.write}) {
        for (auto& x : lst) {
          cache->live_ranges.at(x.id).add_live_instruction(instr_id);
        }
      }
    }

    // and liveliness analysis
    assert(block.live.size() == block.instr_idx.size());
    for (uint32_t i = 0; i < block.live.size(); i++) {
      for (auto& x : block.live[i]) {
        cache->live_ranges.at(x).add_live_instruction(block.instr_idx.at(i));
      }
    }
  }

  // make us alive at any constrained instruction. todo, if this happens is this a sign of an issue
  for (auto& con : in.constraints) {
    cache->live_ranges.at(con.ireg.id).add_live_instruction(con.instr_idx);
  }
}
}  // namespace

void analyze_liveliness(RegAllocCache* cache, const AllocationInput& in) {
  cache->max_var = in.max_vars;
  cache->was_colored.resize(cache->max_var, false);
  cache->iregs.resize(cache->max_var);

  for (auto& instr : in.instructions) {
    for (auto& wr : instr.write) {
      cache->iregs.at(wr.id) = wr;
    }

    for (auto& rd : instr.read) {
      cache->iregs.at(rd.id) = rd;
    }
  }

  // phase 1
  for (auto& block : cache->basic_blocks) {
    block.live.resize(block.instr_idx.size());
    block.dead.resize(block.instr_idx.size());
    block.analyze_liveliness_phase1(in.instructions);
  }

  // phase 2
  bool changed = false;
  do {
    changed = false;
    for (auto& block : cache->basic_blocks) {
      if (block.analyze_liveliness_phase2(cache->basic_blocks, in.instructions)) {
        changed = true;
      }
    }
  } while (changed);

  // phase 3
  for (auto& block : cache->basic_blocks) {
    block.analyze_liveliness_phase3(cache->basic_blocks, in.instructions);
  }

  // phase 4
  compute_live_ranges(cache, in);

  // final setup!
  for (size_t i = 0; i < cache->live_ranges.size(); i++) {
    cache->live_ranges.at(i).prepare_for_allocation(i);
  }
  cache->stack_ops.resize(in.instructions.size());
}

namespace {
template <typename T>
bool in_set(std::set<T>& set, const T& obj) {
  return set.find(obj) != set.end();
}
}  // namespace

void RegAllocBasicBlock::analyze_liveliness_phase1(const std::vector<RegAllocInstr>& instructions) {
  for (int i = instr_idx.size(); i-- > 0;) {
    auto ii = instr_idx.at(i);
    auto& instr = instructions.at(ii);
    auto& lv = live.at(i);
    auto& dd = dead.at(i);

    // make all read live out
    lv.clear();
    for (auto& x : instr.read) {
      lv.insert(x.id);
    }

    // kill things which are overwritten
    dd.clear();
    for (auto& x : instr.write) {
      if (!in_set(lv, x.id)) {
        dd.insert(x.id);
      }
    }

    // b.use = i.liveout
    std::set<int> use_old = use;
    use.clear();
    for (auto& x : lv) {
      use.insert(x);
    }
    // | (bu.use & !i.dead)
    for (auto& x : use_old) {
      if (!in_set(dd, x)) {
        use.insert(x);
      }
    }

    // b.defs = i.dead
    std::set<int> defs_old = defs;
    defs.clear();
    for (auto& x : dd) {
      defs.insert(x);
    }
    // | b.defs & !i.lv
    for (auto& x : defs_old) {
      if (!in_set(lv, x)) {
        defs.insert(x);
      }
    }
  }
}

bool RegAllocBasicBlock::analyze_liveliness_phase2(std::vector<RegAllocBasicBlock>& blocks,
                                                   const std::vector<RegAllocInstr>& instructions) {
  (void)instructions;
  bool changed = false;
  auto out = defs;

  for (auto s : succ) {
    for (auto in : blocks.at(s).input) {
      out.insert(in);
    }
  }

  std::set<int> in = use;
  for (auto x : out) {
    if (!in_set(defs, x)) {
      in.insert(x);
    }
  }

  if (in != input || out != output) {
    changed = true;
    input = in;
    output = out;
  }

  return changed;
}

void RegAllocBasicBlock::analyze_liveliness_phase3(std::vector<RegAllocBasicBlock>& blocks,
                                                   const std::vector<RegAllocInstr>& instructions) {
  (void)instructions;
  std::set<int> live_local;
  for (auto s : succ) {
    for (auto i : blocks.at(s).input) {
      live_local.insert(i);
    }
  }

  for (int i = instr_idx.size(); i-- > 0;) {
    auto& lv = live.at(i);
    auto& dd = dead.at(i);

    std::set<int> new_live = lv;
    for (auto x : live_local) {
      if (!in_set(dd, x)) {
        new_live.insert(x);
      }
    }
    lv = live_local;
    live_local = new_live;
  }
}

std::string RegAllocBasicBlock::print_summary() const {
  std::string result = "block " + std::to_string(idx) + "\nsucc: ";
  for (auto s : succ) {
    result += std::to_string(s) + " ";
  }
  result += "\npred: ";
  for (auto p : pred) {
    result += std::to_string(p) + " ";
  }
  result += "\nuse: ";
  for (auto x : use) {
    result += std::to_string(x) + " ";
  }
  result += "\ndef: ";
  for (auto x : defs) {
    result += std::to_string(x) + " ";
  }
  result += "\ninput: ";
  for (auto x : input) {
    result += std::to_string(x) + " ";
  }
  result += "\noutput: ";
  for (auto x : output) {
    result += std::to_string(x) + " ";
  }

  return result;
}

std::string RegAllocBasicBlock::print(const std::vector<RegAllocInstr>& insts) const {
  std::string result = print_summary() + "\n";
  int k = 0;
  for (auto instr : instr_idx) {
    std::string line = insts.at(instr).print();
    constexpr int pad_len = 30;
    if (line.length() < pad_len) {
      // line.insert(line.begin(), pad_len - line.length(), ' ');
      line.append(pad_len - line.length(), ' ');
    }

    result += "  " + line + " live: ";
    for (auto j : live.at(k)) {
      result += std::to_string(j) + " ";
    }
    result += "\n";

    k++;
  }
  return result;
}