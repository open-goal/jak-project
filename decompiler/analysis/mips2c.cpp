#include <set>

#include "mips2c.h"

#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/print_float.h"
#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/Function/Function.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

/*!
 * Mips2C:
 * The mips2c analysis pass converts mips assembly into C code.  It is a very literal translation.
 * This relies on the helper functions in mips2c_private.h header.
 *
 * We generate a "link" function and an "execute" function. The "link" function performs symbol
 * table lookups and saves the address of the slots in a cache structure used during runtime. It
 * also allocates a stub function on the GOAL heap that jumps to the C++ function in the proper way.
 *
 * The "link" function should be called by the linker when the appropriate object file is linked.
 * It should happen after GOAL linking, but before executing the top-level segment.
 * This _only_ allocates a function, but doesn't set the symbol.
 * You have to do that yourself, in the top level.
 * This order seems weird and annoying, but it makes sure that we get the order of allocations
 * right. It's likely that nothing depends on this, but better to be safe.
 *
 * The "execute" function is the function that should be called from GOAL.
 */

namespace decompiler {

//////////////////////
// Register Helpers
//////////////////////

Register rs7() {
  return make_gpr(Reg::S7);
}

Register rr0() {
  return make_gpr(Reg::R0);
}

Register rfp() {
  return make_gpr(Reg::FP);
}

Register rra() {
  return make_gpr(Reg::RA);
}

Register rt9() {
  return make_gpr(Reg::T9);
}

Register rv0() {
  return make_gpr(Reg::V0);
}

Register rsp() {
  return make_gpr(Reg::SP);
}

Register make_vf(int idx) {
  return Register(Reg::VF, idx);
}

/*!
 * Convert a GOAL symbol name to a valid C++ variable name.
 * dashes become underscores, and !/?/ * are dropped.
 */
std::string goal_to_c_name(const std::string& name) {
  std::string result;
  for (auto c : name) {
    if (c == '!' || c == '?' || c == '*') {
      continue;
    }

    if (c == '-') {
      c = '_';
    }

    result.push_back(c);
  }
  return result;
}

/*!
 * Convert a decompiler function name to a valid C++ variable name.
 */
std::string goal_to_c_function_name(const FunctionName& name) {
  switch (name.kind) {
    case FunctionName::FunctionKind::GLOBAL:
      return goal_to_c_name(name.function_name);
    case FunctionName::FunctionKind::METHOD:
      return fmt::format("method_{}_{}", name.method_id, goal_to_c_name(name.type_name));
    default:
      ASSERT(false);
  }
}

/*!
 * Convert a decompiler register into the name of the register constant in mips2c_private.h
 */
const char* reg_to_name(const InstructionAtom& atom) {
  ASSERT(atom.is_reg());
  return atom.get_reg().to_charp();
}

/*!
 * A line of code in the mips2c function output. Just code + end of line comment.
 */
struct Mips2C_Line {
  std::string code;
  std::string comment;

  Mips2C_Line() = default;
  Mips2C_Line(const std::string& _code) : code(_code) {}
  Mips2C_Line(const std::string& _code, const std::string& _comment)
      : code(_code), comment(_comment) {}
};

/*!
 * Mips2C output. Contains the execute and link function.
 * This is built up in the order of MIPS instructions/labels/comments using the output_* functions.
 * The output is built by write_to_string.
 */
struct Mips2C_Output {
  bool jump_table = false;
  /*!
   * Add a label at the current line.
   */
  void output_label(int block_idx) { lines.push_back(fmt::format("\nblock_{}:", block_idx)); }

  void output_jump_table_block_label(int block_idx) {
    lines.push_back(fmt::format("\ncase {}:", block_idx));
    lines.push_back(fmt::format("next_block = {};", block_idx + 1));
  }

  /*!
   * Add a full line comment at the current line. Includes "//" automatically
   */
  void output_line_comment(const std::string& text) { lines.emplace_back("// " + text); }

  /*!
   * Output code and comment for an instruction.
   */
  void output_instr(const std::string& instr, const std::string& comment) {
    lines.emplace_back(instr, comment);
  }

  /*!
   * Convert the output to a string.
   */
  std::string write_to_string(const FunctionName& goal_func_name,
                              GameVersion version,
                              const std::string& extra = "") const {
    std::string name = goal_to_c_function_name(goal_func_name);
    std::string result = "//--------------------------MIPS2C---------------------\n";
    result += "// clang-format off\n";
    result += "#include \"game/mips2c/mips2c_private.h\"\n";
    result += fmt::format("#include \"game/kernel/{}/kscheme.h\"\n", game_version_names[version]);
    result += fmt::format("using ::{}::intern_from_c;\n", game_version_names[version]);

    // start of namespace for this function
    result += fmt::format("namespace Mips2C::{} {{\n", game_version_names[version]);
    result += fmt::format("namespace {} {{\n", name);

    // definition of the symbol cache.
    if (!symbol_cache.empty()) {
      result += "struct Cache {\n";
      for (auto& sym : symbol_cache) {
        result += fmt::format("  void* {}; // {}\n", goal_to_c_name(sym), sym);
      }
      result += "} cache;\n\n";
    }

    // definition of the function
    // the mips2c_call function will build and pass an ExecutionContext
    result += "u64 execute(void* ctxt) {\n";
    result += "  auto* c = (ExecutionContext*)ctxt;\n";

    // the branch condition (for delay slots)
    result += "  bool bc = false;\n";

    // the function call address (for jalr delay slots)
    result += "  u32 call_addr = 0;\n";

    if (needs_cop1_bc) {
      // the cop1 branch flag (separate from delay slot bc).
      result += "  bool cop1_bc = false;\n";
    }

    if (jump_table) {
      result += "u32 next_block = 0;\n";
      result += "while(true) {\n";
      result += " switch(next_block) {\n";
    }

    // add all lines
    for (auto& line : lines) {
      result += "  ";
      result += line.code;
      if (!line.comment.empty()) {
        if (line.code.length() < 50) {
          for (int i = 0; i < 50 - (int)line.code.length(); i++) {
            result.push_back(' ');
          }
        }
        result.append("// ");
        result.append(line.comment);
      }

      result += '\n';
    }

    if (jump_table) {
      result += " }\n";
      result += "}\n";
    }

    // return!
    result += "end_of_function:\n  return c->gprs[v0].du64[0];\n";
    result += "}\n\n";

    // link function:
    result += "void link() {\n";
    // lookup all symbols
    for (auto& sym : symbol_cache) {
      result += fmt::format("  cache.{} = intern_from_c(\"{}\").c();\n", goal_to_c_name(sym), sym);
    }
    // this adds us to a table for lookup later, and also allocates our trampoline.
    result += fmt::format("  gLinkedFunctionTable.reg(\"{}\", execute, PUT_STACK_SIZE_HERE);\n",
                          goal_func_name.to_string());
    result += "}\n\n";

    result += extra;

    result += fmt::format("}} // namespace {}\n", name);
    result += "} // namespace Mips2C\n";

    // reminder to the user to add a callback to the link function in the linker.
    result +=
        fmt::format("// add {}::link to the link callback table for the object file.\n", name);
    result += "// FWD DEC:\n";
    result += fmt::format("namespace {} {{ extern void link(); }}\n", name);
    return result;
  }

  /*!
   * Adds name to the symbol cache, if it's not there already.
   */
  void require_symbol(const std::string& name) { symbol_cache.insert(name); }

  std::vector<Mips2C_Line> lines;
  std::set<std::string> symbol_cache;
  bool needs_cop1_bc = false;
};

/*!
 * Basic block used for mips2c.
 */
struct M2C_Block {
  int idx = -1;           // block idx
  int succ_branch = -1;   // block idx if we take the branch
  int succ_ft = -1;       // block idx if we don't take the branch (or there is none)
  std::vector<int> pred;  // block idx of predecessors

  int start_instr = -1;  // first instruction idx
  int end_instr = -1;    // last instruction idx (not inclusive)

  bool has_branch = false;     // ends in a branch instruction?
  bool branch_likely = false;  // that branch is likely branch?
  bool branch_always = false;  // that branch is always taken?

  bool has_pred(int pidx) const {
    for (auto p : pred) {
      if (p == pidx) {
        return true;
      }
    }
    return false;
  }
};

/*!
 * Make second_idx be a fallthrough of first_idx.
 */
void link_fall_through(int first_idx, int second_idx, std::vector<M2C_Block>& blocks) {
  auto& first = blocks.at(first_idx);
  auto& second = blocks.at(second_idx);

  ASSERT(first.succ_ft == -1);  // don't want to overwrite something by accident.
  // can only fall through to the next code in memory.
  ASSERT(first_idx + 1 == second_idx);
  first.succ_ft = second_idx;

  if (!second.has_pred(first_idx)) {
    // if a block can block fall through and branch to the same block, we want to avoid adding
    // it as a pred twice. This is rare, but does happen and makes sense with likely branches
    // which only run the delay slot when taken.
    second.pred.push_back(first_idx);
  }
}

/*!
 * Make second_idx be the branch destination of first_idx.
 */
void link_branch(int first_idx, int second_idx, std::vector<M2C_Block>& blocks) {
  auto& first = blocks.at(first_idx);
  auto& second = blocks.at(second_idx);
  ASSERT(first.succ_branch == -1);
  first.succ_branch = second_idx;

  if (!second.has_pred(first_idx)) {
    // see comment in link_fall_through
    second.pred.push_back(first_idx);
  }
}

/*!
 * Make second_idx be the fall through of a likely branch (after the delay slot)
 */
void link_fall_through_likely(int first_idx, int second_idx, std::vector<M2C_Block>& blocks) {
  auto& first = blocks.at(first_idx);
  auto& second = blocks.at(second_idx);
  ASSERT(first.succ_ft == -1);  // don't want to overwrite something by accident.
  // can only fall through to the next code in memory.
  ASSERT(first_idx + 2 == second_idx);

  first.succ_ft = second_idx;

  if (!second.has_pred(first_idx)) {
    // see comment in link_fall_through
    second.pred.push_back(first_idx);
  }
}

/*!
 * Compute predecessor and successor of each block
 *
 * NOTE: due to likely branch delays/etc, succ_ft and succ_branch behave strangely.
 *
 * The succ_ft destination is always taken if has_branch is false.
 * The succ_branch destination is always taken if branch_always it true.
 *
 * Otherwise, has_branch is true, and the succ_branch is taken if the branch condition is true.
 * The succ_ft and succ_branch may be _anywhere_. succ_ft may not always be the next destination.
 */
std::vector<M2C_Block> setup_preds_and_succs(const Function& func,
                                             const LinkedObjectFile& file,
                                             std::unordered_set<int>& likely_delay_slot_blocks) {
  // create m2c blocks
  std::vector<M2C_Block> blocks;
  blocks.resize(func.basic_blocks.size());
  for (size_t i = 0; i < blocks.size(); i++) {
    blocks[i].idx = i;
    blocks[i].start_instr = func.basic_blocks[i].start_word;
    blocks[i].end_instr = func.basic_blocks[i].end_word;
  }

  // set up succ / pred
  for (int i = 0; i < int(func.basic_blocks.size()); i++) {
    auto& b = func.basic_blocks[i];
    if (blocks.at(i).branch_always) {
      // likely branch, already set up.
      ASSERT(likely_delay_slot_blocks.count(i));
      continue;
    } else {
      ASSERT(!likely_delay_slot_blocks.count(i));
    }
    bool not_last = (i + 1) < int(func.basic_blocks.size());

    if (b.end_word == b.start_word) {
      // there's no room for a branch here, fall through to the end
      if (not_last) {
        link_fall_through(i, i + 1, blocks);
      }
    } else {
      // room for at least a likely branch, try that first.
      int likely_branch_idx = b.end_word - 1;
      ASSERT(likely_branch_idx >= b.start_word);
      auto& likely_branch_candidate = func.instructions.at(likely_branch_idx);

      if (is_branch(likely_branch_candidate, true)) {
        // is a likely branch
        blocks.at(i).has_branch = true;
        blocks.at(i).branch_likely = true;
        // blocks.at(i).kind = CfgVtx::DelaySlotKind::NO_DELAY;
        bool branch_always = is_always_branch(likely_branch_candidate);

        // need to find block target
        int block_target = -1;
        int label_target = likely_branch_candidate.get_label_target();
        ASSERT(label_target != -1);
        const auto& label = file.labels.at(label_target);
        // ASSERT(label.target_segment == seg);
        ASSERT((label.offset % 4) == 0);
        int offset = label.offset / 4 - func.start_word;
        ASSERT(offset >= 0);
        for (int j = int(func.basic_blocks.size()); j-- > 0;) {
          if (func.basic_blocks[j].start_word == offset) {
            block_target = j;
            break;
          }
        }

        ASSERT(block_target != -1);
        // "branch" to delay slot, which then "falls through" to the destination.
        link_branch(i, i + 1, blocks);

        if (branch_always) {
          // don't continue to the next one
          blocks.at(i).branch_always = true;
        } else {
          // not an always branch
          if (not_last) {
            // "fall through" to after the delay slot block.
            // don't take the delay slot.
            link_fall_through_likely(i, i + 2, blocks);
          }
        }

        auto& delay_block = blocks.at(i + 1);
        delay_block.branch_likely = false;
        delay_block.branch_always = true;
        delay_block.has_branch = true;
        auto inserted = likely_delay_slot_blocks.insert(i + 1).second;
        ASSERT(inserted);
        link_branch(i + 1, block_target, blocks);

      } else {
        if (b.end_word - b.start_word < 2) {
          // no room for a branch, just fall through
          if (not_last) {
            link_fall_through(i, i + 1, blocks);
          }
        } else {
          // try as a normal branch.
          int idx = b.end_word - 2;
          ASSERT(idx >= b.start_word);
          auto& branch_candidate = func.instructions.at(idx);
          if (is_branch(branch_candidate, false)) {
            blocks.at(i).has_branch = true;
            blocks.at(i).branch_likely = false;
            bool branch_always = is_always_branch(branch_candidate);

            // need to find block target
            int block_target = -1;
            int label_target = branch_candidate.get_label_target();
            ASSERT(label_target != -1);
            const auto& label = file.labels.at(label_target);
            // ASSERT(label.target_segment == seg);
            ASSERT((label.offset % 4) == 0);
            int offset = label.offset / 4 - func.start_word;
            ASSERT(offset >= 0);

            for (int j = int(func.basic_blocks.size()); j-- > 0;) {
              if (func.basic_blocks[j].start_word == offset) {
                block_target = j;
                break;
              }
            }

            ASSERT(block_target != -1);
            link_branch(i, block_target, blocks);

            if (branch_always) {
              // don't continue to the next one
              blocks.at(i).branch_always = true;
            } else {
              // not an always branch
              if (not_last) {
                link_fall_through(i, i + 1, blocks);
              }
            }
          } else {
            // not a branch.
            if (not_last) {
              link_fall_through(i, i + 1, blocks);
            }
          }
        }
      }
    }
  }
  return blocks;
}

/*!
 * Does the given block require a label in front of it?
 */
bool block_requires_label(const Function* f,
                          const std::vector<M2C_Block>& blocks,
                          size_t block_idx) {
  const auto& block = blocks[block_idx];
  if (block.pred.empty()) {
    // no way to get to this block??
    if (block_idx != 0) {
      // don't warn on the first block.
      lg::warn("Mips2C function {} block {} is unreachable.", f->name(), block_idx);
    }
    return false;
  }

  if (block.pred.size() == 1 && block_idx > 0 && block.pred.front() == (int)block_idx - 1 &&
      blocks.at(block_idx - 1).succ_ft == (int)block_idx) {
    // the only way to get to this block is to fall through, no need for a label.
    return false;
  }

  return true;
}

namespace {
// hack counter for total number of unknown instruction. TODO remove
int g_unknown = 0;
}  // namespace

/*!
 * Complain about an unknown instruction.
 */
Mips2C_Line handle_unknown(const std::string& instr_str) {
  g_unknown++;
  lg::warn("mips2c unknown: {}", instr_str);
  return fmt::format("// Unknown instr: {}", instr_str);
}

Mips2C_Line handle_generic_load(const Instruction& i0, const std::string& instr_str) {
  if (!i0.get_src(0).is_imm()) {
    // might be a load relative to a label
    return handle_unknown(instr_str);
  }
  return {fmt::format("c->{}({}, {}, {});", i0.op_name_to_string(), reg_to_name(i0.get_dst(0)),
                      i0.get_src(0).get_imm(), reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_lwc1(const Instruction& i0,
                        const std::string& instr_str,
                        const LinkedObjectFile* file) {
  if (i0.get_src(0).is_label() && i0.get_src(1).is_reg(Register(Reg::GPR, Reg::FP))) {
    auto& label = file->labels.at(i0.get_src(0).get_label());
    auto& word = file->words_by_seg.at(label.target_segment).at(label.offset / 4);
    ASSERT(word.kind() == LinkedWord::PLAIN_DATA);
    float f;
    memcpy(&f, &word.data, 4);
    return {fmt::format("c->fprs[{}] = {};", reg_to_name(i0.get_dst(0)), float_to_string(f)),
            instr_str};
  } else {
    return handle_generic_load(i0, instr_str);
  }
}

Mips2C_Line handle_lw(Mips2C_Output& out,
                      const Instruction& i0,
                      const std::string& instr_str,
                      const LinkedObjectFile* file,
                      GameVersion version) {
  if (i0.get_src(1).is_reg(rs7()) && i0.get_src(0).is_sym()) {
    // symbol load.
    out.require_symbol(i0.get_src(0).get_sym());
    return {fmt::format("c->load_symbol{}({}, cache.{});", version == GameVersion::Jak1 ? "" : "2",
                        reg_to_name(i0.get_dst(0)), goal_to_c_name(i0.get_src(0).get_sym())),
            instr_str};
  }
  if (i0.get_src(1).is_reg(rfp()) && i0.get_src(0).is_label()) {
    const auto& label = file->labels.at(i0.get_src(0).get_label());
    ASSERT((label.offset % 4) == 0);
    const auto& word = file->words_by_seg.at(label.target_segment).at(label.offset / 4);
    ASSERT(word.kind() == LinkedWord::PLAIN_DATA);
    u32 val_u32 = word.data;
    float val_float;
    memcpy(&val_float, &val_u32, 4);
    std::string comment = fmt::format("{} {}", instr_str, float_to_string(val_float));
    return {fmt::format("c->lw_float_constant({}, 0x{:08x});", reg_to_name(i0.get_dst(0)), val_u32),
            comment};
  } else {
    // fall back to standard loads
    return handle_generic_load(i0, instr_str);
  }
}

Mips2C_Line handle_generic_store(Mips2C_Output& /*out*/,
                                 const Instruction& i0,
                                 const std::string& instr_str) {
  return {fmt::format("c->{}({}, {}, {});", i0.op_name_to_string(), reg_to_name(i0.get_src(0)),
                      i0.get_src(1).get_imm(), reg_to_name(i0.get_src(2))),
          instr_str};
}

Mips2C_Line handle_generic_op2_u16(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->{}({}, {}, {});", i0.op_name_to_string(), reg_to_name(i0.get_dst(0)),
                      reg_to_name(i0.get_src(0)), i0.get_src(1).get_imm()),
          instr_str};
}

Mips2C_Line handle_daddiu(Mips2C_Output& out,
                          const Instruction& i0,
                          const std::string& instr_str,
                          GameVersion version) {
  if (i0.get_src(1).is_label()) {
    return {instr_str, instr_str};
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_sym("#t")) {
    return {fmt::format("c->{}({}, {}, {});", i0.op_name_to_string(), reg_to_name(i0.get_dst(0)),
                        reg_to_name(i0.get_src(0)), true_symbol_offset(version)),
            instr_str};
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_sym()) {
    out.require_symbol(i0.get_src(1).get_sym());
    return {fmt::format("c->load_symbol_addr({}, cache.{});", reg_to_name(i0.get_dst(0)),
                        goal_to_c_name(i0.get_src(1).get_sym())),
            instr_str};
  } else {
    return handle_generic_op2_u16(i0, instr_str);
  }
}

Mips2C_Line handle_sw(Mips2C_Output& out,
                      const Instruction& i0,
                      const std::string& instr_str,
                      GameVersion version) {
  if (i0.get_src(1).is_sym() && i0.get_src(2).is_reg(rs7())) {
    out.require_symbol(i0.get_src(1).get_sym());
    return {fmt::format("c->store_symbol{}({}, cache.{});", version == GameVersion::Jak1 ? "" : "2",
                        reg_to_name(i0.get_src(0)), goal_to_c_name(i0.get_src(1).get_sym())),
            instr_str};
    return handle_unknown(instr_str);
    //    auto name = i0.get_src(1).get_sym();
    //    // store into symbol table!
    //    SimpleAtom val;
    //    if (i0.get_src(0).is_reg(rs7())) {
    //      // store a false
    //      val = SimpleAtom::make_sym_val("#f");
    //    } else if (i0.get_src(0).is_reg(rr0())) {
    //      // store a 0
    //      val = SimpleAtom::make_int_constant(0);
    //    } else {
    //      // store a register.
    //      val = make_src_atom(i0.get_src(0).get_reg(), idx);
    //    }
    //    return std::make_unique<StoreOp>(4, StoreOp::Kind::INTEGER,
    //                                     SimpleAtom::make_sym_val(name).as_expr(), val, idx);
  } else {
    return handle_generic_store(out, i0, instr_str);
  }
}

std::string dest_to_char(u8 x) {
  if (x == 0) {
    return "NONE";
  }
  std::string dest;
  if (x & 8)
    dest.push_back('x');
  if (x & 4)
    dest.push_back('y');
  if (x & 2)
    dest.push_back('z');
  if (x & 1)
    dest.push_back('w');
  return dest;
}

Mips2C_Line handle_generic_op3_bc_mask(const Instruction& i0,
                                       const std::string& instr_str,
                                       const std::string& op_name) {
  return {fmt::format("c->{}(DEST::{}, BC::{}, {}, {}, {});", op_name, dest_to_char(i0.cop2_dest),
                      i0.cop2_bc_to_char(), reg_to_name(i0.get_dst(0)), reg_to_name(i0.get_src(0)),
                      reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_generic_op3_mask(const Instruction& i0,
                                    const std::string& instr_str,
                                    const std::string& op_name) {
  return {fmt::format("c->{}(DEST::{}, {}, {}, {});", op_name, dest_to_char(i0.cop2_dest),
                      reg_to_name(i0.get_dst(0)), reg_to_name(i0.get_src(0)),
                      reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_generic_op3(const Instruction& i0,
                               const std::string& instr_str,
                               const std::optional<std::string>& name_override) {
  return {fmt::format("c->{}({}, {}, {});", name_override ? *name_override : i0.op_name_to_string(),
                      reg_to_name(i0.get_dst(0)), reg_to_name(i0.get_src(0)),
                      reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_generic_op2_mask(const Instruction& i0,
                                    const std::string& instr_str,
                                    const std::string& op_name) {
  return {fmt::format("c->{}(DEST::{}, {}, {});", op_name, dest_to_char(i0.cop2_dest),
                      reg_to_name(i0.get_dst(0)), reg_to_name(i0.get_src(0))),
          instr_str};
}

Mips2C_Line handle_generic_op2(const Instruction& i0,
                               const std::string& instr_str,
                               const std::string& op_name) {
  return {fmt::format("c->{}({}, {});", op_name, reg_to_name(i0.get_dst(0)),
                      reg_to_name(i0.get_src(0))),
          instr_str};
}

Mips2C_Line handle_plain_op(const Instruction& /*i0*/,
                            const std::string& instr_str,
                            const std::string& op_name) {
  return {fmt::format("c->{}();", op_name), instr_str};
}

Mips2C_Line handle_or(const Instruction& i0, const std::string& instr_str) {
  if (is_gpr_3(i0, InstructionKind::OR, {}, rs7(), rr0())) {
    // set reg_dest to #f : or reg_dest, s7, r0
    return {
        fmt::format("c->mov64({}, {});", reg_to_name(i0.get_dst(0)), reg_to_name(i0.get_src(0))),
        instr_str};
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, rr0(), rr0())) {
    // set reg_dest to 0 : or reg_dest, r0, r0
    return {fmt::format("c->gprs[{}].du64[0] = 0;", reg_to_name(i0.get_dst(0))), instr_str};
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, {}, rr0())) {
    // set dst to src : or dst, src, r0
    return {
        fmt::format("c->mov64({}, {});", reg_to_name(i0.get_dst(0)), reg_to_name(i0.get_src(0))),
        instr_str};
  } else {
    // actually do a logical OR of two registers: or a0, a1, a2
    return handle_generic_op3(i0, instr_str, "or_");
  }
}

Mips2C_Line handle_sll(const Instruction& i0, const std::string& instr_str) {
  if (is_nop(i0)) {
    return {"// nop", instr_str};
  } else {
    return handle_generic_op2_u16(i0, instr_str);
  }
}

Mips2C_Line handle_vmula_bc(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->vmula_bc(DEST::{}, BC::{}, {}, {});", dest_to_char(i0.cop2_dest),
                      i0.cop2_bc_to_char(), reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_vmadda_bc(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->vmadda_bc(DEST::{}, BC::{}, {}, {});", dest_to_char(i0.cop2_dest),
                      i0.cop2_bc_to_char(), reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_vmadda(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->vmadda(DEST::{},  {}, {});", dest_to_char(i0.cop2_dest),
                      reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_vmsuba(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->vmsuba(DEST::{},  {}, {});", dest_to_char(i0.cop2_dest),
                      reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_vmula(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->vmula(DEST::{},  {}, {});", dest_to_char(i0.cop2_dest),
                      reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_vadda_bc(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->vadda_bc(DEST::{}, BC::{}, {}, {});", dest_to_char(i0.cop2_dest),
                      i0.cop2_bc_to_char(), reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_vmsuba_bc(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->vmsuba_bc(DEST::{}, BC::{}, {}, {});", dest_to_char(i0.cop2_dest),
                      i0.cop2_bc_to_char(), reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_str};
}

std::string reg64_or_zero(const InstructionAtom& atom) {
  if (atom.is_reg(Register(Reg::GPR, Reg::R0))) {
    return "0";
  } else {
    return fmt::format("c->sgpr64({})", atom.get_reg().to_string());
  }
}

Mips2C_Line handle_branch_reg2(const Instruction& i0,
                               const std::string& instr_str,
                               const std::string& op_name) {
  return {fmt::format("bc = {} {} {};", reg64_or_zero(i0.get_src(0)), op_name,
                      reg64_or_zero(i0.get_src(1))),
          instr_str};
}

Mips2C_Line handle_non_likely_branch_bc(const Instruction& i0, const std::string& instr_str) {
  switch (i0.kind) {
    case InstructionKind::BNE:
      return handle_branch_reg2(i0, instr_str, "!=");
    case InstructionKind::BEQ:
      return handle_branch_reg2(i0, instr_str, "==");
    case InstructionKind::BLTZ:
      return {fmt::format("bc = ((s64){}) < 0;", reg64_or_zero(i0.get_src(0))), instr_str};
    case InstructionKind::BGTZ:
      return {fmt::format("bc = ((s64){}) > 0;", reg64_or_zero(i0.get_src(0))), instr_str};
    case InstructionKind::BGEZ:
      return {fmt::format("bc = ((s64){}) >= 0;", reg64_or_zero(i0.get_src(0))), instr_str};
    case InstructionKind::BLEZ:
      return {fmt::format("bc = ((s64){}) <= 0;", reg64_or_zero(i0.get_src(0))), instr_str};
    case InstructionKind::BC1F:
      return {fmt::format("bc = !cop1_bc;"), instr_str};
    case InstructionKind::BC1T:
      return {fmt::format("bc = cop1_bc;"), instr_str};
    default:
      return handle_unknown(instr_str);
  }
}

Mips2C_Line handle_likely_branch_bc(const Instruction& i0, const std::string& instr_str) {
  switch (i0.kind) {
    case InstructionKind::BLTZL:
      return {fmt::format("((s64){}) < 0", reg64_or_zero(i0.get_src(0))), instr_str};
    case InstructionKind::BGEZL:
      return {fmt::format("((s64){}) >= 0", reg64_or_zero(i0.get_src(0))), instr_str};
    case InstructionKind::BGTZL:
      return {fmt::format("((s64){}) > 0", reg64_or_zero(i0.get_src(0))), instr_str};
    case InstructionKind::BNEL:
      return {fmt::format("((s64){}) != ((s64){})", reg64_or_zero(i0.get_src(0)),
                          reg64_or_zero(i0.get_src(1))),
              instr_str};
    case InstructionKind::BEQL:
      return {fmt::format("((s64){}) == ((s64){})", reg64_or_zero(i0.get_src(0)),
                          reg64_or_zero(i0.get_src(1))),
              instr_str};
    case InstructionKind::BC1TL:
      return {"cop1_bc", instr_str};
    case InstructionKind::BC1FL:
      return {"!cop1_bc", instr_str};
    default:
      return handle_unknown(instr_str);
  }
}

Mips2C_Line handle_vdiv(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->vdiv({}, BC::{}, {}, BC::{});", reg_to_name(i0.get_src(0)),
                      "xyzw"[i0.get_src(1).get_vf_field()], reg_to_name(i0.get_src(2)),
                      "xyzw"[i0.get_src(3).get_vf_field()]),
          instr_string};
}

Mips2C_Line handle_vrsqrt(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->vrsqrt({}, BC::{}, {}, BC::{});", reg_to_name(i0.get_src(0)),
                      "xyzw"[i0.get_src(1).get_vf_field()], reg_to_name(i0.get_src(2)),
                      "xyzw"[i0.get_src(3).get_vf_field()]),
          instr_string};
}

Mips2C_Line handle_vsqrt(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->vsqrt({}, BC::{});", reg_to_name(i0.get_src(0)),
                      "xyzw"[i0.get_src(1).get_vf_field()]),
          instr_string};
}

Mips2C_Line handle_vrxor(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->vrxor({}, BC::{});", reg_to_name(i0.get_src(0)), i0.cop2_bc_to_char()),
          instr_string};
}

Mips2C_Line handle_vrget(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->vrget(DEST::{}, {});", dest_to_char(i0.cop2_dest),
                      reg_to_name(i0.get_dst(0))),
          instr_string};
}

Mips2C_Line handle_vrnext(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->vrnext(DEST::{}, {});", dest_to_char(i0.cop2_dest),
                      reg_to_name(i0.get_dst(0))),
          instr_string};
}

Mips2C_Line handle_por(const Instruction& i0, const std::string& instr_string) {
  if (is_gpr_3(i0, InstructionKind::POR, {}, {}, rr0())) {
    return {fmt::format("c->mov128_gpr_gpr({}, {});", reg_to_name(i0.get_dst(0)),
                        reg_to_name(i0.get_src(0))),
            instr_string};
  } else {
    return handle_generic_op3(i0, instr_string, {});
  }
}

Mips2C_Line handle_vopmula(const Instruction& i0, const std::string& instr_string) {
  return {
      fmt::format("c->vopmula({}, {});", reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
      instr_string};
}

Mips2C_Line handle_vopmsub(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->vopmsub({}, {}, {});", reg_to_name(i0.get_dst(0)),
                      reg_to_name(i0.get_src(0)), reg_to_name(i0.get_src(1))),
          instr_string};
}

Mips2C_Line handle_lui(const Instruction& i0,
                       const std::string& instr_string,
                       Mips2C_Output& op,
                       GameVersion version) {
  if (i0.get_src(0).get_imm() == 0x7000) {
    op.require_symbol("*fake-scratchpad-data*");
    return {fmt::format("get_fake_spad_addr{}({}, cache.fake_scratchpad_data, 0, c);",
                        version == GameVersion::Jak1 ? "" : "2", reg_to_name(i0.get_dst(0))),
            instr_string};
  } else {
    return {fmt::format("c->lui({}, {});", reg_to_name(i0.get_dst(0)), i0.get_src(0).get_imm()),
            instr_string};
  }
}

Mips2C_Line handle_clts(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("cop1_bc = c->fprs[{}] < c->fprs[{}];", reg_to_name(i0.get_src(0)),
                      reg_to_name(i0.get_src(1))),
          instr_string};
}

Mips2C_Line handle_cles(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("cop1_bc = c->fprs[{}] <= c->fprs[{}];", reg_to_name(i0.get_src(0)),
                      reg_to_name(i0.get_src(1))),
          instr_string};
}

Mips2C_Line handle_ceqs(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("cop1_bc = c->fprs[{}] == c->fprs[{}];", reg_to_name(i0.get_src(0)),
                      reg_to_name(i0.get_src(1))),
          instr_string};
}

Mips2C_Line handle_pmfhl_lh(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->pmfhl_lh({});", reg_to_name(i0.get_dst(0))), instr_string};
}

Mips2C_Line handle_ctc2(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("{} = c->gpr_src({}).du16[0];", reg_to_name(i0.get_dst(0)),
                      reg_to_name(i0.get_src(0))),
          instr_string};
}

Mips2C_Line handle_cfc2(const Instruction& i0, const std::string& instr_string) {
  return {fmt::format("c->gprs[{}].du64[0] = {};", reg_to_name(i0.get_dst(0)),
                      reg_to_name(i0.get_src(0))),
          instr_string};
}

Mips2C_Line handle_normal_instr(Mips2C_Output& output,
                                const Instruction& i0,
                                const std::string& instr_str,
                                int& unknown_count,
                                const LinkedObjectFile* file,
                                GameVersion version) {
  switch (i0.kind) {
    case InstructionKind::CTC2:
      return handle_ctc2(i0, instr_str);
    case InstructionKind::CFC2:
      return handle_cfc2(i0, instr_str);
    case InstructionKind::LW:
      return handle_lw(output, i0, instr_str, file, version);
    case InstructionKind::LB:
    case InstructionKind::LWL:
    case InstructionKind::LWR:
    case InstructionKind::LDR:
    case InstructionKind::LDL:
    case InstructionKind::LBU:
    case InstructionKind::LWU:
    case InstructionKind::LQ:
    case InstructionKind::LQC2:
    case InstructionKind::LH:
    case InstructionKind::LHU:
    case InstructionKind::LD:
      return handle_generic_load(i0, instr_str);
    case InstructionKind::LWC1:
      return handle_lwc1(i0, instr_str, file);
    case InstructionKind::SQ:
    case InstructionKind::SQC2:
    case InstructionKind::SH:
    case InstructionKind::SD:
    case InstructionKind::SB:
    case InstructionKind::SWC1:
      return handle_generic_store(output, i0, instr_str);
    case InstructionKind::VADD_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vadd_bc");
    case InstructionKind::VMINI_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vmini_bc");
    case InstructionKind::VMAX_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vmax_bc");
    case InstructionKind::VSUB_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vsub_bc");
    case InstructionKind::VMUL_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vmul_bc");
    case InstructionKind::VMADD:
      return handle_generic_op3_mask(i0, instr_str, "vmadd");
    case InstructionKind::VMSUB:
      return handle_generic_op3_mask(i0, instr_str, "vmsub");
    case InstructionKind::VMUL:
      return handle_generic_op3_mask(i0, instr_str, "vmul");
    case InstructionKind::VADD:
      return handle_generic_op3_mask(i0, instr_str, "vadd");
    case InstructionKind::VSUB:
      return handle_generic_op3_mask(i0, instr_str, "vsub");
    case InstructionKind::VMINI:
      return handle_generic_op3_mask(i0, instr_str, "vmini");
    case InstructionKind::VMAX:
      return handle_generic_op3_mask(i0, instr_str, "vmax");
    case InstructionKind::OR:
      return handle_or(i0, instr_str);
    case InstructionKind::SW:
      return handle_sw(output, i0, instr_str, version);
    case InstructionKind::VMOVE:
      return handle_generic_op2_mask(i0, instr_str, "vmove");
    case InstructionKind::VITOF0:
      return handle_generic_op2_mask(i0, instr_str, "vitof0");
    case InstructionKind::VITOF12:
      return handle_generic_op2_mask(i0, instr_str, "vitof12");
    case InstructionKind::VITOF15:
      return handle_generic_op2_mask(i0, instr_str, "vitof15");
    case InstructionKind::VFTOI0:
      return handle_generic_op2_mask(i0, instr_str, "vftoi0");
    case InstructionKind::VFTOI4:
      return handle_generic_op2_mask(i0, instr_str, "vftoi4");
    case InstructionKind::VFTOI12:
      return handle_generic_op2_mask(i0, instr_str, "vftoi12");
    case InstructionKind::VABS:
      return handle_generic_op2_mask(i0, instr_str, "vabs");
    case InstructionKind::VADDQ:
      return handle_generic_op2_mask(i0, instr_str, "vaddq");
    case InstructionKind::VMR32:
      return handle_generic_op2_mask(i0, instr_str, "vmr32");
    case InstructionKind::ANDI:
    case InstructionKind::ORI:
    case InstructionKind::XORI:
    case InstructionKind::SRA:
    case InstructionKind::DSLL:
    case InstructionKind::DSLL32:
    case InstructionKind::DSRA:
    case InstructionKind::DSRA32:
    case InstructionKind::DSRL32:
    case InstructionKind::DSRL:
    case InstructionKind::SRL:
    case InstructionKind::PSRAW:
    case InstructionKind::PSRAH:
    case InstructionKind::PSRLH:
    case InstructionKind::PSLLW:
    case InstructionKind::PSLLH:
      return handle_generic_op2_u16(i0, instr_str);
    case InstructionKind::SLL:
      return handle_sll(i0, instr_str);
    case InstructionKind::DADDU:
    case InstructionKind::DSUBU:
    case InstructionKind::ADDU:
    case InstructionKind::PEXTLH:
    case InstructionKind::PEXTLB:
    case InstructionKind::MOVN:
    case InstructionKind::PEXTUW:
    case InstructionKind::PEXTUH:
    case InstructionKind::PEXTLW:
    case InstructionKind::PCPYUD:
    case InstructionKind::PCPYLD:
    case InstructionKind::PPACH:
    case InstructionKind::PINTEH:
    case InstructionKind::PCGTW:
    case InstructionKind::PPACB:
    case InstructionKind::PADDW:
    case InstructionKind::PADDB:
    case InstructionKind::PEXTUB:
    case InstructionKind::PMULTH:
    case InstructionKind::PMADDH:
    case InstructionKind::PADDH:
    case InstructionKind::PSUBH:
    case InstructionKind::PMINH:
    case InstructionKind::MOVZ:
    case InstructionKind::MULT3:
    case InstructionKind::MULTU3:
    case InstructionKind::PMINW:
    case InstructionKind::PMAXW:
    case InstructionKind::PMAXH:
    case InstructionKind::SUBU:
    case InstructionKind::SLT:
    case InstructionKind::SLTU:
    case InstructionKind::DSRAV:
    case InstructionKind::DSLLV:
    case InstructionKind::DSRLV:
    case InstructionKind::SLLV:
    case InstructionKind::PAND:
    case InstructionKind::PCEQB:
    case InstructionKind::PPACW:
    case InstructionKind::PCEQW:
      return handle_generic_op3(i0, instr_str, {});
    case InstructionKind::MULS:
      return handle_generic_op3(i0, instr_str, "muls");
    case InstructionKind::DIVS:
      return handle_generic_op3(i0, instr_str, "divs");
    case InstructionKind::ADDS:
      return handle_generic_op3(i0, instr_str, "adds");
    case InstructionKind::SUBS:
      return handle_generic_op3(i0, instr_str, "subs");
    case InstructionKind::MINS:
      return handle_generic_op3(i0, instr_str, "mins");
    case InstructionKind::MAXS:
      return handle_generic_op3(i0, instr_str, "maxs");
    case InstructionKind::XOR:
      return handle_generic_op3(i0, instr_str, "xor_");
    case InstructionKind::AND:
      return handle_generic_op3(i0, instr_str, "and_");  // and isn't allowed in C++
    case InstructionKind::NOR:
      return handle_generic_op3(i0, instr_str, "nor");  // and isn't allowed in C++
    case InstructionKind::DADDIU:
      return handle_daddiu(output, i0, instr_str, version);
    case InstructionKind::ADDIU:
    case InstructionKind::SLTI:
    case InstructionKind::SLTIU:
      return handle_generic_op2_u16(i0, instr_str);
    case InstructionKind::QMTC2:
      return handle_generic_op2(i0, instr_str, "mov128_vf_gpr");
    case InstructionKind::QMFC2:
      return handle_generic_op2(i0, instr_str, "mov128_gpr_vf");
    case InstructionKind::VMULA_BC:
      return handle_vmula_bc(i0, instr_str);
    case InstructionKind::VMADDA_BC:
      return handle_vmadda_bc(i0, instr_str);
    case InstructionKind::VADDA_BC:
      return handle_vadda_bc(i0, instr_str);
    case InstructionKind::VMSUBA_BC:
      return handle_vmsuba_bc(i0, instr_str);
    case InstructionKind::VMADDA:
      return handle_vmadda(i0, instr_str);
    case InstructionKind::VMSUBA:
      return handle_vmsuba(i0, instr_str);
    case InstructionKind::VMULA:
      return handle_vmula(i0, instr_str);
    case InstructionKind::VMADD_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vmadd_bc");
    case InstructionKind::VMSUB_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vmsub_bc");
    case InstructionKind::VDIV:
      return handle_vdiv(i0, instr_str);
    case InstructionKind::VSQRT:
      return handle_vsqrt(i0, instr_str);
    case InstructionKind::VRSQRT:
      return handle_vrsqrt(i0, instr_str);
    case InstructionKind::VRXOR:
      return handle_vrxor(i0, instr_str);
    case InstructionKind::VRGET:
      return handle_vrget(i0, instr_str);
    case InstructionKind::VRNEXT:
      return handle_vrnext(i0, instr_str);
    case InstructionKind::POR:
      return handle_por(i0, instr_str);
    case InstructionKind::VMULQ:
      return handle_generic_op2_mask(i0, instr_str, "vmulq");
    case InstructionKind::VNOP:
      return {"// nop", instr_str};
    case InstructionKind::MFC1:
      return handle_generic_op2(i0, instr_str, "mfc1");
    case InstructionKind::MTC1:
      return handle_generic_op2(i0, instr_str, "mtc1");
    case InstructionKind::NEGS:
      return handle_generic_op2(i0, instr_str, "negs");
    case InstructionKind::ABSS:
      return handle_generic_op2(i0, instr_str, "abss");
    case InstructionKind::MOVS:
      return handle_generic_op2(i0, instr_str, "movs");
    case InstructionKind::CVTWS:
      return handle_generic_op2(i0, instr_str, "cvtws");
    case InstructionKind::CVTSW:
      return handle_generic_op2(i0, instr_str, "cvtsw");
    case InstructionKind::PEXEW:
      return handle_generic_op2(i0, instr_str, "pexew");
    case InstructionKind::PEXCW:
      return handle_generic_op2(i0, instr_str, "pexcw");
    case InstructionKind::SQRTS:
      return handle_generic_op2(i0, instr_str, "sqrts");
    case InstructionKind::PLZCW:
      return handle_generic_op2(i0, instr_str, "plzcw");
    case InstructionKind::PCPYH:
      return handle_generic_op2(i0, instr_str, "pcpyh");
    case InstructionKind::PROT3W:
      return handle_generic_op2(i0, instr_str, "prot3w");
    case InstructionKind::LUI:
      return handle_lui(i0, instr_str, output, version);
    case InstructionKind::CLTS:
      output.needs_cop1_bc = true;
      return handle_clts(i0, instr_str);
    case InstructionKind::CLES:
      output.needs_cop1_bc = true;
      return handle_cles(i0, instr_str);
    case InstructionKind::CEQS:
      output.needs_cop1_bc = true;
      return handle_ceqs(i0, instr_str);
    case InstructionKind::VWAITQ:
      return handle_plain_op(i0, instr_str, "vwaitq");
    case InstructionKind::VOPMULA:
      return handle_vopmula(i0, instr_str);
    case InstructionKind::VOPMSUB:
      return handle_vopmsub(i0, instr_str);
    case InstructionKind::PMFHL_LH:
      return handle_pmfhl_lh(i0, instr_str);
    default:
      unknown_count++;
      return handle_unknown(instr_str);

      break;
  }
}

struct JumpTableBlock {
  int idx = -1;
  int start_instr = -1;
  int end_instr = -1;          // not inclusive
  int succ_branch = -1;        // block idx if we take the branch
  int succ_ft = -1;            // block idx if we don't take the branch (or there is none)
  bool has_branch = false;     // ends in a branch instruction?
  bool branch_likely = false;  // that branch is likely branch?
  bool branch_always = false;  // that branch is always taken?
};

void run_mips2c_jump_table(Function* f,
                           const std::vector<int>& jump_table_locations,
                           GameVersion version) {
  lg::info("mips2c-jump on {}", f->name());
  u32 magic_code = std::hash<std::string>()(f->name());
  std::unordered_map<int, int> loc_to_block;
  for (size_t bb_idx = 0; bb_idx < f->basic_blocks.size(); bb_idx++) {
    loc_to_block[f->basic_blocks[bb_idx].start_word] = bb_idx;
  }

  auto* file = f->ir2.env.file;
  std::unordered_set<int> likely_delay_blocks;
  auto blocks = setup_preds_and_succs(*f, *file, likely_delay_blocks);
  Mips2C_Output output;
  output.jump_table = true;
  int unknown_count = 0;

  for (size_t block_idx = 0; block_idx < blocks.size(); block_idx++) {
    const auto& block = blocks[block_idx];

    if (likely_delay_blocks.count(block_idx)) {
      continue;
    }

    output.output_jump_table_block_label(block_idx);

    for (int i = block.start_instr; i < block.end_instr; i++) {
      size_t old_line_count = output.lines.size();
      auto& instr = f->instructions.at(i);
      auto instr_str = instr.to_string(file->labels);

      if (is_branch(instr, {})) {
        if (block.branch_likely) {
          auto branch_line = handle_likely_branch_bc(instr, instr_str);
          output.lines.emplace_back(fmt::format("if ({}) {{", branch_line.code),
                                    branch_line.comment);
          // next block should be the delay slot
          ASSERT((int)block_idx + 1 == block.succ_branch);
          auto& delay_block = blocks.at(block.succ_branch);
          ASSERT(delay_block.end_instr - delay_block.start_instr == 1);  // only 1 instr.
          auto& delay_instr = f->instructions.at(delay_block.start_instr);
          auto delay_instr_str = delay_instr.to_string(file->labels);
          auto delay_instr_line = handle_normal_instr(output, delay_instr, delay_instr_str,
                                                      unknown_count, file, version);
          output.lines.emplace_back(fmt::format("  {}", delay_instr_line.code),
                                    delay_instr_line.comment);
          ASSERT(delay_block.succ_ft == -1);
          output.lines.emplace_back(fmt::format("  next_block = {};", delay_block.succ_branch), "");
          output.lines.emplace_back("break;");
          output.lines.emplace_back("}", "");
        } else {
          if (is_always_branch(instr)) {
            // skip the branch ins.
            output.lines.emplace_back("//" + instr_str, instr_str);
            // then the delay slot
            ASSERT(i + 1 < block.end_instr);
            i++;
            auto& delay_i = f->instructions.at(i);
            auto delay_i_str = delay_i.to_string(file->labels);
            output.lines.push_back(
                handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));
            ASSERT(i + 1 == block.end_instr);
            // then the goto
            output.lines.emplace_back(fmt::format("next_block = {};", block.succ_branch),
                                      "branch always\n");
            output.lines.emplace_back("break;");

          } else {
            // set the branch condition
            output.lines.push_back(handle_non_likely_branch_bc(instr, instr_str));
            // then the delay slot
            ASSERT(i + 1 < block.end_instr);
            i++;
            auto& delay_i = f->instructions.at(i);
            auto delay_i_str = delay_i.to_string(file->labels);
            output.lines.push_back(
                handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));
            ASSERT(i + 1 == block.end_instr);
            // then the goto
            output.lines.emplace_back(
                fmt::format("if (bc) {{next_block = {};}}", block.succ_branch),
                "branch non-likely\n");
            output.lines.emplace_back("break;");
          }
        }
      } else if (is_jr_ra(instr)) {
        // skip
        output.lines.emplace_back("//" + instr_str, instr_str);
        // then the delay slot
        ASSERT(i + 1 < block.end_instr);
        i++;
        auto& delay_i = f->instructions.at(i);
        auto delay_i_str = delay_i.to_string(file->labels);
        output.lines.push_back(
            handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));

        // then the goto
        output.lines.emplace_back(fmt::format("goto end_of_function;", block.succ_branch),
                                  "return\n");
      } else if (instr.kind == InstructionKind::JALR) {
        ASSERT(instr.get_dst(0).is_reg(Register(Reg::GPR, Reg::RA)));
        ASSERT(i < block.end_instr - 1);
        output.lines.emplace_back(
            fmt::format("call_addr = c->gprs[{}].du32[0];", reg_to_name(instr.get_src(0))),
            "function call:");
        i++;
        auto& delay_i = f->instructions.at(i);
        auto delay_i_str = delay_i.to_string(file->labels);
        output.lines.push_back(
            handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));
        output.lines.emplace_back("c->jalr(call_addr);", instr_str);
      } else if (instr.kind == InstructionKind::JR) {
        // special case for jr's to handle the jump tableing.
        output.lines.emplace_back(fmt::format("next_block = 0x{:x} ^ c->gprs[{}].du32[0];",
                                              magic_code, reg_to_name(instr.get_src(0))),
                                  instr_str);
        output.lines.emplace_back(fmt::format("ASSERT(next_block < {});", f->basic_blocks.size()));
        output.lines.emplace_back("break;");

      } else {
        output.lines.push_back(
            handle_normal_instr(output, instr, instr_str, unknown_count, file, version));
      }

      ASSERT(output.lines.size() > old_line_count);
    }
  }

  std::string jump_loc_table =
      fmt::format("u32 jump_table_vals[{}] = {{\n", jump_table_locations.size());
  for (auto loc : jump_table_locations) {
    auto block = loc_to_block.at(loc + 1);
    jump_loc_table +=
        fmt::format("  0x{:x}, // = {} ^ {}\n", ((u32)block) ^ magic_code, block, magic_code);
  }
  jump_loc_table += "};\n\n";

  f->mips2c_output = output.write_to_string(f->guessed_name, version, jump_loc_table);
  if (g_unknown > 0) {
    lg::error("Mips to C pass in {} hit {} unknown instructions", f->name(), g_unknown);
  }
}

void run_mips2c(Function* f, GameVersion version) {
  g_unknown = 0;
  auto* file = f->ir2.env.file;
  std::unordered_set<int> likely_delay_blocks;
  auto blocks = setup_preds_and_succs(*f, *file, likely_delay_blocks);
  Mips2C_Output output;
  int unknown_count = 0;

  for (size_t block_idx = 0; block_idx < blocks.size(); block_idx++) {
    const auto& block = blocks[block_idx];

    if (likely_delay_blocks.count(block_idx)) {
      continue;
    }

    if (block_requires_label(f, blocks, block_idx)) {
      output.output_label(block_idx);
    }

    for (int i = block.start_instr; i < block.end_instr; i++) {
      size_t old_line_count = output.lines.size();
      auto& instr = f->instructions.at(i);
      auto instr_str = instr.to_string(file->labels);

      if (is_branch(instr, {})) {
        if (block.branch_likely) {
          auto branch_line = handle_likely_branch_bc(instr, instr_str);
          output.lines.emplace_back(fmt::format("if ({}) {{", branch_line.code),
                                    branch_line.comment);
          // next block should be the delay slot
          ASSERT((int)block_idx + 1 == block.succ_branch);
          auto& delay_block = blocks.at(block.succ_branch);
          ASSERT(delay_block.end_instr - delay_block.start_instr == 1);  // only 1 instr.
          auto& delay_instr = f->instructions.at(delay_block.start_instr);
          auto delay_instr_str = delay_instr.to_string(file->labels);
          auto delay_instr_line = handle_normal_instr(output, delay_instr, delay_instr_str,
                                                      unknown_count, file, version);
          output.lines.emplace_back(fmt::format("  {}", delay_instr_line.code),
                                    delay_instr_line.comment);
          ASSERT(delay_block.succ_ft == -1);
          output.lines.emplace_back(fmt::format("  goto block_{};", delay_block.succ_branch), "");
          output.lines.emplace_back("}", "");
        } else {
          if (is_always_branch(instr)) {
            // skip the branch ins.
            output.lines.emplace_back("//" + instr_str, instr_str);
            // then the delay slot
            ASSERT(i + 1 < block.end_instr);
            i++;
            auto& delay_i = f->instructions.at(i);
            auto delay_i_str = delay_i.to_string(file->labels);
            output.lines.push_back(
                handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));
            ASSERT(i + 1 == block.end_instr);
            // then the goto
            output.lines.emplace_back(fmt::format("goto block_{};", block.succ_branch),
                                      "branch always\n");
          } else {
            // set the branch condition
            output.lines.push_back(handle_non_likely_branch_bc(instr, instr_str));
            // then the delay slot
            if (!(i + 1 < block.end_instr)) {
              output.lines.emplace_back("DANGER jump to delay slot, this MUST be fixed manually!",
                                        "");
              lg::warn("Delay slot weirdness in {}, block {}", f->name(), block_idx);
            }
            i++;
            auto& delay_i = f->instructions.at(i);
            auto delay_i_str = delay_i.to_string(file->labels);
            output.lines.push_back(
                handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));
            // ASSERT(i + 1 == block.end_instr);
            // then the goto
            output.lines.emplace_back(fmt::format("if (bc) {{goto block_{};}}", block.succ_branch),
                                      "branch non-likely\n");
          }
        }
      } else if (is_jr_ra(instr)) {
        // skip
        output.lines.emplace_back("//" + instr_str, instr_str);
        // then the delay slot
        ASSERT(i + 1 < block.end_instr);
        i++;
        auto& delay_i = f->instructions.at(i);
        auto delay_i_str = delay_i.to_string(file->labels);
        output.lines.push_back(
            handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));

        // then the goto
        output.lines.emplace_back(fmt::format("goto end_of_function;", block.succ_branch),
                                  "return\n");
      } else if (instr.kind == InstructionKind::JALR) {
        ASSERT(instr.get_dst(0).is_reg(Register(Reg::GPR, Reg::RA)));
        ASSERT(i < block.end_instr - 1);
        output.lines.emplace_back(
            fmt::format("call_addr = c->gprs[{}].du32[0];", reg_to_name(instr.get_src(0))),
            "function call:");
        i++;
        auto& delay_i = f->instructions.at(i);
        auto delay_i_str = delay_i.to_string(file->labels);
        output.lines.push_back(
            handle_normal_instr(output, delay_i, delay_i_str, unknown_count, file, version));
        output.lines.emplace_back("c->jalr(call_addr);", instr_str);
      } else {
        output.lines.push_back(
            handle_normal_instr(output, instr, instr_str, unknown_count, file, version));
      }

      ASSERT(output.lines.size() > old_line_count);
    }
  }

  f->mips2c_output = output.write_to_string(f->guessed_name, version);
  if (g_unknown > 0) {
    lg::error("Mips to C pass in {} hit {} unknown instructions", f->name(), g_unknown);
  }
}
}  // namespace decompiler
