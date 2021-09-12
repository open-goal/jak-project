
#include <set>

#include "mips2c.h"

#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/Function/Function.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

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

std::string goal_to_c_function_name(const FunctionName& name) {
  switch (name.kind) {
    case FunctionName::FunctionKind::GLOBAL:
      return goal_to_c_name(name.function_name);
    default:
      assert(false);
  }
}

const char* reg_to_name(const InstructionAtom& atom) {
  assert(atom.is_reg());
  return atom.get_reg().to_charp();
}

struct Mips2C_Line {
  std::string code;
  std::string comment;

  Mips2C_Line() = default;
  Mips2C_Line(const std::string& _code) : code(_code) {}
  Mips2C_Line(const std::string& _code, const std::string& _comment)
      : code(_code), comment(_comment) {}
};

struct Mips2C_Output {
  void output_label(int block_idx) { lines.push_back(fmt::format("\nblock_{}:", block_idx)); }
  void output_line_comment(const std::string& text) { lines.emplace_back("// " + text); }

  void output_instr(const std::string& instr, const std::string& comment) {
    lines.emplace_back(instr, comment);
  }

  std::string write_to_string(const FunctionName& goal_func_name) const {
    std::string name = goal_to_c_function_name(goal_func_name);
    std::string result = "//--------------------------MIPS2C---------------------\n";
    result += "#include \"game/mips2c/mips2c_private.h\"\n";
    result += "#include \"game/kernel/kscheme.h\"\n";

    // start of namespace for this function
    result += "namespace Mips2C {\n";
    result += fmt::format("namespace {} {{\n", name);

    if (!symbol_cache.empty()) {
      result += "struct Cache {\n";
      for (auto& sym : symbol_cache) {
        result += fmt::format("  void* {}; // {}\n", goal_to_c_name(sym), sym);
      }
      result += "} cache;\n\n";
    }

    result += "u64 execute(void* ctxt) {\n";
    result += "  auto* c = (ExecutionContext*)ctxt;\n";
    result += "  bool bc = false;";
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

    result += "end_of_function:\n  return c->gprs[v0].du64[0];\n";
    result += "}\n\n";

    // link function:
    result += "void link() {\n";
    for (auto& sym : symbol_cache) {
      result += fmt::format("  cache.{} = intern_from_c(\"{}\").c();\n", goal_to_c_name(sym), sym);
    }
    result +=
        fmt::format("  gLinkedFunctionTable.reg(\"{}\", execute);\n", goal_func_name.to_string());
    result += "}\n\n";

    result += fmt::format("}} // namespace {}\n", name);
    result += "} // namespace Mips2C\n";

    result +=
        fmt::format("// add {}::link to the link callback table for the object file.\n", name);
    result += "// FWD DEC:\n";
    result += fmt::format("namespace {} {{ extern void link(); }}\n", name);
    return result;
  }

  void require_symbol(const std::string& name) { symbol_cache.insert(name); }

  std::vector<Mips2C_Line> lines;
  std::set<std::string> symbol_cache;
};

struct M2C_Block {
  int idx = -1;
  int succ_branch = -1;
  int succ_ft = -1;
  std::vector<int> pred;

  int start_instr = -1;
  int end_instr = -1;

  bool has_branch = false;
  bool branch_likely = false;
  bool branch_always = false;

  bool has_pred(int pidx) const {
    for (auto p : pred) {
      if (p == pidx) {
        return true;
      }
    }
    return false;
  }
};

void link_fall_through(int first_idx, int second_idx, std::vector<M2C_Block>& blocks) {
  auto& first = blocks.at(first_idx);
  auto& second = blocks.at(second_idx);

  assert(first.succ_ft == -1);  // don't want to overwrite something by accident.
  // can only fall through to the next code in memory.
  assert(first_idx + 1 == second_idx);
  first.succ_ft = second_idx;

  if (!second.has_pred(first_idx)) {
    // if a block can block fall through and branch to the same block, we want to avoid adding
    // it as a pred twice. This is rare, but does happen and makes sense with likely branches
    // which only run the delay slot when taken.
    second.pred.push_back(first_idx);
  }
}

void link_branch(int first_idx, int second_idx, std::vector<M2C_Block>& blocks) {
  auto& first = blocks.at(first_idx);
  auto& second = blocks.at(second_idx);
  assert(first.succ_branch == -1);
  first.succ_branch = second_idx;

  if (!second.has_pred(first_idx)) {
    // see comment in link_fall_through
    second.pred.push_back(first_idx);
  }
}

void link_fall_through_likely(int first_idx, int second_idx, std::vector<M2C_Block>& blocks) {
  auto& first = blocks.at(first_idx);
  auto& second = blocks.at(second_idx);
  assert(first.succ_ft == -1);  // don't want to overwrite something by accident.
  // can only fall through to the next code in memory.
  assert(first_idx + 2 == second_idx);

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
std::vector<M2C_Block> setup_preds_and_succs(const Function& func, const LinkedObjectFile& file) {
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
    assert(!blocks.at(i).branch_always);
    bool not_last = (i + 1) < int(func.basic_blocks.size());

    if (b.end_word == b.start_word) {
      // there's no room for a branch here, fall through to the end
      if (not_last) {
        link_fall_through(i, i + 1, blocks);
      }
    } else {
      // room for at least a likely branch, try that first.
      int likely_branch_idx = b.end_word - 1;
      assert(likely_branch_idx >= b.start_word);
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
        assert(label_target != -1);
        const auto& label = file.labels.at(label_target);
        // assert(label.target_segment == seg);
        assert((label.offset % 4) == 0);
        int offset = label.offset / 4 - func.start_word;
        assert(offset >= 0);
        for (int j = int(func.basic_blocks.size()); j-- > 0;) {
          if (func.basic_blocks[j].start_word == offset) {
            block_target = j;
            break;
          }
        }

        assert(block_target != -1);
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
        // delay_block.kind = CfgVtx::DelaySlotKind::NO_DELAY;
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
          assert(idx >= b.start_word);
          auto& branch_candidate = func.instructions.at(idx);
          // auto& delay_slot_candidate = func.instructions.at(idx + 1);
          if (is_branch(branch_candidate, false)) {
            blocks.at(i).has_branch = true;
            blocks.at(i).branch_likely = false;
            // blocks.at(i).kind = get_delay_slot(delay_slot_candidate);
            bool branch_always = is_always_branch(branch_candidate);

            // need to find block target
            int block_target = -1;
            int label_target = branch_candidate.get_label_target();
            assert(label_target != -1);
            const auto& label = file.labels.at(label_target);
            // assert(label.target_segment == seg);
            assert((label.offset % 4) == 0);
            int offset = label.offset / 4 - func.start_word;
            assert(offset >= 0);

            // the order here matters when there are zero size blocks. Unclear what the best answer
            // is.
            //  i think in end it doesn't actually matter??
            //        for (int j = 0; j < int(func.basic_blocks.size()); j++) {
            for (int j = int(func.basic_blocks.size()); j-- > 0;) {
              if (func.basic_blocks[j].start_word == offset) {
                block_target = j;
                break;
              }
            }

            assert(block_target != -1);
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
    // the only way to get to this block is to fall through.
    return false;
  }

  return true;
}

int g_unknown = 0;
Mips2C_Line handle_unknown(const std::string& instr_str) {
  g_unknown++;
  return fmt::format("// Unknown instr: {}", instr_str);
}

Mips2C_Line handle_generic_load(const Instruction& i0, const std::string& instr_str) {
  if (i0.get_src(1).is_reg(rsp())) {
    return handle_unknown(instr_str);
  } else {
    return {fmt::format("c->{}({}, {}, {});", i0.op_name_to_string(), reg_to_name(i0.get_dst(0)),
                        i0.get_src(0).get_imm(), reg_to_name(i0.get_src(1))),
            instr_str};
  }
}

Mips2C_Line handle_lw(Mips2C_Output& out, const Instruction& i0, const std::string& instr_str) {
  if (i0.get_src(1).is_reg(rs7()) && i0.get_src(0).is_sym()) {
    // symbol load.
    out.require_symbol(i0.get_src(0).get_sym());
    return {fmt::format("c->load_symbol({}, cache.{});", reg_to_name(i0.get_dst(0)),
                        goal_to_c_name(i0.get_src(0).get_sym())),
            instr_str};

  } else {
    // fall back to standard loads
    return handle_generic_load(i0, instr_str);
  }
}

Mips2C_Line handle_generic_store(Mips2C_Output& /*out*/,
                                 const Instruction& i0,
                                 const std::string& instr_str) {
  if (i0.get_src(2).is_reg(Register(Reg::GPR, Reg::SP))) {
    return handle_unknown(instr_str);
  } else {
    return {fmt::format("c->{}({}, {}, {});", i0.op_name_to_string(), reg_to_name(i0.get_src(0)),
                        i0.get_src(1).get_imm(), reg_to_name(i0.get_src(2))),
            instr_str};
  }
}

Mips2C_Line handle_generic_op2_u16(const Instruction& i0, const std::string& instr_str) {
  return {fmt::format("c->{}({}, {}, {});", i0.op_name_to_string(), reg_to_name(i0.get_dst(0)),
                      reg_to_name(i0.get_src(0)), i0.get_src(1).get_imm()),
          instr_str};
}

Mips2C_Line handle_sw(Mips2C_Output& out, const Instruction& i0, const std::string& instr_str) {
  if (i0.get_src(1).is_sym() && i0.get_src(2).is_reg(rs7())) {
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

Mips2C_Line handle_or(const Instruction& i0, const std::string& instr_str) {
  if (is_gpr_3(i0, InstructionKind::OR, {}, rs7(), rr0())) {
    // set reg_dest to #f : or reg_dest, s7, r0
    return handle_unknown(instr_str);
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, rr0(), rr0())) {
    // set reg_dest to 0 : or reg_dest, r0, r0
    return handle_unknown(instr_str);
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, {}, rr0())) {
    // set dst to src : or dst, src, r0
    return {
        fmt::format("c->mov64({}, {});", reg_to_name(i0.get_dst(0)), reg_to_name(i0.get_src(0))),
        instr_str};
  } else {
    // actually do a logical OR of two registers: or a0, a1, a2
    return handle_unknown(instr_str);
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

Mips2C_Line handle_por(const Instruction& i0, const std::string& instr_string) {
  if (is_gpr_3(i0, InstructionKind::POR, {}, {}, rr0())) {
    return {fmt::format("c->mov128_gpr_gpr({}, {});", reg_to_name(i0.get_dst(0)),
                        reg_to_name(i0.get_src(0))),
            instr_string};
  } else {
    return handle_generic_op3(i0, instr_string, {});
  }
}

Mips2C_Line handle_normal_instr(Mips2C_Output& output,
                                const Instruction& i0,
                                const std::string& instr_str,
                                int& unknown_count) {
  switch (i0.kind) {
    case InstructionKind::LW:
      return handle_lw(output, i0, instr_str);
    case InstructionKind::LBU:
    case InstructionKind::LWU:
    case InstructionKind::LQ:
    case InstructionKind::LQC2:
      return handle_generic_load(i0, instr_str);
    case InstructionKind::SQ:
    case InstructionKind::SQC2:
      return handle_generic_store(output, i0, instr_str);
    case InstructionKind::VADD_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vadd_bc");
    case InstructionKind::VSUB_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vsub_bc");
    case InstructionKind::VMUL_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vmul_bc");
    case InstructionKind::VMUL:
      return handle_generic_op3_mask(i0, instr_str, "vmul");
    case InstructionKind::VADD:
      return handle_generic_op3_mask(i0, instr_str, "vadd");
    case InstructionKind::VSUB:
      return handle_generic_op3_mask(i0, instr_str, "vsub");
    case InstructionKind::OR:
      return handle_or(i0, instr_str);
    case InstructionKind::SW:
      return handle_sw(output, i0, instr_str);
    case InstructionKind::VMOVE:
      return handle_generic_op2_mask(i0, instr_str, "vmove");
    case InstructionKind::VITOF0:
      return handle_generic_op2_mask(i0, instr_str, "vitof0");
    case InstructionKind::VFTOI4:
      return handle_generic_op2_mask(i0, instr_str, "vftoi4");
    case InstructionKind::ANDI:
    case InstructionKind::ORI:
    case InstructionKind::SRA:
      return handle_generic_op2_u16(i0, instr_str);
    case InstructionKind::SLL:
      return handle_sll(i0, instr_str);
    case InstructionKind::DADDU:
    case InstructionKind::ADDU:
    case InstructionKind::PEXTLH:
    case InstructionKind::PEXTLB:
    case InstructionKind::MOVN:
    case InstructionKind::MOVZ:
      return handle_generic_op3(i0, instr_str, {});
    case InstructionKind::AND:
      return handle_generic_op3(i0, instr_str, "and_");  // and isn't allowed in C++
    case InstructionKind::DADDIU:
    case InstructionKind::ADDIU:
      return handle_generic_op2_u16(i0, instr_str);
    case InstructionKind::QMTC2:
      return handle_generic_op2(i0, instr_str, "mov128_vf_gpr");
    case InstructionKind::QMFC2:
      return handle_generic_op2(i0, instr_str, "mov128_gpr_vf");
    case InstructionKind::VMULA_BC:
      return handle_vmula_bc(i0, instr_str);
    case InstructionKind::VMADDA_BC:
      return handle_vmadda_bc(i0, instr_str);
    case InstructionKind::VMADD_BC:
      return handle_generic_op3_bc_mask(i0, instr_str, "vmadd_bc");
    case InstructionKind::VDIV:
      return handle_vdiv(i0, instr_str);
    case InstructionKind::POR:
      return handle_por(i0, instr_str);
    case InstructionKind::VMULQ:
      return handle_generic_op2_mask(i0, instr_str, "vmulq");
    case InstructionKind::VNOP:
      return {"// nop", instr_str};
    case InstructionKind::MFC1:
      return handle_generic_op2(i0, instr_str, "mfc1");
    default:
      unknown_count++;
      return handle_unknown(instr_str);

      break;
  }
}

void run_mips2c(Function* f) {
  g_unknown = 0;
  auto* file = f->ir2.env.file;
  auto blocks = setup_preds_and_succs(*f, *file);
  Mips2C_Output output;
  int unknown_count = 0;

  for (size_t block_idx = 0; block_idx < blocks.size(); block_idx++) {
    const auto& block = blocks[block_idx];
    // fmt::print("block {}: {} to {}\n", block_idx, block.start_instr, block.end_instr);
    if (block_requires_label(f, blocks, block_idx)) {
      output.output_label(block_idx);
    } else {
      // output.comment_line("block {}",)
    }

    for (int i = block.start_instr; i < block.end_instr; i++) {
      size_t old_line_count = output.lines.size();
      auto& instr = f->instructions.at(i);
      auto instr_str = instr.to_string(file->labels);

      if (is_branch(instr, {})) {
        if (block.branch_likely) {
          output.lines.push_back(handle_unknown(instr_str));
        } else {
          if (is_always_branch(instr)) {
            // skip the branch ins.
            output.lines.emplace_back("//" + instr_str, instr_str);
            // then the delay slot
            assert(i + 1 < block.end_instr);
            i++;
            auto& delay_i = f->instructions.at(i);
            auto delay_i_str = delay_i.to_string(file->labels);
            output.lines.push_back(
                handle_normal_instr(output, delay_i, delay_i_str, unknown_count));
            assert(i + 1 == block.end_instr);
            // then the goto
            output.lines.emplace_back(fmt::format("goto block_{};", block.succ_branch),
                                      "branch always\n");
          } else {
            // set the branch condition
            output.lines.push_back(handle_non_likely_branch_bc(instr, instr_str));
            // then the delay slot
            assert(i + 1 < block.end_instr);
            i++;
            auto& delay_i = f->instructions.at(i);
            auto delay_i_str = delay_i.to_string(file->labels);
            output.lines.push_back(
                handle_normal_instr(output, delay_i, delay_i_str, unknown_count));
            assert(i + 1 == block.end_instr);
            // then the goto
            output.lines.emplace_back(fmt::format("if (bc) {{goto block_{};}}", block.succ_branch),
                                      "branch non-likely\n");
          }
        }
      } else if (is_jr_ra(instr)) {
        // skip
        output.lines.emplace_back("//" + instr_str, instr_str);
        // then the delay slot
        assert(i + 1 < block.end_instr);
        i++;
        auto& delay_i = f->instructions.at(i);
        auto delay_i_str = delay_i.to_string(file->labels);
        output.lines.push_back(handle_normal_instr(output, delay_i, delay_i_str, unknown_count));
        assert(i + 1 == block.end_instr);
        // then the goto
        output.lines.emplace_back(fmt::format("goto end_of_function;", block.succ_branch),
                                  "return\n");
      } else {
        output.lines.push_back(handle_normal_instr(output, instr, instr_str, unknown_count));
      }
      // fmt::print("I: {}\n", instr_str);

      assert(output.lines.size() > old_line_count);
    }
  }

  f->mips2c_output = output.write_to_string(f->guessed_name);
  if (g_unknown > 0) {
    lg::error("Mips to C pass in {} hit {} unknown instructions", f->name(), g_unknown);
  }
}
}  // namespace decompiler
