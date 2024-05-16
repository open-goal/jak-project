#include "atomic_op_builder.h"

#include <memory>

#include "common/log/log.h"
#include "common/symbols.h"

#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Function/Warnings.h"

namespace decompiler {

namespace {

std::unique_ptr<AtomicOp> convert_1(const Instruction& i0,
                                    int idx,
                                    bool hint_inline_asm,
                                    bool force_asm_branch,
                                    GameVersion version);

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

/////////////////////////
// Variable Helpers
/////////////////////////

RegisterAccess make_dst_var(Register reg, int idx) {
  return RegisterAccess(AccessMode::WRITE, reg, idx);
}

RegisterAccess make_src_var(Register reg, int idx) {
  return RegisterAccess(AccessMode::READ, reg, idx);
}

RegisterAccess make_dst_var(const Instruction& i, int idx) {
  ASSERT(i.n_dst == 1);
  return make_dst_var(i.get_dst(0).get_reg(), idx);
}

////////////////////////
// Atom Helpers
////////////////////////

SimpleAtom false_sym() {
  return SimpleAtom::make_sym_val("#f");
}

SimpleAtom make_src_atom(Register reg, int idx) {
  if (reg == Register(Reg::GPR, Reg::R0)) {
    return SimpleAtom::make_int_constant(0);
  }
  if (reg == Register(Reg::GPR, Reg::S7)) {
    return false_sym();
  }
  return SimpleAtom::make_var(make_src_var(reg, idx));
}

////////////////////////
// Expression Helpers
////////////////////////

SimpleExpression make_2reg_expr(const Instruction& instr, SimpleExpression::Kind kind, int idx) {
  auto src0 = make_src_atom(instr.get_src(0).get_reg(), idx);
  auto src1 = make_src_atom(instr.get_src(1).get_reg(), idx);
  return SimpleExpression(kind, src0, src1);
}

SimpleExpression make_1reg_1imm_expr(const Instruction& instr,
                                     SimpleExpression::Kind kind,
                                     int idx,
                                     int imm_offset = 0) {
  auto src0 = make_src_atom(instr.get_src(0).get_reg(), idx);
  auto src1 = SimpleAtom::make_int_constant(instr.get_src(1).get_imm() + imm_offset);
  return SimpleExpression(kind, src0, src1);
}

SimpleExpression make_1reg_expr(const Instruction& instr, SimpleExpression::Kind kind, int idx) {
  auto src = make_src_atom(instr.get_src(0).get_reg(), idx);
  return SimpleExpression(kind, src);
}

SimpleExpression make_reg_plus_int(Register reg, int integer, int idx) {
  return SimpleExpression(SimpleExpression::Kind::ADD, make_src_atom(reg, idx),
                          SimpleAtom::make_int_constant(integer));
}

////////////////////////
// AtmoicOp Helpers
////////////////////////

/*!
 * Convert a single instruction in the form instr dest_reg, src_reg, src_reg
 * to an atomic op of (set! dst_reg (op src_reg src_reg))
 * Like daddu a0, a1, a2
 */
std::unique_ptr<AtomicOp> make_3reg_op(const Instruction& instr,
                                       SimpleExpression::Kind kind,
                                       int idx) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  return std::make_unique<SetVarOp>(dst, make_2reg_expr(instr, kind, idx), idx);
}

std::unique_ptr<AtomicOp> make_2reg_1imm_op(const Instruction& instr,
                                            SimpleExpression::Kind kind,
                                            int idx,
                                            int imm_offset = 0) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  return std::make_unique<SetVarOp>(dst, make_1reg_1imm_expr(instr, kind, idx, imm_offset), idx);
}

/*!
 * Convert a single instruction in the form instr dest_reg, src_reg
 * to an atomic op of (set! dest_reg (op src_reg))
 */
std::unique_ptr<AtomicOp> make_2reg_op(const Instruction& instr,
                                       SimpleExpression::Kind kind,
                                       int idx) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  return std::make_unique<SetVarOp>(dst, make_1reg_expr(instr, kind, idx), idx);
}

/*!
 * Common load helper. Supports fp relative, 0 offset, or integer constant offset
 */
std::unique_ptr<AtomicOp> make_standard_load(const Instruction& i0,
                                             int idx,
                                             int load_size,
                                             LoadVarOp::Kind kind) {
  if (i0.get_dst(0).is_reg(Register(Reg::GPR, Reg::SP))) {
    return std::make_unique<AsmOp>(i0, idx);
  }
  auto dst = make_dst_var(i0, idx);
  SimpleExpression src;
  if (i0.get_src(0).is_label() && i0.get_src(1).is_reg(rfp())) {
    // it's an FP relative load.
    src = SimpleAtom::make_static_address(i0.get_src(0).get_label()).as_expr();
  } else if (i0.get_src(0).is_imm() && i0.get_src(1).is_reg(rsp())) {
    // it's a stack spill.
    return std::make_unique<StackSpillLoadOp>(make_dst_var(i0, idx), load_size,
                                              i0.get_src(0).get_imm(),
                                              kind == LoadVarOp::Kind::SIGNED, idx);
  } else if (i0.get_src(0).is_imm() && i0.get_src(0).get_imm() == 0) {
    // the offset is 0
    src = make_src_atom(i0.get_src(1).get_reg(), idx).as_expr();
  } else if (i0.get_src(0).is_imm()) {
    // the offset is not 0
    src = make_reg_plus_int(i0.get_src(1).get_reg(), i0.get_src(0).get_imm(), idx);
  } else {
    return nullptr;
  }
  return std::make_unique<LoadVarOp>(kind, load_size, dst, src, idx);
}

std::unique_ptr<AtomicOp> make_standard_store(const Instruction& i0,
                                              int idx,
                                              int store_size,
                                              StoreOp::Kind kind) {
  if (i0.get_src(2).is_reg(Register(Reg::GPR, Reg::SP))) {
    if (kind == StoreOp::Kind::INTEGER && store_size == 4 && i0.get_src(1).get_imm() == 0) {
      // this is a bit of a hack. enter-state does a sw onto the stack that's not a spill, but
      // instead manipulates the stores "ra" register that will later be restored.
      // I believe sw is never used for stack spills, and no stack variable is ever located at
      // sp + 0, so this should be safe.
      return std::make_unique<AsmOp>(i0, idx);
    }
    // it's a stack spill.
    return std::make_unique<StackSpillStoreOp>(make_src_atom(i0.get_src(0).get_reg(), idx),
                                               store_size, i0.get_src(1).get_imm(), idx);
  }
  SimpleAtom val;
  SimpleExpression dst;
  if (i0.get_src(0).is_reg(rs7())) {
    ASSERT(kind == StoreOp::Kind::INTEGER);
    val = SimpleAtom::make_sym_val("#f");
  } else if (i0.get_src(0).is_reg(rr0())) {
    ASSERT(kind == StoreOp::Kind::INTEGER);
    val = SimpleAtom::make_int_constant(0);
  } else {
    val = make_src_atom(i0.get_src(0).get_reg(), idx);
  }

  auto base_reg = make_src_atom(i0.get_src(2).get_reg(), idx);
  auto offset = i0.get_src(1).get_imm();
  if (offset == 0) {
    dst = base_reg.as_expr();
  } else {
    dst = SimpleExpression(SimpleExpression::Kind::ADD, base_reg,
                           SimpleAtom::make_int_constant(offset));
  }

  return std::make_unique<StoreOp>(store_size, kind, dst, val, idx);
}

std::unique_ptr<AtomicOp> make_asm_op(const Instruction& i0, int idx) {
  switch (i0.kind) {
    case InstructionKind::SLLV:  // goal will use dsllv
    case InstructionKind::SLL:   // goal will use dsll
    case InstructionKind::PCPYUD:
    case InstructionKind::LQ:
    case InstructionKind::SQ:
    case InstructionKind::MTC0:
    case InstructionKind::MTDAB:
    case InstructionKind::MTDABM:
    case InstructionKind::SUBU:  // goal uses dsubu
    case InstructionKind::JR:    // normal returns included in epilogue
    case InstructionKind::SYSCALL:
    case InstructionKind::ADDU:  // goal uses daddu
    case InstructionKind::SRL:   // goal uses dsrl.. except maybe to access bitfields?
    case InstructionKind::SRA:
    case InstructionKind::ADDIU:
      // some weird inline assembly macros in nav-mesh stuff use these a lot in a weird way.
    case InstructionKind::SLT:
    case InstructionKind::MOVN:
    case InstructionKind::SLTI:  // a few cases used in inline asm
    case InstructionKind::MOVZ:  // in font.gc

      // VU/COP2
    case InstructionKind::VMOVE:
    case InstructionKind::VFTOI0:
    case InstructionKind::VFTOI4:
    case InstructionKind::VFTOI12:
    case InstructionKind::VFTOI15:
    case InstructionKind::VITOF0:
    case InstructionKind::VITOF12:
    case InstructionKind::VITOF15:
    case InstructionKind::VABS:
    case InstructionKind::VADD:
    case InstructionKind::VSUB:
    case InstructionKind::VMUL:
    case InstructionKind::VMINI:
    case InstructionKind::VMAX:
    case InstructionKind::VOPMSUB:
    case InstructionKind::VMADD:
    case InstructionKind::VMSUB:
    case InstructionKind::VADD_BC:
    case InstructionKind::VSUB_BC:
    case InstructionKind::VMUL_BC:
    case InstructionKind::VMULA_BC:
    case InstructionKind::VMADD_BC:
    case InstructionKind::VADDA_BC:
    case InstructionKind::VMADDA_BC:
    case InstructionKind::VMSUBA_BC:
    case InstructionKind::VMSUB_BC:
    case InstructionKind::VMINI_BC:
    case InstructionKind::VMAX_BC:
    case InstructionKind::VADDQ:
    case InstructionKind::VSUBQ:
    case InstructionKind::VMULQ:
    case InstructionKind::VMSUBQ:
    case InstructionKind::VMULA:
    case InstructionKind::VADDA:
    case InstructionKind::VMADDA:
    case InstructionKind::VOPMULA:
    case InstructionKind::VDIV:
    case InstructionKind::VCLIP:
    case InstructionKind::VMULAQ:
    case InstructionKind::VMTIR:
    case InstructionKind::VIAND:
    case InstructionKind::VLQI:
    case InstructionKind::VIADDI:
    case InstructionKind::VSQI:
    case InstructionKind::VRGET:
    case InstructionKind::VSQRT:
    case InstructionKind::VRSQRT:
    case InstructionKind::VRXOR:
    case InstructionKind::VRNEXT:
    case InstructionKind::VNOP:
    case InstructionKind::VWAITQ:
    case InstructionKind::VCALLMS:

      // FPU/COP1
    case InstructionKind::MULAS:
    case InstructionKind::MADDAS:
    case InstructionKind::MADDS:
    case InstructionKind::MSUBAS:
    case InstructionKind::MSUBS:
    case InstructionKind::ADDAS:
    case InstructionKind::RSQRTS:

      // Moves / Loads / Stores
    case InstructionKind::CTC2:
    case InstructionKind::CFC2:
    case InstructionKind::LDR:
    case InstructionKind::LDL:
    case InstructionKind::QMTC2:
    case InstructionKind::QMFC2:
    case InstructionKind::MFC0:
    case InstructionKind::SYNCL:
    case InstructionKind::SYNCP:
    case InstructionKind::CACHE_DXWBIN:
    case InstructionKind::MTPC:
    case InstructionKind::MFPC:

      // MMI
    case InstructionKind::PSLLW:
    case InstructionKind::PSRAW:
    case InstructionKind::PSRAH:
    case InstructionKind::PLZCW:
    case InstructionKind::PMFHL_UW:
    case InstructionKind::PMFHL_LW:
    case InstructionKind::PMFHL_LH:
    case InstructionKind::PSLLH:
    case InstructionKind::PSRLH:
    case InstructionKind::PEXTLW:
    case InstructionKind::PPACH:
    case InstructionKind::PSUBW:
    case InstructionKind::PCGTB:
    case InstructionKind::PCGTW:
    case InstructionKind::PEXTLH:
    case InstructionKind::PEXTLB:
    case InstructionKind::PMAXH:
    case InstructionKind::PPACB:
    case InstructionKind::PADDW:
    case InstructionKind::PADDH:
    case InstructionKind::PADDB:
    case InstructionKind::PMAXW:
    case InstructionKind::PPACW:
    case InstructionKind::PCEQW:
    case InstructionKind::PEXTUW:
    case InstructionKind::PMINH:
    case InstructionKind::PEXTUH:
    case InstructionKind::PEXTUB:
    case InstructionKind::PCEQB:
    case InstructionKind::PMINW:
    case InstructionKind::PABSW:
    case InstructionKind::PCPYLD:
    case InstructionKind::PROT3W:
    case InstructionKind::PAND:
    case InstructionKind::PXOR:
    case InstructionKind::PMADDH:
    case InstructionKind::PMULTH:
    case InstructionKind::PEXEW:
    case InstructionKind::PEXCW:
    case InstructionKind::PNOR:
    case InstructionKind::PCPYH:
    case InstructionKind::PINTEH:

      return std::make_unique<AsmOp>(i0, idx);
    default:
      return nullptr;
  }
}

std::unique_ptr<AtomicOp> convert_1_allow_asm(const Instruction& i0, int idx, GameVersion version) {
  // only used for delay slots, so fine to assume that this can never be an asm branch itself
  // as there are no branches in delay slots anywhere.
  auto as_normal = convert_1(i0, idx, false, false, version);
  if (as_normal) {
    return as_normal;
  }
  return make_asm_op(i0, idx);
}

////////////////////////
// Branch Helpers
////////////////////////

IR2_BranchDelay get_branch_delay(const Instruction& i0, int idx, GameVersion version) {
  if (is_nop(i0)) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::NOP);
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, rs7(), rr0())) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::SET_REG_FALSE, make_dst_var(i0, idx));
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, {}, rr0()) && !i0.get_src(0).is_reg(rr0())) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::SET_REG_REG, make_dst_var(i0, idx),
                           make_src_var(i0.get_src(0).get_reg(), idx));
  } else if (i0.kind == InstructionKind::DADDIU && i0.get_src(0).is_reg(rs7()) &&
             i0.get_src(1).is_imm(true_symbol_offset(version))) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::SET_REG_TRUE, make_dst_var(i0, idx));
  } else if (i0.kind == InstructionKind::LW && i0.get_src(1).is_reg(rs7()) &&
             i0.get_src(0).is_sym()) {
    if (i0.get_src(0).is_sym("binteger")) {
      return IR2_BranchDelay(IR2_BranchDelay::Kind::SET_BINTEGER, make_dst_var(i0, idx));
    } else if (i0.get_src(0).is_sym("pair")) {
      return IR2_BranchDelay(IR2_BranchDelay::Kind::SET_PAIR, make_dst_var(i0, idx));
    } else {
      return IR2_BranchDelay(IR2_BranchDelay::Kind::UNKNOWN);
    }
  } else if (i0.kind == InstructionKind::DSLLV) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::DSLLV, make_dst_var(i0, idx),
                           make_src_var(i0.get_src(0).get_reg(), idx),
                           make_src_var(i0.get_src(1).get_reg(), idx));
  } else if (is_gpr_3(i0, InstructionKind::DSUBU, {}, rr0(), {})) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::NEGATE, make_dst_var(i0, idx),
                           make_src_var(i0.get_src(1).get_reg(), idx));
  } else {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::UNKNOWN);
  }
}

std::unique_ptr<AtomicOp> make_branch(const IR2_Condition& condition,
                                      const Instruction& delay,
                                      bool likely,
                                      int dest_label,
                                      int my_idx,
                                      bool force_asm,
                                      GameVersion version) {
  ASSERT(!likely);
  auto branch_delay = get_branch_delay(delay, my_idx, version);
  if (!force_asm && branch_delay.is_known()) {
    return std::make_unique<BranchOp>(likely, condition, dest_label, branch_delay, my_idx);
  } else {
    auto delay_op = std::shared_ptr<AtomicOp>(convert_1_allow_asm(delay, my_idx, version));
    if (!delay_op) {
      throw std::runtime_error(
          fmt::format("Failed to convert branch delay slot instruction for branch at {}", my_idx));
    }
    return std::make_unique<AsmBranchOp>(likely, condition, dest_label, delay_op, my_idx);
  }
}

std::unique_ptr<AtomicOp> make_asm_branch_no_delay(const IR2_Condition& condition,
                                                   bool likely,
                                                   int dest_label,
                                                   int my_idx) {
  ASSERT(likely);
  return std::make_unique<AsmBranchOp>(likely, condition, dest_label, nullptr, my_idx);
}

std::unique_ptr<AtomicOp> make_branch_no_delay(const IR2_Condition& condition,
                                               bool likely,
                                               int dest_label,
                                               int my_idx,
                                               bool force_asm_branch) {
  if (force_asm_branch) {
    return make_asm_branch_no_delay(condition, likely, dest_label, my_idx);
  }
  ASSERT(likely);
  IR2_BranchDelay delay(IR2_BranchDelay::Kind::NO_DELAY);
  return std::make_unique<BranchOp>(likely, condition, dest_label, delay, my_idx);
}

std::unique_ptr<AtomicOp> make_asm_branch(const IR2_Condition& condition,
                                          const Instruction& delay,
                                          bool likely,
                                          int dest_label,
                                          int my_idx,
                                          GameVersion version) {
  ASSERT(!likely);
  auto delay_op = std::shared_ptr<AtomicOp>(convert_1_allow_asm(delay, my_idx, version));
  if (!delay_op) {
    throw std::runtime_error(
        fmt::format("Failed to convert branch delay slot instruction for branch at {}", my_idx));
  }
  return std::make_unique<AsmBranchOp>(likely, condition, dest_label, delay_op, my_idx);
}

///////////////////////
// OP 1 Conversions
//////////////////////

std::unique_ptr<AtomicOp> convert_or_1(const Instruction& i0, int idx) {
  if (i0.get_src(1).is_reg(rra())) {
    return std::make_unique<AsmOp>(i0, idx);
  }
  auto dest = make_dst_var(i0, idx);
  SimpleExpression src;

  if (is_gpr_3(i0, InstructionKind::OR, {}, rs7(), rr0())) {
    // set reg_dest to #f : or reg_dest, s7, r0
    src = false_sym().as_expr();
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, rr0(), rr0())) {
    // set reg_dest to 0 : or reg_dest, r0, r0
    src = SimpleAtom::make_int_constant(0).as_expr();
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, {}, rr0())) {
    // set dst to src : or dst, src, r0
    src = make_src_atom(i0.get_src(0).get_reg(), idx).as_expr();
  } else {
    // actually do a logical OR of two registers: or a0, a1, a2
    src = make_2reg_expr(i0, SimpleExpression::Kind::OR, idx);
  }
  return std::make_unique<SetVarOp>(dest, src, idx);
}

std::unique_ptr<AtomicOp> convert_por_1(const Instruction& i0, int idx) {
  if (is_gpr_3(i0, InstructionKind::POR, {}, {}, rr0())) {
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      make_src_atom(i0.get_src(0).get_reg(), idx).as_expr(), idx);
  }
  return std::make_unique<AsmOp>(i0, idx);
}

std::unique_ptr<AtomicOp> convert_ori_1(const Instruction& i0, int idx) {
  auto dest = make_dst_var(i0, idx);
  SimpleExpression src;
  if (i0.get_src(0).is_reg(rr0()) && i0.get_src(1).is_imm()) {
    // load a 16-bit integer constant
    // ori reg, r0, 1234
    src = SimpleAtom::make_int_constant(i0.get_src(1).get_imm()).as_expr();
  } else if (i0.get_src(1).is_imm()) {
    // logical or with constant integer
    // ori dst, a0, 1234
    return make_2reg_1imm_op(i0, SimpleExpression::Kind::OR, idx);
  } else {
    return nullptr;
  }
  return std::make_unique<SetVarOp>(dest, src, idx);
}

std::unique_ptr<AtomicOp> convert_mtc1_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rr0())) {
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleAtom::make_int_constant(0).as_expr(), idx);
  } else {
    return make_2reg_op(i0, SimpleExpression::Kind::GPR_TO_FPR, idx);
  }
}

std::unique_ptr<AtomicOp> convert_mfc1_1(const Instruction& i0, int idx) {
  if (i0.get_dst(0).is_reg(rr0()) && i0.get_src(0).is_reg(Register(Reg::FPR, 31))) {
    // mfc r0, f31 is a nop
    return std::make_unique<SpecialOp>(SpecialOp::Kind::NOP, idx);
  }

  if (i0.get_dst(0).is_reg(rr0()) || i0.get_dst(0).is_reg(make_gpr(Reg::AT)) ||
      i0.get_dst(0).is_reg(rra())) {
    // sometimes mfc1 r0, f31 is used like a 'nop'. No idea why.
    // at used in some inline assembly gpr -> vf conversions. might as well drop to assembly
    // as soon as wel can.
    // cursed mfc1 ra, f0 is also assembly.
    return std::make_unique<AsmOp>(i0, idx);
  } else {
    return make_2reg_op(i0, SimpleExpression::Kind::FPR_TO_GPR, idx);
  }
}

std::unique_ptr<AtomicOp> convert_lw_1(const Instruction& i0, int idx) {
  if (i0.get_dst(0).is_reg(rra()) ||
      (i0.get_dst(0).is_reg(make_gpr(Reg::AT)) && !i0.get_src(1).is_reg(rs7()))) {
    return std::make_unique<AsmOp>(i0, idx);
  }
  if (i0.get_dst(0).is_reg(rr0()) && i0.get_src(0).is_imm(2) && i0.get_src(1).is_reg(rr0())) {
    // lw r0, 2(r0), used to trigger an exception on purpose.
    return std::make_unique<SpecialOp>(SpecialOp::Kind::BREAK, idx);
  } else if (i0.get_src(1).is_reg(rs7()) && i0.get_src(0).is_sym()) {
    // symbol load.
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_sym_val(i0.get_src(0).get_sym()).as_expr(), idx);
  } else {
    // fall back to standard loads
    return make_standard_load(i0, idx, 4, LoadVarOp::Kind::SIGNED);
  }
}

std::unique_ptr<AtomicOp> convert_daddiu_1(const Instruction& i0, int idx, GameVersion version) {
  if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_sym()) {
    // get symbol pointer
    if (i0.get_src(1).kind == InstructionAtom::IMM_SYM_VAL_PTR) {
      return std::make_unique<SetVarOp>(
          make_dst_var(i0, idx), SimpleAtom::make_sym_val_ptr(i0.get_src(1).get_sym()).as_expr(),
          idx);
    } else {
      return std::make_unique<SetVarOp>(
          make_dst_var(i0, idx), SimpleAtom::make_sym_ptr(i0.get_src(1).get_sym()).as_expr(), idx);
    }
  } else if (i0.get_src(0).is_reg(rs7()) &&
             i0.get_src(1).is_imm(empty_pair_offset_from_s7(version))) {
    // get empty pair
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleAtom::make_empty_list().as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(-32768)) {
    // get pointer to beginning of symbol table (this is a bit of a hack)
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_sym_val("__START-OF-TABLE__").as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(true_symbol_offset(version))) {
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleAtom::make_sym_ptr("#t").as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rfp()) && i0.get_src(1).is_label()) {
    // get address of static
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_static_address(i0.get_src(1).get_label()).as_expr(),
        idx);
  } else {
    // fall back to normal add.
    return make_2reg_1imm_op(i0, SimpleExpression::Kind::ADD, idx);
  }
}

std::unique_ptr<AtomicOp> convert_daddu_1(const Instruction& i0, int idx) {
  if (i0.get_src(1).is_reg(rs7())) {
    return std::make_unique<AsmOp>(i0, idx);
  } else if (i0.get_src(0).is_reg(rr0())) {
    // I think the array access code sometimes generates this. To be safe, let's pass it through
    // as an explicit reg + 0 access.
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx),
        SimpleExpression(SimpleExpression::Kind::ADD, make_src_atom(i0.get_src(1).get_reg(), idx),
                         SimpleAtom::make_int_constant(0)),
        idx);
  } else {
    return make_3reg_op(i0, SimpleExpression::Kind::ADD, idx);
  }
}

std::unique_ptr<AtomicOp> convert_dsubu_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rr0())) {
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx),
        SimpleExpression(SimpleExpression::Kind::NEG, make_src_atom(i0.get_src(1).get_reg(), idx)),
        idx);
  } else {
    // fall back
    return make_3reg_op(i0, SimpleExpression::Kind::SUB, idx);
  }
}

std::unique_ptr<AtomicOp> convert_nor_1(const Instruction& i0, int idx) {
  if (i0.get_src(1).is_reg(rr0())) {
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleExpression(SimpleExpression::Kind::LOGNOT,
                                                       make_src_atom(i0.get_src(0).get_reg(), idx)),
                                      idx);
  } else {
    // fall back
    return make_3reg_op(i0, SimpleExpression::Kind::NOR, idx);
  }
}

std::unique_ptr<AtomicOp> convert_addiu_1(const Instruction& i0, int idx) {
  // addiu is used to load a constant. sometimes.
  if (i0.get_src(0).is_reg(rr0())) {
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_int_constant(i0.get_src(1).get_imm()).as_expr(),
        idx);
  } else {
    // may be assembly
    return nullptr;
  }
}

std::unique_ptr<AtomicOp> convert_lui_1(const Instruction& i0, int idx) {
  if (i0.get_dst(0).is_reg(make_gpr(Reg::AT))) {
    return std::make_unique<AsmOp>(i0, idx);
  }
  if (i0.get_src(0).is_imm()) {
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx),
        SimpleAtom::make_int_constant(i0.get_src(0).get_imm() << 16).as_expr(), idx);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_sll_1(const Instruction& i0, int idx) {
  if (is_nop(i0)) {
    return std::make_unique<SpecialOp>(SpecialOp::Kind::NOP, idx);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_sw_1(const Instruction& i0, int idx) {
  if (i0.get_src(1).is_sym() && i0.get_src(2).is_reg(rs7())) {
    auto name = i0.get_src(1).get_sym();
    // store into symbol table!
    SimpleAtom val;
    if (i0.get_src(0).is_reg(rs7())) {
      // store a false
      val = SimpleAtom::make_sym_val("#f");
    } else if (i0.get_src(0).is_reg(rr0())) {
      // store a 0
      val = SimpleAtom::make_int_constant(0);
    } else {
      // store a register.
      val = make_src_atom(i0.get_src(0).get_reg(), idx);
    }
    return std::make_unique<StoreOp>(4, StoreOp::Kind::INTEGER,
                                     SimpleAtom::make_sym_val(name).as_expr(), val, idx);
  } else {
    return make_standard_store(i0, idx, 4, StoreOp::Kind::INTEGER);
  }
}

std::unique_ptr<AtomicOp> convert_swc1_1(const Instruction& i0, int idx) {
  if (i0.get_src(1).is_sym() && i0.get_src(2).is_reg(rs7())) {
    // storing a float in the symbol table. It's very rare, but possible
    return std::make_unique<StoreOp>(4, StoreOp::Kind::FLOAT,
                                     SimpleAtom::make_sym_val(i0.get_src(1).get_sym()).as_expr(),
                                     make_src_atom(i0.get_src(0).get_reg(), idx), idx);
  } else {
    return make_standard_store(i0, idx, 4, StoreOp::Kind::FLOAT);
  }
}

std::unique_ptr<AtomicOp> convert_sd_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rr0()) && i0.get_src(1).is_imm(2) && i0.get_src(2).is_reg(rr0())) {
    return std::make_unique<SpecialOp>(SpecialOp::Kind::CRASH, idx);
  } else {
    return make_standard_store(i0, idx, 8, StoreOp::Kind::INTEGER);
  }
}

// movn or movz
std::unique_ptr<AtomicOp> convert_cmov_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rs7())) {
    return std::make_unique<ConditionalMoveFalseOp>(
        make_dst_var(i0, idx), make_src_var(i0.get_src(1).get_reg(), idx),
        make_src_var(i0.get_dst(0).get_reg(), idx), i0.kind == InstructionKind::MOVZ, idx);
  } else {
    return nullptr;
  }
}

std::unique_ptr<AtomicOp> convert_dsll32_1(const Instruction& i0, int idx) {
  if (i0.get_dst(0).is_reg(rra())) {
    return std::make_unique<AsmOp>(i0, idx);
  }
  return make_2reg_1imm_op(i0, SimpleExpression::Kind::LEFT_SHIFT, idx, 32);
}

std::unique_ptr<AtomicOp> convert_dsrl32_1(const Instruction& i0, int idx) {
  if (i0.get_dst(0).is_reg(rra())) {
    return std::make_unique<AsmOp>(i0, idx);
  }
  return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_LOGIC, idx, 32);
}

std::unique_ptr<AtomicOp> convert_likely_branch_1(const Instruction& i0,
                                                  IR2_Condition::Kind kind,
                                                  bool likely,
                                                  int idx,
                                                  bool force_asm) {
  return make_branch_no_delay(IR2_Condition(kind, make_src_atom(i0.get_src(0).get_reg(), idx)),
                              likely, i0.get_src(1).get_label(), idx, force_asm);
}

std::unique_ptr<AtomicOp> convert_beql_1(const Instruction& i0,
                                         int idx,
                                         bool likely,
                                         bool force_asm) {
  auto s0 = i0.get_src(0).get_reg();
  auto s1 = i0.get_src(1).get_reg();
  auto dest = i0.get_src(2).get_label();
  IR2_Condition condition;
  if (s0 == rr0() && s1 == rr0()) {
    condition = IR2_Condition(IR2_Condition::Kind::ALWAYS);
  } else if (s1 == rr0()) {
    condition = IR2_Condition(IR2_Condition::Kind::ZERO, make_src_atom(s0, idx));
  } else if (i0.get_src(0).is_reg(rs7())) {
    if (s1 == rs7()) {
      // (if #f ...) type code?
      condition = IR2_Condition(IR2_Condition::Kind::FALSE, SimpleAtom::make_sym_val("#f"));
    } else {
      condition = IR2_Condition(IR2_Condition::Kind::FALSE, make_src_atom(s1, idx));
    }
  } else if (s1 == rs7()) {
    // likely a case where somebody wrote (= x #f) or (!= x #f). much rarer than the flipped one
    condition = IR2_Condition(IR2_Condition::Kind::EQUAL, make_src_atom(s0, idx),
                              SimpleAtom::make_sym_val("#f"));
  } else {
    condition =
        IR2_Condition(IR2_Condition::Kind::EQUAL, make_src_atom(s0, idx), make_src_atom(s1, idx));
    condition.make_flipped();
  }
  return make_branch_no_delay(condition, likely, dest, idx, force_asm);
}

std::unique_ptr<AtomicOp> convert_bnel_1(const Instruction& i0,
                                         int idx,
                                         bool likely,
                                         bool force_asm) {
  auto s0 = i0.get_src(0).get_reg();
  auto s1 = i0.get_src(1).get_reg();
  auto dest = i0.get_src(2).get_label();
  IR2_Condition condition;
  if (s1 == rr0()) {
    condition = IR2_Condition(IR2_Condition::Kind::NONZERO, make_src_atom(s0, idx));
  } else if (i0.get_src(0).is_reg(rs7())) {
    condition = IR2_Condition(IR2_Condition::Kind::TRUTHY, make_src_atom(s1, idx));
  } else if (s1 == rs7()) {
    // likely a case where somebody wrote (= x #f) or (!= x #f). much rarer than the flipped one
    condition = IR2_Condition(IR2_Condition::Kind::NOT_EQUAL, make_src_atom(s0, idx),
                              SimpleAtom::make_sym_val("#f"));
  } else {
    condition = IR2_Condition(IR2_Condition::Kind::NOT_EQUAL, make_src_atom(s0, idx),
                              make_src_atom(s1, idx));
    condition.make_flipped();
  }
  return make_branch_no_delay(condition, likely, dest, idx, force_asm);
}

std::unique_ptr<AtomicOp> convert_subu_1(const Instruction& i0, int idx) {
  // subu a2, v1, s7
  if (i0.get_src(1).is_reg(rs7())) {
    auto src = make_src_atom(i0.get_src(0).get_reg(), idx);
    auto expr = SimpleExpression(SimpleExpression::Kind::SUBU_L32_S7, src);
    auto dst = make_dst_var(i0.get_dst(0).get_reg(), idx);
    return std::make_unique<SetVarOp>(dst, expr, idx);
  } else {
    return nullptr;  // go to asm fallback
  }
}

std::unique_ptr<AtomicOp> convert_1(const Instruction& i0,
                                    int idx,
                                    bool hint_inline_asm,
                                    bool force_asm_branch,
                                    GameVersion version) {
  switch (i0.kind) {
    case InstructionKind::OR:
      return convert_or_1(i0, idx);
    case InstructionKind::POR:
      return convert_por_1(i0, idx);
    case InstructionKind::ORI:
      return convert_ori_1(i0, idx);
    case InstructionKind::AND:
      return make_3reg_op(i0, SimpleExpression::Kind::AND, idx);
    case InstructionKind::PCPYLD:
      if (hint_inline_asm) {
        break;
      } else {
        return make_3reg_op(i0, SimpleExpression::Kind::PCPYLD, idx);
      }
    case InstructionKind::MTC1:
      return convert_mtc1_1(i0, idx);
    case InstructionKind::MFC1:
      return convert_mfc1_1(i0, idx);
    case InstructionKind::LWC1:
      return make_standard_load(i0, idx, 4, LoadVarOp::Kind::FLOAT);
    case InstructionKind::LB:
      return make_standard_load(i0, idx, 1, LoadVarOp::Kind::SIGNED);
    case InstructionKind::LBU:
      return make_standard_load(i0, idx, 1, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LHU:
      return make_standard_load(i0, idx, 2, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LH:
      return make_standard_load(i0, idx, 2, LoadVarOp::Kind::SIGNED);
    case InstructionKind::LWU:
      return make_standard_load(i0, idx, 4, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LW:
      return convert_lw_1(i0, idx);
    case InstructionKind::LD:
      return make_standard_load(i0, idx, 8, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LQ:
      return make_standard_load(i0, idx, 16, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LQC2:
      return make_standard_load(i0, idx, 16, LoadVarOp::Kind::VECTOR_FLOAT);
    case InstructionKind::DSLL:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::LEFT_SHIFT, idx);
    case InstructionKind::DSLL32:
      return convert_dsll32_1(i0, idx);
    case InstructionKind::DSRA:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_ARITH, idx);
    case InstructionKind::DSRA32:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_ARITH, idx, 32);
    case InstructionKind::DSRL:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_LOGIC, idx);
    case InstructionKind::DSRL32:
      return convert_dsrl32_1(i0, idx);
    case InstructionKind::DIVS:
      return make_3reg_op(i0, SimpleExpression::Kind::DIV_S, idx);
    case InstructionKind::SUBS:
      return make_3reg_op(i0, SimpleExpression::Kind::SUB_S, idx);
    case InstructionKind::ADDS:
      return make_3reg_op(i0, SimpleExpression::Kind::ADD_S, idx);
    case InstructionKind::MULS:
      return make_3reg_op(i0, SimpleExpression::Kind::MUL_S, idx);
    case InstructionKind::MINS:
      return make_3reg_op(i0, SimpleExpression::Kind::MIN_S, idx);
    case InstructionKind::MAXS:
      return make_3reg_op(i0, SimpleExpression::Kind::MAX_S, idx);
    case InstructionKind::DADDIU:
      return convert_daddiu_1(i0, idx, version);
    case InstructionKind::DADDU:
      return convert_daddu_1(i0, idx);
    case InstructionKind::DSUBU:
      return convert_dsubu_1(i0, idx);
    case InstructionKind::MULT3:
      return make_3reg_op(i0, SimpleExpression::Kind::MUL_SIGNED, idx);
    case InstructionKind::MULTU3:
      return make_3reg_op(i0, SimpleExpression::Kind::MUL_UNSIGNED, idx);
    case InstructionKind::ANDI:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::AND, idx);
    case InstructionKind::XORI:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::XOR, idx);
    case InstructionKind::NOR:
      return convert_nor_1(i0, idx);
    case InstructionKind::XOR:
      return make_3reg_op(i0, SimpleExpression::Kind::XOR, idx);
    case InstructionKind::ADDIU:
      return convert_addiu_1(i0, idx);
    case InstructionKind::LUI:
      return convert_lui_1(i0, idx);
    case InstructionKind::SLL:
      return convert_sll_1(i0, idx);
    case InstructionKind::DSRAV:
      return make_3reg_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_ARITH, idx);
    case InstructionKind::DSRLV:
      return make_3reg_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_LOGIC, idx);
    case InstructionKind::DSLLV:
      return make_3reg_op(i0, SimpleExpression::Kind::LEFT_SHIFT, idx);
    case InstructionKind::SB:
      return make_standard_store(i0, idx, 1, StoreOp::Kind::INTEGER);
    case InstructionKind::SH:
      return make_standard_store(i0, idx, 2, StoreOp::Kind::INTEGER);
    case InstructionKind::SW:
      return convert_sw_1(i0, idx);
    case InstructionKind::SD:
      return convert_sd_1(i0, idx);
    case InstructionKind::SQ:
      return make_standard_store(i0, idx, 16, StoreOp::Kind::INTEGER);
    case InstructionKind::SQC2:
      return make_standard_store(i0, idx, 16, StoreOp::Kind::VECTOR_FLOAT);
    case InstructionKind::SWC1:
      return convert_swc1_1(i0, idx);
    case InstructionKind::CVTWS:  // float to int
      return make_2reg_op(i0, SimpleExpression::Kind::FLOAT_TO_INT, idx);
    case InstructionKind::CVTSW:  // int to float
      return make_2reg_op(i0, SimpleExpression::Kind::INT_TO_FLOAT, idx);
    case InstructionKind::ABSS:
      return make_2reg_op(i0, SimpleExpression::Kind::ABS_S, idx);
    case InstructionKind::NEGS:
      return make_2reg_op(i0, SimpleExpression::Kind::NEG_S, idx);
    case InstructionKind::SQRTS:
      return make_2reg_op(i0, SimpleExpression::Kind::SQRT_S, idx);
    case InstructionKind::MOVS:
      return make_2reg_op(i0, SimpleExpression::Kind::IDENTITY, idx);
    case InstructionKind::MOVN:
    case InstructionKind::MOVZ:
      return convert_cmov_1(i0, idx);
    case InstructionKind::BGTZL:
      return convert_likely_branch_1(i0, IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED, true, idx,
                                     force_asm_branch);
    case InstructionKind::BGEZL:
      return convert_likely_branch_1(i0, IR2_Condition::Kind::GEQ_ZERO_SIGNED, true, idx,
                                     force_asm_branch);
    case InstructionKind::BLTZL:
      return convert_likely_branch_1(i0, IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED, true, idx,
                                     force_asm_branch);
    case InstructionKind::BEQL:
      return convert_beql_1(i0, idx, true, force_asm_branch);
    case InstructionKind::BNEL:
      return convert_bnel_1(i0, idx, true, force_asm_branch);
    case InstructionKind::SUBU:
      return convert_subu_1(i0, idx);  // may fail
    default:
      return nullptr;
  }
  return nullptr;
}

///////////////////////
// OP 2 Conversions
//////////////////////

std::unique_ptr<AtomicOp> convert_fp_branch_asm(const Instruction& i0,
                                                const Instruction& i1,
                                                IR2_Condition::Kind kind,
                                                int idx) {
  if (i1.kind == InstructionKind::BC1TL || i1.kind == InstructionKind::BC1FL) {
    IR2_Condition condition(kind, make_src_atom(i0.get_src(0).get_reg(), idx),
                            make_src_atom(i0.get_src(1).get_reg(), idx));
    if (i1.kind == InstructionKind::BC1FL) {
      condition.invert();
    }
    // return make_branch(condition, i2, false, i1.get_src(0).get_label(), idx);
    return make_asm_branch_no_delay(condition, true, i1.get_src(0).get_label(), idx);
  }

  return nullptr;
}

std::unique_ptr<AtomicOp> convert_division_2(const Instruction& i0,
                                             const Instruction& i1,
                                             int idx,
                                             bool is_signed) {
  if (i1.kind == InstructionKind::MFLO) {
    // divide
    auto src = make_2reg_expr(
        i0, is_signed ? SimpleExpression::Kind::DIV_SIGNED : SimpleExpression::Kind::DIV_UNSIGNED,
        idx);
    return std::make_unique<SetVarOp>(make_dst_var(i1, idx), src, idx);
  } else if (i1.kind == InstructionKind::MFHI) {
    // mod
    auto src = make_2reg_expr(
        i0, is_signed ? SimpleExpression::Kind::MOD_SIGNED : SimpleExpression::Kind::MOD_UNSIGNED,
        idx);
    return std::make_unique<SetVarOp>(make_dst_var(i1, idx), src, idx);
  } else {
    return nullptr;
  }
}

std::unique_ptr<AtomicOp> convert_jalr_2(const Instruction& i0, const Instruction& i1, int idx) {
  if (i0.kind == InstructionKind::JALR && i0.get_dst(0).is_reg(rra()) &&
      i0.get_src(0).is_reg(rt9()) && is_gpr_2_imm_int(i1, InstructionKind::SLL, rv0(), rra(), 0)) {
    return std::make_unique<CallOp>(idx);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_bne_2(const Instruction& i0,
                                        const Instruction& i1,
                                        int idx,
                                        bool likely,
                                        bool force_asm,
                                        GameVersion version) {
  auto s0 = i0.get_src(0).get_reg();
  auto s1 = i0.get_src(1).get_reg();
  auto dest = i0.get_src(2).get_label();
  IR2_Condition condition;
  if (s1 == rr0()) {
    condition = IR2_Condition(IR2_Condition::Kind::NONZERO, make_src_atom(s0, idx));
  } else if (i0.get_src(0).is_reg(rs7())) {
    condition = IR2_Condition(IR2_Condition::Kind::TRUTHY, make_src_atom(s1, idx));
  } else if (s1 == rs7()) {
    // likely a case where somebody wrote (= x #f) or (!= x #f). much rarer than the flipped one
    condition = IR2_Condition(IR2_Condition::Kind::NOT_EQUAL, make_src_atom(s0, idx),
                              SimpleAtom::make_sym_val("#f"));
  } else {
    condition = IR2_Condition(IR2_Condition::Kind::NOT_EQUAL, make_src_atom(s0, idx),
                              make_src_atom(s1, idx));
    condition.make_flipped();
  }
  return make_branch(condition, i1, likely, dest, idx, force_asm, version);
}

std::unique_ptr<AtomicOp> convert_beq_2(const Instruction& i0,
                                        const Instruction& i1,
                                        int idx,
                                        bool likely,
                                        bool force_asm,
                                        GameVersion version) {
  auto s0 = i0.get_src(0).get_reg();
  auto s1 = i0.get_src(1).get_reg();
  auto dest = i0.get_src(2).get_label();
  IR2_Condition condition;
  if (s0 == rr0() && s1 == rr0()) {
    condition = IR2_Condition(IR2_Condition::Kind::ALWAYS);
  } else if (s1 == rr0()) {
    condition = IR2_Condition(IR2_Condition::Kind::ZERO, make_src_atom(s0, idx));
  } else if (i0.get_src(0).is_reg(rs7())) {
    if (s1 == rs7()) {
      // (if #f ...) type code?
      condition = IR2_Condition(IR2_Condition::Kind::FALSE, SimpleAtom::make_sym_val("#f"));
    } else {
      condition = IR2_Condition(IR2_Condition::Kind::FALSE, make_src_atom(s1, idx));
    }
  } else if (s1 == rs7()) {
    // likely a case where somebody wrote (= x #f) or (!= x #f). much rarer than the flipped one
    condition = IR2_Condition(IR2_Condition::Kind::EQUAL, make_src_atom(s0, idx),
                              SimpleAtom::make_sym_val("#f"));
  } else {
    condition =
        IR2_Condition(IR2_Condition::Kind::EQUAL, make_src_atom(s0, idx), make_src_atom(s1, idx));
    condition.make_flipped();
  }
  return make_branch(condition, i1, likely, dest, idx, force_asm, version);
}

std::unique_ptr<AtomicOp> convert_daddiu_2(const Instruction& i0,
                                           const Instruction& i1,
                                           int idx,
                                           GameVersion version) {
  // daddiu dest, s7, 8
  // mov{n,z} dest, s7, src
  if (i1.kind == InstructionKind::MOVN || i1.kind == InstructionKind::MOVZ) {
    auto dest = i0.get_dst(0).get_reg();
    auto src = i1.get_src(1).get_reg();
    if (!i0.get_src(0).is_reg(rs7())) {
      return nullptr;
    }
    ASSERT(i0.get_src(0).is_reg(rs7()));
    ASSERT(i0.get_src(1).is_imm(true_symbol_offset(version)));
    ASSERT(i1.get_dst(0).is_reg(dest));
    ASSERT(i1.get_src(0).is_reg(rs7()));
    auto kind =
        i1.kind == InstructionKind::MOVN ? IR2_Condition::Kind::ZERO : IR2_Condition::Kind::NONZERO;
    return std::make_unique<SetVarConditionOp>(make_dst_var(dest, idx),
                                               IR2_Condition(kind, make_src_atom(src, idx)), idx);
  }

  //  daddiu v1, v1, L152
  //  daddu v1, v1, fp
  if (i1.kind == InstructionKind::DADDU && i0.get_src(1).is_label()) {
    auto dest = i0.get_src(0).get_reg();
    if (!i0.get_src(0).is_reg(dest)) {
      return nullptr;
    }
    if (!i1.get_src(0).is_reg(dest)) {
      return nullptr;
    }
    if (!i1.get_src(0).is_reg(dest)) {
      return nullptr;
    }
    if (!i1.get_src(1).is_reg(rfp())) {
      return nullptr;
    }
    auto stat = SimpleAtom::make_static_address(i0.get_src(1).get_label());
    auto expr = SimpleExpression(SimpleExpression::Kind::ADD, make_src_atom(dest, idx), stat);
    return std::make_unique<SetVarOp>(make_dst_var(dest, idx), expr, idx);
  }

  return nullptr;
}

std::unique_ptr<AtomicOp> convert_lui_2(const Instruction& i0, const Instruction& i1, int idx) {
  if (i1.kind == InstructionKind::ORI) {
    // lui temp, <>
    // ori dst, temp, <>
    // possibly temp = dst.
    auto temp = i0.get_dst(0).get_reg();
    if (i1.get_src(0).get_reg() != temp) {
      return nullptr;
    }
    auto dst = i1.get_dst(0).get_reg();

    SimpleAtom src;
    if (i0.get_src(0).is_imm() && i1.get_src(1).is_imm()) {
      src = SimpleAtom::make_int_constant(s64(i1.get_src(1).get_imm()) +
                                          (s64(i0.get_src(0).get_imm()) << 16));
    } else if (i0.get_src(0).is_label() && i1.get_src(1).is_label()) {
      auto label = i0.get_src(0).get_label();
      ASSERT(label == i1.get_src(1).get_label());
      src = SimpleAtom::make_static_address(label);
    }

    auto result = std::make_unique<SetVarOp>(make_dst_var(dst, idx), src.as_expr(), idx);
    if (temp != dst) {
      result->add_clobber_reg(temp);
    }
    return result;
  }

  return nullptr;
}

std::unique_ptr<AtomicOp> convert_slt_2(const Instruction& i0,
                                        const Instruction& i1,
                                        int idx,
                                        bool is_signed) {
  // this is to do a min or max.
  // possibly due to a GOAL compiler bug, the output always goes in left and there is always
  // a clobbered register. Likely there was a swapped register allocation setup here.
  // it doesn't generate wrong code, just not optimal.
  // slt temp, left, right
  // mov{n,z} left, right, temp
  auto temp = i0.get_dst(0).get_reg();
  auto left = i0.get_src(0).get_reg();
  auto right = i0.get_src(1).get_reg();
  if (temp == left) {
    return nullptr;
  }
  ASSERT(temp != left);
  ASSERT(temp != right);
  ASSERT(left != right);
  std::unique_ptr<AtomicOp> result;
  SimpleExpression::Kind kind;
  if (is_gpr_3(i1, InstructionKind::MOVZ, left, right, temp)) {
    kind = is_signed ? SimpleExpression::Kind::MIN_SIGNED : SimpleExpression::Kind::MIN_UNSIGNED;
  } else if (is_gpr_3(i1, InstructionKind::MOVN, left, right, temp)) {
    kind = is_signed ? SimpleExpression::Kind::MAX_SIGNED : SimpleExpression::Kind::MAX_UNSIGNED;
  } else {
    return nullptr;
  }
  result = std::make_unique<SetVarOp>(
      make_dst_var(left, idx),
      SimpleExpression(kind, make_src_atom(left, idx), make_src_atom(right, idx)), idx);
  result->add_clobber_reg(temp);
  return result;
}

std::unique_ptr<AtomicOp> convert_bltz_2(const Instruction& i0,
                                         const Instruction& i1,
                                         int idx,
                                         GameVersion version) {
  // bltz is never emitted outside of inline asm.
  auto dest = i0.get_src(1).get_label();
  return make_asm_branch(IR2_Condition(IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED,
                                       make_src_atom(i0.get_src(0).get_reg(), idx)),
                         i1, false, dest, idx, version);
}

std::unique_ptr<AtomicOp> convert_bgez_2(const Instruction& i0,
                                         const Instruction& i1,
                                         int idx,
                                         GameVersion version) {
  // bgez is never emitted outside of inline asm.
  auto dest = i0.get_src(1).get_label();
  return make_asm_branch(IR2_Condition(IR2_Condition::Kind::GEQ_ZERO_SIGNED,
                                       make_src_atom(i0.get_src(0).get_reg(), idx)),
                         i1, false, dest, idx, version);
}

std::unique_ptr<AtomicOp> convert_blez_2(const Instruction& i0,
                                         const Instruction& i1,
                                         int idx,
                                         GameVersion version) {
  // blez is never emitted outside of inline asm.
  auto dest = i0.get_src(1).get_label();
  return make_asm_branch(IR2_Condition(IR2_Condition::Kind::LEQ_ZERO_SIGNED,
                                       make_src_atom(i0.get_src(0).get_reg(), idx)),
                         i1, false, dest, idx, version);
}

std::unique_ptr<AtomicOp> convert_bgtz_2(const Instruction& i0,
                                         const Instruction& i1,
                                         int idx,
                                         GameVersion version) {
  // bgtz is never emitted outside of inline asm.
  auto dest = i0.get_src(1).get_label();
  return make_asm_branch(IR2_Condition(IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED,
                                       make_src_atom(i0.get_src(0).get_reg(), idx)),
                         i1, false, dest, idx, version);
}

std::unique_ptr<AtomicOp> convert_2(const Instruction& i0,
                                    const Instruction& i1,
                                    int idx,
                                    bool force_asm_branch,
                                    GameVersion version) {
  switch (i0.kind) {
    case InstructionKind::DIV:
      return convert_division_2(i0, i1, idx, true);
    case InstructionKind::DIVU:
      return convert_division_2(i0, i1, idx, false);
    case InstructionKind::JALR:
      return convert_jalr_2(i0, i1, idx);
    case InstructionKind::BNE:
      return convert_bne_2(i0, i1, idx, false, force_asm_branch, version);
    case InstructionKind::BEQ:
      return convert_beq_2(i0, i1, idx, false, force_asm_branch, version);
    case InstructionKind::DADDIU:
      return convert_daddiu_2(i0, i1, idx, version);
    case InstructionKind::LUI:
      return convert_lui_2(i0, i1, idx);
    case InstructionKind::SLT:
      return convert_slt_2(i0, i1, idx, true);
    case InstructionKind::SLTU:
      return convert_slt_2(i0, i1, idx, false);
    case InstructionKind::CLTS:
      return convert_fp_branch_asm(i0, i1, IR2_Condition::Kind::FLOAT_LESS_THAN, idx);
    case InstructionKind::BLTZ:
      return convert_bltz_2(i0, i1, idx, version);
    case InstructionKind::BGEZ:
      return convert_bgez_2(i0, i1, idx, version);
    case InstructionKind::BGTZ:
      return convert_bgtz_2(i0, i1, idx, version);
    case InstructionKind::BLEZ:
      return convert_blez_2(i0, i1, idx, version);
    default:
      return nullptr;
  }
}

///////////////////////
// OP 3 Conversions
//////////////////////

std::unique_ptr<AtomicOp> convert_lui_3(const Instruction& i0,
                                        const Instruction& i1,
                                        const Instruction& i2,
                                        int idx) {
  if (i1.kind == InstructionKind::ORI && i0.get_src(0).is_label() && i1.get_src(1).is_label() &&
      is_gpr_3(i2, InstructionKind::ADDU, {}, rfp(), {})) {
    // lui temp, <>
    // ori dst, temp, <>
    // addu dst, fp, dst
    ASSERT(i0.get_dst(0).get_reg() == i1.get_src(0).get_reg());      // temp
    ASSERT(i0.get_src(0).get_label() == i1.get_src(1).get_label());  // labels
    ASSERT(i2.get_dst(0).get_reg() == i2.get_src(1).get_reg());      // dst
    ASSERT(i2.get_dst(0).get_reg() == i1.get_dst(0).get_reg());      // dst
    auto temp = i0.get_dst(0).get_reg();
    auto dst = i2.get_dst(0).get_reg();
    auto label = i0.get_src(0).get_label();
    auto result = std::make_unique<SetVarOp>(make_dst_var(dst, idx),
                                             SimpleAtom::make_static_address(label).as_expr(), idx);
    if (dst != temp) {
      result->add_clobber_reg(temp);
    }
    return result;
  } else if (i1.kind == InstructionKind::ORI && i1.get_src(1).is_label() &&
             is_gpr_3(i2, InstructionKind::DADDU, {}, {}, rfp())) {
    // lui temp, <>
    // ori temp, temp, <>
    // daddu dst, temp, fp
    ASSERT(i0.get_dst(0).get_reg() == i1.get_src(0).get_reg());      // temp
    ASSERT(i0.get_src(0).get_label() == i1.get_src(1).get_label());  // labels
    ASSERT(i0.get_dst(0).get_reg() == i1.get_dst(0).get_reg());      // temp
    ASSERT(i2.get_src(0).get_reg() == i0.get_dst(0).get_reg());      // temp
    auto temp = i0.get_dst(0).get_reg();
    auto dst = i2.get_dst(0).get_reg();
    auto label = i0.get_src(0).get_label();
    auto result = std::make_unique<SetVarOp>(make_dst_var(dst, idx),
                                             SimpleAtom::make_static_address(label).as_expr(), idx);
    if (dst != temp) {
      result->add_clobber_reg(temp);
    }
    return result;
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_dsubu_3(const Instruction& i0,
                                          const Instruction& i1,
                                          const Instruction& i2,
                                          int idx,
                                          GameVersion version) {
  if (i1.kind == InstructionKind::DADDIU &&
      (i2.kind == InstructionKind::MOVN || i2.kind == InstructionKind::MOVZ)) {
    // dsubu temp, a, b
    // daddiu dst, s7, 8
    // mov{n,z} dst, s7, temp
    auto temp = i0.get_dst(0).get_reg();
    auto a = i0.get_src(0).get_reg();
    auto b = i0.get_src(1).get_reg();
    auto dest = i1.get_dst(0).get_reg();
    ASSERT(i1.get_src(0).is_reg(rs7()));
    ASSERT(i1.get_src(1).is_imm(true_symbol_offset(version)));
    ASSERT(i2.get_dst(0).get_reg() == dest);
    ASSERT(i2.get_src(0).is_reg(rs7()));
    ASSERT(i2.get_src(1).get_reg() == temp);
    ASSERT(temp != dest);
    auto kind = i2.kind == InstructionKind::MOVN ? IR2_Condition::Kind::EQUAL
                                                 : IR2_Condition::Kind::NOT_EQUAL;
    std::unique_ptr<AtomicOp> result;
    if (b == rs7()) {
      // some sort of not gone wrong?
      result = std::make_unique<SetVarConditionOp>(
          make_dst_var(dest, idx),
          IR2_Condition(kind, make_src_atom(a, idx), SimpleAtom::make_sym_val("#f")), idx);
    } else if (b == rr0()) {
      // not the greatest codegen...
      result = std::make_unique<SetVarConditionOp>(
          make_dst_var(dest, idx),
          IR2_Condition(kind, make_src_atom(a, idx), SimpleAtom::make_int_constant(0)), idx);
    } else {
      result = std::make_unique<SetVarConditionOp>(
          make_dst_var(dest, idx),
          IR2_Condition(kind, make_src_atom(a, idx), make_src_atom(b, idx)), idx);
    }

    result->add_clobber_reg(temp);
    return result;
  }
  return nullptr;
}

void add_clobber_if_unwritten(AtomicOp& op, Register clobber) {
  op.update_register_info();
  std::vector<Register> clobber_regs = op.clobber_regs();
  if (std::find(op.write_regs().begin(), op.write_regs().end(), clobber) == op.write_regs().end()) {
    clobber_regs.push_back(clobber);
  }
  op.clear_register_info();
  for (auto& reg : clobber_regs) {
    op.add_clobber_reg(reg);
  }
}

std::unique_ptr<AtomicOp> convert_slt_3(const Instruction& i0,
                                        const Instruction& i1,
                                        const Instruction& i2,
                                        bool is_signed,
                                        int idx,
                                        GameVersion version) {
  auto s0 = i0.get_src(0).get_reg();
  auto s1 = i0.get_src(1).get_reg();
  std::unique_ptr<AtomicOp> result;
  if (i1.kind == InstructionKind::BNE || i1.kind == InstructionKind::BEQ) {
    // assume bne, invert at the end if it's beq
    // slt temp, a0, a1
    // bne temp, r0, dest
    // delay slot
    auto temp = i0.get_dst(0).get_reg();
    auto dest = i1.get_src(2).get_label();
    ASSERT(i1.get_src(0).get_reg() == temp);
    ASSERT(i1.get_src(1).is_reg(rr0()));

    IR2_Condition condition;
    if (s1 == rr0()) {
      // ????
      auto kind = is_signed ? IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED
                            : IR2_Condition::Kind::LESS_THAN_ZERO_UNSIGNED;
      condition = IR2_Condition(kind, make_src_atom(s0, idx));
    } else if (s0 == rr0()) {
      auto kind = is_signed ? IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED
                            : IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED;
      condition = IR2_Condition(kind, make_src_atom(s1, idx));
    } else {
      auto kind = is_signed ? IR2_Condition::Kind::LESS_THAN_SIGNED
                            : IR2_Condition::Kind::LESS_THAN_UNSIGNED;
      condition = IR2_Condition(kind, make_src_atom(s0, idx), make_src_atom(s1, idx));
    }

    if (i1.kind == InstructionKind::BEQ) {
      condition.invert();
    }
    result = make_branch(condition, i2, false, dest, idx, false, version);
    add_clobber_if_unwritten(*result, temp);
    return result;
  } else if (i1.kind == InstructionKind::DADDIU &&
             (i2.kind == InstructionKind::MOVZ || i2.kind == InstructionKind::MOVN)) {
    // all this assumes movz. Then at the end we invert it if it's actually a movn
    // slt temp, a0, a1
    // daddiu dest, s7, 8
    // movz dest, s7, temp
    auto temp = i0.get_dst(0).get_reg();
    auto dest = i1.get_dst(0).get_reg();
    ASSERT(i1.get_src(0).is_reg(rs7()));
    ASSERT(i1.get_src(1).is_imm(true_symbol_offset(version)));
    ASSERT(i2.get_dst(0).get_reg() == dest);
    ASSERT(i2.get_src(0).is_reg(rs7()));
    ASSERT(i2.get_src(1).get_reg() == temp);
    ASSERT(temp != dest);
    IR2_Condition condition;
    if (s1 == rr0()) {
      auto kind = is_signed ? IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED
                            : IR2_Condition::Kind::LESS_THAN_ZERO_UNSIGNED;
      // < 0
      condition = IR2_Condition(kind, make_src_atom(s0, idx));
    } else if (s0 == rr0()) {
      auto kind = is_signed ? IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED
                            : IR2_Condition::Kind::GREATER_THAN_ZERO_UNSIGNED;
      condition = IR2_Condition(kind, make_src_atom(s1, idx));
    } else {
      auto kind = is_signed ? IR2_Condition::Kind::LESS_THAN_SIGNED
                            : IR2_Condition::Kind::LESS_THAN_UNSIGNED;
      condition = IR2_Condition(kind, make_src_atom(s0, idx), make_src_atom(s1, idx));
    }
    if (i2.kind == InstructionKind::MOVN) {
      condition.invert();
    }
    result = std::make_unique<SetVarConditionOp>(make_dst_var(dest, idx), condition, idx);
    add_clobber_if_unwritten(*result, temp);
    return result;
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_slti_3(const Instruction& i0,
                                         const Instruction& i1,
                                         const Instruction& i2,
                                         bool is_signed,
                                         int idx,
                                         GameVersion version) {
  auto s0 = i0.get_src(0).get_reg();
  auto s1 = SimpleAtom::make_int_constant(i0.get_src(1).get_imm());
  std::unique_ptr<AtomicOp> result;
  if (i1.kind == InstructionKind::BNE || i1.kind == InstructionKind::BEQ) {
    // assume bne, invert at the end if it's beq
    // slt temp, a0, <>
    // bne temp, r0, dest
    // delay slot
    auto temp = i0.get_dst(0).get_reg();
    auto dest = i1.get_src(2).get_label();
    ASSERT(i1.get_src(0).get_reg() == temp);
    ASSERT(i1.get_src(1).is_reg(rr0()));
    auto kind =
        is_signed ? IR2_Condition::Kind::LESS_THAN_SIGNED : IR2_Condition::Kind::LESS_THAN_UNSIGNED;
    auto condition = IR2_Condition(kind, make_src_atom(s0, idx), s1);
    if (i1.kind == InstructionKind::BEQ) {
      condition.invert();
    }
    result = make_branch(condition, i2, false, dest, idx, false, version);
    add_clobber_if_unwritten(*result, temp);
    return result;
  } else if (i1.kind == InstructionKind::DADDIU &&
             (i2.kind == InstructionKind::MOVZ || i2.kind == InstructionKind::MOVN)) {
    // all this assumes movz. Then at the end we invert it if it's actually a movn
    // slt temp, a0, <>
    // daddiu dest, s7, 8
    // movz dest, s7, temp
    auto temp = i0.get_dst(0).get_reg();
    auto dest = i1.get_dst(0).get_reg();
    ASSERT(i1.get_src(0).is_reg(rs7()));
    ASSERT(i1.get_src(1).is_imm(true_symbol_offset(version)));
    ASSERT(i2.get_dst(0).get_reg() == dest);
    ASSERT(i2.get_src(0).is_reg(rs7()));
    ASSERT(i2.get_src(1).get_reg() == temp);
    ASSERT(temp != dest);
    IR2_Condition condition;

    auto kind =
        is_signed ? IR2_Condition::Kind::LESS_THAN_SIGNED : IR2_Condition::Kind::LESS_THAN_UNSIGNED;
    condition = IR2_Condition(kind, make_src_atom(s0, idx), s1);
    if (i2.kind == InstructionKind::MOVN) {
      condition.invert();
    }
    result = std::make_unique<SetVarConditionOp>(make_dst_var(dest, idx), condition, idx);
    add_clobber_if_unwritten(*result, temp);
    return result;
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_fp_branch(const Instruction& i0,
                                            const Instruction& i1,
                                            const Instruction& i2,
                                            IR2_Condition::Kind kind,
                                            int idx,
                                            GameVersion version) {
  if (i1.kind == InstructionKind::BC1T || i1.kind == InstructionKind::BC1F) {
    IR2_Condition condition(kind, make_src_atom(i0.get_src(0).get_reg(), idx),
                            make_src_atom(i0.get_src(1).get_reg(), idx));
    if (i1.kind == InstructionKind::BC1F) {
      condition.invert();
    }
    return make_branch(condition, i2, false, i1.get_src(0).get_label(), idx, false, version);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_3(const Instruction& i0,
                                    const Instruction& i1,
                                    const Instruction& i2,
                                    int idx,
                                    GameVersion version) {
  switch (i0.kind) {
    case InstructionKind::LUI:
      return convert_lui_3(i0, i1, i2, idx);
    case InstructionKind::DSUBU:
      return convert_dsubu_3(i0, i1, i2, idx, version);
    case InstructionKind::SLT:
      return convert_slt_3(i0, i1, i2, true, idx, version);
    case InstructionKind::SLTU:
      return convert_slt_3(i0, i1, i2, false, idx, version);
    case InstructionKind::SLTI:
      return convert_slti_3(i0, i1, i2, true, idx, version);
    case InstructionKind::SLTIU:
      return convert_slti_3(i0, i1, i2, false, idx, version);
    case InstructionKind::CEQS:
      return convert_fp_branch(i0, i1, i2, IR2_Condition::Kind::FLOAT_EQUAL, idx, version);
    case InstructionKind::CLTS:
      return convert_fp_branch(i0, i1, i2, IR2_Condition::Kind::FLOAT_LESS_THAN, idx, version);
    case InstructionKind::CLES:
      return convert_fp_branch(i0, i1, i2, IR2_Condition::Kind::FLOAT_LEQ, idx, version);
    default:
      return nullptr;
  }
}

///////////////////////
// OP 4 Conversions
//////////////////////

std::unique_ptr<AtomicOp> convert_dsll32_4(const Instruction& i0,
                                           const Instruction& i1,
                                           const Instruction& i2,
                                           const Instruction& i3,
                                           int idx,
                                           GameVersion version) {
  if (i1.kind == InstructionKind::SLT && i2.kind == InstructionKind::BEQ) {
    // dsll32 temp, a0, 30
    // slt temp, temp, r0
    // beq temp, r0, <>
    // delay

    auto temp = i0.get_dst(0).get_reg();
    auto arg = i0.get_src(0).get_reg();
    auto sa = i0.get_src(1).get_imm();
    // 30 = 64 - (32 + log2(0b10) + 1)
    if (sa != 30) {
      return nullptr;
    }
    ASSERT(i1.get_dst(0).get_reg() == temp);
    ASSERT(i1.get_src(0).get_reg() == temp);
    ASSERT(i1.get_src(1).is_reg(rr0()));
    ASSERT(i2.get_src(0).get_reg() == temp);
    ASSERT(i2.get_src(1).is_reg(rr0()));

    IR2_Condition condition(IR2_Condition::Kind::IS_NOT_PAIR, make_src_atom(arg, idx));
    auto result = make_branch(condition, i3, false, i2.get_src(2).get_label(), idx, false, version);
    result->add_clobber_reg(temp);
    return result;
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_fp_branch_with_nop(const Instruction& i0,
                                                     const Instruction& i1,
                                                     const Instruction& i2,
                                                     const Instruction& i3,
                                                     IR2_Condition::Kind kind,
                                                     int idx,
                                                     GameVersion version) {
  if (i1.kind != InstructionKind::VNOP) {
    return nullptr;
  }
  if (i2.kind == InstructionKind::BC1T || i2.kind == InstructionKind::BC1F) {
    IR2_Condition condition(kind, make_src_atom(i0.get_src(0).get_reg(), idx),
                            make_src_atom(i0.get_src(1).get_reg(), idx));
    if (i2.kind == InstructionKind::BC1F) {
      condition.invert();
    }
    return make_branch(condition, i3, false, i2.get_src(0).get_label(), idx, false, version);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_4(const Instruction& i0,
                                    const Instruction& i1,
                                    const Instruction& i2,
                                    const Instruction& i3,
                                    int idx,
                                    GameVersion version) {
  switch (i0.kind) {
    case InstructionKind::DSLL32:
      return convert_dsll32_4(i0, i1, i2, i3, idx, version);
    case InstructionKind::CEQS:
      return convert_fp_branch_with_nop(i0, i1, i2, i3, IR2_Condition::Kind::FLOAT_EQUAL, idx,
                                        version);
    default:
      return nullptr;
  }
}

///////////////////////
// OP 5 Conversions
//////////////////////

std::unique_ptr<AtomicOp> convert_vector_plus(const Instruction& i0,
                                              const Instruction& i1,
                                              const Instruction& i2,
                                              const Instruction& i3,
                                              const Instruction& i4,
                                              int idx) {
  // vmove.w vf6, vf0
  if (i0.kind != InstructionKind::VMOVE || i0.get_src(0).get_reg() != make_vf(0) ||
      i0.get_dst(0).get_reg() != make_vf(6) || i0.cop2_dest != 1) {
    return nullptr;
  }

  // lqc2 vf4, 0(a1) (src1)
  if (i1.kind != InstructionKind::LQC2 || i1.get_dst(0).get_reg() != make_vf(4) ||
      !i1.get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register src1 = i1.get_src(1).get_reg();

  // lqc2 vf5, 0(a2) (src2)
  if (i2.kind != InstructionKind::LQC2 || i2.get_dst(0).get_reg() != make_vf(5) ||
      !i2.get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register src2 = i2.get_src(1).get_reg();

  // vadd.xyz vf6, vf4, vf5
  if (i3.kind != InstructionKind::VADD || i3.get_dst(0).get_reg() != make_vf(6) ||
      i3.get_src(0).get_reg() != make_vf(4) || i3.get_src(1).get_reg() != make_vf(5) ||
      i3.cop2_dest != 14) {
    return nullptr;
  }

  // sqc2 vf6, 0(a0) (dst)
  if (i4.kind != InstructionKind::SQC2 || i4.get_src(0).get_reg() != make_vf(6) ||
      !i4.get_src(1).is_imm(0)) {
    return nullptr;
  }
  Register dst = i4.get_src(2).get_reg();

  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_PLUS, make_src_atom(dst, idx),
                       make_src_atom(src1, idx), make_src_atom(src2, idx)),
      idx);
}

std::unique_ptr<AtomicOp> convert_vector_minus(const Instruction& i0,
                                               const Instruction& i1,
                                               const Instruction& i2,
                                               const Instruction& i3,
                                               const Instruction& i4,
                                               int idx) {
  // lqc2 vf4, 0(a1) (src1)
  if (i0.kind != InstructionKind::LQC2 || i0.get_dst(0).get_reg() != make_vf(4) ||
      !i0.get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register src1 = i0.get_src(1).get_reg();

  // lqc2 vf5, 0(a2) (src2)
  if (i1.kind != InstructionKind::LQC2 || i1.get_dst(0).get_reg() != make_vf(5) ||
      !i1.get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register src2 = i1.get_src(1).get_reg();

  // vmove.w vf6, vf0
  if (i2.kind != InstructionKind::VMOVE || i2.get_src(0).get_reg() != make_vf(0) ||
      i2.get_dst(0).get_reg() != make_vf(6) || i2.cop2_dest != 1) {
    return nullptr;
  }

  // vadd.xyz vf6, vf4, vf5
  if (i3.kind != InstructionKind::VSUB || i3.get_dst(0).get_reg() != make_vf(6) ||
      i3.get_src(0).get_reg() != make_vf(4) || i3.get_src(1).get_reg() != make_vf(5) ||
      i3.cop2_dest != 14) {
    return nullptr;
  }

  // sqc2 vf6, 0(a0) (dst)
  if (i4.kind != InstructionKind::SQC2 || i4.get_src(0).get_reg() != make_vf(6) ||
      !i4.get_src(1).is_imm(0)) {
    return nullptr;
  }
  Register dst = i4.get_src(2).get_reg();

  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_MINUS, make_src_atom(dst, idx),
                       make_src_atom(src1, idx), make_src_atom(src2, idx)),
      idx);
}

std::unique_ptr<AtomicOp> convert_vector_cross(const Instruction& i0,
                                               const Instruction& i1,
                                               const Instruction& i2,
                                               const Instruction& i3,
                                               const Instruction& i4,
                                               int idx) {
  // lqc2 vf1, 0(v1) (src1)
  if (i0.kind != InstructionKind::LQC2 || i0.get_dst(0).get_reg() != make_vf(1) ||
      !i0.get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register src1 = i0.get_src(1).get_reg();

  // lqc2 vf5, 0(a2) (src2)
  if (i1.kind != InstructionKind::LQC2 || i1.get_dst(0).get_reg() != make_vf(2) ||
      !i1.get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register src2 = i1.get_src(1).get_reg();

  // vopmula.xyz acc, vf1, vf2
  if (i2.kind != InstructionKind::VOPMULA || i2.get_src(0).get_reg() != make_vf(1) ||
      i2.get_src(1).get_reg() != make_vf(2) || i2.cop2_dest != 14) {
    return nullptr;
  }

  // vopmsub.xyz vf3, vf2, vf1
  if (i3.kind != InstructionKind::VOPMSUB || i3.get_dst(0).get_reg() != make_vf(3) ||
      i3.get_src(0).get_reg() != make_vf(2) || i3.get_src(1).get_reg() != make_vf(1) ||
      i3.cop2_dest != 14) {
    return nullptr;
  }

  // sqc2 vf3, 0(a0)
  if (i4.kind != InstructionKind::SQC2 || i4.get_src(0).get_reg() != make_vf(3) ||
      !i4.get_src(1).is_imm(0)) {
    return nullptr;
  }
  Register dst = i4.get_src(2).get_reg();

  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_CROSS, make_src_atom(dst, idx),
                       make_src_atom(src1, idx), make_src_atom(src2, idx)),
      idx);
}

std::unique_ptr<AtomicOp> convert_5(const Instruction& i0,
                                    const Instruction& i1,
                                    const Instruction& i2,
                                    const Instruction& i3,
                                    const Instruction& i4,
                                    int idx,
                                    GameVersion version) {
  auto s6 = make_gpr(Reg::S6);

  int process_offset = -1;
  switch (version) {
    case GameVersion::Jak1:
      process_offset = 44;
      break;
    case GameVersion::Jak2:
    case GameVersion::Jak3:
      process_offset = 48;
      break;
    default:
      ASSERT(false);
  }
  if (i0.kind == InstructionKind::LWU && i0.get_dst(0).is_reg(s6) &&
      i0.get_src(0).get_imm() == process_offset && i0.get_src(1).is_reg(s6) &&
      i1.kind == InstructionKind::MTLO1 && i1.get_src(0).is_reg(s6) &&
      i2.kind == InstructionKind::LWU && i2.get_dst(0).is_reg(s6) &&
      i2.get_src(0).get_imm() == 12 && i2.get_src(1).is_reg(s6) &&
      i3.kind == InstructionKind::JALR && i3.get_dst(0).is_reg(make_gpr(Reg::RA)) &&
      i3.get_src(0).is_reg(s6) && i4.kind == InstructionKind::MFLO1 && i4.get_dst(0).is_reg(s6)) {
    return std::make_unique<SpecialOp>(SpecialOp::Kind::SUSPEND, idx);
  }

  auto as_vector_plus = convert_vector_plus(i0, i1, i2, i3, i4, idx);
  if (as_vector_plus) {
    return as_vector_plus;
  }

  auto as_vector_minus = convert_vector_minus(i0, i1, i2, i3, i4, idx);
  if (as_vector_minus) {
    return as_vector_minus;
  }

  auto as_vector_cross = convert_vector_cross(i0, i1, i2, i3, i4, idx);
  if (as_vector_cross) {
    return as_vector_cross;
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_vector_float_product(const Instruction& i0,
                                                       const Instruction& i1,
                                                       const Instruction& i2,
                                                       const Instruction& i3,
                                                       const Instruction& i4,
                                                       const Instruction& i5,
                                                       int idx) {
  // lqc2 vf1, 0(vect_in)
  if (i0.kind != InstructionKind::LQC2 || i0.get_dst(0).get_reg() != make_vf(1) ||
      !i0.get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register vec_src = i0.get_src(1).get_reg();

  // mfc1 gpr_temp, float_in
  if (i1.kind != InstructionKind::MFC1) {
    return nullptr;
  }
  Register gpr_temp = i1.get_dst(0).get_reg();
  Register float_src = i1.get_src(0).get_reg();

  // qmtc2.i vf2, gpr_temp
  if (i2.kind != InstructionKind::QMTC2 || i2.get_dst(0).get_reg() != make_vf(2) ||
      i2.get_src(0).get_reg() != gpr_temp) {
    return nullptr;
  }

  // vaddx.w vf1, vf0, vf0
  if (i3.kind != InstructionKind::VADD_BC || i3.get_dst(0).get_reg() != make_vf(1) ||
      i3.get_src(0).get_reg() != make_vf(0) || i3.get_src(1).get_reg() != make_vf(0) ||
      i3.cop2_bc != 0 || i3.cop2_dest != 1) {
    return nullptr;
  }

  // vmulx.xyz vf1, vf1, vf2
  if (i4.kind != InstructionKind::VMUL_BC || i4.get_dst(0).get_reg() != make_vf(1) ||
      i4.get_src(0).get_reg() != make_vf(1) || i4.get_src(1).get_reg() != make_vf(2) ||
      i4.cop2_dest != 14 || i4.cop2_bc != 0) {
    return nullptr;
  }

  // sqc2 vf1, 0(gE)
  if (i5.kind != InstructionKind::SQC2 || i5.get_src(0).get_reg() != make_vf(1) ||
      !i5.get_src(1).is_imm(0)) {
    return nullptr;
  }
  Register dst = i5.get_src(2).get_reg();

  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_FLOAT_PRODUCT, make_src_atom(dst, idx),
                       make_src_atom(vec_src, idx), make_src_atom(float_src, idx)),
      idx);
}

std::unique_ptr<AtomicOp> convert_6(const Instruction& i0,
                                    const Instruction& i1,
                                    const Instruction& i2,
                                    const Instruction& i3,
                                    const Instruction& i4,
                                    const Instruction& i5,
                                    int idx) {
  auto as_vector_float_product = convert_vector_float_product(i0, i1, i2, i3, i4, i5, idx);
  if (as_vector_float_product) {
    return as_vector_float_product;
  }

  return nullptr;
}

std::unique_ptr<AtomicOp> convert_vector_plus_float_times(const Instruction* instrs, int idx) {
  //  lqc2 vf2, 0(a2)
  if (instrs[0].kind != InstructionKind::LQC2 || instrs[0].get_dst(0).get_reg() != make_vf(2) ||
      !instrs[0].get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register vec_src_2 = instrs[0].get_src(1).get_reg();

  //  lqc2 vf1, 0(a1)
  if (instrs[1].kind != InstructionKind::LQC2 || instrs[1].get_dst(0).get_reg() != make_vf(1) ||
      !instrs[1].get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register vec_src_1 = instrs[1].get_src(1).get_reg();

  // mfc1 a0, f0
  if (instrs[2].kind != InstructionKind::MFC1) {
    return nullptr;
  }

  Register flt_src_3 = instrs[2].get_src(0).get_reg();
  Register temp = instrs[2].get_dst(0).get_reg();

  //  qmtc2.i vf3, a3
  if (instrs[3].kind != InstructionKind::QMTC2 || instrs[3].get_dst(0).get_reg() != make_vf(3) ||
      instrs[3].get_src(0).get_reg() != temp) {
    return nullptr;
  }

  //  vaddx.w vf4, vf0, vf0
  if (instrs[4].kind != InstructionKind::VADD_BC || instrs[4].get_dst(0).get_reg() != make_vf(4) ||
      instrs[4].get_src(0).get_reg() != make_vf(0) ||
      instrs[4].get_src(1).get_reg() != make_vf(0) || instrs[4].cop2_bc != 0 ||
      instrs[4].cop2_dest != 0b0001) {
    return nullptr;
  }

  //  vmulax.xyzw acc, vf2, vf3
  if (instrs[5].kind != InstructionKind::VMULA_BC || instrs[5].get_src(0).get_reg() != make_vf(2) ||
      instrs[5].get_src(1).get_reg() != make_vf(3) || instrs[5].cop2_dest != 0b1111 ||
      instrs[5].cop2_bc != 0) {
    return nullptr;
  }

  //  vmaddw.xyz vf4, vf1, vf0
  if (instrs[6].kind != InstructionKind::VMADD_BC || instrs[6].get_dst(0).get_reg() != make_vf(4) ||
      instrs[6].get_src(0).get_reg() != make_vf(1) ||
      instrs[6].get_src(1).get_reg() != make_vf(0) || instrs[6].cop2_dest != 0b1110 ||
      instrs[6].cop2_bc != 3) {
    return nullptr;
  }

  //  sqc2 vf4, 0(a0)
  if (instrs[7].kind != InstructionKind::SQC2 || instrs[7].get_src(0).get_reg() != make_vf(4) ||
      !instrs[7].get_src(1).is_imm(0)) {
    return nullptr;
  }
  Register dst = instrs[7].get_src(2).get_reg();

  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_PLUS_FLOAT_TIMES, make_src_atom(dst, idx),
                       make_src_atom(vec_src_1, idx), make_src_atom(vec_src_2, idx),
                       make_src_atom(flt_src_3, idx)),
      idx);
}

std::unique_ptr<AtomicOp> convert_8(const Instruction* instrs, int idx) {
  auto as_vector_float_plus_times = convert_vector_plus_float_times(instrs, idx);
  if (as_vector_float_plus_times) {
    return as_vector_float_plus_times;
  }
  return nullptr;
}

bool is_lwc(const Instruction& instr, int offset) {
  return instr.kind == InstructionKind::LWC1 && instr.get_src(0).is_imm(offset);
}

// 9 instructions
std::unique_ptr<AtomicOp> convert_vector3_dot(const Instruction* instrs, int idx) {
  //    lwc1 f0, 0(a0)
  if (!is_lwc(instrs[0], 0)) {
    return nullptr;
  }
  auto t0 = instrs[0].get_dst(0).get_reg();

  //    lwc1 f1, 4(a0)
  if (!is_lwc(instrs[1], 4)) {
    return nullptr;
  }
  auto t1 = instrs[1].get_dst(0).get_reg();

  //    lwc1 f2, 8(a0)
  if (!is_lwc(instrs[2], 8)) {
    return nullptr;
  }
  auto t2 = instrs[2].get_dst(0).get_reg();

  //    lwc1 f3, 0(v1)
  if (!is_lwc(instrs[3], 0)) {
    return nullptr;
  }
  auto t3 = instrs[3].get_dst(0).get_reg();

  //    lwc1 f4, 4(v1)
  if (!is_lwc(instrs[4], 4)) {
    return nullptr;
  }
  auto t4 = instrs[4].get_dst(0).get_reg();

  //    lwc1 f5, 8(v1)
  if (!is_lwc(instrs[5], 8)) {
    return nullptr;
  }
  auto t5 = instrs[5].get_dst(0).get_reg();

  auto src0 = instrs[0].get_src(1).get_reg();
  auto src1 = instrs[3].get_src(1).get_reg();
  if (instrs[1].get_src(1).get_reg() != src0) {
    return nullptr;
  }
  if (instrs[2].get_src(1).get_reg() != src0) {
    return nullptr;
  }
  if (instrs[4].get_src(1).get_reg() != src1) {
    return nullptr;
  }
  if (instrs[5].get_src(1).get_reg() != src1) {
    return nullptr;
  }

  //    mula.s f0, f3
  if (instrs[6].kind != InstructionKind::MULAS || instrs[6].get_src(0).get_reg() != t0 ||
      instrs[6].get_src(1).get_reg() != t3) {
    return nullptr;
  }

  //    madda.s f1, f4
  if (instrs[7].kind != InstructionKind::MADDAS || instrs[7].get_src(0).get_reg() != t1 ||
      instrs[7].get_src(1).get_reg() != t4) {
    return nullptr;
  }

  //    madd.s f0, f2, f5
  if (instrs[8].kind != InstructionKind::MADDS || instrs[8].get_src(0).get_reg() != t2 ||
      instrs[8].get_src(1).get_reg() != t5) {
    return nullptr;
  }

  auto dst = instrs[8].get_dst(0).get_reg();

  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_3_DOT, make_src_atom(src0, idx),
                       make_src_atom(src1, idx)),
      idx);
}

std::unique_ptr<AtomicOp> convert_9(const Instruction* instrs, int idx) {
  auto as_vector3_dot = convert_vector3_dot(instrs, idx);
  if (as_vector3_dot) {
    return as_vector3_dot;
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_vector_length(const Instruction* instrs, int idx) {
  // 0:  lqc2 vf1, 0(a1)
  if (instrs[0].kind != InstructionKind::LQC2 || instrs[0].get_dst(0).get_reg() != make_vf(1) ||
      !instrs[0].get_src(0).is_imm(0)) {
    return nullptr;
  }
  Register vec_src = instrs[0].get_src(1).get_reg();

  auto vf0 = make_vf(0);
  auto vf1 = make_vf(1);

  // 1: vmul.xyzw vf1, vf1, vf1
  std::vector<DecompilerLabel> labels;
  if (instrs[1].kind != InstructionKind::VMUL || instrs[1].get_dst(0).get_reg() != vf1 ||
      instrs[1].get_src(0).get_reg() != vf1 || instrs[1].get_src(1).get_reg() != vf1 ||
      instrs[1].cop2_dest != 0b1111) {
    return nullptr;
  }

  // 2:  vmulax.w acc, vf0, vf1
  if (instrs[2].kind != InstructionKind::VMULA_BC || instrs[2].get_src(0).get_reg() != vf0 ||
      instrs[2].get_src(1).get_reg() != vf1 || instrs[2].cop2_dest != 0b0001 ||
      instrs[2].cop2_bc != 0) {
    return nullptr;
  }

  // 3: vmadday.w acc, vf0, vf1
  if (instrs[3].kind != InstructionKind::VMADDA_BC || instrs[3].get_src(0).get_reg() != vf0 ||
      instrs[3].get_src(1).get_reg() != vf1 || instrs[3].cop2_dest != 0b0001 ||
      instrs[3].cop2_bc != 1) {
    return nullptr;
  }

  // 4: vmaddz.w vf1, vf0, vf1
  if (instrs[4].kind != InstructionKind::VMADD_BC || instrs[4].get_dst(0).get_reg() != vf1 ||
      instrs[4].get_src(0).get_reg() != vf0 || instrs[4].get_src(1).get_reg() != vf1 ||
      instrs[4].cop2_dest != 0b0001 || instrs[4].cop2_bc != 2) {
    return nullptr;
  }

  // 5: vsqrt Q, vf1.w
  if (instrs[5].kind != InstructionKind::VSQRT || instrs[5].get_src(0).get_reg() != vf1) {
    return nullptr;
  }

  // 6: vaddw.x vf1, vf0, vf0
  if (instrs[6].kind != InstructionKind::VADD_BC || instrs[6].get_dst(0).get_reg() != vf1 ||
      instrs[6].get_src(0).get_reg() != vf0 || instrs[6].get_src(1).get_reg() != vf0 ||
      instrs[6].cop2_dest != 0b1000 || instrs[6].cop2_bc != 3) {
    return nullptr;
  }

  // 7:  vwaitq
  if (instrs[7].kind != InstructionKind::VWAITQ) {
    return nullptr;
  }

  // 8: vmulq.x vf1, vf1, Q
  if (instrs[8].kind != InstructionKind::VMULQ || instrs[8].get_dst(0).get_reg() != vf1 ||
      instrs[8].get_src(0).get_reg() != vf1 || instrs[8].cop2_dest != 0b1000) {
    return nullptr;
  }

  // 9:  vnop
  if (instrs[9].kind != InstructionKind::VNOP) {
    return nullptr;
  }
  // 10:  vnop
  if (instrs[10].kind != InstructionKind::VNOP) {
    return nullptr;
  }
  // 11:  qmfc2.i a1, vf1
  if (instrs[11].kind != InstructionKind::QMFC2 || instrs[11].src->get_reg() != vf1) {
    return nullptr;
  }
  auto dst = instrs[11].get_dst(0).get_reg();
  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_LENGTH, make_src_atom(vec_src, idx)), idx);
}

// 12 instructions
std::unique_ptr<AtomicOp> convert_vector4_dot(const Instruction* instrs, int idx) {
  //    lwc1 f0, 0(a0)
  if (!is_lwc(instrs[0], 0)) {
    return nullptr;
  }
  auto t0 = instrs[0].get_dst(0).get_reg();

  //    lwc1 f1, 4(a0)
  if (!is_lwc(instrs[1], 4)) {
    return nullptr;
  }
  auto t1 = instrs[1].get_dst(0).get_reg();

  //    lwc1 f2, 8(a0)
  if (!is_lwc(instrs[2], 8)) {
    return nullptr;
  }
  auto t2 = instrs[2].get_dst(0).get_reg();

  //    lwc1 f3, 12(a0)
  if (!is_lwc(instrs[3], 12)) {
    return nullptr;
  }
  auto t3 = instrs[3].get_dst(0).get_reg();

  //    lwc1 f3, 0(v1)
  if (!is_lwc(instrs[4], 0)) {
    return nullptr;
  }
  auto t4 = instrs[4].get_dst(0).get_reg();

  //    lwc1 f5, 4(v1)
  if (!is_lwc(instrs[5], 4)) {
    return nullptr;
  }
  auto t5 = instrs[5].get_dst(0).get_reg();

  //    lwc1 f5, 8(v1)
  if (!is_lwc(instrs[6], 8)) {
    return nullptr;
  }
  auto t6 = instrs[6].get_dst(0).get_reg();

  //    lwc1 f5, 12(v1)
  if (!is_lwc(instrs[7], 12)) {
    return nullptr;
  }
  auto t7 = instrs[7].get_dst(0).get_reg();

  auto src0 = instrs[0].get_src(1).get_reg();
  auto src1 = instrs[4].get_src(1).get_reg();
  if (instrs[1].get_src(1).get_reg() != src0) {
    return nullptr;
  }
  if (instrs[2].get_src(1).get_reg() != src0) {
    return nullptr;
  }
  if (instrs[3].get_src(1).get_reg() != src0) {
    return nullptr;
  }
  if (instrs[5].get_src(1).get_reg() != src1) {
    return nullptr;
  }
  if (instrs[6].get_src(1).get_reg() != src1) {
    return nullptr;
  }
  if (instrs[7].get_src(1).get_reg() != src1) {
    return nullptr;
  }

  //    mula.s f0, f4
  if (instrs[8].kind != InstructionKind::MULAS || instrs[8].get_src(0).get_reg() != t0 ||
      instrs[8].get_src(1).get_reg() != t4) {
    return nullptr;
  }

  //    madda.s f1, f5
  if (instrs[9].kind != InstructionKind::MADDAS || instrs[9].get_src(0).get_reg() != t1 ||
      instrs[9].get_src(1).get_reg() != t5) {
    return nullptr;
  }

  //    madda.s f2, f6
  if (instrs[10].kind != InstructionKind::MADDAS || instrs[10].get_src(0).get_reg() != t2 ||
      instrs[10].get_src(1).get_reg() != t6) {
    return nullptr;
  }

  //    madd.s f0, f3, f7
  if (instrs[11].kind != InstructionKind::MADDS || instrs[11].get_src(0).get_reg() != t3 ||
      instrs[11].get_src(1).get_reg() != t7) {
    return nullptr;
  }

  auto dst = instrs[11].get_dst(0).get_reg();

  return std::make_unique<SetVarOp>(
      make_dst_var(dst, idx),
      SimpleExpression(SimpleExpression::Kind::VECTOR_4_DOT, make_src_atom(src0, idx),
                       make_src_atom(src1, idx)),
      idx);
}

std::unique_ptr<AtomicOp> convert_12(const Instruction* instrs, int idx) {
  auto as_vector4_dot = convert_vector4_dot(instrs, idx);
  if (as_vector4_dot) {
    return as_vector4_dot;
  }

  auto as_vector_length = convert_vector_length(instrs, idx);
  if (as_vector_length) {
    return as_vector_length;
  }
  return nullptr;
}

}  // namespace

/*!
 * Convert an entire basic block and add the results to a FunctionAtomicOps
 * @param block_id  : the index of the block
 * @param begin     : the start of the instructions for the block
 * @param end       : the end of the instructions for the block
 * @param container : the container to add to
 */
int convert_block_to_atomic_ops(int begin_idx,
                                std::vector<Instruction>::const_iterator begin,
                                std::vector<Instruction>::const_iterator end,
                                const std::vector<DecompilerLabel>& labels,
                                FunctionAtomicOps* container,
                                DecompWarnings& warnings,
                                GameVersion version,
                                bool hint_inline_asm,
                                bool block_ends_in_asm_branch) {
  container->block_id_to_first_atomic_op.push_back(container->ops.size());
  for (auto& instr = begin; instr < end;) {
    // how many instructions can we look at, at most?
    int n_instr = end - instr;
    // how many instructions did we use?
    int length = 0;
    // what is the index of the atomic op we would add
    int op_idx = int(container->ops.size());

    bool converted = false;
    std::unique_ptr<AtomicOp> op;

    if (instr[0].kind == InstructionKind::SQ || instr[0].kind == InstructionKind::LQ) {
      warnings.unique_info("Used lq/sq");
    }

    if (!converted && n_instr >= 12) {
      op = convert_12(&instr[0], op_idx);
      if (op) {
        converted = true;
        length = 12;
      }
    }

    if (!converted && n_instr >= 9) {
      op = convert_9(&instr[0], op_idx);
      if (op) {
        converted = true;
        length = 9;
      }
    }

    if (!converted && n_instr >= 8) {
      op = convert_8(&instr[0], op_idx);
      if (op) {
        converted = true;
        length = 8;
      }
    }

    if (!converted && n_instr >= 6) {
      // try 6 instructions
      op = convert_6(instr[0], instr[1], instr[2], instr[3], instr[4], instr[5], op_idx);
      if (op) {
        converted = true;
        length = 6;
      }
    }

    if (!converted && n_instr >= 5) {
      // try 5 instructions
      op = convert_5(instr[0], instr[1], instr[2], instr[3], instr[4], op_idx, version);
      if (op) {
        converted = true;
        length = 5;
      }
    }

    if (!converted && n_instr >= 4) {
      // try 4 instructions
      op = convert_4(instr[0], instr[1], instr[2], instr[3], op_idx, version);
      if (op) {
        converted = true;
        length = 4;
      }
    }

    if (!converted && n_instr >= 3) {
      // try 3 instructions
      op = convert_3(instr[0], instr[1], instr[2], op_idx, version);
      if (op) {
        converted = true;
        length = 3;
      }
    }

    if (!converted && n_instr >= 2) {
      // try 2 instructions
      op = convert_2(instr[0], instr[1], op_idx, block_ends_in_asm_branch, version);
      if (op) {
        converted = true;
        length = 2;
      }
    }

    if (!converted) {
      // try 1 instruction
      bool force_asm_branch = n_instr == 1 && block_ends_in_asm_branch;
      op = convert_1(*instr, op_idx, hint_inline_asm, force_asm_branch, version);
      if (op) {
        converted = true;
        length = 1;
      }
    }

    if (!converted) {
      // try assembly fallback.
      op = make_asm_op(*instr, op_idx);
      if (op) {
        converted = true;
        length = 1;
      }
    }

    if (!converted) {
      // failed!
      throw std::runtime_error(
          fmt::format("Failed to convert ({} instrs) {}\n", n_instr, instr->to_string(labels)));
      //      lg::die("Failed to convert instruction {} to an atomic op",
      //      instr->to_string(labels));
    }

    ASSERT(converted && length && op);
    // add mappings:
    container->atomic_op_to_instruction[container->ops.size()] = begin_idx;
    for (int i = 0; i < length; i++) {
      container->instruction_to_atomic_op[begin_idx + i] = container->ops.size();
    }
    // add
    op->update_register_info();
    container->ops.emplace_back(std::move(op));
    instr += length;
    begin_idx += length;
  }
  container->block_id_to_end_atomic_op.push_back(container->ops.size());
  return int(container->ops.size());
}

FunctionAtomicOps convert_function_to_atomic_ops(
    const Function& func,
    const std::vector<DecompilerLabel>& labels,
    DecompWarnings& warnings,
    bool hint_inline_asm,
    const std::unordered_set<int>& blocks_ending_in_asm_branches,
    GameVersion version) {
  FunctionAtomicOps result;

  int last_op = 0;
  for (int i = 0; i < int(func.basic_blocks.size()); i++) {
    const auto& block = func.basic_blocks.at(i);
    // we should only consider the blocks which actually have instructions:
    if (block.end_word > block.start_word) {
      auto begin = func.instructions.begin() + block.start_word;
      auto end = func.instructions.begin() + block.end_word;
      last_op = convert_block_to_atomic_ops(
          block.start_word, begin, end, labels, &result, warnings, version, hint_inline_asm,
          blocks_ending_in_asm_branches.find(i) != blocks_ending_in_asm_branches.end());
      if (i == int(func.basic_blocks.size()) - 1) {
        // we're the last block. insert the function end op.
        result.ops.push_back(std::make_unique<FunctionEndOp>(int(result.ops.size())));
        result.ops.back()->update_register_info();
        // add to block.
        result.block_id_to_end_atomic_op.back()++;
      }
    } else {
      if (i == int(func.basic_blocks.size()) - 1) {
        // we're the last block. insert the function end op.
        result.ops.push_back(std::make_unique<FunctionEndOp>(int(result.ops.size())));
        result.ops.back()->update_register_info();
        // add block (no longer a zero-size block)
        result.block_id_to_first_atomic_op.push_back(last_op);
        result.block_id_to_end_atomic_op.push_back(last_op + 1);
      } else {
        result.block_id_to_first_atomic_op.push_back(last_op);
        result.block_id_to_end_atomic_op.push_back(last_op);
      }
    }
  }

  ASSERT(func.basic_blocks.size() == result.block_id_to_end_atomic_op.size());
  ASSERT(func.basic_blocks.size() == result.block_id_to_first_atomic_op.size());
  return result;
}
}  // namespace decompiler
