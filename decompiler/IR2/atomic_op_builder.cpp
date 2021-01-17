#include "atomic_op_builder.h"

#include <memory>
#include "common/log/log.h"
#include "common/symbols.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Disasm/InstructionMatching.h"

namespace decompiler {

namespace {

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

/////////////////////////
// Variable Helpers
/////////////////////////

Variable make_dst_var(Register reg, int idx) {
  return Variable(VariableMode::WRITE, reg, idx);
}

Variable make_src_var(Register reg, int idx) {
  return Variable(VariableMode::READ, reg, idx);
}

Variable make_dst_var(const Instruction& i, int idx) {
  assert(i.n_dst == 1);
  return make_dst_var(i.get_dst(0).get_reg(), idx);
}

////////////////////////
// Atom Helpers
////////////////////////

SimpleAtom make_src_atom(Register reg, int idx) {
  return SimpleAtom::make_var(make_src_var(reg, idx));
}

SimpleAtom false_sym() {
  return SimpleAtom::make_sym_ptr("#f");
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
  if (i0.get_dst(0).is_reg(rra())) {
    return std::make_unique<AsmOp>(i0, idx);
  }
  auto dst = make_dst_var(i0, idx);
  SimpleExpression src;
  if (i0.get_src(0).is_label() && i0.get_src(1).is_reg(rfp())) {
    // it's an FP relative load.
    src = SimpleAtom::make_static_address(i0.get_src(0).get_label()).as_expr();
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
                                              bool is_float) {
  SimpleAtom val;
  SimpleExpression dst;
  if (i0.get_src(0).is_reg(rs7())) {
    assert(!is_float);
    val = SimpleAtom::make_sym_ptr("#f");
  } else if (i0.get_src(0).is_reg(rr0())) {
    assert(!is_float);
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

  return std::make_unique<StoreOp>(store_size, is_float, dst, val, idx);
}

std::unique_ptr<AtomicOp> make_asm_op(const Instruction& i0, int idx) {
  switch (i0.kind) {
    case InstructionKind::POR:
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

      // VU/COP2
    case InstructionKind::VMOVE:
    case InstructionKind::VFTOI0:
    case InstructionKind::VFTOI4:
    case InstructionKind::VFTOI12:
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
    case InstructionKind::ADDAS:

      // Moves / Loads / Stores
    case InstructionKind::CTC2:
    case InstructionKind::CFC2:
    case InstructionKind::SQC2:
    case InstructionKind::LQC2:
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
    case InstructionKind::PCGTW:
    case InstructionKind::PEXTLH:
    case InstructionKind::PEXTLB:
    case InstructionKind::PMAXH:
    case InstructionKind::PPACB:
    case InstructionKind::PADDW:
    case InstructionKind::PADDH:
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
    case InstructionKind::PMADDH:
    case InstructionKind::PMULTH:
    case InstructionKind::PEXEW:
    case InstructionKind::PNOR:
    case InstructionKind::PCPYH:
    case InstructionKind::PINTEH:

      return std::make_unique<AsmOp>(i0, idx);
    default:
      return nullptr;
  }
}

////////////////////////
// Branch Helpers
////////////////////////

IR2_BranchDelay get_branch_delay(const Instruction& i0, int idx) {
  if (is_nop(i0)) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::NOP);
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, rs7(), rr0())) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::SET_REG_FALSE, make_dst_var(i0, idx));
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, {}, rr0())) {
    return IR2_BranchDelay(IR2_BranchDelay::Kind::SET_REG_REG, make_dst_var(i0, idx),
                           make_src_var(i0.get_src(0).get_reg(), idx));
  } else if (i0.kind == InstructionKind::DADDIU && i0.get_src(0).is_reg(rs7()) &&
             i0.get_src(1).is_imm(FIX_SYM_TRUE)) {
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
                                      int my_idx) {
  auto branch_delay = get_branch_delay(delay, my_idx);
  if (branch_delay.is_known()) {
    return std::make_unique<BranchOp>(likely, condition, dest_label, branch_delay, my_idx);
  } else {
    return nullptr;
  }
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
  if (i0.get_dst(0).is_reg(rra()) || i0.get_dst(0).is_reg(make_gpr(Reg::AT))) {
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

std::unique_ptr<AtomicOp> convert_daddiu_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_sym()) {
    // get symbol pointer
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_sym_ptr(i0.get_src(1).get_sym()).as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(FIX_SYM_EMPTY_PAIR)) {
    // get empty pair
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleAtom::make_empty_list().as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(-32768)) {
    // get pointer to beginning of symbol table (this is a bit of a hack)
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_sym_val("__START-OF-TABLE__").as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(FIX_SYM_TRUE)) {
    // get pointer to beginning of symbol table (this is a bit of a hack)
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
      val = SimpleAtom::make_sym_ptr("#f");
    } else if (i0.get_src(0).is_reg(rr0())) {
      // store a 0
      val = SimpleAtom::make_int_constant(0);
    } else {
      // store a register.
      val = make_src_atom(i0.get_src(0).get_reg(), idx);
    }
    return std::make_unique<StoreOp>(4, false, SimpleAtom::make_sym_val(name).as_expr(), val, idx);
  } else {
    return make_standard_store(i0, idx, 4, false);
  }
}

std::unique_ptr<AtomicOp> convert_sd_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rr0()) && i0.get_src(1).is_imm(2) && i0.get_src(2).is_reg(rr0())) {
    return std::make_unique<SpecialOp>(SpecialOp::Kind::CRASH, idx);
  } else {
    return make_standard_store(i0, idx, 8, false);
  }
}

// movn or movz
std::unique_ptr<AtomicOp> convert_cmov_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rs7())) {
    return std::make_unique<ConditionalMoveFalseOp>(make_dst_var(i0, idx),
                                                    make_src_var(i0.get_src(1).get_reg(), idx),
                                                    i0.kind == InstructionKind::MOVZ, idx);
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

std::unique_ptr<AtomicOp> convert_1(const Instruction& i0, int idx) {
  switch (i0.kind) {
    case InstructionKind::OR:
      return convert_or_1(i0, idx);
    case InstructionKind::ORI:
      return convert_ori_1(i0, idx);
    case InstructionKind::AND:
      return make_3reg_op(i0, SimpleExpression::Kind::AND, idx);
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
      return convert_daddiu_1(i0, idx);
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
      return make_standard_store(i0, idx, 1, false);
    case InstructionKind::SH:
      return make_standard_store(i0, idx, 2, false);
    case InstructionKind::SW:
      return convert_sw_1(i0, idx);
    case InstructionKind::SD:
      return convert_sd_1(i0, idx);
    case InstructionKind::SWC1:
      return make_standard_store(i0, idx, 4, true);
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
    default:
      return nullptr;
  }
}

///////////////////////
// OP 2 Conversions
//////////////////////

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
                                        bool likely) {
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
                              SimpleAtom::make_sym_ptr("#f"));
  } else {
    condition = IR2_Condition(IR2_Condition::Kind::NOT_EQUAL, make_src_atom(s0, idx),
                              make_src_atom(s1, idx));
  }
  return make_branch(condition, i1, likely, dest, idx);
}

std::unique_ptr<AtomicOp> convert_beq_2(const Instruction& i0,
                                        const Instruction& i1,
                                        int idx,
                                        bool likely) {
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
      condition = IR2_Condition(IR2_Condition::Kind::FALSE, SimpleAtom::make_sym_ptr("#f"));
    } else {
      condition = IR2_Condition(IR2_Condition::Kind::FALSE, make_src_atom(s1, idx));
    }
  } else if (s1 == rs7()) {
    // likely a case where somebody wrote (= x #f) or (!= x #f). much rarer than the flipped one
    condition = IR2_Condition(IR2_Condition::Kind::EQUAL, make_src_atom(s0, idx),
                              SimpleAtom::make_sym_ptr("#f"));
  } else {
    condition =
        IR2_Condition(IR2_Condition::Kind::EQUAL, make_src_atom(s0, idx), make_src_atom(s1, idx));
  }
  return make_branch(condition, i1, likely, dest, idx);
}

std::unique_ptr<AtomicOp> convert_branch_r1_2(const Instruction& i0,
                                              const Instruction& i1,
                                              IR2_Condition::Kind kind,
                                              bool likely,
                                              int idx) {
  return make_branch(IR2_Condition(kind, make_src_atom(i0.get_src(0).get_reg(), idx)), i1, likely,
                     i0.get_src(1).get_label(), idx);
}

std::unique_ptr<AtomicOp> convert_daddiu_2(const Instruction& i0, const Instruction& i1, int idx) {
  // daddiu dest, s7, 8
  // mov{n,z} dest, s7, src
  if (i1.kind == InstructionKind::MOVN || i1.kind == InstructionKind::MOVZ) {
    auto dest = i0.get_dst(0).get_reg();
    auto src = i1.get_src(1).get_reg();
    if (!i0.get_src(0).is_reg(rs7())) {
      return nullptr;
    }
    assert(i0.get_src(0).is_reg(rs7()));
    assert(i0.get_src(1).is_imm(8));
    assert(i1.get_dst(0).is_reg(dest));
    assert(i1.get_src(0).is_reg(rs7()));
    auto kind =
        i1.kind == InstructionKind::MOVN ? IR2_Condition::Kind::ZERO : IR2_Condition::Kind::NONZERO;
    return std::make_unique<SetVarConditionOp>(make_dst_var(dest, idx),
                                               IR2_Condition(kind, make_src_atom(src, idx)), idx);
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
      assert(label == i1.get_src(1).get_label());
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
  assert(temp != left);
  assert(temp != right);
  assert(left != right);
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

std::unique_ptr<AtomicOp> convert_2(const Instruction& i0, const Instruction& i1, int idx) {
  switch (i0.kind) {
    case InstructionKind::DIV:
      return convert_division_2(i0, i1, idx, true);
    case InstructionKind::DIVU:
      return convert_division_2(i0, i1, idx, false);
    case InstructionKind::JALR:
      return convert_jalr_2(i0, i1, idx);
    case InstructionKind::BNE:
      return convert_bne_2(i0, i1, idx, false);
    case InstructionKind::BNEL:
      return convert_bne_2(i0, i1, idx, true);
    case InstructionKind::BEQ:
      return convert_beq_2(i0, i1, idx, false);
    case InstructionKind::BEQL:
      return convert_beq_2(i0, i1, idx, true);
    case InstructionKind::BGTZL:
      return convert_branch_r1_2(i0, i1, IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED, true, idx);
    case InstructionKind::BGEZL:
      return convert_branch_r1_2(i0, i1, IR2_Condition::Kind::GEQ_ZERO_SIGNED, true, idx);
    case InstructionKind::BLTZL:
      return convert_branch_r1_2(i0, i1, IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED, true, idx);
    case InstructionKind::DADDIU:
      return convert_daddiu_2(i0, i1, idx);
    case InstructionKind::LUI:
      return convert_lui_2(i0, i1, idx);
    case InstructionKind::SLT:
      return convert_slt_2(i0, i1, idx, true);
    case InstructionKind::SLTU:
      return convert_slt_2(i0, i1, idx, false);
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
    assert(i0.get_dst(0).get_reg() == i1.get_src(0).get_reg());      // temp
    assert(i0.get_src(0).get_label() == i1.get_src(1).get_label());  // labels
    assert(i2.get_dst(0).get_reg() == i2.get_src(1).get_reg());      // dst
    assert(i2.get_dst(0).get_reg() == i1.get_dst(0).get_reg());      // dst
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
    assert(i0.get_dst(0).get_reg() == i1.get_src(0).get_reg());      // temp
    assert(i0.get_src(0).get_label() == i1.get_src(1).get_label());  // labels
    assert(i0.get_dst(0).get_reg() == i1.get_dst(0).get_reg());      // temp
    assert(i2.get_src(0).get_reg() == i0.get_dst(0).get_reg());      // temp
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
                                          int idx) {
  if (i1.kind == InstructionKind::DADDIU &&
      (i2.kind == InstructionKind::MOVN || i2.kind == InstructionKind::MOVZ)) {
    // dsubu temp, a, b
    // daddiu dst, s7, 8
    // mov{n,z} dst, s7, temp
    auto temp = i0.get_dst(0).get_reg();
    auto a = i0.get_src(0).get_reg();
    auto b = i0.get_src(1).get_reg();
    auto dest = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(rs7()));
    assert(i1.get_src(1).is_imm(FIX_SYM_TRUE));
    assert(i2.get_dst(0).get_reg() == dest);
    assert(i2.get_src(0).is_reg(rs7()));
    assert(i2.get_src(1).get_reg() == temp);
    assert(temp != dest);
    auto kind = i2.kind == InstructionKind::MOVN ? IR2_Condition::Kind::EQUAL
                                                 : IR2_Condition::Kind::NOT_EQUAL;
    std::unique_ptr<AtomicOp> result;
    if (b == rs7()) {
      // some sort of not gone wrong?
      result = std::make_unique<SetVarConditionOp>(
          make_dst_var(dest, idx),
          IR2_Condition(kind, make_src_atom(a, idx), SimpleAtom::make_sym_ptr("#f")), idx);
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
                                        int idx) {
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
    assert(i1.get_src(0).get_reg() == temp);
    assert(i1.get_src(1).is_reg(rr0()));

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
    result = make_branch(condition, i2, false, dest, idx);
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
    assert(i1.get_src(0).is_reg(rs7()));
    assert(i1.get_src(1).is_imm(FIX_SYM_TRUE));
    assert(i2.get_dst(0).get_reg() == dest);
    assert(i2.get_src(0).is_reg(rs7()));
    assert(i2.get_src(1).get_reg() == temp);
    assert(temp != dest);
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
                                         int idx) {
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
    assert(i1.get_src(0).get_reg() == temp);
    assert(i1.get_src(1).is_reg(rr0()));
    auto kind =
        is_signed ? IR2_Condition::Kind::LESS_THAN_SIGNED : IR2_Condition::Kind::LESS_THAN_UNSIGNED;
    auto condition = IR2_Condition(kind, make_src_atom(s0, idx), s1);
    if (i1.kind == InstructionKind::BEQ) {
      condition.invert();
    }
    result = make_branch(condition, i2, false, dest, idx);
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
    assert(i1.get_src(0).is_reg(rs7()));
    assert(i1.get_src(1).is_imm(FIX_SYM_TRUE));
    assert(i2.get_dst(0).get_reg() == dest);
    assert(i2.get_src(0).is_reg(rs7()));
    assert(i2.get_src(1).get_reg() == temp);
    assert(temp != dest);
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
                                            int idx) {
  if (i1.kind == InstructionKind::BC1T || i1.kind == InstructionKind::BC1F) {
    IR2_Condition condition(kind, make_src_atom(i0.get_src(0).get_reg(), idx),
                            make_src_atom(i0.get_src(1).get_reg(), idx));
    if (i1.kind == InstructionKind::BC1F) {
      condition.invert();
    }
    return make_branch(condition, i2, false, i1.get_src(0).get_label(), idx);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_3(const Instruction& i0,
                                    const Instruction& i1,
                                    const Instruction& i2,
                                    int idx) {
  switch (i0.kind) {
    case InstructionKind::LUI:
      return convert_lui_3(i0, i1, i2, idx);
    case InstructionKind::DSUBU:
      return convert_dsubu_3(i0, i1, i2, idx);
    case InstructionKind::SLT:
      return convert_slt_3(i0, i1, i2, true, idx);
    case InstructionKind::SLTU:
      return convert_slt_3(i0, i1, i2, false, idx);
    case InstructionKind::SLTI:
      return convert_slti_3(i0, i1, i2, true, idx);
    case InstructionKind::SLTIU:
      return convert_slti_3(i0, i1, i2, false, idx);
    case InstructionKind::CEQS:
      return convert_fp_branch(i0, i1, i2, IR2_Condition::Kind::FLOAT_EQUAL, idx);
    case InstructionKind::CLTS:
      return convert_fp_branch(i0, i1, i2, IR2_Condition::Kind::FLOAT_LESS_THAN, idx);
    case InstructionKind::CLES:
      return convert_fp_branch(i0, i1, i2, IR2_Condition::Kind::FLOAT_LEQ, idx);
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
                                           int idx) {
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
    assert(i1.get_dst(0).get_reg() == temp);
    assert(i1.get_src(0).get_reg() == temp);
    assert(i1.get_src(1).is_reg(rr0()));
    assert(i2.get_src(0).get_reg() == temp);
    assert(i2.get_src(1).is_reg(rr0()));

    IR2_Condition condition(IR2_Condition::Kind::IS_NOT_PAIR, make_src_atom(arg, idx));
    auto result = make_branch(condition, i3, false, i2.get_src(2).get_label(), idx);
    result->add_clobber_reg(temp);
    return result;
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_4(const Instruction& i0,
                                    const Instruction& i1,
                                    const Instruction& i2,
                                    const Instruction& i3,
                                    int idx) {
  switch (i0.kind) {
    case InstructionKind::DSLL32:
      return convert_dsll32_4(i0, i1, i2, i3, idx);
    default:
      return nullptr;
  }
}

///////////////////////
// OP 5 Conversions
//////////////////////

std::unique_ptr<AtomicOp> convert_5(const Instruction& i0,
                                    const Instruction& i1,
                                    const Instruction& i2,
                                    const Instruction& i3,
                                    const Instruction& i4,
                                    int idx) {
  auto s6 = make_gpr(Reg::S6);

  if (i0.kind == InstructionKind::LWU && i0.get_dst(0).is_reg(s6) &&
      i0.get_src(0).get_imm() == 44 && i0.get_src(1).is_reg(s6) &&
      i1.kind == InstructionKind::MTLO1 && i1.get_src(0).is_reg(s6) &&
      i2.kind == InstructionKind::LWU && i2.get_dst(0).is_reg(s6) &&
      i2.get_src(0).get_imm() == 12 && i2.get_src(1).is_reg(s6) &&
      i3.kind == InstructionKind::JALR && i3.get_dst(0).is_reg(make_gpr(Reg::RA)) &&
      i3.get_src(0).is_reg(s6) && i4.kind == InstructionKind::MFLO1 && i4.get_dst(0).is_reg(s6)) {
    return std::make_unique<SpecialOp>(SpecialOp::Kind::SUSPEND, idx);
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
                                FunctionAtomicOps* container) {
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

    if (n_instr >= 5) {
      // try 5 instructions
      op = convert_5(instr[0], instr[1], instr[2], instr[3], instr[4], op_idx);
      if (op) {
        converted = true;
        length = 5;
      }
    }

    if (!converted && n_instr >= 4) {
      // try 4 instructions
      op = convert_4(instr[0], instr[1], instr[2], instr[3], op_idx);
      if (op) {
        converted = true;
        length = 4;
      }
    }

    if (!converted && n_instr >= 3) {
      // try 3 instructions
      op = convert_3(instr[0], instr[1], instr[2], op_idx);
      if (op) {
        converted = true;
        length = 3;
      }
    }

    if (!converted && n_instr >= 2) {
      // try 2 instructions
      op = convert_2(instr[0], instr[1], op_idx);
      if (op) {
        converted = true;
        length = 2;
      }
    }

    if (!converted) {
      // try 1 instruction
      op = convert_1(*instr, op_idx);
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
      throw std::runtime_error("Failed to convert " + instr->to_string(labels));
      //      lg::die("Failed to convert instruction {} to an atomic op",
      //      instr->to_string(labels));
    }

    assert(converted && length && op);
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

FunctionAtomicOps convert_function_to_atomic_ops(const Function& func,
                                                 const std::vector<DecompilerLabel>& labels) {
  FunctionAtomicOps result;

  int last_op = 0;
  for (const auto& block : func.basic_blocks) {
    // we should only consider the blocks which actually have instructions:
    if (block.end_word > block.start_word) {
      auto begin = func.instructions.begin() + block.start_word;
      auto end = func.instructions.begin() + block.end_word;
      last_op = convert_block_to_atomic_ops(block.start_word, begin, end, labels, &result);
    } else {
      result.block_id_to_first_atomic_op.push_back(last_op);
      result.block_id_to_end_atomic_op.push_back(last_op);
    }
  }

  assert(func.basic_blocks.size() == result.block_id_to_end_atomic_op.size());
  assert(func.basic_blocks.size() == result.block_id_to_first_atomic_op.size());
  return result;
}
}  // namespace decompiler