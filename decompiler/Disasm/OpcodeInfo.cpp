/*!
 * @file OpcodeInfo.cpp
 * Decoding info for each opcode.
 */

#include "OpcodeInfo.h"

#include "common/util/Assert.h"

namespace decompiler {
OpcodeInfo gOpcodeInfo[(uint32_t)InstructionKind::EE_OP_MAX];

namespace {
bool opcodes_initialized = false;
}

typedef InstructionKind IK;
typedef FieldType FT;
typedef DecodeType DT;

static OpcodeInfo& def(IK k, const char* name) {
  gOpcodeInfo[(uint32_t)k].defined = true;
  gOpcodeInfo[(uint32_t)k].name = name;
  return gOpcodeInfo[(uint32_t)k];
}

static OpcodeInfo& def_branch(IK k, const char* name) {
  auto& result = def(k, name);
  result.is_branch = true;
  result.has_delay_slot = true;
  return result;
}

static OpcodeInfo& def_branch_likely(IK k, const char* name) {
  auto& result = def(k, name);
  result.is_branch = true;
  result.is_branch_likely = true;
  result.has_delay_slot = true;
  return result;
}

static OpcodeInfo& def_store(IK k, const char* name) {
  auto& result = def(k, name);
  result.is_store = true;
  return result;
}

static OpcodeInfo& def_load(IK k, const char* name) {
  auto& result = def(k, name);
  result.is_load = true;
  return result;
}

static OpcodeInfo& drt_srs_ssimm16(OpcodeInfo& info) {
  return info.dst_gpr(FT::RT).src_gpr(FT::RS).src(FT::SIMM16, DT::IMM);
}

static OpcodeInfo& srt_ssimm16_srs(OpcodeInfo& info) {
  return info.src_gpr(FT::RT).src(FT::SIMM16, DT::IMM).src_gpr(FT::RS);
}

static OpcodeInfo& drt_ssimm16_srs(OpcodeInfo& info) {
  return info.dst_gpr(FT::RT).src(FT::SIMM16, DT::IMM).src_gpr(FT::RS);
}

static OpcodeInfo& drd_srs_srt(OpcodeInfo& info) {
  return info.dst_gpr(FT::RD).src_gpr(FT::RS).src_gpr(FT::RT);
}

static OpcodeInfo& drd_srt_srs(OpcodeInfo& info) {
  return info.dst_gpr(FT::RD).src_gpr(FT::RT).src_gpr(FT::RS);
}

static OpcodeInfo& drd_srt_ssa(OpcodeInfo& info) {
  return info.dst_gpr(FT::RD).src_gpr(FT::RT).src(FT::SA, DT::IMM);
}

static OpcodeInfo& srs_srt_bt(OpcodeInfo& info) {
  return info.src_gpr(FT::RS).src_gpr(FT::RT).src(FT::SIMM16, DT::BRANCH_TARGET);
}

static OpcodeInfo& srs_bt(OpcodeInfo& info) {
  return info.src_gpr(FT::RS).src(FT::SIMM16, DT::BRANCH_TARGET);
}

static OpcodeInfo& bt(OpcodeInfo& info) {
  return info.src(FT::SIMM16, DT::BRANCH_TARGET);
}

static OpcodeInfo& dfd_sfs_sft(OpcodeInfo& info) {
  return info.dst_fpr(FT::FD).src_fpr(FT::FS).src_fpr(FT::FT);
}

static OpcodeInfo& sfs_sft(OpcodeInfo& info) {
  return info.src_fpr(FT::FS).src_fpr(FT::FT);
}

static OpcodeInfo& dfd_sfs(OpcodeInfo& info) {
  return info.dst_fpr(FT::FD).src_fpr(FT::FS);
}

static OpcodeInfo& dfd_sft(OpcodeInfo& info) {
  return info.dst_fpr(FT::FD).src_fpr(FT::FT);
}

static OpcodeInfo& drd(OpcodeInfo& info) {
  return info.dst_gpr(FT::RD);
}

static OpcodeInfo& cd_dvft_svfs(OpcodeInfo& info) {
  return info.src(FT::DEST, DT::DEST).dst_vf(FT::FT).src_vf(FT::FS);
}

static OpcodeInfo& cd_dvfd_svfs_svft(OpcodeInfo& info) {
  return info.src(FT::DEST, DT::DEST).dst_vf(FT::FD).src_vf(FT::FS).src_vf(FT::FT);
}

static OpcodeInfo& cb_cd_dvfd_svfs_svft(OpcodeInfo& info) {
  return info.src(FT::BC, DT::BC)
      .src(FT::DEST, DT::DEST)
      .dst_vf(FT::FD)
      .src_vf(FT::FS)
      .src_vf(FT::FT);
}

static OpcodeInfo& cb_cd_dacc_svfs_svft(OpcodeInfo& info) {
  return info.src(FT::BC, DT::BC)
      .src(FT::DEST, DT::DEST)
      .dst(FT::ZERO, DT::VU_ACC)
      .src_vf(FT::FS)
      .src_vf(FT::FT);
}

static OpcodeInfo& cd_dvfd_svfs_sq(OpcodeInfo& info) {
  return info.src(FT::DEST, DT::DEST).dst_vf(FT::FD).src_vf(FT::FS).src(FT::ZERO, DT::VU_Q);
}

static OpcodeInfo& cd_dacc_svfs_svft(OpcodeInfo& info) {
  return info.src(FT::DEST, DT::DEST).dst(FT::ZERO, DT::VU_ACC).src_vf(FT::FS).src_vf(FT::FT);
}

void init_opcode_info() {
  if (opcodes_initialized) {
    return;
  }
  gOpcodeInfo[0].name = ";; ??????";

  // RT, RS, SIMM
  drt_srs_ssimm16(def(IK::DADDIU, "daddiu"));  // Doubleword Add Immediate Unsigned
  drt_srs_ssimm16(def(IK::ADDIU, "addiu"));    // Add Immediate Unsigned Word
  drt_srs_ssimm16(def(IK::SLTI, "slti"));      // Set on Less Than Immediate
  drt_srs_ssimm16(def(IK::SLTIU, "sltiu"));    // Set on Less Than Immediate Unsigned

  // stores in srt_ssimm16_srs
  srt_ssimm16_srs(def_store(IK::SB, "sb"));  // Store Byte
  srt_ssimm16_srs(def_store(IK::SH, "sh"));  // Store Halfword
  srt_ssimm16_srs(def_store(IK::SW, "sw"));  // Store Word
  srt_ssimm16_srs(def_store(IK::SD, "sd"));  // Store Doubleword
  srt_ssimm16_srs(def_store(IK::SQ, "sq"));  // Store Quadword

  // loads in dsrt_ssimm16_srs
  drt_ssimm16_srs(def_load(IK::LB, "lb"));    // Load Byte
  drt_ssimm16_srs(def_load(IK::LBU, "lbu"));  // Load Byte Unsigned
  drt_ssimm16_srs(def_load(IK::LH, "lh"));    // Load Halfword
  drt_ssimm16_srs(def_load(IK::LHU, "lhu"));  // Load Halfword Unsigned
  drt_ssimm16_srs(def_load(IK::LW, "lw"));    // Load Word
  drt_ssimm16_srs(def_load(IK::LWU, "lwu"));  // Load Word Unsigned
  drt_ssimm16_srs(def_load(IK::LD, "ld"));    // Load Doubleword
  drt_ssimm16_srs(def_load(IK::LQ, "lq"));    // Load Quadword
  drt_ssimm16_srs(def_load(IK::LDR, "ldr"));  // Load Doubleword Left
  drt_ssimm16_srs(def_load(IK::LDL, "ldl"));  // Load Doubleword Right
  drt_ssimm16_srs(def_load(IK::LWL, "lwl"));  // Load Word Left
  drt_ssimm16_srs(def_load(IK::LWR, "lwr"));  // Load Word Right

  // drd_srs_srt
  drd_srs_srt(def(IK::DADDU, "daddu"));    // Doubleword Add Unsigned
  drd_srs_srt(def(IK::SUBU, "subu"));      // Subtract Unsigned Word
  drd_srs_srt(def(IK::ADDU, "addu"));      // Add Unsigned Word
  drd_srs_srt(def(IK::DSUBU, "dsubu"));    // Doubleword Subtract Unsigned
  drd_srs_srt(def(IK::MULT3, "mult3"));    // Multiply Word
  drd_srs_srt(def(IK::MULTU3, "multu3"));  // Multiply Unsigned Word
  drd_srs_srt(def(IK::AND, "and"));        // And
  drd_srs_srt(def(IK::OR, "or"));          // Or
  drd_srs_srt(def(IK::NOR, "nor"));        // Not Or
  drd_srs_srt(def(IK::XOR, "xor"));        // Exclusive Or
  drd_srs_srt(def(IK::MOVN, "movn"));      // Move Conditional on Not Zero
  drd_srs_srt(def(IK::MOVZ, "movz"));      // Move Conditional on Zero
  drd_srs_srt(def(IK::SLT, "slt"));        // Set on Less Than
  drd_srs_srt(def(IK::SLTU, "sltu"));      // Set on Less Than Unsigned

  // fixed shifts
  drd_srt_ssa(def(IK::SLL, "sll"));        // Shift Left Logical
  drd_srt_ssa(def(IK::SRA, "sra"));        // Shift Right Arithmetic
  drd_srt_ssa(def(IK::SRL, "srl"));        // Shift Right Logical
  drd_srt_ssa(def(IK::DSLL, "dsll"));      // Doubleword Shift Left Logical
  drd_srt_ssa(def(IK::DSLL32, "dsll32"));  // Doubleword Shift Left Logical Plus 32
  drd_srt_ssa(def(IK::DSRA, "dsra"));      // Doubleword Shift Right Arithmetic
  drd_srt_ssa(def(IK::DSRA32, "dsra32"));  // Doubleword Shift Right Arithmetic Plus 32
  drd_srt_ssa(def(IK::DSRL, "dsrl"));      // Doubleword Shift Right Logical
  drd_srt_ssa(def(IK::DSRL32, "dsrl32"));  // Doubleword Shift Right Logical Plus 32

  // variable shifts
  drd_srt_srs(def(IK::DSRAV, "dsrav"));  // Doubleword Shift Right Arithmetic Variable
  drd_srt_srs(def(IK::SLLV, "sllv"));    // Shift Word Left Logical Variable
  drd_srt_srs(def(IK::DSLLV, "dsllv"));  // Doubleword Shift Left Logical Variable
  drd_srt_srs(def(IK::DSRLV, "dsrlv"));  // Doubleword Shift Right Logical Variable

  // branch (two registers)
  srs_srt_bt(def_branch(IK::BEQ, "beq"));           // Branch on Equal
  srs_srt_bt(def_branch(IK::BNE, "bne"));           // Branch on Not Equal
  srs_srt_bt(def_branch_likely(IK::BEQL, "beql"));  // Branch on Equal Likely
  srs_srt_bt(def_branch_likely(IK::BNEL, "bnel"));  // Branch on Not Equal Likely

  // branch (one register)
  srs_bt(def_branch(IK::BLTZ, "bltz"));      // Branch on Less Than Zero
  srs_bt(def_branch(IK::BGEZ, "bgez"));      // Branch on Greater Than or Equal to Zero
  srs_bt(def_branch(IK::BLEZ, "blez"));      // Branch on Less Than or Equal to Zero
  srs_bt(def_branch(IK::BGTZ, "bgtz"));      // Branch on Greater Than Zero
  srs_bt(def_branch(IK::BGEZAL, "bgezal"));  // Branch on Greater Than or Equal to Zero and Link
  srs_bt(def_branch_likely(IK::BLTZL, "bltzl"));  // Branch on Less Than Zero Likely
  srs_bt(def_branch_likely(IK::BGTZL, "bgtzl"));  // Branch on Greater Than Zero Likely
  srs_bt(def_branch_likely(IK::BGEZL, "bgezl"));  // Branch on Greater Than or Equal to Zero Likely

  // weird ones
  def(IK::DIV, "div").src_gpr(FT::RS).src_gpr(FT::RT);    // Divide Word
  def(IK::DIVU, "divu").src_gpr(FT::RS).src_gpr(FT::RT);  // Divide Unsigned Word

  def(IK::ORI, "ori").dst_gpr(FT::RT).src_gpr(FT::RS).src(FT::ZIMM16, DT::IMM);  // Or Immediate
  def(IK::XORI, "xori")
      .dst_gpr(FT::RT)
      .src_gpr(FT::RS)
      .src(FT::ZIMM16, DT::IMM);  // Exclusive Or Immediate
  def(IK::ANDI, "andi").dst_gpr(FT::RT).src_gpr(FT::RS).src(FT::ZIMM16, DT::IMM);  // And Immediate

  def(IK::LUI, "lui").dst_gpr(FT::RT).src(FT::SIMM16, DT::IMM);  // Load Upper Immediate
  def(IK::JALR, "jalr").dst_gpr(FT::RD).src_gpr(FT::RS).has_delay_slot =
      true;                                                 // Jump and Link Register
  def(IK::JR, "jr").src_gpr(FT::RS).has_delay_slot = true;  // Jump Register

  def_load(IK::LWC1, "lwc1")
      .dst_fpr(FT::FT)
      .src(FT::SIMM16, DT::IMM)
      .src_gpr(FT::RS);  // Load Word to Floating Point
  def_store(IK::SWC1, "swc1")
      .src_fpr(FT::FT)
      .src(FT::SIMM16, DT::IMM)
      .src_gpr(FT::RS);  // Store Word from Floating Point

  // weird moves
  def(IK::MFC1, "mfc1").dst_gpr(FT::RT).src_fpr(FT::FS);  // Move Word from Floating Point
  def(IK::MTC1, "mtc1").dst_fpr(FT::FS).src_gpr(FT::RT);  // Move Word to Floating Point
  def(IK::MTC0, "mtc0")
      .src_gpr(FT::RT)
      .dst(FT::RD, DT::COP0);  // Move to System Control Coprocessor
  def(IK::MFC0, "mfc0")
      .dst_gpr(FT::RT)
      .src(FT::RD, DT::COP0);                   // Move from System Control Coprocessor
  def(IK::MTDAB, "mtdab").src_gpr(FT::RT);      // Move to Data Address Breakpoint Register
  def(IK::MTDABM, "mtdabm").src_gpr(FT::RT);    // Move to Data Address Breakpoint Mask Register
  drd(def(IK::MFHI, "mfhi"));                   // Move from HI Register
  drd(def(IK::MFLO, "mflo"));                   // Move from LO Register
  def(IK::MTLO1, "mtlo1").src_gpr(FT::RS);      // Move to LO1 Register
  drd(def(IK::MFLO1, "mflo1"));                 // Move from LO1 Register
  drd(def(IK::PMFHL_UW, "pmfhl.uw").gpr128());  // Parallel Move From HI/LO Register
  drd(def(IK::PMFHL_LW, "pmfhl.lw").gpr128());
  drd(def(IK::PMFHL_LH, "pmfhl.lh").gpr128());
  def(IK::MFPC, "mfpc").dst_gpr(FT::RT).src(FT::PCR, DT::PCR);  // Move from Performance Counter
  def(IK::MTPC, "mtpc").src_gpr(FT::RT).dst(FT::PCR, DT::PCR);  // Move to Performance Counter

  // other weirds
  def(IK::SYSCALL, "syscall").src(FT::SYSCALL, DT::IMM);  // System Call
  def(IK::CACHE_DXWBIN, "cache dxwbin")
      .src_gpr(FT::RS)
      .src(FT::SIMM16, DT::IMM);  // Cache Operation (Index Writeback Invalidate)
  def(IK::PREF, "pref").src_gpr(FT::RT).src(FT::SIMM16, DT::IMM).src_gpr(FT::RS);  // Prefetch

  // plains
  def(IK::SYNCP, "sync.p");  // Synchronize Shared Memory (Pipeline)
  def(IK::SYNCL, "sync.l");  // Synchronize Shared Memory (Load)
  def(IK::ERET, "eret");     // Exception Return
  def(IK::EI, "ei");         // Enable Interrupt
  def(IK::MTSAB, "mtsab")
      .src_gpr(FT::RS)
      .src(FT::ZIMM16, DT::IMM);                  // Move Byte Count to Shift Amount Register
  drd_srs_srt(def(IK::QFSRV, "qfsrv")).gpr128();  // Quadword Funnel Shift Right Variable

  drd_srs_srt(def(IK::PPACB, "ppacb").gpr128());    // Parallel Pack to Byte
  drd_srs_srt(def(IK::PPACH, "ppach").gpr128());    // Parallel Pack to Halfword
  drd_srs_srt(def(IK::PPACW, "ppacw").gpr128());    // Parallel Pack to Word
  drd_srs_srt(def(IK::PADDB, "paddb").gpr128());    // Parallel Add Byte
  drd_srs_srt(def(IK::PADDH, "paddh").gpr128());    // Parallel Add Halfword
  drd_srs_srt(def(IK::PADDW, "paddw").gpr128());    // Parallel Add Word
  drd_srs_srt(def(IK::PSUBH, "psubh").gpr128());    // Parallel Subtract Halfword
  drd_srs_srt(def(IK::PSUBW, "psubw").gpr128());    // Parallel Subtract Word
  drd_srs_srt(def(IK::PMINH, "pminh").gpr128());    // Parallel Minimize Halfword
  drd_srs_srt(def(IK::PMINW, "pminw").gpr128());    // Parallel Minimize Word
  drd_srs_srt(def(IK::PMAXH, "pmaxh").gpr128());    // Parallel Maximize Halfword
  drd_srs_srt(def(IK::PMAXW, "pmaxw").gpr128());    // Parallel Maximize Word
  drd_srs_srt(def(IK::PEXTLB, "pextlb").gpr128());  // Parallel Extend Lower from Byte
  drd_srs_srt(def(IK::PEXTLH, "pextlh").gpr128());  // Parallel Extend Lower from Halfword
  drd_srs_srt(def(IK::PEXTLW, "pextlw").gpr128());  // Parallel Extend Lower from Word
  drd_srs_srt(def(IK::PCGTW, "pcgtw").gpr128());    // Parallel Compare for Greater Than Word
  drd_srs_srt(def(IK::PCGTB, "pcgtb").gpr128());    // Parallel Compare for Greater Than Byte
  drd_srs_srt(def(IK::PCEQB, "pceqb").gpr128());    // Parallel Compare for Equal Byte
  drd_srs_srt(def(IK::PCEQW, "pceqw").gpr128());    // Parallel Compare for Equal Word
  drd_srs_srt(def(IK::PEXTUB, "pextub").gpr128());  // Parallel Extend Upper from Byte
  drd_srs_srt(def(IK::PEXTUH, "pextuh").gpr128());  // Parallel Extend Upper from Halfword
  drd_srs_srt(def(IK::PEXTUW, "pextuw").gpr128());  // Parallel Extend Upper from Word
  drd_srs_srt(def(IK::PCPYUD, "pcpyud").gpr128());  // Parallel Copy Upper Doubleword
  drd_srs_srt(def(IK::PCPYLD, "pcpyld").gpr128());  // Parallel Copy Lower Doubleword
  drd_srs_srt(def(IK::PMADDH, "pmaddh").gpr128());  // Parallel Multiply-Add Halfword
  drd_srs_srt(def(IK::PMULTH, "pmulth").gpr128());  // Parallel Multiply Halfword
  drd_srs_srt(def(IK::PINTEH, "pinteh").gpr128());  // Parallel Interleave Even Halfword
  drd_srs_srt(def(IK::PAND, "pand").gpr128());      // Parallel And
  drd_srs_srt(def(IK::POR, "por").gpr128());        // Parallel Or
  drd_srs_srt(def(IK::PNOR, "pnor").gpr128());      // Parallel Not Or
  drd_srs_srt(def(IK::PXOR, "pxor").gpr128());      // Parallel Exclusive Or

  def(IK::PEXEW, "pexew").gpr128().dst_gpr(FT::RD).src_gpr(FT::RT);  // Parallel Exchange Even Word

  drd_srt_ssa(def(IK::PSLLW, "psllw").gpr128());  // Parallel Shift Left Logical Word
  drd_srt_ssa(def(IK::PSLLH, "psllh").gpr128());  // Parallel Shift Left Logical Halfword
  drd_srt_ssa(def(IK::PSRAW, "psraw").gpr128());  // Parallel Shift Right Arithmetic Word
  drd_srt_ssa(def(IK::PSRAH, "psrah").gpr128());  // Parallel Shift Right Arithmetic Halfword
  drd_srt_ssa(def(IK::PSRLH, "psrlh").gpr128());  // Parallel Shift Right Logical Halfword

  def(IK::PLZCW, "plzcw")
      .dst_gpr(FT::RD)
      .src_gpr(FT::RS)
      .gpr128();  // Parallel Leading Zero Count Word
  def(IK::PABSW, "pabsw").dst_gpr(FT::RD).src_gpr(FT::RT).gpr128();    // Parallel Absolute Word
  def(IK::PROT3W, "prot3w").dst_gpr(FT::RD).src_gpr(FT::RT).gpr128();  // Parallel Rotate 3 Word
  def(IK::PCPYH, "pcpyh").dst_gpr(FT::RD).src_gpr(FT::RT).gpr128();    // Parallel Copy Halfword

  // COP1

  // branch (no registers)
  bt(def_branch(IK::BC1F, "bc1f"));           // Branch on FP False
  bt(def_branch(IK::BC1T, "bc1t"));           // Branch on FP True
  bt(def_branch_likely(IK::BC1FL, "bc1fl"));  // Branch on FP False Likely
  bt(def_branch_likely(IK::BC1TL, "bc1tl"));  // Branch on FP True Likely

  dfd_sfs_sft(def(IK::ADDS, "add.s"));      // Floating Point Add
  dfd_sfs_sft(def(IK::SUBS, "sub.s"));      // Floating Point Subtract
  dfd_sfs_sft(def(IK::MULS, "mul.s"));      // Floating Point Multiply
  dfd_sfs_sft(def(IK::DIVS, "div.s"));      // Floating Point Divide
  dfd_sfs_sft(def(IK::MINS, "min.s"));      // Floating Point Minimum
  dfd_sfs_sft(def(IK::MAXS, "max.s"));      // Floating Point Maximum
  dfd_sfs_sft(def(IK::MADDS, "madd.s"));    // Floating Point Multiply-Add
  dfd_sfs_sft(def(IK::MSUBS, "msub.s"));    // Floating Point Multiply and Subtract
  dfd_sfs_sft(def(IK::RSQRTS, "rsqrt.s"));  // Floating Point Reciporcal Square Root

  dfd_sfs(def(IK::ABSS, "abs.s"));     // Floating Point Absolute Value
  dfd_sfs(def(IK::NEGS, "neg.s"));     // Floating Point Negate
  dfd_sfs(def(IK::CVTSW, "cvt.s.w"));  // Fixed-point Convert to Single Floating Point
  dfd_sfs(def(IK::CVTWS, "cvt.w.s"));  // Floating Point Convert to Word Fixed-point
  dfd_sfs(def(IK::MOVS, "mov.s"));     // Floating Point Move
  dfd_sft(def(IK::SQRTS, "sqrt.s"));   // Floating Point Square Root

  sfs_sft(def(IK::CLTS, "c.lt.s"));     // Floating Point Compare
  sfs_sft(def(IK::CLES, "c.le.s"));     // Floating Point Compare
  sfs_sft(def(IK::CEQS, "c.eq.s"));     // Floating Point Compare
  sfs_sft(def(IK::MULAS, "mula.s"));    // Floating Point Multiply to Accumulator
  sfs_sft(def(IK::MADDAS, "madda.s"));  // Floating Point Multiply-Add to Accumulator
  sfs_sft(def(IK::ADDAS, "adda.s"));    // Floating Point Add to Accumulator
  sfs_sft(def(IK::MSUBAS, "msuba.s"));  // Floating Point Multiply and Subtract from Accumulator

  // COP2 weirds
  def_store(IK::SQC2, "sqc2")
      .src(FT::FT, DT::VF)
      .src(FT::SIMM16, DT::IMM)
      .src_gpr(FT::RS);  // Store Quadword from COP2
  def_load(IK::LQC2, "lqc2")
      .dst(FT::FT, DT::VF)
      .src(FT::SIMM16, DT::IMM)
      .src_gpr(FT::RS);  // Load Quadword to COP2

  // COP2
  // NOTE: if adding more here, update AtomicOp.cpp AsmOp::update_register_info()
  cd_dvft_svfs(def(IK::VMOVE, "vmove"));      // Transfer between Floating-Point Registers
  cd_dvft_svfs(def(IK::VFTOI0, "vftoi0"));    // Conversion to Fixed Point
  cd_dvft_svfs(def(IK::VFTOI4, "vftoi4"));    // Conversion to Fixed Point
  cd_dvft_svfs(def(IK::VFTOI12, "vftoi12"));  // Conversion to Fixed Point
  cd_dvft_svfs(def(IK::VFTOI15, "vftoi15"));  // Conversion to Fixed Point
  cd_dvft_svfs(def(IK::VITOF0, "vitof0"));    // Conversion to Floating Point Number
  cd_dvft_svfs(def(IK::VITOF12, "vitof12"));  // Conversion to Floating Point Number
  cd_dvft_svfs(def(IK::VITOF15, "vitof15"));  // Conversion to Floating Point Number
  cd_dvft_svfs(def(IK::VABS, "vabs"));        // Absolute Value

  cd_dvfd_svfs_svft(def(IK::VADD, "vadd"));
  cd_dvfd_svfs_svft(def(IK::VSUB, "vsub"));
  cd_dvfd_svfs_svft(def(IK::VMUL, "vmul"));
  cd_dvfd_svfs_svft(def(IK::VMINI, "vmini"));
  cd_dvfd_svfs_svft(def(IK::VMAX, "vmax"));
  cd_dvfd_svfs_svft(def(IK::VOPMSUB, "vopmsub"));
  cd_dvfd_svfs_svft(def(IK::VMADD, "vmadd"));
  cd_dvfd_svfs_svft(def(IK::VMSUB, "vmsub"));

  cb_cd_dvfd_svfs_svft(def(IK::VSUB_BC, "vsub"));
  cb_cd_dvfd_svfs_svft(def(IK::VADD_BC, "vadd"));
  cb_cd_dvfd_svfs_svft(def(IK::VMADD_BC, "vmadd"));
  cb_cd_dvfd_svfs_svft(def(IK::VMSUB_BC, "vmsub"));
  cb_cd_dvfd_svfs_svft(def(IK::VMUL_BC, "vmul"));
  cb_cd_dvfd_svfs_svft(def(IK::VMINI_BC, "vmini"));
  cb_cd_dvfd_svfs_svft(def(IK::VMAX_BC, "vmax"));

  cb_cd_dacc_svfs_svft(def(IK::VADDA_BC, "vadda"));
  cb_cd_dacc_svfs_svft(def(IK::VMADDA_BC, "vmadda"));
  cb_cd_dacc_svfs_svft(def(IK::VMULA_BC, "vmula"));
  cb_cd_dacc_svfs_svft(def(IK::VMSUBA_BC, "vmsuba"));

  cd_dvfd_svfs_sq(def(IK::VADDQ, "vaddq"));
  cd_dvfd_svfs_sq(def(IK::VSUBQ, "vsubq"));
  cd_dvfd_svfs_sq(def(IK::VMULQ, "vmulq"));
  cd_dvfd_svfs_sq(def(IK::VMSUBQ, "vmsubq"));

  cd_dacc_svfs_svft(def(IK::VMULA, "vmula"));
  cd_dacc_svfs_svft(def(IK::VADDA, "vadda"));
  cd_dacc_svfs_svft(def(IK::VMADDA, "vmadda"));
  cd_dacc_svfs_svft(def(IK::VMSUBA, "vmsuba"));

  cd_dacc_svfs_svft(def(IK::VOPMULA, "vopmula"));

  // weird
  def(IK::VDIV, "vdiv")
      .dst(FT::ZERO, DT::VU_Q)
      .src_vf(FT::FS)
      .src(FT::FS_F, DT::VF_F)
      .src_vf(FT::FT)
      .src(FT::FT_F, DT::VF_F);
  def(IK::VRSQRT, "vrsqrt")
      .dst(FT::ZERO, DT::VU_Q)
      .src_vf(FT::FS)
      .src(FT::FS_F, DT::VF_F)
      .src_vf(FT::FT)
      .src(FT::FT_F, DT::VF_F);
  def(IK::VCLIP, "vclip").src(FT::DEST, DT::DEST).src_vf(FT::FS).src_vf(FT::FT);
  def(IK::VMULAQ, "vmulaq")
      .src(FT::DEST, DT::DEST)
      .dst(FT::ZERO, DT::VU_ACC)
      .src_vf(FT::FS)
      .src(FT::ZERO, DT::VU_Q);

  def(IK::VRGET, "vrget").src(FT::DEST, DT::DEST).dst_vf(FT::FT);
  def(IK::VMR32, "vmr32").src(FT::DEST, DT::DEST).src_vf(FT::FT).dst_vf(FT::FS);

  // integer
  def(IK::VMTIR, "vmtir").dst(FT::RT, DT::VI).src_vf(FT::FS).src(FT::BC, DT::BC);
  def(IK::VIAND, "viand").dst_vi(FT::FD).src_vi(FT::FS).src_vi(FT::FT);
  def(IK::VLQI, "vlqi").src(FT::DEST, DT::DEST).dst_vf(FT::FT).src_vi(FT::FS);  // todo inc
  def(IK::VSQI, "vsqi").src(FT::DEST, DT::DEST).src_vf(FT::FS).src_vi(FT::FT);  // todo inc
  def(IK::VIADDI, "viaddi").dst_vi(FT::FT).src_vi(FT::FS).src(FT::IMM5, DT::IMM);

  def(IK::QMFC2, "qmfc2").src(FT::IL, DT::IL).dst_gpr(FT::RT).src_vf(FT::FS);
  def(IK::QMTC2, "qmtc2").src(FT::IL, DT::IL).dst_vf(FT::FS).src_gpr(FT::RT);
  def(IK::VSQRT, "vsqrt").dst(FT::ZERO, DT::VU_Q).src_vf(FT::FT).src(FT::FT_F, DT::VF_F);
  def(IK::VRXOR, "vrxor").src(FT::BC, DT::BC).src_vf(FT::FS);
  def(IK::VRNEXT, "vrnext").src(FT::DEST, DT::DEST).dst_vf(FT::FT);
  def(IK::CTC2, "ctc2").src(FT::IL, DT::IL).src_gpr(FT::RT).dst(FT::RD, DT::VI);
  def(IK::CFC2, "cfc2").src(FT::IL, DT::IL).dst_gpr(FT::RT).src(FT::RD, DT::VI);

  def(IK::VCALLMS, "vcallms").src(FT::IMM15, DT::VCALLMS_TARGET);

  def(IK::VNOP, "vnop");
  def(IK::VWAITQ, "vwaitq");

  uint32_t valid_count = 0, total_count = 0;
  for (auto& info : gOpcodeInfo) {
    if (info.defined) {
      valid_count++;
    }
    total_count++;
  }

  // for the UNKNOWN op which shouldn't be valid.
  total_count--;
  ASSERT(total_count == valid_count);
  opcodes_initialized = true;
}

void OpcodeInfo::step(DecodeStep& s) {
  ASSERT(step_count < MAX_DECODE_STEPS);
  steps[step_count] = s;
  step_count++;
  defined = true;
}

OpcodeInfo& OpcodeInfo::src(FieldType field, DecodeType decode) {
  DecodeStep new_step;
  new_step.is_src = true;
  new_step.field = field;
  new_step.decode = decode;
  step(new_step);
  return *this;
}

OpcodeInfo& OpcodeInfo::src_gpr(FieldType field) {
  return src(field, DT::GPR);
}

OpcodeInfo& OpcodeInfo::src_fpr(FieldType field) {
  return src(field, DT::FPR);
}

OpcodeInfo& OpcodeInfo::src_vf(FieldType field) {
  return src(field, DT::VF);
}

OpcodeInfo& OpcodeInfo::src_vi(FieldType field) {
  return src(field, DT::VI);
}

OpcodeInfo& OpcodeInfo::dst(FieldType field, DecodeType decode) {
  DecodeStep new_step;
  new_step.is_src = false;
  new_step.field = field;
  new_step.decode = decode;
  step(new_step);
  return *this;
}

OpcodeInfo& OpcodeInfo::dst_gpr(FieldType field) {
  return dst(field, DT::GPR);
}

OpcodeInfo& OpcodeInfo::dst_fpr(FieldType field) {
  return dst(field, DT::FPR);
}

OpcodeInfo& OpcodeInfo::dst_vf(FieldType field) {
  return dst(field, DT::VF);
}

OpcodeInfo& OpcodeInfo::dst_vi(FieldType field) {
  return dst(field, DT::VI);
}

OpcodeInfo& OpcodeInfo::gpr128() {
  gpr_128 = true;
  return *this;
}
}  // namespace decompiler
