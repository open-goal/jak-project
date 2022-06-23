/*!
 * @file InstructionMatching.cpp
 * Utilities for checking if an instruction matches some criteria.
 */

#include "InstructionMatching.h"

#include "common/util/Assert.h"

namespace decompiler {
/*!
 * Check if the given instruction stores a GPR with the specified parameters.
 */
bool is_no_link_gpr_store(const Instruction& instr,
                          MatchParam<int> size,
                          MatchParam<Register> src,
                          MatchParam<int> offset,
                          MatchParam<Register> dest) {
  // match the opcode
  if (!size.is_wildcard) {
    switch (size.value) {
      case 1:
        if (instr.kind != InstructionKind::SB) {
          return false;
        }
        break;
      case 2:
        if (instr.kind != InstructionKind::SH) {
          return false;
        }
        break;
      case 4:
        if (instr.kind != InstructionKind::SW) {
          return false;
        }
        break;
      case 8:
        if (instr.kind != InstructionKind::SD) {
          return false;
        }
        break;
      case 16:
        if (instr.kind != InstructionKind::SQ) {
          return false;
        }
        break;
      default:
        ASSERT(false);
    }
  } else {
    // just make sure it's a gpr store
    if (!is_gpr_store(instr)) {
      return false;
    }
  }

  ASSERT(instr.n_src == 3);

  // match other arguments
  return src == instr.src[0].get_reg() && offset == instr.src[1].get_imm() &&
         dest == instr.src[2].get_reg();
}

/*!
 * Check if the given instruction loads a GPR with the specified parameters.
 * LD and LQ count as signed, unsigned, and "wildcard signed" loads.
 * LWL/LWR/LDL/LDR will never match.
 *
 * "no ll" means no link or label
 */
bool is_no_ll_gpr_load(const Instruction& instr,
                       MatchParam<int> size,
                       MatchParam<bool> is_signed,
                       MatchParam<Register> dst_reg,
                       MatchParam<int> offset,
                       MatchParam<Register> mem_reg) {
  // match the opcode
  if (!size.is_wildcard) {
    if (is_signed.is_wildcard) {
      switch (size.value) {
        case 1:
          if (instr.kind != InstructionKind::LB && instr.kind != InstructionKind::LBU) {
            return false;
          }
          break;
        case 2:
          if (instr.kind != InstructionKind::LH && instr.kind != InstructionKind::LHU) {
            return false;
          }
          break;
        case 4:
          if (instr.kind != InstructionKind::LW && instr.kind != InstructionKind::LWU) {
            return false;
          }
          break;
        case 8:
          if (instr.kind != InstructionKind::LD) {
            return false;
          }
          break;
        case 16:
          if (instr.kind != InstructionKind::LQ) {
            return false;
          }
          break;
        default:
          ASSERT(false);
      }
    } else {
      if (is_signed.value) {
        switch (size.value) {
          case 1:
            if (instr.kind != InstructionKind::LB) {
              return false;
            }
            break;
          case 2:
            if (instr.kind != InstructionKind::LH) {
              return false;
            }
            break;
          case 4:
            if (instr.kind != InstructionKind::LW) {
              return false;
            }
            break;
          case 8:
            if (instr.kind != InstructionKind::LD) {
              return false;
            }
            break;
          case 16:
            if (instr.kind != InstructionKind::LQ) {
              return false;
            }
            break;
          default:
            ASSERT(false);
        }
      } else {
        switch (size.value) {
          case 1:
            if (instr.kind != InstructionKind::LBU) {
              return false;
            }
            break;
          case 2:
            if (instr.kind != InstructionKind::LHU) {
              return false;
            }
            break;
          case 4:
            if (instr.kind != InstructionKind::LWU) {
              return false;
            }
            break;
          case 8:
            if (instr.kind != InstructionKind::LD) {
              return false;
            }
            break;
          case 16:
            if (instr.kind != InstructionKind::LQ) {
              return false;
            }
            break;
          default:
            ASSERT(false);
        }
      }
    }
  } else {
    // just make sure it's a gpr store
    if (!is_gpr_load(instr, is_signed)) {
      return false;
    }
  }

  // match other arguments
  return dst_reg == instr.get_dst(0).get_reg() && offset == instr.get_src(0).get_imm() &&
         mem_reg == instr.get_src(1).get_reg();
}

/*!
 * Check if the instruction stores an FPR (SWC1)
 * "no ll" means that there is no label or linking involved.
 */
bool is_no_ll_fpr_store(const Instruction& instr,
                        MatchParam<Register> src,
                        MatchParam<int> offset,
                        MatchParam<Register> dest) {
  return instr.kind == InstructionKind::SWC1 && src == instr.src[0].get_reg() &&
         offset == instr.src[1].get_imm() && dest == instr.src[2].get_reg();
}
/*!
 * Check if the instruction loads an FPR (LWC1)
 * "no ll" means that there is no label or linking involved.
 */
bool is_no_ll_fpr_load(const Instruction& instr,
                       MatchParam<Register> dst_reg,
                       MatchParam<int> offset,
                       MatchParam<Register> mem_reg) {
  return instr.kind == InstructionKind::LWC1 && dst_reg == instr.get_dst(0).get_reg() &&
         offset == instr.get_src(0).get_imm() && mem_reg == instr.get_src(1).get_reg();
}

namespace {
auto gpr_stores = {InstructionKind::SB, InstructionKind::SH, InstructionKind::SW,
                   InstructionKind::SD, InstructionKind::SQ};
auto gpr_signed_loads = {InstructionKind::LB, InstructionKind::LH, InstructionKind::LW,
                         InstructionKind::LD, InstructionKind::LQ};
auto gpr_unsigned_loads = {InstructionKind::LBU, InstructionKind::LHU, InstructionKind::LWU,
                           InstructionKind::LD, InstructionKind::LQ};
auto gpr_all_loads = {InstructionKind::LBU, InstructionKind::LB, InstructionKind::LH,
                      InstructionKind::LHU, InstructionKind::LW, InstructionKind::LWU,
                      InstructionKind::SD,  InstructionKind::SQ};
}  // namespace

/*!
 * Is this a GPR store instruction? sb,sh,sw,sd,sq
 */
bool is_gpr_store(const Instruction& instr) {
  for (auto x : gpr_stores) {
    if (instr.kind == x) {
      return true;
    }
  }
  return false;
}

/*!
 * Is this a GPR load instruction?
 * Only LB/LBU,LH/LHU,LW/LWU,LD,LQ are treated as loads
 * The LD, LQ opcodes are both signed, unsigned, and "wildcard signed"
 */
bool is_gpr_load(const Instruction& instr, MatchParam<bool> is_signed) {
  if (is_signed.is_wildcard) {
    for (auto x : gpr_all_loads) {
      if (instr.kind == x) {
        return true;
      }
    }
    return false;
  } else if (is_signed.value) {
    for (auto x : gpr_signed_loads) {
      if (instr.kind == x) {
        return true;
      }
    }
    return false;
  } else {
    for (auto x : gpr_unsigned_loads) {
      if (instr.kind == x) {
        return true;
      }
    }
    return false;
  }
}

/*!
 * Given a store, get the offset as an integer.
 */
int32_t get_gpr_store_offset_as_int(const Instruction& instr) {
  ASSERT(is_gpr_store(instr));
  ASSERT(instr.n_src == 3);
  return instr.src[1].get_imm();
}

/*!
 * Match an instruction in the form OP, dst, src0, src1 where all args are registers.
 */
bool is_gpr_3(const Instruction& instr,
              MatchParam<InstructionKind> kind,
              MatchParam<Register> dst,
              MatchParam<Register> src0,
              MatchParam<Register> src1) {
  return kind == instr.kind && dst == instr.get_dst(0).get_reg() &&
         src0 == instr.get_src(0).get_reg() && src1 == instr.get_src(1).get_reg();
}

/*!
 * Match an instruction in the form OP, dst, src0, src1 where all args are registers, except for
 * src1, which is an integer.
 */
bool is_gpr_2_imm_int(const Instruction& instr,
                      MatchParam<InstructionKind> kind,
                      MatchParam<Register> dst,
                      MatchParam<Register> src,
                      MatchParam<int32_t> imm) {
  return kind == instr.kind && dst == instr.get_dst(0).get_reg() &&
         src == instr.get_src(0).get_reg() && instr.get_src(1).is_imm() &&
         imm == instr.get_src(1).get_imm();
}

/*!
 * Create a Register for a GPR.
 */
Register make_gpr(Reg::Gpr gpr) {
  return Register(Reg::GPR, gpr);
}

/*!
 * Create a Register for an FPR.
 */
Register make_fpr(int fpr) {
  return Register(Reg::FPR, fpr);
}

/*!
 * Is this a "nop"?  More specifically, it checks for sll r0, r0, 0, the recommended MIPS nop.
 */
bool is_nop(const Instruction& instr) {
  return is_gpr_2_imm_int(instr, InstructionKind::SLL, make_gpr(Reg::R0), make_gpr(Reg::R0), 0);
}

/*!
 * Is this jr ra?
 */
bool is_jr_ra(const Instruction& instr) {
  return instr.kind == InstructionKind::JR && instr.get_src(0).get_reg() == make_gpr(Reg::RA);
}

bool is_branch(const Instruction& instr, MatchParam<bool> likely) {
  const auto& info = instr.get_info();
  if (likely.is_wildcard) {
    return info.is_branch || info.is_branch_likely;
  } else if (likely.value) {
    return info.is_branch_likely;
  } else {
    return info.is_branch && !info.is_branch_likely;
  }
}

bool is_always_branch(const Instruction& instr) {
  if (!is_branch(instr, {})) {
    return false;
  }

  auto r0 = make_gpr(Reg::R0);
  if (instr.kind == InstructionKind::BEQ && instr.get_src(0).get_reg() == r0 &&
      instr.get_src(1).get_reg() == r0) {
    return true;
  }

  if (instr.kind == InstructionKind::BEQL && instr.get_src(0).get_reg() == r0 &&
      instr.get_src(1).get_reg() == r0) {
    ASSERT(false);
    return true;
  }

  return false;
}
}  // namespace decompiler
