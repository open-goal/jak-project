#pragma once

/*!
 * @file InstructionMatching.h
 * Utilities for checking if an instruction matches some criteria.
 */

#include "Instruction.h"

#include "decompiler/util/MatchParam.h"

namespace decompiler {
bool is_no_link_gpr_store(const Instruction& instr,
                          MatchParam<int> size,
                          MatchParam<Register> src,
                          MatchParam<int> offset,
                          MatchParam<Register> dest);
bool is_no_ll_gpr_load(const Instruction& instr,
                       MatchParam<int> size,
                       MatchParam<bool> is_signed,
                       MatchParam<Register> dst_reg,
                       MatchParam<int> offset,
                       MatchParam<Register> mem_reg);

bool is_no_ll_fpr_store(const Instruction& instr,
                        MatchParam<Register> src,
                        MatchParam<int> offset,
                        MatchParam<Register> dest);
bool is_no_ll_fpr_load(const Instruction& instr,
                       MatchParam<Register> dst_reg,
                       MatchParam<int> offset,
                       MatchParam<Register> mem_reg);

bool is_gpr_store(const Instruction& instr);
bool is_gpr_load(const Instruction& instr, MatchParam<bool> is_signed);
int32_t get_gpr_store_offset_as_int(const Instruction& instr);

bool is_gpr_3(const Instruction& instr,
              MatchParam<InstructionKind> kind,
              MatchParam<Register> dst,
              MatchParam<Register> src0,
              MatchParam<Register> src1);

bool is_gpr_2_imm_int(const Instruction& instr,
                      MatchParam<InstructionKind> kind,
                      MatchParam<Register> dst,
                      MatchParam<Register> src,
                      MatchParam<int32_t> imm);

bool is_nop(const Instruction& instr);
bool is_jr_ra(const Instruction& instr);

Register make_gpr(Reg::Gpr gpr);
Register make_fpr(int fpr);

bool is_branch(const Instruction& instr, MatchParam<bool> likely);
bool is_always_branch(const Instruction& instr);
}  // namespace decompiler
