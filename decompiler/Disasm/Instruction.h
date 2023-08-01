#pragma once

/*!
 * @file Instruction.h
 * An EE instruction, represented as an operation, plus a list of source/destination atoms.
 * Can print itself (within the context of a LinkedObjectFile).
 */

#include <vector>

#include "OpcodeInfo.h"
#include "Register.h"

namespace decompiler {
struct DecompilerLabel;

constexpr int MAX_INSTRUCTION_SOURCE = 4;
constexpr int MAX_INTRUCTION_DEST = 1;

// An "atom", representing a single register, immediate, etc... for use in an Instruction.
struct InstructionAtom {
  enum AtomKind {
    REGISTER,         // An EE Register
    IMM,              // An immediate value (stored as int32)
    IMM_SYM,          // An immediate value (a symbolic link)
    IMM_SYM_VAL_PTR,  // pointer to value of a symbol
    LABEL,            // A label in a LinkedObjectFile
    VU_ACC,           // The VU0 Accumulator
    VU_Q,             // The VU0 Q Register
    VF_FIELD,         // Field specifier
    INVALID
  } kind = INVALID;

  void set_reg(Register r);
  void set_imm(int32_t i);
  void set_label(int id);
  void set_vu_q();
  void set_vu_acc();
  void set_sym(const std::string& _sym);
  void set_sym_val_ptr(const std::string& _sym);
  void set_vf_field(uint32_t value);

  Register get_reg() const;
  int32_t get_imm() const;
  int32_t get_vf_field() const;
  int get_label() const;
  std::string get_sym() const;

  std::string to_string(const std::vector<DecompilerLabel>& labels) const;

  bool is_reg() const { return kind == REGISTER; }
  bool is_imm() const { return kind == IMM; }
  bool is_label() const { return kind == LABEL; }
  bool is_sym() const { return kind == IMM_SYM || kind == IMM_SYM_VAL_PTR; }

  bool is_reg(Register r) const { return kind == REGISTER && reg == r; }
  bool is_imm(int32_t i) const { return kind == IMM && imm == i; }
  bool is_sym(const std::string& name) const { return is_sym() && name == sym; }

  bool operator==(const InstructionAtom& other) const;
  bool operator!=(const InstructionAtom& other) const { return !((*this) == other); }

 private:
  int32_t imm;
  int label_id;
  Register reg;
  std::string sym;
};

// An "Instruction", consisting of a "kind" (the opcode), and the source/destination atoms it
// operates on.
class Instruction {
 public:
  InstructionKind kind = InstructionKind::UNKNOWN;

  std::string op_name_to_string() const;
  std::string to_string(const std::vector<DecompilerLabel>& labels) const;
  bool is_valid() const;

  void add_src(InstructionAtom& a);
  void add_dst(InstructionAtom& a);

  InstructionAtom& get_src(size_t idx);
  InstructionAtom& get_dst(size_t idx);
  const InstructionAtom& get_src(size_t idx) const;
  const InstructionAtom& get_dst(size_t idx) const;

  // source and destination atoms
  uint8_t n_src = 0, n_dst = 0;
  InstructionAtom src[MAX_INSTRUCTION_SOURCE];
  InstructionAtom dst[MAX_INTRUCTION_DEST];

  InstructionAtom& get_imm_src();
  int32_t get_imm_src_int();

  const OpcodeInfo& get_info() const;

  int get_label_target() const;

  bool operator==(const Instruction& other) const;
  bool operator!=(const Instruction& other) const { return !((*this) == other); }

  // extra fields for some COP2 instructions.
  // this is stored like in the PS2 instruction, which is different from intel
  uint8_t cop2_dest = 0xff;  // 0xff indicates "don't print dest"
  uint8_t cop2_bc = 0xff;    // 0xff indicates "don't print bc"
  uint8_t il = 0xff;         // 0xff indicates "don't print il"

  char cop2_bc_to_char() const;
  std::string cop2_dest_to_char() const;
  int cop2_dest_mask_intel() const;
};
}  // namespace decompiler
