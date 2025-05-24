/*!
 * @file Instruction.cpp
 * An EE instruction, represented as an operation, plus a list of source/destination atoms.
 * Can print itself (within the context of a LinkedObjectFile).
 */

#include "Instruction.h"

#include "common/util/Assert.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

#include "fmt/core.h"

namespace decompiler {
/*!
 * Convert atom to a string for disassembly.
 */
std::string InstructionAtom::to_string(const std::vector<DecompilerLabel>& labels) const {
  switch (kind) {
    case REGISTER:
      return reg.to_string();
    case IMM:
      return std::to_string(imm);
    case LABEL:
      return labels.at(label_id).name;
    case VU_ACC:
      return "acc";
    case VU_Q:
      return "Q";
    case IMM_SYM:
      return sym;
    case IMM_SYM_VAL_PTR:
      return sym;
    case VF_FIELD:
      ASSERT(imm >= 0 && imm < 4);
      return fmt::format(".{}", "xyzw"[imm]);
    default:
      throw std::runtime_error("Unsupported InstructionAtom");
  }
}

/*!
 * Make this atom a register.
 */
void InstructionAtom::set_reg(Register r) {
  kind = REGISTER;
  reg = r;
}

/*!
 * Make this atom an immediate.
 */
void InstructionAtom::set_imm(int32_t i) {
  kind = IMM;
  imm = i;
}

/*!
 * Make this atom a label.
 */
void InstructionAtom::set_label(int id) {
  kind = LABEL;
  label_id = id;
}

/*!
 * Make this atom the VU ACC register.
 */
void InstructionAtom::set_vu_acc() {
  kind = VU_ACC;
}

/*!
 * Make this atom the VU0 Q register.
 */
void InstructionAtom::set_vu_q() {
  kind = VU_Q;
}

/*!
 * Make this atom a symbol.
 */
void InstructionAtom::set_sym(const std::string& _sym) {
  kind = IMM_SYM;
  sym = _sym;
}

/*!
 * Make this atom a symbol value pointer.
 */
void InstructionAtom::set_sym_val_ptr(const std::string& _sym) {
  kind = IMM_SYM_VAL_PTR;
  sym = _sym;
}

/*!
 * Make this atom a field (x,y,z,w) of a vf.
 */
void InstructionAtom::set_vf_field(uint32_t value) {
  kind = VF_FIELD;
  imm = value;
  ASSERT(value < 4);
}

/*!
 * Get as register, or error if not a register.
 */
Register InstructionAtom::get_reg() const {
  ASSERT(kind == REGISTER);
  return reg;
}

/*!
 * Get as integer immediate, or error if not an integer immediate.
 */
int32_t InstructionAtom::get_imm() const {
  ASSERT(kind == IMM);
  return imm;
}

/*!
 * Get the VF_FIELD as an integer immediate, or error if not applicable.
 */
int32_t InstructionAtom::get_vf_field() const {
  ASSERT(kind == VF_FIELD);
  return imm;
}

/*!
 * Get as label index, or error if not a label.
 */
int InstructionAtom::get_label() const {
  ASSERT(kind == LABEL);
  return label_id;
}

/*!
 * Get as symbol, or error if not a symbol.
 */
std::string InstructionAtom::get_sym() const {
  ASSERT(kind == IMM_SYM || kind == IMM_SYM_VAL_PTR);
  return sym;
}

bool InstructionAtom::operator==(const InstructionAtom& other) const {
  if (kind != other.kind) {
    return false;
  }
  switch (kind) {
    case REGISTER:
      return reg == other.reg;
    case IMM:
      return imm == other.imm;
    case LABEL:
      return label_id == other.label_id;
    case VU_ACC:
    case VU_Q:
      return true;
    default:
      ASSERT(false);
      return false;
  }
}

char Instruction::cop2_bc_to_char() const {
  switch (cop2_bc) {
    case 0:
      return 'x';
    case 1:
      return 'y';
    case 2:
      return 'z';
    case 3:
      return 'w';
    default:
      return '?';
  }
}
std::string Instruction::cop2_dest_to_char() const {
  std::string dest = ".";
  if (cop2_dest & 8)
    dest.push_back('x');
  if (cop2_dest & 4)
    dest.push_back('y');
  if (cop2_dest & 2)
    dest.push_back('z');
  if (cop2_dest & 1)
    dest.push_back('w');
  return dest;
}

int Instruction::cop2_dest_mask_intel() const {
  int mask = 0;
  for (int i = 0; i < 4; i++) {        // x,y,z,w order
    if (cop2_dest & (1 << (3 - i))) {  // set for ps2
      mask |= (1 << i);
    }
  }
  return mask;
}

/*!
 * Convert just the name of the opcode to a string, omitting src/dst, but including
 * suffixes (interlock, broadcasts and destination)
 */
std::string Instruction::op_name_to_string() const {
  auto& info = gOpcodeInfo[(int)kind];

  // the name
  std::string result = info.name;

  // optional "interlock" specification.
  if (il != 0xff) {
    result.append(il ? ".i" : ".ni");
  }

  // optional "broadcast" specification for COP2 opcodes.
  if (cop2_bc != 0xff) {
    result.push_back(cop2_bc_to_char());
  }

  // optional "destination" specification for COP2 opcodes.
  if (cop2_dest != 0xff) {
    result.append(cop2_dest_to_char());
  }

  return result;
}

/*!
 * Convert entire instruction to a string.
 */
std::string Instruction::to_string(const std::vector<DecompilerLabel>& labels) const {
  auto& info = gOpcodeInfo[(int)kind];
  auto result = op_name_to_string();

  // relative store and load instructions have a special syntax in MIPS
  if (info.is_store) {
    ASSERT(n_dst == 0);
    ASSERT(n_src == 3);
    result += " ";
    result += src[0].to_string(labels);
    result += ", ";
    result += src[1].to_string(labels);
    result += "(";
    result += src[2].to_string(labels);
    result += ")";
  } else if (info.is_load) {
    ASSERT(n_dst == 1);
    ASSERT(n_src == 2);
    result += " ";
    result += dst[0].to_string(labels);
    result += ", ";
    result += src[0].to_string(labels);
    result += "(";
    result += src[1].to_string(labels);
    result += ")";
  } else {
    // for instructions that aren't a store or load, the dest/sources are comma separated.
    bool end_comma = false;

    for (uint8_t i = 0; i < n_dst; i++) {
      result += " " + dst[i].to_string(labels) + ",";
      end_comma = true;
    }

    for (uint8_t i = 0; i < n_src; i++) {
      if (src[i].kind == InstructionAtom::VF_FIELD) {
        if (end_comma) {
          result.pop_back();
        }
        result += src[i].to_string(labels) + ",";
      } else {
        result += " " + src[i].to_string(labels) + ",";
      }
      end_comma = true;
    }

    if (end_comma) {
      result.pop_back();
    }
  }

  return result;
}

/*!
 * Was this instruction successfully decoded?
 */
bool Instruction::is_valid() const {
  return kind != InstructionKind::UNKNOWN;
}

/*!
 * Add a destination atom to this Instruction
 */
void Instruction::add_dst(InstructionAtom& a) {
  ASSERT(n_dst < MAX_INTRUCTION_DEST);
  dst[n_dst++] = a;
}

/*!
 * Add a source atom to this Instruction
 */
void Instruction::add_src(InstructionAtom& a) {
  ASSERT(n_src < MAX_INSTRUCTION_SOURCE);
  src[n_src++] = a;
}

/*!
 * Get a source atom that's an immediate, or error if it doesn't exist.
 */
InstructionAtom& Instruction::get_imm_src() {
  for (int i = 0; i < n_src; i++) {
    if (src[i].kind == InstructionAtom::IMM) {
      return src[i];
    }
  }
  ASSERT(false);
  return src[0];
}

/*!
 * Try to find a src which is an integer immediate, and return it as an integer.
 */
int32_t Instruction::get_imm_src_int() {
  return get_imm_src().get_imm();
}

/*!
 * Safe get dst atom
 */
InstructionAtom& Instruction::get_dst(size_t idx) {
  ASSERT(idx < n_dst);
  return dst[idx];
}

/*!
 * Safe get src atom
 */
InstructionAtom& Instruction::get_src(size_t idx) {
  ASSERT(idx < n_src);
  return src[idx];
}

/*!
 * Safe get dst atom
 */
const InstructionAtom& Instruction::get_dst(size_t idx) const {
  ASSERT(idx < n_dst);
  return dst[idx];
}

/*!
 * Safe get src atom
 */
const InstructionAtom& Instruction::get_src(size_t idx) const {
  ASSERT(idx < n_src);
  return src[idx];
}

/*!
 * Get OpcodeInfo for the opcode used in this instruction.
 */
const OpcodeInfo& Instruction::get_info() const {
  return gOpcodeInfo[int(kind)];
}

/*!
 * Get the target label for this instruction. If the instruction doesn't have a target label,
 * return -1.
 */
int Instruction::get_label_target() const {
  int result = -1;
  for (int i = 0; i < n_src; i++) {
    if (src[i].kind == InstructionAtom::AtomKind::LABEL) {
      ASSERT(result == -1);
      result = src[i].get_label();
    }
  }
  return result;
}

bool Instruction::operator==(const Instruction& other) const {
  if (kind != other.kind || n_src != other.n_src || n_dst != other.n_dst ||
      cop2_dest != other.cop2_dest || cop2_bc != other.cop2_bc || il != other.il) {
    return false;
  }

  for (int i = 0; i < n_dst; i++) {
    if (dst[i] != other.dst[i]) {
      return false;
    }
  }

  for (int i = 0; i < n_src; i++) {
    if (src[i] != other.src[i]) {
      return false;
    }
  }

  return true;
}
}  // namespace decompiler
