/*!
 * @file Instruction.cpp
 * An EE instruction, represented as an operation, plus a list of source/destination atoms.
 * Can print itself (within the context of a LinkedObjectFile).
 */

#include "Instruction.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include <cassert>

/*!
 * Convert atom to a string for disassembly.
 */
std::string InstructionAtom::to_string(const LinkedObjectFile& file) const {
  switch (kind) {
    case REGISTER:
      return reg.to_string();
    case IMM:
      return std::to_string(imm);
    case LABEL:
      return file.get_label_name(label_id);
    case VU_ACC:
      return "acc";
    case VU_Q:
      return "Q";
    case IMM_SYM:
      return sym;
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
void InstructionAtom::set_sym(std::string _sym) {
  kind = IMM_SYM;
  sym = std::move(_sym);
}

/*!
 * Get as register, or error if not a register.
 */
Register InstructionAtom::get_reg() const {
  assert(kind == REGISTER);
  return reg;
}

/*!
 * Get as integer immediate, or error if not an integer immediate.
 */
int32_t InstructionAtom::get_imm() const {
  assert(kind == IMM);
  return imm;
}

/*!
 * Get as label index, or error if not a label.
 */
int InstructionAtom::get_label() const {
  assert(kind == LABEL);
  return label_id;
}

/*!
 * Get as symbol, or error if not a symbol.
 */
std::string InstructionAtom::get_sym() const {
  assert(kind == IMM_SYM);
  return sym;
}

/*!
 * True if this atom is some sort of constant that doesn't involve linking.
 */
bool InstructionAtom::is_link_or_label() const {
  return kind == IMM_SYM || kind == LABEL;
}

/*!
 * Convert entire instruction to a string.
 */
std::string Instruction::to_string(const LinkedObjectFile& file) const {
  auto& info = gOpcodeInfo[(int)kind];

  // the name
  std::string result = info.name;

  // optional "interlock" specification.
  if (il != 0xff) {
    result.append(il ? ".i" : ".ni");
  }

  // optional "broadcast" specification for COP2 opcodes.
  if (cop2_bc != 0xff) {
    switch (cop2_bc) {
      case 0:
        result.push_back('x');
        break;
      case 1:
        result.push_back('y');
        break;
      case 2:
        result.push_back('z');
        break;
      case 3:
        result.push_back('w');
        break;
      default:
        result.push_back('?');
        break;
    }
  }

  // optional "destination" specification for COP2 opcodes.
  if (cop2_dest != 0xff) {
    result += ".";
    if (cop2_dest & 8)
      result.push_back('x');
    if (cop2_dest & 4)
      result.push_back('y');
    if (cop2_dest & 2)
      result.push_back('z');
    if (cop2_dest & 1)
      result.push_back('w');
  }

  // relative store and load instructions have a special syntax in MIPS
  if (info.is_store) {
    assert(n_dst == 0);
    assert(n_src == 3);
    result += " ";
    result += src[0].to_string(file);
    result += ", ";
    result += src[1].to_string(file);
    result += "(";
    result += src[2].to_string(file);
    result += ")";
  } else if (info.is_load) {
    assert(n_dst == 1);
    assert(n_src == 2);
    result += " ";
    result += dst[0].to_string(file);
    result += ", ";
    result += src[0].to_string(file);
    result += "(";
    result += src[1].to_string(file);
    result += ")";
  } else {
    // for instructions that aren't a store or load, the dest/sources are comma separated.
    bool end_comma = false;

    for (uint8_t i = 0; i < n_dst; i++) {
      result += " " + dst[i].to_string(file) + ",";
      end_comma = true;
    }

    for (uint8_t i = 0; i < n_src; i++) {
      result += " " + src[i].to_string(file) + ",";
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
  assert(n_dst < MAX_INTRUCTION_DEST);
  dst[n_dst++] = a;
}

/*!
 * Add a source atom to this Instruction
 */
void Instruction::add_src(InstructionAtom& a) {
  assert(n_src < MAX_INSTRUCTION_SOURCE);
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
  assert(false);
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
  assert(idx < n_dst);
  return dst[idx];
}

/*!
 * Safe get src atom
 */
InstructionAtom& Instruction::get_src(size_t idx) {
  assert(idx < n_src);
  return src[idx];
}

/*!
 * Safe get dst atom
 */
const InstructionAtom& Instruction::get_dst(size_t idx) const {
  assert(idx < n_dst);
  return dst[idx];
}

/*!
 * Safe get src atom
 */
const InstructionAtom& Instruction::get_src(size_t idx) const {
  assert(idx < n_src);
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
      assert(result == -1);
      result = src[i].get_label();
    }
  }
  return result;
}
