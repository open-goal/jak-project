#include "InstructionParser.h"

#include <algorithm>
#include <optional>
#include <stdexcept>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "fmt/core.h"

namespace decompiler {
InstructionParser::InstructionParser() {
  init_opcode_info();

  // we only support a subset of the total instructions. These are common used and don't have
  // strange formatting.
  int added = 0;
  for (auto i : {InstructionKind::DADDIU, InstructionKind::ADDIU,  InstructionKind::SLTI,
                 InstructionKind::SLTIU,  InstructionKind::SB,     InstructionKind::SH,
                 InstructionKind::SW,     InstructionKind::SD,     InstructionKind::SQ,
                 InstructionKind::LB,     InstructionKind::LBU,    InstructionKind::LH,
                 InstructionKind::LHU,    InstructionKind::LW,     InstructionKind::LWU,
                 InstructionKind::LD,     InstructionKind::LQ,     InstructionKind::LDR,
                 InstructionKind::LDL,    InstructionKind::LWL,    InstructionKind::LWR,
                 InstructionKind::DADDU,  InstructionKind::SUBU,   InstructionKind::ADDU,
                 InstructionKind::DSUBU,  InstructionKind::MULT3,  InstructionKind::MULTU3,
                 InstructionKind::AND,    InstructionKind::OR,     InstructionKind::NOR,
                 InstructionKind::XOR,    InstructionKind::MOVN,   InstructionKind::MOVZ,
                 InstructionKind::SLT,    InstructionKind::SLTU,   InstructionKind::SLL,
                 InstructionKind::SRA,    InstructionKind::SRL,    InstructionKind::DSLL,
                 InstructionKind::DSLL32, InstructionKind::DSRA,   InstructionKind::DSRA32,
                 InstructionKind::DSRL,   InstructionKind::DSRL32, InstructionKind::DSRAV,
                 InstructionKind::SLLV,   InstructionKind::DSLLV,  InstructionKind::DSRLV,
                 InstructionKind::DIV,    InstructionKind::DIVU,   InstructionKind::ORI,
                 InstructionKind::XORI,   InstructionKind::ANDI,   InstructionKind::LUI,
                 InstructionKind::JALR,   InstructionKind::JR,     InstructionKind::LWC1,
                 InstructionKind::SWC1,   InstructionKind::ADDS,   InstructionKind::SUBS,
                 InstructionKind::MULS,   InstructionKind::DIVS,   InstructionKind::MINS,
                 InstructionKind::MAXS,   InstructionKind::MADDS,  InstructionKind::MSUBS,
                 InstructionKind::RSQRTS, InstructionKind::ABSS,   InstructionKind::NEGS,
                 InstructionKind::CVTSW,  InstructionKind::CVTWS,  InstructionKind::MOVS,
                 InstructionKind::SQRTS,  InstructionKind::CLTS,   InstructionKind::CLES,
                 InstructionKind::CEQS,   InstructionKind::BC1F,   InstructionKind::BC1T,
                 InstructionKind::BEQ,    InstructionKind::BNE,    InstructionKind::BEQL,
                 InstructionKind::BNEL,   InstructionKind::BC1FL,  InstructionKind::BC1TL,
                 InstructionKind::BLTZ,   InstructionKind::BGEZ,   InstructionKind::BLEZ,
                 InstructionKind::BGTZ,   InstructionKind::BLTZL,  InstructionKind::BGTZL,
                 InstructionKind::BGEZL,  InstructionKind::MTC1,   InstructionKind::MFC1,
                 InstructionKind::MFLO,   InstructionKind::MFHI,   InstructionKind::MTLO1,
                 InstructionKind::MFLO1,  InstructionKind::SYNCL,  InstructionKind::PCPYUD,
                 InstructionKind::PEXTUW, InstructionKind::POR,    InstructionKind::VMOVE,
                 InstructionKind::VSUB,   InstructionKind::LQC2,   InstructionKind::SQC2,
                 InstructionKind::MULAS,  InstructionKind::MADDAS, InstructionKind::QMTC2,
                 InstructionKind::QMFC2,  InstructionKind::VITOF0, InstructionKind::VFTOI0,
                 InstructionKind::PSLLW,  InstructionKind::PSRAW}) {
    auto& info = gOpcodeInfo[int(i)];
    if (info.defined) {
      m_opcode_name_lookup[info.name] = int(i);
      added++;
    }
  }

  for (auto i : {InstructionKind::VMUL_BC}) {
    auto& info = gOpcodeInfo[int(i)];
    if (info.defined) {
      m_opcode_name_broadcast_lookup[info.name] = int(i);
      added++;
    }
  }
  ASSERT(added == int(m_opcode_name_lookup.size()) + int(m_opcode_name_broadcast_lookup.size()));
}

namespace {
std::string get_until_space(std::string& instr) {
  ASSERT(!instr.empty());
  size_t i;
  for (i = 0; i < instr.length(); i++) {
    if (instr[i] == ' ') {
      break;
    }
  }
  auto name = instr.substr(0, i);
  if (i == instr.length()) {
    instr.clear();
  } else {
    instr = instr.substr(i + 1);
  }
  return name;
}

std::string get_instr_name(std::string& instr) {
  ASSERT(!instr.empty());
  size_t i;
  for (i = 0; i < instr.length(); i++) {
    if (instr[i] == ' ') {
      break;
    }

    // add.s should not stop at the .
    if (instr.size() > (i + 1) && instr[0] == 'v' && instr[i + 1] != 's' && instr[i + 1] != 'l' &&
        instr[i + 1] != 'e' && instr[i] == '.') {
      break;
    }
  }
  auto name = instr.substr(0, i);

  // qmXc2.i should not grab the i.
  if (name == "qmtc2.i") {
    name = "qmtc2";  // strip .i
    i -= 2;          // leave the i for the next step.
  }

  if (name == "qmfc2.i") {
    name = "qmfc2";  // strip .i
    i -= 2;          // leave the i for the next step.
  }

  if (i == instr.length()) {
    instr.clear();
  } else {
    instr = instr.substr(i + 1);
  }
  return name;
}

std::string get_comma_separated(std::string& instr) {
  ASSERT(!instr.empty());
  auto arg = get_until_space(instr);
  if (instr.empty()) {
    ASSERT(arg.back() != ',');
  } else {
    ASSERT(arg.back() == ',');
    arg.pop_back();
  }
  return arg;
}

std::string get_before_paren(std::string& instr) {
  size_t i;
  for (i = 0; i < instr.length(); i++) {
    if (instr[i] == '(') {
      auto result = instr.substr(0, i);
      instr = instr.substr(i);
      return result;
    }
  }
  ASSERT(false);
  return {};
}

std::string get_in_paren(std::string& instr) {
  ASSERT(instr.length() > 2);
  ASSERT(instr.front() == '(');
  size_t i;
  for (i = 0; i < instr.length(); i++) {
    if (instr[i] == ')') {
      auto result = instr.substr(1, i - 1);
      if (i == instr.length()) {
        instr.clear();
      } else {
        instr = instr.substr(i + 1);
      }
      return result;
    }
  }
  ASSERT(false);
  return {};
}

bool is_integer(const std::string& str) {
  ASSERT(!str.empty());
  char* end;
  std::strtol(str.c_str(), &end, 10);
  return end == str.c_str() + str.length();
}

int parse_integer(const std::string& str) {
  ASSERT(!str.empty());
  char* end;
  int result = std::strtol(str.c_str(), &end, 10);
  ASSERT(end == str.c_str() + str.length());
  return result;
}

std::vector<std::string> string_to_lines(const std::string& str) {
  std::vector<std::string> result;
  std::string::size_type i;
  std::string::size_type start = 0;
  while (true) {
    i = str.find('\n', start);
    if (i == std::string::npos) {
      if (start < str.length()) {
        result.push_back(str.substr(start));
      }
      return result;
    } else {
      result.push_back(str.substr(start, i - start));
      start = i + 1;
    }
  }
}

u8 cop2_dst(const std::string& str) {
  auto ptr = str.data();
  u8 result = 0;
  if (!*ptr) {
    return result;
  }
  if (*ptr == 'x') {
    result |= 8;
    ptr++;
  }

  if (!*ptr) {
    return result;
  }
  if (*ptr == 'y') {
    result |= 4;
    ptr++;
  }

  if (!*ptr) {
    return result;
  }
  if (*ptr == 'z') {
    result |= 2;
    ptr++;
  }

  if (!*ptr) {
    return result;
  }
  if (*ptr == 'w') {
    result |= 1;
    ptr++;
  }

  if (*ptr) {
    ASSERT(false);
  }
  return result;
}
}  // namespace

Instruction InstructionParser::parse_single_instruction(
    std::string str,
    const std::vector<DecompilerLabel>& labels) {
  auto name = get_instr_name(str);

  std::optional<int> op_idx;
  auto lookup = m_opcode_name_lookup.find(name);
  if (lookup == m_opcode_name_lookup.end()) {
    // it might be a VU with broadcast.
    if (!name.empty() && name.front() == 'v') {
      char last_char = name.back();
      if (last_char == 'x' || last_char == 'y' || last_char == 'z' || last_char == 'w') {
        str.insert(str.begin(), ' ');
        str.insert(str.begin(), last_char);
        name.pop_back();
        auto bc_lookup = m_opcode_name_broadcast_lookup.find(name);
        if (bc_lookup != m_opcode_name_broadcast_lookup.end()) {
          op_idx = bc_lookup->second;
        }
      }
    }
  } else {
    op_idx = lookup->second;
  }

  if (!op_idx) {
    throw std::runtime_error("InstructionParser cannot handle opcode " + name);
  }

  Instruction instr;
  instr.kind = InstructionKind(*op_idx);
  auto& info = gOpcodeInfo[*op_idx];
  for (u8 i = 0; i < info.step_count; i++) {
    auto& step = info.steps[i];
    switch (step.decode) {
      case DecodeType::GPR: {
        std::string gpr_name;
        if ((info.is_store || info.is_load) && i == 2) {
          gpr_name = get_in_paren(str);
        } else {
          gpr_name = get_comma_separated(str);
        }

        Register reg(gpr_name);
        ASSERT(reg.get_kind() == Reg::GPR);
        InstructionAtom atom;
        atom.set_reg(reg);
        if (step.is_src) {
          instr.add_src(atom);
        } else {
          instr.add_dst(atom);
        }
      } break;

      case DecodeType::FPR: {
        auto reg_name = get_comma_separated(str);
        Register reg(reg_name);
        ASSERT(reg.get_kind() == Reg::FPR);
        InstructionAtom atom;
        atom.set_reg(reg);
        if (step.is_src) {
          instr.add_src(atom);
        } else {
          instr.add_dst(atom);
        }
      } break;

      case DecodeType::VF: {
        auto reg_name = get_comma_separated(str);
        Register reg(reg_name);
        ASSERT(reg.get_kind() == Reg::VF);
        InstructionAtom atom;
        atom.set_reg(reg);
        if (step.is_src) {
          instr.add_src(atom);
        } else {
          instr.add_dst(atom);
        }
      } break;

      case DecodeType::IMM: {
        InstructionAtom atom;
        std::string atom_str;
        if ((info.is_store || info.is_load) && i == 1) {
          // number before paren
          atom_str = get_before_paren(str);
        } else {
          atom_str = get_comma_separated(str);
        }

        if (is_integer(atom_str)) {
          auto amt = parse_integer(atom_str);
          atom.set_imm(amt);
        } else if (!atom_str.empty() && atom_str.front() == 'L') {
          bool found_label = false;
          for (size_t id = 0; id < labels.size(); id++) {
            if (labels[id].name == atom_str) {
              found_label = true;
              atom.set_label(id);
              break;
            }
          }
          if (!found_label) {
            atom.set_sym(atom_str);
          }
        } else {
          atom.set_sym(atom_str);
        }
        if (step.is_src) {
          instr.add_src(atom);
        } else {
          instr.add_dst(atom);
        }

      } break;

      case DecodeType::BRANCH_TARGET: {
        auto label = get_comma_separated(str);
        auto f = std::find_if(labels.begin(), labels.end(),
                              [&](const DecompilerLabel& l) { return l.name == label; });
        ASSERT(f != labels.end());
        auto idx = f - labels.begin();
        InstructionAtom atom;
        atom.set_label(idx);
        if (step.is_src) {
          instr.add_src(atom);
        } else {
          instr.add_dst(atom);
        }
      } break;

      case DecodeType::DEST: {
        auto thing = get_until_space(str);
        instr.cop2_dest = cop2_dst(thing);
        break;
      }

      case DecodeType::IL: {
        auto thing = get_until_space(str);
        if (thing == "i") {
          instr.il = 1;
        } else if (thing == "ni") {
          instr.il = 0;
        } else {
          ASSERT_MSG(false, fmt::format("Bad interlock specification. Got {}", thing.c_str()));
        }
      } break;

      case DecodeType::BC: {
        auto thing = get_until_space(str);
        if (thing == "x") {
          instr.cop2_bc = 0;
        } else if (thing == "y") {
          instr.cop2_bc = 1;
        } else if (thing == "z") {
          instr.cop2_bc = 2;
        } else if (thing == "w") {
          instr.cop2_bc = 3;
        } else {
          ASSERT_MSG(false, fmt::format("Bad broadcast. Got {}", thing.c_str()));
        }
      } break;

      default:
        ASSERT_MSG(false, fmt::format("missing DecodeType: {}", (int)step.decode));
    }
  }

  ASSERT(str.empty());
  return instr;
}

ParsedProgram InstructionParser::parse_program(const std::string& str,
                                               const std::vector<std::string>& predefined_labels) {
  ParsedProgram program;
  for (auto& x : predefined_labels) {
    DecompilerLabel label;
    label.target_segment = 0;
    label.offset = 0;
    label.name = x;
    program.labels.push_back(label);
  }

  auto lines = string_to_lines(str);
  int byte_offset = 0;
  // first pass
  for (auto& line : lines) {
    // strip off leading white space
    size_t i;
    for (i = 0; i < line.length(); i++) {
      if (line[i] != ' ') {
        line = line.substr(i);
        break;
      }
    }

    if (line.empty()) {
      continue;
    }

    if (line.front() == 'L') {
      if (line.back() == ':') {
        line.pop_back();
      } else {
        ASSERT(false);
      }
      DecompilerLabel label;
      label.target_segment = 0;
      label.offset = byte_offset;
      label.name = line;
      program.labels.push_back(label);
    } else {
      byte_offset += 4;
    }
  }

  // second pass
  for (auto& line : lines) {
    if (!line.empty() && line.front() != 'L') {
      program.instructions.push_back(parse_single_instruction(line, program.labels));
    }
  }

  return program;
}

std::string ParsedProgram::print() {
  std::string result;

  int offset = 0;
  for (auto& instr : instructions) {
    for (auto& label : labels) {
      if (label.offset == offset) {
        result += label.name;
        result += ":\n";
      }
    }
    result += ' ';
    result += ' ';
    result += instr.to_string(labels);
    result += '\n';
    offset += 4;
  }
  return result;
}
}  // namespace decompiler
