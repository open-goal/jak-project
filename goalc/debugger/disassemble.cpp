#include "disassemble.h"
#include "Zydis/Zydis.h"
#include "third-party/fmt/core.h"

std::string disassemble_x86(u8* data, int len, u64 base_addr) {
  std::string result;
  ZydisDecoder decoder;
  ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_ADDRESS_WIDTH_64);
  ZydisFormatter formatter;
  ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);
  ZydisDecodedInstruction instr;

  constexpr int print_buff_size = 512;
  char print_buff[print_buff_size];
  int offset = 0;
  while (ZYAN_SUCCESS(ZydisDecoderDecodeBuffer(&decoder, data + offset, len - offset, &instr))) {
    result += fmt::format("[0x{:x}] ", base_addr);
    ZydisFormatterFormatInstruction(&formatter, &instr, print_buff, print_buff_size, base_addr);
    result += print_buff;
    result += "\n";

    offset += instr.length;
    base_addr += instr.length;
  }

  return result;
}

std::string disassemble_x86(u8* data, int len, u64 base_addr, u64 highlight_addr) {
  std::string result;
  ZydisDecoder decoder;
  ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_ADDRESS_WIDTH_64);
  ZydisFormatter formatter;
  ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);
  ZydisDecodedInstruction instr;

  constexpr int print_buff_size = 512;
  char print_buff[print_buff_size];
  int offset = 0;

  assert(highlight_addr >= base_addr);
  int mark_offset = int(highlight_addr - base_addr);
  while (offset < len) {
    char prefix = (offset == mark_offset) ? '-' : ' ';
    if (ZYAN_SUCCESS(ZydisDecoderDecodeBuffer(&decoder, data + offset, len - offset, &instr))) {
      result += fmt::format("{:c} [0x{:x}] ", prefix, base_addr);
      ZydisFormatterFormatInstruction(&formatter, &instr, print_buff, print_buff_size, base_addr);
      result += print_buff;
      result += "\n";
      offset += instr.length;
      base_addr += instr.length;
    } else {
      result += fmt::format("{:c} [0x{:x}] INVALID (0x{:02x})\n", prefix, base_addr, data[offset]);
      offset++;
    }
  }

  return result;
}

std::string disassemble_x86_function(u8* data,
                                     int len,
                                     u64 base_addr,
                                     u64 highlight_addr,
                                     const std::vector<InstructionInfo>& x86_instructions,
                                     const std::vector<std::string>& irs,
                                     bool* had_failure) {
  std::string result;
  ZydisDecoder decoder;
  ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_ADDRESS_WIDTH_64);
  ZydisFormatter formatter;
  ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);
  ZydisDecodedInstruction instr;

  constexpr int print_buff_size = 512;
  char print_buff[print_buff_size];
  int offset = 0;

  int current_instruction_idx = -1;
  int current_ir_idx = -1;

  assert(highlight_addr >= base_addr);
  int mark_offset = int(highlight_addr - base_addr);
  while (offset < len) {
    char prefix = (offset == mark_offset) ? '-' : ' ';
    if (ZYAN_SUCCESS(ZydisDecoderDecodeBuffer(&decoder, data + offset, len - offset, &instr))) {
      bool warn_messed_up = false;
      bool print_ir = false;
      // we should have a next instruction.
      if (current_instruction_idx + 1 >= int(x86_instructions.size())) {
        warn_messed_up = true;
        if (had_failure) {
          *had_failure = true;
        }
      } else {
        // we should line up with the next instruction
        if (x86_instructions.at(current_instruction_idx + 1).offset == offset) {
          // perfect, everything is lined up!
          current_instruction_idx++;
          while (current_instruction_idx + 1 < int(x86_instructions.size()) &&
                 x86_instructions.at(current_instruction_idx + 1).offset == offset) {
            current_instruction_idx++;
          }
        } else {
          printf("offset mess up, at %d, expected %d\n", offset,
                 x86_instructions.at(current_instruction_idx + 1).offset);
          warn_messed_up = true;
          if (had_failure) {
            *had_failure = true;
          }
        }
      }

      if (current_instruction_idx >= 0 && current_instruction_idx < int(x86_instructions.size())) {
        const auto& debug_instr = x86_instructions.at(current_instruction_idx);
        if (debug_instr.kind == InstructionInfo::Kind::IR && debug_instr.ir_idx != current_ir_idx) {
          current_ir_idx = debug_instr.ir_idx;
          print_ir = true;
        }
      }

      std::string line;

      line += fmt::format("{:c} [0x{:x}] ", prefix, base_addr);
      ZydisFormatterFormatInstruction(&formatter, &instr, print_buff, print_buff_size, base_addr);
      line += print_buff;

      if (print_ir && current_ir_idx >= 0 && current_ir_idx < int(irs.size())) {
        if (line.size() < 50) {
          line.append(50 - line.size(), ' ');
        }
        line += " ";
        line += irs.at(current_ir_idx);
      }

      if (warn_messed_up) {
        line += " ;; function's instruction do not align with debug data, something is wrong.";
      }
      line += "\n";
      result += line;
      offset += instr.length;
      base_addr += instr.length;
    } else {
      result += fmt::format("{:c} [0x{:x}] INVALID (0x{:02x})\n", prefix, base_addr, data[offset]);
      offset++;
    }
  }

  return result;
}