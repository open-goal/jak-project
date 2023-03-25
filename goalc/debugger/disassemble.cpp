#include "disassemble.h"

#include "common/goos/Reader.h"

#include "Zydis/Zydis.h"
#include "goalc/compiler/Env.h"
#include "goalc/compiler/IR.h"

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"

std::string disassemble_x86(u8* data, int len, u64 base_addr) {
  std::string result;
  ZydisDecoder decoder;
  ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_STACK_WIDTH_64);
  ZydisFormatter formatter;
  ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);
  ZydisDecodedInstruction instr;
  ZydisDecodedOperand op[ZYDIS_MAX_OPERAND_COUNT_VISIBLE];

  constexpr int print_buff_size = 512;
  char print_buff[print_buff_size];
  int offset = 0;
  while (ZYAN_SUCCESS(ZydisDecoderDecodeFull(&decoder, data + offset, len - offset, &instr, op,
                                             ZYDIS_MAX_OPERAND_COUNT_VISIBLE,
                                             ZYDIS_DFLAG_VISIBLE_OPERANDS_ONLY))) {
    result += fmt::format("[0x{:x}] ", base_addr);
    ZydisFormatterFormatInstruction(&formatter, &instr, op, instr.operand_count_visible, print_buff,
                                    print_buff_size, base_addr);
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
  ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_STACK_WIDTH_64);
  ZydisFormatter formatter;
  ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);
  ZydisDecodedInstruction instr;
  ZydisDecodedOperand op[ZYDIS_MAX_OPERAND_COUNT_VISIBLE];

  constexpr int print_buff_size = 512;
  char print_buff[print_buff_size];
  int offset = 0;

  ASSERT(highlight_addr >= base_addr);
  int mark_offset = int(highlight_addr - base_addr);
  while (offset < len) {
    char prefix = (offset == mark_offset) ? '-' : ' ';
    if (ZYAN_SUCCESS(ZydisDecoderDecodeFull(&decoder, data + offset, len - offset, &instr, op,
                                            ZYDIS_MAX_OPERAND_COUNT_VISIBLE,
                                            ZYDIS_DFLAG_VISIBLE_OPERANDS_ONLY))) {
      result += fmt::format("{:c} [0x{:x}] ", prefix, base_addr);
      ZydisFormatterFormatInstruction(&formatter, &instr, op, instr.operand_count_visible,
                                      print_buff, print_buff_size, base_addr);
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

// how many "forms" to look at ahead of / behind rip when stopping
static constexpr int FORM_DUMP_SIZE_REV = 4;
static constexpr int FORM_DUMP_SIZE_FWD = 4;

std::string disassemble_x86_function(
    u8* data,
    int len,
    const goos::Reader* reader,
    u64 base_addr,
    u64 highlight_addr,
    const std::vector<InstructionInfo>& x86_instructions,
    const std::vector<std::shared_ptr<goos::HeapObject>>& code_sources,
    const std::vector<std::string>& ir_strings,
    bool* had_failure,
    bool print_whole_function) {
  std::string result;
  ZydisDecoder decoder;
  ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_STACK_WIDTH_64);
  ZydisFormatter formatter;
  ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);
  ZydisDecodedInstruction instr;
  ZydisDecodedOperand op[ZYDIS_MAX_OPERAND_COUNT_VISIBLE];

  constexpr int print_buff_size = 512;
  char print_buff[print_buff_size];
  int offset = 0;

  int current_instruction_idx = -1;
  int current_ir_idx = -1;
  int current_src_idx = -1;
  int rip_src_idx = -1;

  std::string current_filename;
  int current_file_line = -1;
  int current_offset_in_line = -1;

  std::vector<std::pair<int, std::string>> lines;

  ASSERT(highlight_addr >= base_addr);
  int mark_offset = int(highlight_addr - base_addr);
  while (offset < len) {
    char prefix = (offset == mark_offset) ? '-' : ' ';
    if (ZYAN_SUCCESS(ZydisDecoderDecodeFull(&decoder, data + offset, len - offset, &instr, op,
                                            ZYDIS_MAX_OPERAND_COUNT_VISIBLE,
                                            ZYDIS_DFLAG_VISIBLE_OPERANDS_ONLY))) {
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

      if (current_ir_idx >= 0 && current_ir_idx < int(ir_strings.size())) {
        auto source = reader->db.try_get_short_info(code_sources.at(current_ir_idx));
        if (source) {
          if (source->filename != current_filename ||
              source->line_idx_to_display != current_file_line ||
              source->pos_in_line != current_offset_in_line) {
            current_filename = source->filename;
            current_file_line = source->line_idx_to_display;
            current_offset_in_line = source->pos_in_line;
            ++current_src_idx;
            line +=
                fmt::format(fmt::emphasis::bold, "\n{}:{}\n", current_filename, current_file_line);
            line += fmt::format(fg(fmt::color::orange), "-> {}\n", source->line_text);
            std::string pointer(current_offset_in_line + 3, ' ');
            pointer += "^\n";
            line += fmt::format(fmt::emphasis::bold | fg(fmt::color::lime_green), "{}", pointer);
          }
        }
      }

      if (prefix != ' ') {
        line += fmt::format(fmt::emphasis::bold | fg(fmt::color::red), "{:c} [0x{:X}] ", prefix,
                            base_addr);
        rip_src_idx = current_src_idx;
      } else {
        line += fmt::format("{:c} [0x{:X}] ", prefix, base_addr);
      }

      ZydisFormatterFormatInstruction(&formatter, &instr, op, instr.operand_count_visible,
                                      print_buff, print_buff_size, base_addr);
      line += print_buff;

      if (print_ir && current_ir_idx >= 0 && current_ir_idx < int(ir_strings.size())) {
        if (line.size() < 50) {
          line.append(50 - line.size(), ' ');
        }
        line += " ";
        line += ir_strings.at(current_ir_idx);
      }

      if (warn_messed_up) {
        line += " ;; function's instruction do not align with debug data, something is wrong.";
      }
      line += "\n";
      lines.push_back(std::make_pair(current_src_idx, line));
      offset += instr.length;
      base_addr += instr.length;
    } else {
      lines.push_back(std::make_pair(
          current_src_idx,
          fmt::format("{:c} [0x{:x}] INVALID (0x{:02x})\n", prefix, base_addr, data[offset])));
      offset++;
    }
  }

  for (auto& line : lines) {
    if (print_whole_function || (line.first >= rip_src_idx - FORM_DUMP_SIZE_REV &&
                                 line.first < rip_src_idx + FORM_DUMP_SIZE_FWD)) {
      result.append(line.second);
    }
  }

  return result;
}
