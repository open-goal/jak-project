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

  assert(highlight_addr > base_addr);
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