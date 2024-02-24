#include "common/common_types.h"

#include "gtest/gtest.h"

#include "third-party/zydis/include/Zydis/Zydis.h"

TEST(Zydis, Basic) {
  // int3, return
  u8 code[] = {0xcc, 0xc3};
  ZydisDecoder decoder;
  ZydisDecoderInit(&decoder, ZYDIS_MACHINE_MODE_LONG_64, ZYDIS_STACK_WIDTH_64);
  ZydisFormatter formatter;
  ZydisFormatterInit(&formatter, ZYDIS_FORMATTER_STYLE_INTEL);

  ZydisDecodedInstruction instr;
  ZydisDecodedOperand op[ZYDIS_MAX_OPERAND_COUNT];

  // should get first instruction
  EXPECT_TRUE(ZYAN_SUCCESS(ZydisDecoderDecodeFull(&decoder, code, 2, &instr, op)));
  char result[256];
  ZydisFormatterFormatInstruction(&formatter, &instr, op, instr.operand_count_visible, result,
                                  sizeof(result), 0, ZYAN_NULL);
  EXPECT_EQ(std::string("int3"), result);
  EXPECT_EQ(1, instr.length);

  // should get second instruction
  EXPECT_TRUE(ZYAN_SUCCESS(ZydisDecoderDecodeFull(&decoder, code + 1, 1, &instr, op)));
  ZydisFormatterFormatInstruction(&formatter, &instr, op, instr.operand_count_visible, result,
                                  sizeof(result), 0, ZYAN_NULL);
  EXPECT_EQ(std::string("ret"), result);
  EXPECT_EQ(1, instr.length);
}
