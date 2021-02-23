
#include "gtest/gtest.h"
#include "decompiler/util/DataParser.h"
#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/util/data_decompile.h"

using namespace decompiler;

TEST(DataParser, Basic) {
  // just test we can parse a very simple data.
  std::string input =
      "L123: (offset 2)\n"
      "    .type test\n"
      "    .symbol #t\n"
      "L234:\n"
      "    .empty-list\n"
      "    .word L123\n"
      "    .word L555\n"
      "L555:\n"
      "    .symbol asdf\n"
      "    .word 0x0\n"
      "    .word 0xdeadbeef\n"
      "    .word 0xffffffff\n";
  auto result = parse_data(input);
  EXPECT_EQ(input, result.print());
}

TEST(DataParser, FromDecomp) {
  std::string input =
      "    .symbol finalboss\n"
      "    .empty-list\n"
      "    .symbol *finalboss-mood*\n"
      "    .symbol update-mood-finalboss\n"
      "    .symbol #f\n"
      "    .symbol #t\n"
      "    .word 0x3f800000\n"
      "    .word L55\n"
      "    .empty-list\n"
      "    .word 0x64\n"
      "    .empty-list\n"
      "    .empty-list\n"
      "    .word 0x0\n"
      "L63:\n"
      "    .word 0xffffffff\n"
      "L62:\n"
      "    .word 0xffffffff\n"
      "L968:\n"
      "    .word L54\n"
      "    .word 0x5b\n"
      "    .word 0xc8e40000\n"
      "    .word L53\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .symbol #f\n"
      "    .word 0x0\n"
      "L53: (offset 2)\n"
      "    .word L968\n"
      "    .empty-list\n"
      "L54:\n"
      "    .word 0x4b34a000\n"
      "    .word 0x4a020000\n"
      "    .word 0xcb956000\n"
      "    .word 0x493e0000\n"
      "L55: (offset 2)\n"
      "    .word L63\n"
      "    .word L56\n"
      "L56: (offset 2)\n"
      "    .word L57\n"
      "    .empty-list\n"
      "    .type continue-point\n"
      "L57:\n"
      "    .word L62\n"
      "    .symbol finalboss\n"
      "    .word 0x0\n"
      "    .word 0x4b3b814f\n"
      "    .word 0x49f088ef\n"
      "    .word 0xcb976ea5\n"
      "    .word 0x3f800000\n"
      "    .word 0x0\n"
      "    .word 0xbf0930be\n"
      "    .word 0x0\n";
  auto result = parse_data(input);
  EXPECT_EQ(input, result.print());
}

TEST(DataDecompiler, String) {
  TypeSystem ts;
  std::string input =
      "    .type string\n"
      "L62:\n"
      "    .word 0xf\n"
      "    .word 0x616e6966\n"
      "    .word 0x736f626c\n"
      "    .word 0x69662d73\n"
      "    .word 0x746867\n"
      "    .word 0x0\n"
      "    .word 0x0\n";
  auto parsed = parse_data(input);
  auto decomp = decompile_at_label_guess_type(parsed.label("L62"), {parsed.words}, ts);
  EXPECT_EQ(decomp.print(), "\"finalboss-fight\"");
}