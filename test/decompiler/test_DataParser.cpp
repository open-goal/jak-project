
#include "gtest/gtest.h"
#include "decompiler/util/DataParser.h"
#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/util/data_decompile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "third-party/fmt/core.h"
#include "common/goos/PrettyPrinter.h"

using namespace decompiler;

class DataDecompTest : public ::testing::Test {
 protected:
  static std::unique_ptr<decompiler::DecompilerTypeSystem> dts;

  static void SetUpTestCase() {
    dts = std::make_unique<DecompilerTypeSystem>();
    dts->parse_type_defs({"decompiler", "config", "all-types.gc"});
  }

  static void TearDownTestCase() { dts.reset(); }

  void check_forms_equal(const std::string& expected, const std::string& actual) {
    auto expected_form =
        pretty_print::get_pretty_printer_reader().read_from_string(expected, false).as_pair()->car;
    auto actual_form =
        pretty_print::get_pretty_printer_reader().read_from_string(actual, false).as_pair()->car;
    if (expected_form != actual_form) {
      printf("Got:\n%s\n\nExpected\n%s\n", pretty_print::to_string(actual_form).c_str(),
             pretty_print::to_string(expected_form).c_str());
    }
    EXPECT_TRUE(expected_form == actual_form);
  }
};

std::unique_ptr<DecompilerTypeSystem> DataDecompTest::dts;

TEST_F(DataDecompTest, Basic) {
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

TEST_F(DataDecompTest, FromDecomp) {
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

TEST_F(DataDecompTest, String) {
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
  auto decomp =
      decompile_at_label_guess_type(parsed.label("L62"), parsed.labels, {parsed.words}, dts->ts);
  EXPECT_EQ(decomp.print(), "\"finalboss-fight\"");
}

TEST_F(DataDecompTest, SimpleStructure) {
  std::string input =
      "L217:\n"
      "    .word 0x7f\n"
      "    .word 0x1\n"
      "    .word 0x0\n"
      "    .word 0x2\n"
      "    .word L218\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .type string\n"
      "L218:\n"
      "    .word 0x6\n"
      "    .word 0x79637473\n"
      "    .word 0x6c63\n";
  auto parsed = parse_data(input);
  auto decomp = decompile_at_label(TypeSpec("vif-disasm-element"), parsed.label("L217"),
                                   parsed.labels, {parsed.words}, dts->ts);
  check_forms_equal(
      decomp.print(),
      "(new 'static 'vif-disasm-element :mask #x7f :tag #x1 :print #x2 :string1 \"stcycl\")");
}

TEST_F(DataDecompTest, VifDisasmArray) {
  std::string input =
      ".type array\n"
      "L148:\n"
      "    .word 0x3\n"
      "    .word 0x3\n"
      "    .type vif-disasm-element\n"
      "    .word L219\n"
      "    .word L217\n"
      "    .word L215\n"
      "    .word 0x0\n"
      "L215:\n"
      "    .word 0x7f\n"
      "    .word 0x2\n"
      "    .word 0x0\n"
      "    .word 0x1\n"
      "    .word L216\n"
      "    .word L216\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .type string\n"
      "L216:\n"
      "    .word 0x6\n"
      "    .word 0x7366666f\n"
      "    .word 0x7465\n"
      "L217:\n"
      "    .word 0x7f\n"
      "    .word 0x1\n"
      "    .word 0x0\n"
      "    .word 0x2\n"
      "    .word L218\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .type string\n"
      "L218:\n"
      "    .word 0x6\n"
      "    .word 0x79637473\n"
      "    .word 0x6c63\n"
      "L219:\n"
      "    .word 0x7f\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .word L220\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .type string\n"
      "L220:\n"
      "    .word 0x3\n"
      "    .word 0x706f6e\n"
      "    .word 0x0";
  auto parsed = parse_data(input);
  auto decomp =
      decompile_at_label_guess_type(parsed.label("L148"), parsed.labels, {parsed.words}, dts->ts);
  check_forms_equal(
      decomp.print(),
      "(new 'static 'boxed-array vif-disasm-element 3\n"
      "  (new 'static 'vif-disasm-element :mask #x7f :string1 \"nop\")\n"
      "  (new 'static 'vif-disasm-element :mask #x7f :tag #x1 :print #x2 :string1 \"stcycl\")\n"
      "  (new 'static 'vif-disasm-element :mask #x7f :tag #x2 :print #x1 :string1 \"offset\" "
      "      :string2 \"offset\"))");
}