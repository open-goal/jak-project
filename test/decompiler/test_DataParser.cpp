
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

TEST_F(DataDecompTest, ContinuePoint) {
  std::string input =
      "    .type continue-point\n"
      "L63:\n"
      "    .word L69\n"
      "    .symbol finalboss\n"
      "    .word 0x0\n"
      "    .word 0x4b303728\n"
      "    .word 0x4a073f00\n"
      "    .word 0xcb94152d\n"
      "    .word 0x3f800000\n"
      "    .word 0x0\n"
      "    .word 0x3f3b851f\n"
      "    .word 0x0\n"
      "    .word 0x3f2e425b\n"
      "    .word 0x4b2faddf\n"
      "    .word 0x4a0869de\n"
      "    .word 0xcb94485e\n"
      "    .word 0x3f800000\n"
      "    .word 0x3f169ad4\n"
      "    .word 0x0\n"
      "    .word 0xbf4ef9db\n"
      "    .word 0x3ddbf488\n"
      "    .word 0x3f7db8bb\n"
      "    .word 0x3d9ff2e5\n"
      "    .word 0x3f4d288d\n"
      "    .word 0xbe07fcb9\n"
      "    .word 0x3f15460b\n"
      "    .word L64\n"
      "    .symbol fin\n"
      "    .symbol finalboss\n"
      "    .symbol display\n"
      "    .symbol citadel\n"
      "    .symbol special\n"
      "    .word 0x0\n"
      "L64: (offset 2)\n"
      "    .word L65\n"
      "    .empty-list\n"
      "L65: (offset 2)\n"
      "    .symbol special\n"
      "    .word L66\n"
      "L66: (offset 2)\n"
      "    .word L68\n"
      "    .word L67\n"
      "L67: (offset 2)\n"
      "    .symbol #t\n"
      "    .empty-list\n"
      "    .type string\n"
      "L68:\n"
      "    .word 0x10\n"
      "    .word 0x62746963\n"
      "    .word 0x6978652d\n"
      "    .word 0x6c702d74\n"
      "    .word 0x342d7461\n"
      "    .word 0x0\n"
      "    .word 0x0\n"
      "    .type string\n"
      "L69:\n"
      "    .word 0xf\n"
      "    .word 0x616e6966\n"
      "    .word 0x736f626c\n"
      "    .word 0x74732d73\n"
      "    .word 0x747261";
  auto parsed = parse_data(input);
  auto decomp =
      decompile_at_label_guess_type(parsed.label("L63"), parsed.labels, {parsed.words}, dts->ts);
  check_forms_equal(decomp.print(),
                    "(new 'static 'continue-point\n"
                    "     :name \"finalboss-start\"\n"
                    "     :level 'finalboss\n"
                    "     :trans (new 'static 'vector\n"
                    "                 :x 11548456.0\n"
                    "                 :y 2215872.0\n"
                    "                 :z -19409498.0\n"
                    "                 :w 1.0\n"
                    "                 )\n"
                    "     :quat (new 'static 'vector\n"
                    "                :y 0.7325\n"
                    "                :w 0.6807\n"
                    "                )\n"
                    "     :camera-trans (new 'static 'vector\n"
                    "                        :x 11513311.0\n"
                    "                        :y 2234999.5\n"
                    "                        :z -19435708.0\n"
                    "                        :w 1.0\n"
                    "                        )\n"
                    "     :camera-rot (new 'static 'array 'float 9\n"
                    "                      0.5883\n"
                    "                      0.0\n"
                    "                      -0.8085\n"
                    "                      0.1074\n"
                    "                      0.9911\n"
                    "                      0.0781\n"
                    "                      0.8014\n"
                    "                      -0.1328\n"
                    "                      0.5831\n"
                    "                      )\n"
                    "     :load-commands '('('special \"citb-exit-plat-4\" #t))\n"
                    "     :vis-nick 'fin\n"
                    "     :lev0 'finalboss\n"
                    "     :disp0 'display\n"
                    "     :lev1 'citadel\n"
                    "     :disp1 'special\n"
                    "     )");
}

TEST_F(DataDecompTest, FloatArray) {
  std::string input =
      "    .type continue-point\n"
      "L63:\n"
      "    .word 0x3f800000\n"
      "    .word 0x0\n"
      "    .word 0x3f800000\n"
      "    .word 0x0\n"
      "    .word 0x3f800000\n"
      "    .word 0x0\n"
      "    .word 0x3f800000\n\n";
  auto parsed = parse_data(input);
  auto decomp = decompile_at_label_with_hint({"(pointer float)", true, 7}, parsed.label("L63"),
                                             parsed.labels, {parsed.words}, *dts);
  check_forms_equal(decomp.print(),
                    "(new 'static 'array 'float 7\n"
                    "1.0 0.0 1.0 0.0 1.0 0.0 1.0)");
}

TEST_F(DataDecompTest, Bitfield) {
  // this is for testing bitfields from a 64-bit static constant.
  std::string input =
      "L80:\n"
      "    .word 0x80400040\n"
      "    .word 0x0";
  auto parsed = parse_data(input);
  auto& ts = dts->ts;
  auto typespec = ts.make_typespec("rgba");
  auto info = dynamic_cast<BitFieldType*>(ts.lookup_type(typespec));
  auto decomp =
      decompile_bitfield(typespec, info, parsed.label("L80"), parsed.labels, {parsed.words}, ts);
  check_forms_equal(decomp.print(), "(new 'static 'rgba :r #x40 :b #x40 :a #x80)");
}