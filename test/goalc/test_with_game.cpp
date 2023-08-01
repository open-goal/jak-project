#include <cstdio>
#include <random>
#include <string>
#include <thread>

#include "common/util/FileUtil.h"

#include "game/mips2c/mips2c_table.h"
#include "game/runtime.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/listener/Listener.h"
#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

#include "third-party/fmt/core.h"

class WithGameTests : public ::testing::Test {
 public:
  static void SetUpTestSuite() {
    shared_compiler = std::make_unique<SharedCompiler>(GameVersion::Jak1);
    try {
      shared_compiler->compiler.run_test_no_load(
          "test/goalc/source_templates/with_game/test-build-game.gc");
      shared_compiler->compiler.run_front_end_on_string(
          "(asm-text-file text :files (\"test/test_data/test_game_text.gp\"))");
    } catch (std::exception& e) {
      fprintf(stderr, "caught exception %s\n", e.what());
      EXPECT_TRUE(false);
    }
    shared_compiler->runtime_thread = std::thread(GoalTest::runtime_with_kernel_jak1);
    shared_compiler->runner.c = &shared_compiler->compiler;

    shared_compiler->compiler.run_test_from_file(
        "test/goalc/source_templates/with_game/test-load-game.gc");
    shared_compiler->compiler.run_test_from_string("(set! *use-old-listener-print* #t)");
  }

  static void TearDownTestSuite() {
    shared_compiler->compiler.shutdown_target();
    shared_compiler->runtime_thread.join();
    shared_compiler.reset();
  }

  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  void TearDown() {}

  struct SharedCompiler {
    SharedCompiler(GameVersion v) : compiler(v) {}
    std::thread runtime_thread;
    Compiler compiler;
    GoalTest::CompilerTestRunner runner;
  };

  static std::unique_ptr<SharedCompiler> shared_compiler;

  std::string testCategory = "with_game";
};

std::unique_ptr<WithGameTests::SharedCompiler> WithGameTests::shared_compiler;

namespace {
std::vector<std::string> get_test_pass_string(const std::string& name, int count) {
  return {fmt::format("Test \"{}\": {} Passes\n0\n", name, count)};
}
}  // namespace

TEST_F(WithGameTests, MakeSystem) {
  shared_compiler->compiler.run_front_end_on_string("(make \"out/jak1/iso/ENGINE.CGO\")");
}

TEST_F(WithGameTests, ReturnConstant) {
  shared_compiler->runner.run_static_test(testCategory, "defun-return-constant.static.gc",
                                          {"12\n"});
}

TEST_F(WithGameTests, ReturnSymbol) {
  shared_compiler->runner.run_static_test(testCategory, "defun-return-symbol.static.gc", {"42\n"});
}

TEST_F(WithGameTests, MinMax) {
  shared_compiler->runner.run_static_test(testCategory, "test-min-max.gc", {"10\n"});
}

TEST_F(WithGameTests, BoxedFloat) {
  shared_compiler->runner.run_static_test(testCategory, "test-bfloat.gc",
                                          {"data 1.2330 print 1.2330 type bfloat\n0\n"});
}

TEST_F(WithGameTests, BasicTypeCheck) {
  shared_compiler->runner.run_static_test(testCategory, "test-basic-type-check.gc",
                                          {"#f#t#t#f#t#f#t#t\n0\n"});
}

TEST_F(WithGameTests, ConditionBoolean) {
  shared_compiler->runner.run_static_test(testCategory, "test-condition-boolean.gc", {"4\n"});
}

TEST_F(WithGameTests, TypeType) {
  shared_compiler->runner.run_static_test(testCategory, "test-type-type.gc", {"#t#f\n0\n"});
}

TEST_F(WithGameTests, AccessInlineArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-access-inline-array.gc",
                                          {"1.2345\n0\n"});
}

TEST_F(WithGameTests, FindParentMethod) {
  shared_compiler->runner.run_static_test(testCategory, "test-find-parent-method.gc",
                                          {"\"test pass!\"\n0\n"});
}

TEST_F(WithGameTests, Ref) {
  shared_compiler->runner.run_static_test(testCategory, "test-ref.gc", {"83\n"});
}

TEST_F(WithGameTests, PairASzie) {
  shared_compiler->runner.run_static_test(testCategory, "test-pair-asize.gc", {"8\n"});
}

TEST_F(WithGameTests, Last) {
  shared_compiler->runner.run_static_test(testCategory, "test-last.gc", {"d\n0\n"});
}

TEST_F(WithGameTests, Sort) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-sort.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(1234 654 72 56 32 25 24 16 -34)\n0\n"});
}

TEST_F(WithGameTests, Sort2) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-sort-2.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
}

TEST_F(WithGameTests, Sort3) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-sort-3.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
}

TEST_F(WithGameTests, PairLength) {
  shared_compiler->runner.run_static_test(testCategory, "test-pair-length.gc", {"6\n"});
}

TEST_F(WithGameTests, Member1) {
  shared_compiler->runner.run_static_test(testCategory, "test-member-1.gc", {"(c d)\n0\n"});
}

TEST_F(WithGameTests, Member2) {
  shared_compiler->runner.run_static_test(testCategory, "test-member-2.gc", {"#f\n0\n"});
}

TEST_F(WithGameTests, Assoc1) {
  shared_compiler->runner.run_static_test(testCategory, "test-assoc-1.gc", {"w\n0\n"});
}

TEST_F(WithGameTests, Assoc2) {
  shared_compiler->runner.run_static_test(testCategory, "test-assoc-2.gc", {"#f\n0\n"});
}

TEST_F(WithGameTests, Assoce1) {
  shared_compiler->runner.run_static_test(testCategory, "test-assoce-1.gc", {"x\n0\n"});
}

TEST_F(WithGameTests, Assoce2) {
  shared_compiler->runner.run_static_test(testCategory, "test-assoce-2.gc", {"x\n0\n"});
}

TEST_F(WithGameTests, Append) {
  shared_compiler->runner.run_static_test(testCategory, "test-append.gc", {"(a b c d e)\n0\n"});
}

TEST_F(WithGameTests, DeleteList) {
  shared_compiler->runner.run_static_test(testCategory, "test-delete-list.gc", {"(a b d e)\n0\n"});
}

TEST_F(WithGameTests, DeleteCar) {
  shared_compiler->runner.run_static_test(testCategory, "test-delete-car.gc",
                                          {"((a . b) (e . f))\n#f\n0\n"});
}

TEST_F(WithGameTests, InsertCar) {
  shared_compiler->runner.run_static_test(testCategory, "test-insert-cons.gc",
                                          {"((c . w) (a . b) (e . f))\n0\n"});
}

TEST_F(WithGameTests, InlineArrayClass) {
  shared_compiler->runner.run_static_test(testCategory, "test-new-inline-array-class.gc",
                                          {"2824\n"});
}

TEST_F(WithGameTests, Memcpy) {
  shared_compiler->runner.run_static_test(testCategory, "test-memcpy.gc", {"13\n"});
}

TEST_F(WithGameTests, Memset) {
  shared_compiler->runner.run_static_test(testCategory, "test-memset.gc", {"11\n"});
}

TEST_F(WithGameTests, BintegerPrint) {
  shared_compiler->runner.run_static_test(testCategory, "test-binteger-print.gc", {"-17\n0\n"});
}

TEST_F(WithGameTests, TestTests) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-tests.gc",
      {"Test Failed On Test 0: \"unknown\"\nTest Failed On Test 0: "
       "\"test\"\nTest \"test-of-test\": 1 Passes\n0\n"});
}

TEST_F(WithGameTests, TypeArrays) {
  shared_compiler->runner.run_static_test(testCategory, "test-type-arrays.gc",
                                          {"Test \"test-type-arrays\": 3 Passes\n0\n"});
}

TEST_F(WithGameTests, NumberComparison) {
  shared_compiler->runner.run_static_test(testCategory, "test-number-comparison.gc",
                                          {"Test \"number-comparison\": 14 Passes\n0\n"});
}

TEST_F(WithGameTests, ApproxPi) {
  shared_compiler->runner.run_static_test(testCategory, "test-approx-pi.gc",
                                          get_test_pass_string("approx-pi", 4));
}

TEST_F(WithGameTests, ApproxPiStack) {
  shared_compiler->runner.run_static_test(testCategory, "test-approx-pi-stack.gc",
                                          get_test_pass_string("approx-pi-stack", 4));
}

TEST_F(WithGameTests, DynamicType) {
  shared_compiler->runner.run_static_test(testCategory, "test-dynamic-type.gc",
                                          get_test_pass_string("dynamic-type", 4));
}

TEST_F(WithGameTests, StringType) {
  shared_compiler->runner.run_static_test(testCategory, "test-string-type.gc",
                                          get_test_pass_string("string-type", 4));
}

TEST_F(WithGameTests, NewString) {
  shared_compiler->runner.run_static_test(testCategory, "test-new-string.gc",
                                          get_test_pass_string("new-string", 5));
}

TEST_F(WithGameTests, AddrOf) {
  shared_compiler->runner.run_static_test(testCategory, "test-addr-of.gc",
                                          get_test_pass_string("addr-of", 2));
}

TEST_F(WithGameTests, SetSelf) {
  shared_compiler->runner.run_static_test(testCategory, "test-set-self.gc", {"#t\n0\n"});
}

TEST_F(WithGameTests, NewArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-new-array.gc",
                                          get_test_pass_string("new-array", 8));
}

TEST_F(WithGameTests, NewStaticStructureIntegers) {
  shared_compiler->runner.run_static_test(testCategory, "test-new-static-structure-integers.gc",
                                          get_test_pass_string("new-static-structure-integers", 7));
}

TEST_F(WithGameTests, NewStaticBasic) {
  shared_compiler->runner.run_static_test(testCategory, "test-new-static-basic.gc",
                                          get_test_pass_string("new-static-basic", 12));
}

TEST_F(WithGameTests, VectorDot) {
  shared_compiler->runner.run_static_test(testCategory, "test-vector-dot.gc",
                                          get_test_pass_string("vector-dot", 1));
}

TEST_F(WithGameTests, DebuggerMemoryMap) {
  auto mem_map = shared_compiler->compiler.listener().build_memory_map();

  // we should have gkernel main segment
  listener::MemoryMapEntry gk_main;
  EXPECT_TRUE(mem_map.lookup("gkernel", MAIN_SEGMENT, &gk_main));
  auto lookup_2 = mem_map.lookup(gk_main.start_addr + 12);
  EXPECT_TRUE(lookup_2.obj_name == "gkernel");
  EXPECT_FALSE(lookup_2.empty);
  EXPECT_EQ(lookup_2.seg_id, MAIN_SEGMENT);
}

TEST_F(WithGameTests, DebuggerDisassemble) {
  auto di = shared_compiler->compiler.get_debugger().get_debug_info_for_object("gcommon");
  bool fail = false;
  auto result = di.disassemble_all_functions(&fail, &shared_compiler->compiler.get_goos().reader);
  // printf("Got\n%s\n", result.c_str());
  EXPECT_FALSE(fail);
}

TEST_F(WithGameTests, GameText) {
  shared_compiler->runner.run_static_test(testCategory, "test-game-text.gc",
                                          get_test_pass_string("game-text", 5));
}

TEST_F(WithGameTests, GameCount) {
  shared_compiler->compiler.run_test_from_string(
      "(asm-data-file game-count \"test/test_data/test_game_counts.txt\")");
  shared_compiler->compiler.run_test_from_string(
      "(build-dgos \"test/test_data/test_game_count_dgos.txt\")");
  shared_compiler->compiler.run_test_from_string(
      "(dgo-load \"engine\" global (link-flag output-load-msg output-load-true-msg execute-login "
      "print-login) #x200000)");
  shared_compiler->runner.run_static_test(testCategory, "test-game-count.gc",
                                          get_test_pass_string("game-count", 4));
  // don't leave behind a weird version of the game-count file.
  fs::remove(file_util::get_file_path({"out", "jak1", "iso", "ENGINE.CGO"}));
  fs::remove(file_util::get_file_path({"out", "jak1", "obj", "game-cnt.go"}));
}

TEST_F(WithGameTests, BitFieldAccess) {
  shared_compiler->runner.run_static_test(testCategory, "test-bitfield-access.gc",
                                          {"#tfffffffffffff344f213ffffffffffffffff\n0\n"});
}

TEST_F(WithGameTests, SimpleBitField) {
  shared_compiler->runner.run_static_test(testCategory, "test-set-bitfield.gc", {"#t50.3432\n0\n"});
}

TEST_F(WithGameTests, StaticBitField) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-bitfield.gc",
                                          {"#t50.3432\n0\n"});
}

TEST_F(WithGameTests, TrickyBitField) {
  shared_compiler->runner.run_static_test(testCategory, "test-bitfield-tricky-access.gc",
                                          get_test_pass_string("bitfield-tricky-access", 14));
}

TEST_F(WithGameTests, Bitfield128) {
  shared_compiler->runner.run_static_test(testCategory, "test-access-bitfield128.gc",
                                          {"-abcdbeef 77777777 66666666 12347890\n"
                                           "-abcdbeef 77777777 66666666 00000001\n"
                                           "-abcdbeef 77777777 00000002 00000001\n"
                                           "-abcdbeef 00000003 00000002 00000001\n"
                                           "00000004 00000003 00000002 00000001\n"
                                           "12341234 00000007 00000666 -deadbeef\n"
                                           "12124545 -92929292 78787878 23232323\n"
                                           "00009878 00003333 00002222 00001212\n"
                                           "0\n"});
}

TEST_F(WithGameTests, Math) {
  shared_compiler->runner.run_static_test(testCategory, "test-math.gc",
                                          get_test_pass_string("math", 31));
}

TEST_F(WithGameTests, Sqrtf) {
  shared_compiler->runner.run_static_test(testCategory, "sqrtf.gc", {"2.2360\n0\n"});
}

TEST_F(WithGameTests, StaticPairs) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-static-pair-1.gc",
      {"(1 (w . a) beans 2 (-1 -2) twelve (a . \"test\"))\n0\n"});
}

TEST_F(WithGameTests, FancyStatic) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-fancy-static-fields.gc",
      {"\"name\" 12 12.3400 (a b c) 5 33 4 kernel-context asdf\n0\n"});
}

TEST_F(WithGameTests, IntegerBoxedArray) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-integer-boxed-array.gc",
      {"0 0  1 2  2 4  3 6  4 8  5 10  6 12  7 14  8 16  9 18  10 20  11 22  12 40 6 array\n0\n"});
}

TEST_F(WithGameTests, StaticBoxedArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-boxed-array.gc",
                                          {"4 asdf \"test\" (a b) 0 object 12 12\n0\n"});
}

TEST_F(WithGameTests, SizeOf) {
  shared_compiler->runner.run_static_test(testCategory, "test-size-of.gc",
                                          {"size of dma-bucket is 16\n"
                                           "size of ints: 2 4 16\n"
                                           "size of stack array is 16\n0\n"});
}

TEST_F(WithGameTests, EnumAndBitfieldTypes) {
  shared_compiler->runner.run_static_test(testCategory, "test-bitfield-and-enum-types.gc",
                                          {"content type: uint16\n"  // runtime type is u16
                                           "content type: uint16\n"
                                           "bitfield spacing: 2\n"        // u16 spacing
                                           "enum spacing: 2\n"            // u16 spacing
                                           "bitfield array spacing: 2\n"  // u16 spacing
                                           "enum array spacing: 2\n"      // u16 spacing
                                           "9\n"                          // 4 + 5
                                           "sizes: 2 2\n"                 // size-of should work
                                           "0\n"});
}

TEST_F(WithGameTests, Trig) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-trig.gc",
      {"2.0000\n"    // 2 deg
       "-45.0000\n"  // -45 deg
       "1.2000\n"
       "-1.2000\n"
       "-2.2831\n"  // wrap
       "1.2831\n"   // wrapped
       "\n"
       "0.4999\n"  // sin
       "1.0000\n"
       "-0.7071\n"
       "-0.8659\n"
       "\n"
       "0.4999\n"  // sin-rads
       "1.0000\n"
       "-0.7071\n"
       "-0.8660\n"
       "\n"
       "0.4999 -0.7071 0.8660 -1.0000\n"  // vector-sin-rad!
       "\n"
       "0.8660\n"  // cos-rads
       "0.0000\n"
       "0.7071\n"
       "0.4999\n"
       "\n"
       "0.8660 0.7071 0.4999 0.0000\n"  // vector-cos-rad
       "\n"
       "0.4999 -0.7071 0.8660 -1.0000\n"  // vector-sincos
       "0.8660 0.7071 0.4999 0.0000\n"
       "\n"
       "0.7071 0.7082\n"   // sincos with cosine bug
       "-1.0000 0.0047\n"  // sincos, with cosine bug
       "\n"
       "sincos!\n"
       "0.7071 0.7082\n"  // also with cosine bugs
       "0.4999 0.8665\n"
       "\n"
       "0.0174 -3.1066 3.0892 1.2217\n"  // vector-rad<-vector deg
       "\n"
       "0.0174 -3.1066 3.0892 1.2217\n"  // with div/2
       "\n"
       "0.0000\n"  // tan
       "1.0000\n"
       "-0.5773\n"
       "\n"
       "0.4636\n"  // atan-rad
       "0.6154\n"
       "\n"
       "0.4636\n"  // atan2
       "-0.4636\n"
       "2.6779\n"
       "-2.6779\n"
       "\n"
       "1.0000\n"  // exp
       "1.2214\n"
       "2.7182\n"
       "3.6692\n"
       "31.5003\n"
       "0.3678\n"
       "0.6065\n"
       "0.0963\n"
       "\n"
       "26.5650\n"
       "-35.2603\n"
       "139.1074\n"
       "-116.5650\n"
       "\n"
       "44.9913\n"
       "-59.9970\n"
       "\n"
       "0\n"});
}

TEST_F(WithGameTests, Vector) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-vector.gc",
      {"[     -4.0000] [      8.0000] [     -4.0000] [      0.0000]\n"
       "[      3.0000] [      4.0000] [      5.0000] [      1.0000]\n"
       "[      5.0000] [     12.0000] [     21.0000] [      1.0000]\n"
       "[     11.0000] [     14.0000] [     17.0000] [      1.0000]\n"
       "[     -9.0000] [    -10.0000] [    -11.0000] [      1.0000]\n"
       "[      0.2000] [      0.3333] [      0.4285] [      1.0000]\n"
       "[     -2.0000] [     -4.0000] [     -6.0000] [      1.0000]\n"
       "[      3.0000] [      4.0000] [      5.0000] [      1.0000]\n"
       "[     11.0000] [     14.0000] [     17.0000] [      1.0000]\n"
       "[     -9.0000] [    -10.0000] [    -11.0000] [      1.0000]\n"
       "[     -0.5000] [     -1.0000] [     -1.5000] [      1.0000]\n"
       "[     -1.0000] [     -2.0000] [     -3.0000] [      1.0000]\n"
       "part 2\n"
       "[     -1.0000] [     -2.0000] [     -3.0000] [      4.0000]\n"
       "#f #t #t #f\n"
       "4.0000\n"
       "part 3\n"
       "[      1.5000] [      2.5000] [      3.5000] [      1.0000]\n"
       "[      5.0000] [      6.0000] [      7.0000] [      1.0000]\n"
       "7.0000\n"
       "49.0000\n"
       "7.0000\n"
       "[     14.0000] [     21.0000] [     42.0000] [     10.0000]\n"
       "3.5000\n"
       "-1 2 -3 4\n"
       "[     -1.0000] [      2.0000] [     -3.0000] [      4.0000]\n"
       "0.0000 720.0000 360.0000 0.0000\n"
       "-20.0006 20.0006 20.0006 20.0006\n"
       "0\n"});
}

TEST_F(WithGameTests, InlinedPackedBasics) {
  shared_compiler->runner.run_static_test(testCategory, "inlined-packed-basics.gc",
                                          {"rec stride: 48\n"
                                           "offset of float: 40\n"
                                           "offset: 16\n"
                                           "offset2: 184\n"
                                           "array: #x0\n"
                                           "first: #x0\n"
                                           "0\n"});
}

TEST_F(WithGameTests, PartialDefineTypeField) {
  shared_compiler->runner.run_static_test(testCategory, "test-partial-define-type-field.gc",
                                          {"#f\n"
                                           "0\n"});
}

// VECTOR FLOAT TESTS

// ---- One off Tests

TEST_F(WithGameTests, VFOuterProduct) {
  shared_compiler->runner.run_static_test(testCategory, "test-vector-outer-product.gc",
                                          {"(-4.0000, 8.0000, -4.0000, 999.0000)\n0\n"});
}

TEST_F(WithGameTests, VFLoadAndStore) {
  shared_compiler->runner.run_static_test(testCategory, "test-vf-load-and-store.gc",
                                          {"2.0000\n0\n"});
}

TEST_F(WithGameTests, VFSimpleMath) {
  shared_compiler->runner.run_static_test(testCategory, "test-basic-vector-math.gc",
                                          {"55.0000\n0\n"});
}

TEST_F(WithGameTests, VFLoadStatic) {
  shared_compiler->runner.run_static_test(testCategory, "test-load-static-vector.gc",
                                          {"5.3000\n0\n"});
}

TEST_F(WithGameTests, XMMSpill) {
  shared_compiler->runner.run_static_test(testCategory, "test-xmm-spill.gc", {"253.0000\n0\n"});
}

TEST_F(WithGameTests, BoxedArrayIndex) {
  shared_compiler->runner.run_static_test(testCategory, "test-boxed-array-index.gc", {"18\n0\n"});
}

TEST_F(WithGameTests, LocalVars) {
  shared_compiler->runner.run_static_test(testCategory, "test-local-vars.gc",
                                          {"y is \"test\", x is 12, z is 3.2000\n0\n"});
}

TEST_F(WithGameTests, ShortCircuit) {
  shared_compiler->runner.run_static_test(testCategory, "test-short-circuit.gc",
                                          get_test_pass_string("short-circuit", 13));
}

TEST_F(WithGameTests, VectorFloatToInt) {
  shared_compiler->runner.run_static_test(testCategory, "test-vector-int-float-conversions.gc",
                                          {"1.0000 -2.0000 3.0000 4.0000\n"
                                           "1 -2 3 4\n"
                                           "1.0000 -2.0000 3.0000 4.0000\n"
                                           "0\n"});
}

TEST_F(WithGameTests, PWShifts) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-pw-shifts.gc",
      {"ffffffffaafffff0 ffffffffbbfffff0 ffffffffccfffff0 ffffffffddfffff0\n"
       "ffffffffeabffffc ffffffffeefffffc fffffffff33ffffc fffffffff77ffffc\n"
       "ffffffffafffff00 ffffffffbfffff00 ffffffffcfffff00 ffffffffdfffff00\n"
       "2bffffc0 2fffffc0 33ffffc0 37ffffc0\n"
       "0\n"});
}

TEST_F(WithGameTests, StaticArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-array.gc",
                                          {"1 2 -10\n"
                                           "0\n"});
}

TEST_F(WithGameTests, StaticInlineArray) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-static-inline-array.gc",
      {"test-basic-for-static-inline test-basic-for-static-inline #x4 #x4 \"hello\"\n"
       "#x0 #x0 \"hello\"\n"
       "0\n"});
}

TEST_F(WithGameTests, StaticArrayField) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-array-field.gc",
                                          {"\"ghjkl\"\n"
                                           "\"hello\"\n"
                                           "\"world\"\n"
                                           "0\n"});
}

TEST_F(WithGameTests, ArrayRefStatic) {
  shared_compiler->runner.run_static_test(testCategory, "test-array-ref-static.gc",
                                          {"test-not-inline-inline-array-type 12 asdf 13 bean 14\n"
                                           "0\n"});
}

TEST_F(WithGameTests, TypeReference) {
  shared_compiler->runner.run_static_test(testCategory, "test-type-ref.gc",
                                          {"string #t basic some-unknown-type 20 0\n"
                                           "0\n"});
}

TEST_F(WithGameTests, StaticFieldInlineArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-field-inline-arrays.gc",
                                          {"\"second\" \"first\"\n"
                                           "basic-elt #x4 #x4\n"
                                           "two\n"
                                           "\"second\" \"first\"\n"
                                           "basic-elt #x4 #x4\n"
                                           "two\n"
                                           "0\n"});
}

TEST_F(WithGameTests, I128Simple) {
  shared_compiler->runner.run_static_test(testCategory, "test-i128-simple.gc",
                                          {"[0] #x707172737475767778797a7b7c7d7e7f\n"
                                           "[1] #x606162636465666768696a6b6c6d6e6f\n"
                                           "[2] #x505152535455565758595a5b5c5d5e5f\n"
                                           "[3] #x404142434445464748494a4b4c4d4e4f\n"
                                           "[4] #x303132333435363738393a3b3c3d3e3f\n"
                                           "[5] #x202122232425262728292a2b2c2d2e2f\n"
                                           "[6] #x101112131415161718191a1b1c1d1e1f\n"
                                           "[7] #x000102030405060708090a0b0c0d0e0f\n"
                                           "12344321\n"});
}

// TODO - add tests

TEST_F(WithGameTests, Pextlw) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-pextlw.gc",
      {"#x07060504171615140302010013121110\n"
       "#x0f0e0d0c1f1e1d1c0b0a09081b1a1918\n"
       "#x07060504030201001716151413121110\n"
       "#x1f1e1d1c1b1a19180f0e0d0c0b0a0908\n"
       "#x0d0c0908050401001d1c191815141110\n"
       "#x0e0c0a08060402001e1c1a1816141210\n"
       "#xffffffff00000000ffffffff00000000\n"
       "#x00090000000000fefffffffe000002ff\n"
       "1 + 7 = 8 (8)\n2 + 8 = a (a)\n3 + 3 = 6 (6)\n4 + 4 = 8 (8)\n0 + 1 = 1 (1)\n0 + 2 = 2 "
       "(2)\n0 + 3 = 3 (3)\n0 + 4 = 4 (4)\n8 + 9 = 11 (11)\n8 + 8 = 10 (10)\n8 + 7 = f (f)\n8 + 6 "
       "= e (e)\nf0 + f0 = e0 (e0)\nf0 + f0 = e0 (e0)\nf1 + f1 = e2 (e2)\nf3 + f3 = e6 (e6)\n"
       "0\n"});
}

TEST_F(WithGameTests, Matrix) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-matrix.gc",
      {"mat-mult\n"
       "\t[     80.0000] [     70.0000] [     60.0000] [     50.0000]\n"
       "\t[    240.0000] [    214.0000] [    188.0000] [    162.0000]\n"
       "\t[    400.0000] [    358.0000] [    316.0000] [    274.0000]\n"
       "\t[    560.0000] [    502.0000] [    444.0000] [    386.0000]\n"
       "transpose\n"
       "\t[      1.0000] [      5.0000] [      9.0000] [     13.0000]\n"
       "\t[      2.0000] [      6.0000] [     10.0000] [     14.0000]\n"
       "\t[      3.0000] [      7.0000] [     11.0000] [     15.0000]\n"
       "\t[      4.0000] [      8.0000] [     12.0000] [     16.0000]\n"
       "inv-4x4\n"
       "\t[      1.0000] [      0.0000] [      0.0000] [      0.0000]\n"
       "\t[      0.0000] [      1.0000] [      0.0000] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      1.0000] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      1.0000]\n"
       "axis-angle\n"
       "\t[      0.9961] [      0.0506] [      0.0715] [      0.0000]\n"
       "\t[     -0.0393] [      0.9876] [     -0.1516] [      0.0000]\n"
       "\t[     -0.0783] [      0.1482] [      0.9858] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      1.0000]\n"
       "\n"
       "\t[      1.0000] [      0.0000] [      0.0000] [      0.0000]\n"
       "\t[      0.0000] [      0.9848] [     -0.1736] [      0.0000]\n"
       "\t[      0.0000] [      0.1736] [      0.9848] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      1.0000]\n"
       "\n"
       "\t[      0.9848] [      0.0000] [      0.1736] [      0.0000]\n"
       "\t[      0.0000] [      1.0000] [      0.0000] [      0.0000]\n"
       "\t[     -0.1736] [      0.0000] [      0.9848] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      1.0000]\n"
       "\n"
       "\t[      0.9848] [     -0.1736] [      0.0000] [      0.0000]\n"
       "\t[      0.1736] [      0.9848] [      0.0000] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      1.0000] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      1.0000]\n"
       "\n"
       "\t[      1.0000] [      0.0000] [      0.0000] [      0.0000]\n"
       "\t[      0.0000] [      1.0000] [      0.0000] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      1.0000] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      1.0000]\n"
       "\n"
       "3x3-inverse\n"
       "\t[      0.0952] [      0.0238] [     -0.0833] [      0.0000]\n"
       "\t[      0.3333] [     -0.1666] [      0.0833] [      0.0000]\n"
       "\t[      0.0476] [      0.2619] [      0.0833] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      0.0000]\n"
       "\n"
       "\t[      0.0952] [      0.3333] [      0.0476] [      0.0000]\n"
       "\t[      0.0238] [     -0.1666] [      0.2619] [      0.0000]\n"
       "\t[     -0.0833] [      0.0833] [      0.0833] [      0.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      0.0000]\n"
       "transform-many\n"
       "vec-array\n"
       "\t[    -13.0000] [      5.0000] [     10.0000] [      1.0000]\n"
       "\t[    -22.0000] [     14.0000] [     28.0000] [      1.0000]\n"
       "\t[    -31.0000] [     23.0000] [     46.0000] [      1.0000]\n"
       "\t[    -40.0000] [     32.0000] [     64.0000] [      1.0000]\n"
       "\t[    -49.0000] [     41.0000] [     82.0000] [      1.0000]\n"
       "\t[    -58.0000] [     50.0000] [    100.0000] [      1.0000]\n"
       "\t[    -67.0000] [     59.0000] [    118.0000] [      1.0000]\n"
       "\t[    -76.0000] [     68.0000] [    136.0000] [      1.0000]\n"
       "\t[    -85.0000] [     77.0000] [    154.0000] [      1.0000]\n"
       "\t[    -94.0000] [     86.0000] [    172.0000] [      1.0000]\n"
       "\t[   -103.0000] [     95.0000] [    190.0000] [      1.0000]\n"
       "\t[      0.0000] [      0.0000] [      0.0000] [      0.0000]\n"
       "0\n"});
}

TEST_F(WithGameTests, WeirdMultiply) {
  shared_compiler->runner.run_static_test(testCategory, "test-weird-multiplies.gc",
                                          {"2 100000002\n"
                                           "100000000 100000000\n"
                                           "55555552 -3 7ffffffffffffffb -5\n"
                                           "0\n"});
}

TEST_F(WithGameTests, Function128) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-function128.gc",
      {"#<vector       1.0000       2.0000       3.0000       4.0000 @ #x400000003f800000>\n"
       "#<vector       1.0000      20.0000       3.0000       4.0000 @ #x41a000003f800000>\n"
       "#<vector      10.0000       2.0000       3.0000       4.0000 @ #x4000000041200000>\n"
       " 0 1 2 3 4 5 6 7 8 9 a b c d e\n"
       "arg0: 1 arg2: 2\n"
       "0\n"});
}

TEST_F(WithGameTests, AddrOfVar) {
  shared_compiler->runner.run_static_test(testCategory, "test-addr-of-var.gc",
                                          {"x: 25 y: 35 z: 35\n"
                                           "x: 13 y: 35 z: 15\n"
                                           "0\n"});
}

TEST_F(WithGameTests, SoundName) {
  shared_compiler->runner.run_static_test(testCategory, "test-sound-name.gc",
                                          {"#t #f #f\n"
                                           "0\n"});
}

TEST_F(WithGameTests, StaticLambda) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-lambda.gc",
                                          {"Add: 30 sub: -10\n0\n"});
}

TEST_F(WithGameTests, StaticLambdaArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-array-of-lambdas.gc",
                                          {"2\n1\n0\n"});
}

TEST_F(WithGameTests, StaticTypeArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-array-of-types.gc",
                                          {"matched!\n0\n"});
}

TEST_F(WithGameTests, StaticArraySubtypeDraft) {
  shared_compiler->runner.run_static_test(testCategory, "test-static-array-subtype.gc",
                                          {"length - 2\ntest\n1\n0\n"});
}

TEST_F(WithGameTests, MethodReplace) {
  shared_compiler->runner.run_static_test(testCategory, "test-method-replace.gc",
                                          {"relocate! foo: 123 heap: 1 name: 2\n0\n"});
}

TEST_F(WithGameTests, Behaviors) {
  shared_compiler->runner.run_static_test(testCategory, "test-behaviors.gc",
                                          {"function self: 123\n"
                                           "method obj: 456 self: 123\n0\n"});
}

TEST_F(WithGameTests, RaySphere) {
  shared_compiler->runner.run_static_test(testCategory, "test-ray-sphere.gc",
                                          {"Got 0.2346\n"
                                           "Got -100000000.0000\n"
                                           "Got -100000000.0000\n"
                                           "Got 0.0000\n"
                                           "Got -100000000.0000\n0\n"});
}

TEST_F(WithGameTests, PandPorPnor) {
  shared_compiler->runner.run_static_test(testCategory, "test-pand-por-pnor.gc",
                                          {"#x1f1f1d0f0f0f0d0b0f0f0d0707070503\n"
                                           "#xe0e0e2f0f0f0f2f4f0f0f2f8f8f8fafc\n"
                                           "#x0200000c0a0808080200000402000000\n"
                                           "\n"
                                           "#x1f1f1d0f0f0f0d0b0f0f0d0707070503\n"
                                           "#xe0e0e2f0f0f0f2f4f0f0f2f8f8f8fafc\n"
                                           "#x0200000c0a0808080200000402000000\n0\n"});
}

TEST_F(WithGameTests, StackInlineArray) {
  shared_compiler->runner.run_static_test(testCategory, "test-stack-inline-array.gc",
                                          {"#x8\n"
                                           "#x30\n0\n"});
}

TEST_F(WithGameTests, GetEnumVals) {
  shared_compiler->runner.run_static_test(testCategory, "test-get-enum-vals.gc",
                                          {"((thing1 . 1) (thing3 . 3) "
                                           "(thing5 . 5))\n0\n"});
}

TEST_F(WithGameTests, SetU64FromFloat) {
  shared_compiler->runner.run_static_test(
      testCategory, "test-set-u64-from-float.gc",
      {"-12.0000 #xffffffffc1400000 #xc1400000 #xffffffff\n0\n"});
}

TEST_F(WithGameTests, TrickyFloatBehavior) {
  shared_compiler->runner.run_static_test(testCategory, "tricky-floats.gc",
                                          {"#xffffffff80000000 1.0000 #xffffffffbf800000\n0\n"});
}

TEST_F(WithGameTests, ProcessAllocation) {
  shared_compiler->runner.run_static_test(testCategory, "test-kernel-alloc.gc",
                                          {"diff is 16\n0\n"});
}

TEST_F(WithGameTests, MethodCallForwardDeclared) {
  shared_compiler->runner.run_static_test(testCategory, "test-forward-declared-method.gc",
                                          {"4 12\n0\n"});
}

TEST_F(WithGameTests, PointerInStatic) {
  shared_compiler->runner.run_static_test(testCategory, "test-false-in-static-pointer.gc",
                                          {"#f\n0\n"});
}

TEST_F(WithGameTests, StackSingleton) {
  shared_compiler->runner.run_static_test(testCategory, "test-stack-singleton.gc",
                                          {"#f #f #f #f #t\n0\n"});
}

TEST_F(WithGameTests, StackSingletonType) {
  shared_compiler->runner.run_static_test(testCategory, "test-stack-singleton-type.gc",
                                          {"#t\n0\n"});
}

namespace Mips2C::jak1 {
namespace test_func {
extern u64 execute(void*);
}
namespace goal_call_test {
extern u64 execute(void*);
extern void link();
}  // namespace goal_call_test
}  // namespace Mips2C::jak1

TEST_F(WithGameTests, Mips2CBasic) {
  Mips2C::gLinkedFunctionTable.reg("test-func", Mips2C::jak1::test_func::execute, 0);
  shared_compiler->runner.run_static_test(testCategory, "test-mips2c-call.gc", {"36\n0\n"});
}

TEST_F(WithGameTests, Mips2C_CallGoal) {
  Mips2C::gLinkedFunctionTable.reg("test-func2", Mips2C::jak1::goal_call_test::execute, 128);
  Mips2C::jak1::goal_call_test::link();
  shared_compiler->runner.run_static_test(testCategory, "test-mips2c-goal.gc",
                                          {"1 2 3 4 5 6 7 8\n12\n"});
}
