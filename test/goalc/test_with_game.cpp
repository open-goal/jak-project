#include <thread>
#include <chrono>

#include "gtest/gtest.h"
#include "game/runtime.h"
#include "goalc/listener/Listener.h"
#include "goalc/compiler/Compiler.h"

#include "inja.hpp"
#include "third-party/json.hpp"
#include "common/util/FileUtil.h"
#include <test/goalc/framework/test_runner.h>
#include "third-party/fmt/core.h"

#include <iostream>
#include <string>
#include <cstdio>
#include <sstream>
#include <iostream>
#include <random>
#include <filesystem>

struct WithGameParam {
  // TODO - Not Needed Yet
};

class WithGameTests : public testing::TestWithParam<WithGameParam> {
 public:
  static void SetUpTestSuite() {
    try {
      compiler.run_test_no_load("test/goalc/source_templates/with_game/test-build-game.gc");
    } catch (std::exception& e) {
      fprintf(stderr, "caught exception %s\n", e.what());
      EXPECT_TRUE(false);
    }
    runtime_thread = std::thread((GoalTest::runtime_with_kernel));
    runner.c = &compiler;

    compiler.run_test_from_file("test/goalc/source_templates/with_game/test-load-game.gc");
  }

  static void TearDownTestSuite() {
    compiler.shutdown_target();
    runtime_thread.join();
  }

  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  void TearDown() {}

  static std::thread runtime_thread;
  static Compiler compiler;
  static GoalTest::CompilerTestRunner runner;

  std::string testCategory = "with_game";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread WithGameTests::runtime_thread;
Compiler WithGameTests::compiler;
GoalTest::CompilerTestRunner WithGameTests::runner;

namespace {
std::vector<std::string> get_test_pass_string(const std::string& name, int count) {
  return {fmt::format("Test \"{}\": {} Passes\n0\n", name, count)};
}
}  // namespace

TEST_F(WithGameTests, ReturnConstant) {
  runner.run_static_test(env, testCategory, "defun-return-constant.static.gc", {"12\n"});
}

TEST_F(WithGameTests, ReturnSymbol) {
  runner.run_static_test(env, testCategory, "defun-return-symbol.static.gc", {"42\n"});
}

TEST_F(WithGameTests, MinMax) {
  runner.run_static_test(env, testCategory, "test-min-max.gc", {"10\n"});
}

TEST_F(WithGameTests, BoxedFloat) {
  runner.run_static_test(env, testCategory, "test-bfloat.gc",
                         {"data 1.2330 print 1.2330 type bfloat\n0\n"});
}

TEST_F(WithGameTests, BasicTypeCheck) {
  runner.run_static_test(env, testCategory, "test-basic-type-check.gc", {"#f#t#t#f#t#f#t#t\n0\n"});
}

TEST_F(WithGameTests, ConditionBoolean) {
  runner.run_static_test(env, testCategory, "test-condition-boolean.gc", {"4\n"});
}

TEST_F(WithGameTests, TypeType) {
  runner.run_static_test(env, testCategory, "test-type-type.gc", {"#t#f\n0\n"});
}

TEST_F(WithGameTests, AccessInlineArray) {
  runner.run_static_test(env, testCategory, "test-access-inline-array.gc", {"1.2345\n0\n"});
}

TEST_F(WithGameTests, FindParentMethod) {
  runner.run_static_test(env, testCategory, "test-find-parent-method.gc", {"\"test pass!\"\n0\n"});
}

TEST_F(WithGameTests, Ref) {
  runner.run_static_test(env, testCategory, "test-ref.gc", {"83\n"});
}

TEST_F(WithGameTests, PairASzie) {
  runner.run_static_test(env, testCategory, "test-pair-asize.gc", {"8\n"});
}

TEST_F(WithGameTests, Last) {
  runner.run_static_test(env, testCategory, "test-last.gc", {"d\n0\n"});
}

TEST_F(WithGameTests, Sort) {
  runner.run_static_test(
      env, testCategory, "test-sort.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(1234 654 72 56 32 25 24 16 -34)\n0\n"});
}

TEST_F(WithGameTests, Sort2) {
  runner.run_static_test(
      env, testCategory, "test-sort-2.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
}

TEST_F(WithGameTests, Sort3) {
  runner.run_static_test(
      env, testCategory, "test-sort-3.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
}

TEST_F(WithGameTests, PairLength) {
  runner.run_static_test(env, testCategory, "test-pair-length.gc", {"6\n"});
}

TEST_F(WithGameTests, Member1) {
  runner.run_static_test(env, testCategory, "test-member-1.gc", {"(c d)\n0\n"});
}

TEST_F(WithGameTests, Member2) {
  runner.run_static_test(env, testCategory, "test-member-2.gc", {"#f\n0\n"});
}

TEST_F(WithGameTests, Assoc1) {
  runner.run_static_test(env, testCategory, "test-assoc-1.gc", {"w\n0\n"});
}

TEST_F(WithGameTests, Assoc2) {
  runner.run_static_test(env, testCategory, "test-assoc-2.gc", {"#f\n0\n"});
}

TEST_F(WithGameTests, Assoce1) {
  runner.run_static_test(env, testCategory, "test-assoce-1.gc", {"x\n0\n"});
}

TEST_F(WithGameTests, Assoce2) {
  runner.run_static_test(env, testCategory, "test-assoce-2.gc", {"x\n0\n"});
}

TEST_F(WithGameTests, Append) {
  runner.run_static_test(env, testCategory, "test-append.gc", {"(a b c d e)\n0\n"});
}

TEST_F(WithGameTests, DeleteList) {
  runner.run_static_test(env, testCategory, "test-delete-list.gc", {"(a b d e)\n0\n"});
}

TEST_F(WithGameTests, DeleteCar) {
  runner.run_static_test(env, testCategory, "test-delete-car.gc", {"((a . b) (e . f))\n#f\n0\n"});
}

TEST_F(WithGameTests, InsertCar) {
  runner.run_static_test(env, testCategory, "test-insert-cons.gc",
                         {"((c . w) (a . b) (e . f))\n0\n"});
}

TEST_F(WithGameTests, InlineArrayClass) {
  runner.run_static_test(env, testCategory, "test-new-inline-array-class.gc", {"2824\n"});
}

TEST_F(WithGameTests, Memcpy) {
  runner.run_static_test(env, testCategory, "test-memcpy.gc", {"13\n"});
}

TEST_F(WithGameTests, Memset) {
  runner.run_static_test(env, testCategory, "test-memset.gc", {"11\n"});
}

TEST_F(WithGameTests, BintegerPrint) {
  runner.run_static_test(env, testCategory, "test-binteger-print.gc", {"-17\n0\n"});
}

TEST_F(WithGameTests, TestTests) {
  runner.run_static_test(env, testCategory, "test-tests.gc",
                         {"Test Failed On Test 0: \"unknown\"\nTest Failed On Test 0: "
                          "\"test\"\nTest \"test-of-test\": 1 Passes\n0\n"});
}

TEST_F(WithGameTests, TypeArrays) {
  runner.run_static_test(env, testCategory, "test-type-arrays.gc",
                         {"Test \"test-type-arrays\": 3 Passes\n0\n"});
}

TEST_F(WithGameTests, NumberComparison) {
  runner.run_static_test(env, testCategory, "test-number-comparison.gc",
                         {"Test \"number-comparison\": 14 Passes\n0\n"});
}

TEST_F(WithGameTests, ApproxPi) {
  runner.run_static_test(env, testCategory, "test-approx-pi.gc",
                         get_test_pass_string("approx-pi", 4));
}

TEST_F(WithGameTests, ApproxPiStack) {
  runner.run_static_test(env, testCategory, "test-approx-pi-stack.gc",
                         get_test_pass_string("approx-pi-stack", 4));
}

TEST_F(WithGameTests, DynamicType) {
  runner.run_static_test(env, testCategory, "test-dynamic-type.gc",
                         get_test_pass_string("dynamic-type", 4));
}

TEST_F(WithGameTests, StringType) {
  runner.run_static_test(env, testCategory, "test-string-type.gc",
                         get_test_pass_string("string-type", 4));
}

TEST_F(WithGameTests, NewString) {
  runner.run_static_test(env, testCategory, "test-new-string.gc",
                         get_test_pass_string("new-string", 5));
}

TEST_F(WithGameTests, AddrOf) {
  runner.run_static_test(env, testCategory, "test-addr-of.gc", get_test_pass_string("addr-of", 2));
}

TEST_F(WithGameTests, SetSelf) {
  runner.run_static_test(env, testCategory, "test-set-self.gc", {"#t\n0\n"});
}

TEST_F(WithGameTests, NewArray) {
  runner.run_static_test(env, testCategory, "test-new-array.gc",
                         get_test_pass_string("new-array", 8));
}

TEST_F(WithGameTests, NewStaticStructureIntegers) {
  runner.run_static_test(env, testCategory, "test-new-static-structure-integers.gc",
                         get_test_pass_string("new-static-structure-integers", 7));
}

TEST_F(WithGameTests, NewStaticBasic) {
  runner.run_static_test(env, testCategory, "test-new-static-basic.gc",
                         get_test_pass_string("new-static-basic", 9));
}

TEST_F(WithGameTests, VectorDot) {
  runner.run_static_test(env, testCategory, "test-vector-dot.gc",
                         get_test_pass_string("vector-dot", 1));
}

TEST_F(WithGameTests, DebuggerMemoryMap) {
  auto mem_map = compiler.listener().build_memory_map();

  // we should have gkernel main segment
  listener::MemoryMapEntry gk_main;
  EXPECT_TRUE(mem_map.lookup("gkernel", MAIN_SEGMENT, &gk_main));
  auto lookup_2 = mem_map.lookup(gk_main.start_addr + 12);
  EXPECT_TRUE(lookup_2.obj_name == "gkernel");
  EXPECT_FALSE(lookup_2.empty);
  EXPECT_EQ(lookup_2.seg_id, MAIN_SEGMENT);
}

TEST_F(WithGameTests, DebuggerDisassemble) {
  auto di = compiler.get_debugger().get_debug_info_for_object("gcommon");
  bool fail = false;
  auto result = di.disassemble_debug_functions(&fail);
  // printf("Got\n%s\n", result.c_str());
  EXPECT_FALSE(fail);
}

TEST_F(WithGameTests, GameText) {
  compiler.run_test_from_string("(asm-data-file game-text \"test/test_data/test_game_text.txt\")");
  runner.run_static_test(env, testCategory, "test-game-text.gc",
                         get_test_pass_string("game-text", 5));
}

TEST_F(WithGameTests, GameCount) {
  compiler.run_test_from_string(
      "(asm-data-file game-count \"test/test_data/test_game_counts.txt\")");
  compiler.run_test_from_string("(build-dgos \"test/test_data/test_game_count_dgos.txt\")");
  compiler.run_test_from_string("(dgo-load \"game\" global #xf #x200000)");
  runner.run_static_test(env, testCategory, "test-game-count.gc",
                         get_test_pass_string("game-count", 4));
}

TEST(TypeConsistency, TypeConsistency) {
  Compiler compiler;
  compiler.enable_throw_on_redefines();
  compiler.run_test_no_load("test/goalc/source_templates/with_game/test-build-game.gc");
  compiler.run_test_no_load("decompiler/config/all-types.gc");
}