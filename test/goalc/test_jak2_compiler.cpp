#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

// Tests for Jak 2's compiler/runtime, without game-specific code (ENGINE/GAME)

struct Jak2Param {
  // TODO - Not Needed Yet
};

class Jak2GoalcTests : public testing::TestWithParam<Jak2Param> {
 public:
  static void SetUpTestSuite() {
    runtime_thread = std::make_unique<std::thread>(std::thread(GoalTest::runtime_no_kernel_jak2));
    compiler = std::make_unique<Compiler>(GameVersion::Jak2);
    runner = std::make_unique<GoalTest::CompilerTestRunner>();
    runner->c = compiler.get();
  }

  static void TearDownTestSuite() {
    compiler->shutdown_target();
    runtime_thread->join();

    runtime_thread.reset();
    compiler.reset();
    runner.reset();
  }

  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  void TearDown() {}

  static std::unique_ptr<std::thread> runtime_thread;
  static std::unique_ptr<Compiler> compiler;
  static std::unique_ptr<GoalTest::CompilerTestRunner> runner;

  std::string testCategory = "jak2";
};

std::unique_ptr<std::thread> Jak2GoalcTests::runtime_thread;
std::unique_ptr<Compiler> Jak2GoalcTests::compiler;
std::unique_ptr<GoalTest::CompilerTestRunner> Jak2GoalcTests::runner;

TEST_F(Jak2GoalcTests, All) {
  runner->run_static_test(testCategory, "jak2-mega-test.gc",
                          {"empty pair: () () () #t #f\n"
                           "empty pair type: pair\n"
                           "non-empty pair: (a b -12) a pair (a . b)\n"
                           "basic types: type symbol string function\n"
                           "bools: #t #f #t #f #f #t\n"
                           "zero: 0\n"
                           "parent of type: basic structure object object\n"
                           "array: asdf #f\n0\n"});
}