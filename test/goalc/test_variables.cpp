#include <string>
#include <thread>

#include "game/runtime.h"
#include "goalc/compiler/Compiler.h"
#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

struct VariableParam {
  // TODO - Not Needed Yet
};

class VariableTests : public testing::TestWithParam<VariableParam> {
 public:
  static void SetUpTestSuite() {
    shared_compiler = std::make_unique<SharedCompiler>(GameVersion::Jak1);
    shared_compiler->runtime_thread = std::thread(GoalTest::runtime_no_kernel_jak1);
    shared_compiler->runner.c = &shared_compiler->compiler;
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
    SharedCompiler(GameVersion version) : compiler(version) {}
    std::thread runtime_thread;
    Compiler compiler;
    GoalTest::CompilerTestRunner runner;
  };

  static std::unique_ptr<SharedCompiler> shared_compiler;

  std::string testCategory = "variables";
};

std::unique_ptr<VariableTests::SharedCompiler> VariableTests::shared_compiler;

TEST_F(VariableTests, Globals) {
  shared_compiler->runner.run_static_test(testCategory, "defglobalconstant-1.static.gc", {"17\n"});
  shared_compiler->runner.run_static_test(testCategory, "defglobalconstant-2.static.gc", {"18\n"});
}

TEST_F(VariableTests, Definitions) {
  shared_compiler->runner.run_static_test(testCategory, "define.static.gc", {"17\n"});
}

TEST_F(VariableTests, Let) {
  shared_compiler->runner.run_static_test(testCategory, "let.static.gc", {"30\n"});
  shared_compiler->runner.run_static_test(testCategory, "let-star.static.gc", {"30\n"});
  shared_compiler->runner.run_static_test(testCategory, "mlet.static.gc", {"10\n"});
}

TEST_F(VariableTests, StackVars) {
  shared_compiler->runner.run_static_test(testCategory, "stack-ints.gc", {"12\n"});
  shared_compiler->runner.run_static_test(testCategory, "stack-ints-2.gc", {"1\n"});
}

TEST_F(VariableTests, Bitfields) {
  shared_compiler->runner.run_static_test(testCategory, "bitfield-enums.gc", {"5\n"});
  shared_compiler->runner.run_static_test(testCategory, "integer-enums.gc", {"11\n"});
}

TEST_F(VariableTests, InlineAsm) {
  shared_compiler->runner.run_static_test(testCategory, "inline-asm.static.gc", {"1\n"});
}

TEST_F(VariableTests, StaticBitfieldField) {
  shared_compiler->runner.run_static_test(testCategory, "static-bitfield-field.gc", {"22\n"});
}

TEST_F(VariableTests, StackArrayAlignment) {
  shared_compiler->runner.run_static_test(testCategory, "stack-array-align.gc", {"3\n"});
}

TEST_F(VariableTests, StackStructureAlignment) {
  shared_compiler->runner.run_static_test(testCategory, "stack-structure-align.gc", {"1234\n"});
}

TEST_F(VariableTests, StackBoxedArray) {
  shared_compiler->runner.run_static_test(testCategory, "stack-boxed-array.gc", {"36\n"});
}

TEST_F(VariableTests, GetSymbol) {
  shared_compiler->runner.run_static_test(testCategory, "get-symbol-1.static.gc",
                                          {"1375524\n"});  // 0x14fd24 in hex
  shared_compiler->runner.run_static_test(testCategory, "get-symbol-2.static.gc",
                                          {"1375532\n"});  // 0x14fd2c in hex
}

TEST_F(VariableTests, Constants) {
  // TODO - shared_compiler->runner.run_static_test(testCategory,
  // "string-constant-1.static.gc");
  std::string expected = "\"test string!\"";
  shared_compiler->runner.run_static_test(testCategory, "string-constant-2.static.gc", {expected},
                                          expected.size());
}

TEST_F(VariableTests, Symbols) {
  shared_compiler->runner.run_static_test(testCategory, "quote-symbol.static.gc", {"banana\n0\n"});
  std::string expected = "test-string";
  shared_compiler->runner.run_static_test(testCategory, "string-symbol.static.gc", {expected},
                                          expected.size());
}

TEST_F(VariableTests, Formatting) {
  shared_compiler->runner.run_static_test(testCategory, "format-reg-order.static.gc",
                                          {"test 1 2 3 4 5 6\n0\n"});
}

TEST_F(VariableTests, DeReference) {
  shared_compiler->runner.run_static_test(testCategory, "deref-simple.static.gc",
                                          {"structure\n0\n"});
}

TEST_F(VariableTests, Pointers) {
  shared_compiler->runner.run_static_test(testCategory, "pointers.static.gc", {"13\n"});
}

//  expected =
//      "test newline\nnewline\ntest tilde ~ \ntest A print boxed-string: \"boxed string!\"\ntest
//      A " "print symbol: a-symbol\ntest A make boxed object longer:             \"srt\"!\ntest A
//      " "non-default pad: zzzzzzpad-me\ntest A shorten(4): a23~\ntest A don'tchange(4):
//      a234\ntest A " "shorten with pad(4): sho~\ntest A a few things \"one thing\" a-second
//      integer #<compiled " "function @ #x161544>\n";
//
//  expected += "test S a string a-symbol another string!\n";
//  expected += "test C ) ]\n";
//  expected += "test P (no type) #<compiled function @ #x161544>\n";
//  expected += "test P (with type) 1447236\n";
//
//  // todo, finish format testing.
//  shared_compiler->runner.run_test_from_file("test-format.gc", {expected}, expected.size());
