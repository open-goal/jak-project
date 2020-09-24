#include <thread>
#include <chrono>

#include "gtest/gtest.h"
#include "game/runtime.h"
#include "goalc/listener/Listener.h"
#include "goalc/compiler/Compiler.h"

TEST(CompilerAndRuntime, ConstructCompiler) {
  Compiler compiler;
}

TEST(CompilerAndRuntime, StartRuntime) {
  std::thread runtime_thread([]() { exec_runtime(0, nullptr); });

  listener::Listener listener;
  while (!listener.is_connected()) {
    listener.connect_to_target();
    std::this_thread::sleep_for(std::chrono::microseconds(1000));
  }

  listener.send_reset(true);
  runtime_thread.join();
}

TEST(CompilerAndRuntime, SendProgram) {
  std::thread runtime_thread([]() { exec_runtime(0, nullptr); });
  Compiler compiler;
  compiler.run_test("goal_src/test/test-return-integer-1.gc");
  compiler.shutdown_target();
  runtime_thread.join();
}

namespace {
std::string escaped_string(const std::string& in) {
  std::string result;
  for (auto x : in) {
    switch (x) {
      case '\n':
        result.append("\\n");
        break;
      case '\t':
        result.append("\\t");
        break;
      default:
        result.push_back(x);
    }
  }
  return result;
}

struct CompilerTestRunner {
  Compiler* c = nullptr;

  struct Test {
    std::vector<std::string> expected, actual;
    std::string test_name;
    bool auto_pass = false;
  };

  std::vector<Test> tests;

  void run_test(const std::string& test_file,
                const std::vector<std::string>& expected,
                MatchParam<int> truncate = {}) {
    fprintf(stderr, "Testing %s\n", test_file.c_str());
    auto result = c->run_test("goal_src/test/" + test_file);
    if (!truncate.is_wildcard) {
      for (auto& x : result) {
        x = x.substr(0, truncate.value);
      }
    }

    EXPECT_EQ(result, expected);
    tests.push_back({expected, result, test_file, false});
  }

  void run_always_pass(const std::string& test_file) {
    c->run_test("goal_src/test/" + test_file);
    tests.push_back({{}, {}, test_file, true});
  }

  void print_summary() {
    fmt::print("~~ Compiler Test Summary for {} tests... ~~\n", tests.size());
    int passed = 0;
    int passable = 0;
    int auto_pass = 0;
    for (auto& test : tests) {
      if (test.auto_pass) {
        auto_pass++;
        fmt::print("[{:40}] AUTO-PASS!\n", test.test_name);
      } else {
        passable++;
        if (test.expected == test.actual) {
          fmt::print("[{:40}] PASS!\n", test.test_name);
          passed++;
        } else {
          fmt::print("[{:40}] FAIL!\n", test.test_name);
          fmt::print("expected:\n");
          for (auto& x : test.expected) {
            fmt::print(" \"{}\"\n", escaped_string(x));
          }
          fmt::print("result:\n");
          for (auto& x : test.actual) {
            fmt::print(" \"{}\"\n", escaped_string(x));
          }
        }
      }
    }
    fmt::print("Total: passed {}/{} passable tests, {} auto-passed\n", passed, passable, auto_pass);
  }
};

std::vector<std::string> get_test_pass_string(const std::string& name, int n_tests) {
  return {fmt::format("Test \"{}\": {} Passes\n0\n", name, n_tests)};
}

}  // namespace

TEST(CompilerAndRuntime, BuildGameAndTest) {
  std::thread runtime_thread([]() { exec_runtime(0, nullptr); });
  Compiler compiler;

  try {
    compiler.run_test("goal_src/test/test-build-game.gc");
  } catch (std::exception& e) {
    fprintf(stderr, "caught exception %s\n", e.what());
    EXPECT_TRUE(false);
  }

  // todo, tests after loading the game.
  CompilerTestRunner runner;
  runner.c = &compiler;

  runner.run_test("test-min-max.gc", {"10\n"});
  runner.run_test("test-bfloat.gc", {"data 1.2330 print 1.2330 type bfloat\n0\n"});
  runner.run_test("test-basic-type-check.gc", {"#f#t#t#f#t#f#t#t\n0\n"});
  runner.run_test("test-condition-boolean.gc", {"4\n"});
  runner.run_test("test-type-type.gc", {"#t#f\n0\n"});
  runner.run_test("test-access-inline-array.gc", {"1.2345\n0\n"});
  runner.run_test("test-find-parent-method.gc", {"\"test pass!\"\n0\n"});
  runner.run_test("test-ref.gc", {"83\n"});
  runner.run_test("test-pair-asize.gc", {"8\n"});
  runner.run_test("test-last.gc", {"d\n0\n"});
  runner.run_test("test-sort.gc",
                  {"(24 16 32 56 72 1234 -34 25 654)\n(1234 654 72 56 32 25 24 16 -34)\n0\n"});
  runner.run_test("test-sort-2.gc",
                  {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
  runner.run_test("test-sort-3.gc",
                  {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
  runner.run_test("test-pair-length.gc", {"6\n"});
  runner.run_test("test-member-1.gc", {"(c d)\n0\n"});
  runner.run_test("test-member-2.gc", {"#f\n0\n"});
  runner.run_test("test-assoc-1.gc", {"w\n0\n"});
  runner.run_test("test-assoc-2.gc", {"#f\n0\n"});
  runner.run_test("test-assoce-1.gc", {"x\n0\n"});
  runner.run_test("test-assoce-2.gc", {"x\n0\n"});
  runner.run_test("test-append.gc", {"(a b c d e)\n0\n"});
  runner.run_test("test-delete-list.gc", {"(a b d e)\n0\n"});
  runner.run_test("test-delete-car.gc", {"((a . b) (e . f))\n#f\n0\n"});
  runner.run_test("test-insert-cons.gc", {"((c . w) (a . b) (e . f))\n0\n"});
  runner.run_test("test-new-inline-array-class.gc", {"2820\n"});
  runner.run_test("test-memcpy.gc", {"13\n"});
  runner.run_test("test-memset.gc", {"11\n"});
  runner.run_test("test-binteger-print.gc", {"-17\n0\n"});
  runner.run_test("test-tests.gc", {"Test Failed On Test 0: \"unknown\"\nTest Failed On Test 0: "
                                    "\"test\"\nTest \"test-of-test\": 1 Passes\n0\n"});
  runner.run_test("test-type-arrays.gc", {"Test \"test-type-arrays\": 3 Passes\n0\n"});
  runner.run_test("test-number-comparison.gc", {"Test \"number-comparison\": 14 Passes\n0\n"});
  runner.run_test("test-approx-pi.gc", get_test_pass_string("approx-pi", 4));
  runner.run_test("test-dynamic-type.gc", get_test_pass_string("dynamic-type", 4));

  runner.print_summary();

  compiler.shutdown_target();
  runtime_thread.join();
}

// TODO -move these into another file?
TEST(CompilerAndRuntime, InlineIsInline) {
  Compiler compiler;
  auto code =
      compiler.get_goos().reader.read_from_file({"goal_src", "test", "test-declare-inline.gc"});
  auto compiled = compiler.compile_object_file("test-code", code, true);
  EXPECT_EQ(compiled->functions().size(), 2);
  auto& ir = compiled->top_level_function().code();
  bool got_mult = false;
  for (auto& x : ir) {
    EXPECT_EQ(dynamic_cast<IR_FunctionCall*>(x.get()), nullptr);
    auto as_im = dynamic_cast<IR_IntegerMath*>(x.get());
    if (as_im) {
      EXPECT_EQ(as_im->get_kind(), IntegerMathKind::IMUL_32);
      got_mult = true;
    }
  }
  EXPECT_TRUE(got_mult);
}

TEST(CompilerAndRuntime, AllowInline) {
  Compiler compiler;
  auto code =
      compiler.get_goos().reader.read_from_file({"goal_src", "test", "test-inline-call.gc"});
  auto compiled = compiler.compile_object_file("test-code", code, true);
  EXPECT_EQ(compiled->functions().size(), 2);
  auto& ir = compiled->top_level_function().code();
  int got_mult = 0;
  int got_call = 0;
  for (auto& x : ir) {
    if (dynamic_cast<IR_FunctionCall*>(x.get())) {
      got_call++;
    }
    auto as_im = dynamic_cast<IR_IntegerMath*>(x.get());
    if (as_im && as_im->get_kind() == IntegerMathKind::IMUL_32) {
      got_mult++;
    }
  }
  EXPECT_EQ(got_mult, 1);
  EXPECT_EQ(got_call, 1);
}

TEST(CompilerAndRuntime, CompilerTests) {
  std::thread runtime_thread([]() { exec_runtime(0, nullptr); });
  Compiler compiler;
  CompilerTestRunner runner;
  runner.c = &compiler;

  runner.run_test("test-return-integer-1.gc", {"4886718345\n"});
  runner.run_test("test-return-integer-2.gc", {"23\n"});
  runner.run_test("test-return-integer-3.gc", {"-17\n"});
  runner.run_test("test-return-integer-4.gc", {"-2147483648\n"});
  runner.run_test("test-return-integer-5.gc", {"-2147483649\n"});
  runner.run_test("test-return-integer-6.gc", {"0\n"});
  runner.run_test("test-return-integer-7.gc", {"-123\n"});
  runner.run_test("test-conditional-compilation-1.gc", {"3\n"});
  // todo, test-conditional-compilation-2.gc
  // these numbers match the game's memory layout for where the symbol table lives.
  // it's probably not 100% needed to get this exactly, but it's a good sign that the global
  // heap lives in the right spot because there should be no divergence in memory layout when its
  // built.  This also checks that #t, #f get "hashed" to the correct spot.
  runner.run_test("test-get-symbol-1.gc", {"1342756\n"});  // 0x147d24 in hex
  runner.run_test("test-get-symbol-2.gc", {"1342764\n"});  // 0x147d2c in hex
  runner.run_test("test-define-1.gc", {"17\n"});
  runner.run_test("test-nested-blocks-1.gc", {"7\n"});
  runner.run_test("test-nested-blocks-2.gc", {"8\n"});
  runner.run_test("test-nested-blocks-3.gc", {"7\n"});
  runner.run_test("test-goto-1.gc", {"3\n"});
  runner.run_test("test-defglobalconstant-1.gc", {"17\n"});
  runner.run_test("test-defglobalconstant-2.gc", {"18\n"});
  runner.run_test("test-simple-function-call.gc", {"30\n"});
  runner.run_test("test-application-lambda-1.gc", {"2\n"});
  runner.run_test("test-let-1.gc", {"30\n"});
  runner.run_test("test-let-star-1.gc", {"30\n"});
  runner.run_always_pass("test-string-constant-1.gc");

  std::string expected = "\"test string!\"";
  runner.run_test("test-string-constant-2.gc", {expected}, expected.size());
  runner.run_test("test-defun-return-constant.gc", {"12\n"});
  runner.run_test("test-defun-return-symbol.gc", {"42\n"});
  runner.run_test("test-function-return-arg.gc", {"23\n"});
  runner.run_test("test-nested-function-call.gc", {"2\n"});

  // math
  runner.run_test("test-add-int-constants.gc", {"13\n"});
  runner.run_test("test-add-int-vars.gc", {"7\n"});
  runner.run_test("test-add-int-multiple.gc", {"15\n"});
  runner.run_test("test-add-int-multiple-2.gc", {"15\n"});
  runner.run_test("test-add-function-returns.gc", {"21\n"});
  runner.run_test("test-sub-1.gc", {"4\n"});
  runner.run_test("test-sub-2.gc", {"4\n"});
  runner.run_test("test-mul-1.gc", {"-12\n"});
  runner.run_test("test-three-reg-add.gc", {"7\n"});
  runner.run_test("test-three-reg-sub.gc", {"3\n"});
  runner.run_test("test-three-reg-mult.gc", {"3\n"});
  runner.run_test("test-div-1.gc", {"6\n"});
  runner.run_test("test-div-2.gc", {"7\n"});
  runner.run_test("test-shiftvs.gc", {"11\n"});
  runner.run_test("test-ash.gc", {"18\n"});
  runner.run_test("test-negative-integer-symbol.gc", {"-123\n"});
  runner.run_test("test-mod.gc", {"7\n"});
  runner.run_test("test-nested-function-call-2.gc", {"10\n"});
  runner.run_test("test-logand.gc", {"4\n"});
  runner.run_test("test-logior.gc", {"60\n"});
  runner.run_test("test-logxor.gc", {"56\n"});

  expected = "test-string";
  runner.run_test("test-string-symbol.gc", {expected}, expected.size());
  runner.run_test("test-declare-inline.gc", {"32\n"});
  runner.run_test("test-inline-call.gc", {"44\n"});

  // float
  runner.run_test("test-floating-point-1.gc", {"1067316150\n"});

  runner.run_test("test-mlet.gc", {"10\n"});
  runner.run_test("test-set-symbol.gc", {"22\n"});
  runner.run_test("test-defsmacro-defgmacro.gc", {"20\n"});
  runner.run_test("test-desfun.gc", {"4\n"});

  runner.run_test("test-factorial-recursive.gc", {"3628800\n"});
  runner.run_test("test-factorial-loop.gc", {"3628800\n"});
  runner.run_test("test-protect.gc", {"33\n"});

  runner.run_test("test-format-reg-order.gc", {"test 1 2 3 4 5 6\n0\n"});
  runner.run_test("test-quote-symbol.gc", {"banana\n0\n"});

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
  //  runner.run_test("test-format.gc", {expected}, expected.size());

  runner.run_test("test-float-product.gc", {"120.0000\n0\n"});
  runner.run_test("test-float-in-symbol.gc", {"2345.6000\n0\n"});
  runner.run_test("test-function-return-constant-float.gc", {"3.14149\n0\n"});
  runner.run_test("test-float-function.gc", {"10.152\n0\n"});
  runner.run_test("test-float-pow-function.gc", {"256\n0\n"});
  runner.run_test("test-nested-float-functions.gc",
                  {"i 1.4400 3.4000\nr 10.1523\ni 1.2000 10.1523\nr 17.5432\n17.543 10.152\n0\n"});
  runner.run_test("test-deref-simple.gc", {"structure\n0\n"});
  runner.run_test("test-align16-1.gc", {"80\n"});
  runner.run_test("test-align16-2.gc", {"64\n"});
  runner.run_test("test-return-from-f.gc", {"77\n"});
  runner.run_test("test-return-from-f-tricky-color.gc", {"77\n"});
  runner.run_test("test-signed-int-compare.gc", {"12\n"});
  runner.run_test("test-return-value-of-if.gc", {"123\n"});
  runner.run_test("test-inline-array-field.gc", {"16\n"});
  runner.run_test("test-empty-pair.gc", {"()\n0\n"});
  runner.run_test("test-pair-check.gc", {"#t#f\n0\n"});
  runner.run_test("test-cons.gc", {"(a . b)\n0\n"});
  runner.run_test("test-list.gc", {"(a b c d)\n0\n"});
  runner.run_test("test-car-cdr-get.gc", {"ab\n0\n"});
  runner.run_test("test-car-cdr-set.gc", {"(c . d)\n0\n"});
  runner.run_test("test-nested-car-cdr-set.gc", {"efgh\n((e . g) f . h)\n0\n"});
  runner.run_test("test-dotimes.gc", {"4950\n"});
  runner.run_test("test-methods.gc", {"#t#t\n0\n"});
  runner.run_test("test-pointers-1.gc", {"13\n"});

  compiler.shutdown_target();
  runtime_thread.join();
  runner.print_summary();
}
