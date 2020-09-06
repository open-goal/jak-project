#include <thread>

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
    usleep(1000);
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
  };

  std::vector<Test> tests;

  void run_test(const std::string& test_file, const std::vector<std::string>& expected) {
    auto result = c->run_test("goal_src/test/" + test_file);
    EXPECT_EQ(result, expected);
    tests.push_back({expected, result, test_file});
  }

  void print_summary() {
    fmt::print("~~ Compiler Test Summary for {} tests... ~~\n", tests.size());
    int passed = 0;
    for (auto& test : tests) {
      if (test.expected == test.actual) {
        fmt::print("[{:30}] PASS!\n", test.test_name);
        passed++;
      } else {
        fmt::print("[{:30}] FAIL!\n", test.test_name);
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
    fmt::print("Total: passed {}/{} tests\n", passed, tests.size());
  }
};

}  // namespace

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

  compiler.shutdown_target();
  runtime_thread.join();
  runner.print_summary();
}