#include <memory>
#include <thread>

#include "goalc/compiler/Compiler.h"
#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

class Jak1KernelTest : public testing::Test {
 public:
  static void SetUpTestSuite() {
    shared_compiler = std::make_unique<SharedCompiler>(GameVersion::Jak1);
    printf("Building kernel...\n");
    try {
      // a macro in goal-lib.gc
      shared_compiler->compiler.run_front_end_on_string("(build-kernel)");
    } catch (std::exception& e) {
      fprintf(stderr, "caught exception %s\n", e.what());
      EXPECT_TRUE(false);
    }

    printf("Starting GOAL Kernel...\n");
    shared_compiler->runtime_thread = std::thread(GoalTest::runtime_with_kernel_jak1);
    shared_compiler->runner.c = &shared_compiler->compiler;
    shared_compiler->compiler.run_test_from_string("(set! *use-old-listener-print* #t)");
  }

  static void TearDownTestSuite() {
    // send message to shutdown
    shared_compiler->compiler.shutdown_target();
    // wait for shutdown.
    shared_compiler->runtime_thread.join();
    shared_compiler.reset();
  }

  void SetUp() {}

  void TearDown() {}

  struct SharedCompiler {
    SharedCompiler(GameVersion v) : compiler(v) {}
    std::thread runtime_thread;
    Compiler compiler;
    GoalTest::CompilerTestRunner runner;
  };

  static std::unique_ptr<SharedCompiler> shared_compiler;
};

std::unique_ptr<Jak1KernelTest::SharedCompiler> Jak1KernelTest::shared_compiler;

namespace {
std::string send_code_and_get_multiple_responses(const std::string& code,
                                                 int n_responses,
                                                 GoalTest::CompilerTestRunner* runner) {
  auto& listener = runner->c->listener();
  // record all print messages
  listener.record_messages(ListenerMessageKind::MSG_PRINT);

  // run the test.
  runner->c->compile_and_send_from_string(code);
  std::string result;
  while (listener.get_received_message_count() < n_responses) {
    std::this_thread::sleep_for(std::chrono::microseconds(1000));
  }

  auto messages = listener.stop_recording_messages();
  for (auto& m : messages) {
    result += m;
  }
  return result;
}
}  // namespace

TEST_F(Jak1KernelTest, Basic) {
  shared_compiler->runner.c->run_test_from_string(
      "(ml \"test/goalc/source_templates/kernel/kernel-test.gc\")");
  std::string result =
      send_code_and_get_multiple_responses("(kernel-test)", 10, &shared_compiler->runner);

  std::string expected =
      "0\n"
      "proc2: 0\n"
      "TARGET FUNCTION 1 2 3\n"
      "4 5 6\n"
      "Stack Alignemnt 0/16\n"
      "proc1: 0\n"
      "proc2: 6\n"
      "proc1: 1\n"
      "proc2: 12\n"
      "proc1: 2\n"
      "proc2: 18\n"
      "proc1: 3\n"
      "proc2: 24\n"
      "proc1: 4\n"
      "proc2: 30\n"
      "proc1: 5\n"
      "DEACTIVATE PROC 1\n"
      "proc2: 36\n"
      "proc2: 42\n"
      "proc2: 48\n"
      "proc2: 54\n";

  EXPECT_EQ(expected, result);
}

TEST_F(Jak1KernelTest, RunFunctionInProcess) {
  shared_compiler->runner.c->run_test_from_string(
      "(ml \"test/goalc/source_templates/kernel/kernel-test.gc\")");
  std::string result =
      send_code_and_get_multiple_responses("(kernel-test-2)", 1, &shared_compiler->runner);

  std::string expected =
      "0\n"
      "Args: 1 2 3\n"
      "4 5 6\n"
      "Stack Alignemnt 0/16\n"
      "run-function-in-process result: init-child-proc-result\n"
      "Args: 0 2 3\n"
      "4 5 6\n"
      "Stack Alignemnt 0/16\n"
      "run-function-in-process result: #f\n";
  EXPECT_EQ(expected, result);
}

TEST_F(Jak1KernelTest, StateAndXmm) {
  shared_compiler->runner.c->run_test_from_string(
      "(ml \"test/goalc/source_templates/kernel/kernel-test.gc\")");
  std::string result =
      send_code_and_get_multiple_responses("(state-test)", 5, &shared_compiler->runner);

  std::string expected =
      "0\nenter wreck: 3 4 5 6\nwreck: 3 4 5 6\nenter check: 9 8 7 6\nrun xmm-check 12.3400 "
      "45.6300 9 8 7 6\nwreck: 3 4 5 6\nrun xmm-check 12.3400 45.6300 9 8 7 6\nwreck: 3 4 5 6\nrun "
      "xmm-check 12.3400 45.6300 9 8 7 6\nwreck: 3 4 5 6\nexit check\nenter die\ntime to "
      "die!\nexit die\nexit wreck\nenter die\ntime to die!\nexit die\n";
  EXPECT_EQ(expected, result);
}

TEST_F(Jak1KernelTest, ThrowXmm) {
  shared_compiler->runner.c->run_test_from_string(
      "(ml \"test/goalc/source_templates/kernel/kernel-test.gc\")");
  std::string result =
      send_code_and_get_multiple_responses("(throw-backup-test)", 1, &shared_compiler->runner);

  std::string expected =
      "value now is 10.1000\n"
      "now its 10.1000\n"
      "0\n";
  EXPECT_EQ(expected, result);
}
