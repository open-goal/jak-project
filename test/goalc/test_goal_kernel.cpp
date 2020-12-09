#include <thread>
#include "goalc/compiler/Compiler.h"
#include "test/goalc/framework/test_runner.h"
#include "gtest/gtest.h"

class KernelTest : public testing::Test {
 public:
  static void SetUpTestSuite() {
    printf("Building kernel...\n");
    try {
      // a macro in goal-lib.gc
      compiler.run_front_end_on_string("(build-kernel)");
    } catch (std::exception& e) {
      fprintf(stderr, "caught exception %s\n", e.what());
      EXPECT_TRUE(false);
    }

    printf("Starting GOAL Kernel...\n");
    runtime_thread = std::thread(GoalTest::runtime_with_kernel);
    runner.c = &compiler;
  }

  static void TearDownTestSuite() {
    // send message to shutdown
    compiler.shutdown_target();
    // wait for shutdown.
    runtime_thread.join();
  }

  void SetUp() {}

  void TearDown() {}

  static std::thread runtime_thread;
  static Compiler compiler;
  static GoalTest::CompilerTestRunner runner;
};

std::thread KernelTest::runtime_thread;
Compiler KernelTest::compiler;
GoalTest::CompilerTestRunner KernelTest::runner;

TEST_F(KernelTest, Basic) {
  // first, let's load the kernel test code
  runner.c->run_test_from_string("(ml \"test/goalc/source_templates/kernel/kernel-test.gc\")");
  auto& listener = runner.c->listener();

  // record all print messages
  listener.record_messages(ListenerMessageKind::MSG_PRINT);

  // run the test.
  runner.c->compile_and_send_from_string("(kernel-test)");

  // kinda hacky, but wait until the kernel runs and sends all the messages
  while (listener.get_received_message_count() < 10) {
    std::this_thread::sleep_for(std::chrono::microseconds(1000));
  }

  auto messages = listener.stop_recording_messages();
  std::string result;
  for (auto& m : messages) {
    result += m;
  }

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