#include "gtest/gtest.h"
#include "goalc/compiler/Compiler.h"
#include "test/goalc/framework/test_runner.h"

#ifdef __linux
TEST(Debugger, DebuggerBasicConnect) {
  Compiler compiler;
  // evidently you can't ptrace threads in your own process, so we need to run the runtime in a
  // separate process.
  if (!fork()) {
    GoalTest::runtime_no_kernel();
  } else {
    compiler.connect_to_target();
    compiler.poke_target();
    compiler.run_test("test/goalc/source_templates/with_game/attach-debugger.gc");
    EXPECT_TRUE(compiler.get_debugger().is_valid());
    EXPECT_TRUE(compiler.get_debugger().is_halted());
    compiler.shutdown_target();  // will detach/unhalt, then send the usual shutdown message

    // and now the child process should be done!
    EXPECT_TRUE(wait(nullptr) >= 0);
  }
}
#endif
