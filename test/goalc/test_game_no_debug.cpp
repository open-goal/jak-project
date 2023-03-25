// Test jak1 running without loading debug segments.

#include "goalc/compiler/Compiler.h"
#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

TEST(Jak1NoDebugSegment, Init) {
  Compiler compiler(GameVersion::Jak1);
  compiler.run_front_end_on_string("(build-kernel)");
  std::thread runtime_thread = std::thread(GoalTest::runtime_with_kernel_no_debug_segment);

  compiler.run_test_from_string("(set! *use-old-listener-print* #t)");

  // this shouldn't crash
  compiler.run_test_from_string("(inspect *kernel-context*)");

  // these should be equal, both the fallback inspect method
  EXPECT_TRUE(compiler.run_test_from_string("(printl (eq? (method-of-type kernel-context inspect) "
                                            "(method-of-type cpu-thread inspect))) 0") ==
              std::vector<std::string>{"#t\n0\n"});

  // should be below the debug heap.
  EXPECT_TRUE(compiler.run_test_from_string("(printl (< (the uint (method-of-type kernel-context "
                                            "inspect)) (the uint (-> debug base)))) 0") ==
              std::vector<std::string>{"#t\n0\n"});

  // debug segment flag should be disabled.
  EXPECT_TRUE(compiler.run_test_from_string("(printl *debug-segment*) 0") ==
              std::vector<std::string>{"#f\n0\n"});

  compiler.shutdown_target();
  runtime_thread.join();
}