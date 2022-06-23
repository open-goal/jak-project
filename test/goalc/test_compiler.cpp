#include <chrono>
#include <thread>

#include "game/runtime.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/listener/Listener.h"
#include "gtest/gtest.h"

TEST(CompilerAndRuntime, ConstructCompiler) {
  Compiler compiler;
}