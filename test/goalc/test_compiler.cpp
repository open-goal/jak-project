#include "goalc/compiler/Compiler.h"
#include "gtest/gtest.h"

TEST(CompilerAndRuntime, ConstructCompiler) {
  Compiler compiler1(GameVersion::Jak1, emitter::kNativeInstructionSet);
  Compiler compiler2(GameVersion::Jak2, emitter::kNativeInstructionSet);
}
