#include <cstdio>
#include "goalc/compiler/Compiler.h"
#include "common/versions.h"

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;
  printf("OpenGOAL Compiler %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  Compiler compiler;
  compiler.execute_repl();

  return 0;
}
