#include <cstdio>
#include "goalc/compiler/Compiler.h"

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;
  printf("goal compiler\n");

  Compiler compiler;
  compiler.execute_repl();

  return 0;
}
