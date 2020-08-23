#include <cstdio>
#include "goalc/goos/Interpreter.h"

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;
  printf("goal compiler\n");

  goos::Interpreter interp;
  interp.execute_repl();

  return 0;
}

