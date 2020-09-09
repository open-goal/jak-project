#include <cstdio>
#include "goalc/compiler/Compiler.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"
#include "third-party/spdlog/include/spdlog/fmt/fmt.h"

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;
  spdlog::debug("This is a spdlog debug message");    
  printf("goal compiler\n");

  Compiler compiler;
  compiler.execute_repl();

  return 0;
}
