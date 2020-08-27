#include <cstdio>
#include "goos/Goos.h"
#include "goos/GoosTest.h"
#include "goal/Goal.h"
#include "shared_config.h"

int main(int argc, char** argv) {
  printf("Goal III v. %d.%d alpha (Game Oriented Assembly Lisp)\n", GOAL_VERSION_MAJOR,
         GOAL_VERSION_MINOR);

  if (argc > 1 && std::string("--test-goos") == argv[1])
    run_all_tests();

  Goal goal;
  goal.execute_repl();

  return 0;
}