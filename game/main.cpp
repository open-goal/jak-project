/*!
 * @file main.cpp
 * Main for the game. Launches the runtime.
 */
#include <cstdio>
#include "runtime.h"
#include "common/versions.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"
#include "third-party/spdlog/include/spdlog/sinks/basic_file_sink.h"

int main(int argc, char** argv) {
  while (true) {
    // run the runtime in a loop so we can reset the game and have it restart cleanly
    printf("gk %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

    spdlog::info("spdlog initialized");
    spdlog::error("test error", 1);
    if (exec_runtime(argc, argv) == 2) {
      return 0;
    }
  }
  return 0;
}