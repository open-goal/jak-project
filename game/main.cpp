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
    spdlog::set_level(spdlog::level::debug);
    auto game_logger = spdlog::basic_logger_mt("GOAL Runtime", "logs/runtime.log");
    spdlog::set_default_logger(game_logger);
    spdlog::flush_on(spdlog::level::info);

    // run the runtime in a loop so we can reset the game and have it restart cleanly
    spdlog::info("OpenGOAL Runtime {}.{}", versions::GOAL_VERSION_MAJOR,
                 versions::GOAL_VERSION_MINOR);

    if (exec_runtime(argc, argv) == 2) {
      return 0;
    }
  }
  return 0;
}
