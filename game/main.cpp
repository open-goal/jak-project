/*!
 * @file main.cpp
 * Main for the game. Launches the runtime.
 */

#include <string>
#include "runtime.h"
#include "common/versions.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"
#include "third-party/spdlog/include/spdlog/sinks/basic_file_sink.h"
#include "third-party/spdlog/include/spdlog/sinks/stdout_color_sinks.h"

void setup_logging(bool verbose) {
  spdlog::set_level(spdlog::level::debug);
  if (verbose) {
    auto game_logger = spdlog::stdout_color_mt("GOAL Runtime");
    spdlog::set_default_logger(game_logger);
    spdlog::flush_on(spdlog::level::info);
    spdlog::info("Verbose logging enabled");
  } else {
    auto game_logger = spdlog::basic_logger_mt("GOAL Runtime", "logs/runtime.log");
    spdlog::set_default_logger(game_logger);
    spdlog::flush_on(spdlog::level::debug);
    printf("OpenGOAL Runtime %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  }
}

int main(int argc, char** argv) {
  bool verbose = false;
  for (int i = 1; i < argc; i++) {
    if (std::string("-v") == argv[i]) {
      verbose = true;
      break;
    }
  }

  setup_logging(verbose);

  while (true) {
    // run the runtime in a loop so we can reset the game and have it restart cleanly
    spdlog::info("OpenGOAL Runtime {}.{}", versions::GOAL_VERSION_MAJOR,
                 versions::GOAL_VERSION_MINOR);

    if (exec_runtime(argc, argv) == 2) {
      return 0;
    }
  }
  return 0;
}
