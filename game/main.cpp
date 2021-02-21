/*!
 * @file main.cpp
 * Main for the game. Launches the runtime.
 */

#include <string>
#include "runtime.h"
#include "common/versions.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"

void setup_logging(bool verbose) {
  lg::set_file(file_util::get_file_path({"log/game.txt"}));
  if (verbose) {
    lg::set_file_level(lg::level::debug);
    lg::set_stdout_level(lg::level::debug);
    lg::set_flush_level(lg::level::debug);
  } else {
    lg::set_file_level(lg::level::warn);
    lg::set_stdout_level(lg::level::warn);
    lg::set_flush_level(lg::level::warn);
  }
  lg::initialize();
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
    lg::info("OpenGOAL Runtime {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

    if (exec_runtime(argc, argv) == 2) {
      return 0;
    }
  }
  return 0;
}
