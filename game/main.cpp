/*!
 * @file main.cpp
 * Main for the game. Launches the runtime.
 */

#include <string>
#include "runtime.h"
#include "common/versions.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "game/discord.h"
#include "common/util/os.h"

// Discord RPC
extern int64_t gStartTime;

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
  // do this as soon as possible - stuff like memcpy might use AVX instructions and we want to
  // warn the user instead of just crashing.
  setup_cpu_info();
  if (!get_cpu_info().has_avx) {
    printf("Your CPU does not support AVX, which is required for OpenGOAL.\n");
    return -1;
  }

  bool verbose = false;
  bool disable_avx2 = false;
  for (int i = 1; i < argc; i++) {
    if (std::string("-v") == argv[i]) {
      verbose = true;
      break;
    }

    if (std::string("-no-avx2") == argv[i]) {
      disable_avx2 = true;
    }
  }

  if (!file_util::setup_project_path()) {
    return 1;
  }

  gStartTime = time(0);
  init_discord_rpc();

  if (disable_avx2) {
    // for debugging the non-avx2 code paths, there's a flag to manually disable.
    printf("Note: AVX2 code has been manually disabled.\n");
    get_cpu_info().has_avx2 = false;
  }

#ifndef __AVX2__
  if (get_cpu_info().has_avx2) {
    printf("Note: your CPU supports AVX2, but this build was not compiled with AVX2 support\n");
    get_cpu_info().has_avx2 = false;
  }
#endif

  if (get_cpu_info().has_avx2) {
    printf("AVX2 mode enabled\n");
  } else {
    printf("AVX2 mode disabled\n");
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
