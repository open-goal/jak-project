/*!
 * @file main.cpp
 * Main for the game. Launches the runtime.
 */

#define STBI_WINDOWS_UTF8

#include <string>

#include "runtime.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/os.h"
#include "common/util/unicode_util.h"
#include "common/versions.h"

#ifdef _WIN32
extern "C" {
__declspec(dllexport) unsigned long NvOptimusEnablement = 0x00000001;
__declspec(dllexport) int AmdPowerXpressRequestHighPerformance = 1;
}
#endif

/*!
 * Set up logging system to log to file.
 * @param verbose : should we print debug-level messages to stdout?
 */
void setup_logging(bool verbose) {
  lg::set_file(file_util::get_file_path({"log", "game.log"}));
  if (verbose) {
    lg::set_file_level(lg::level::debug);
    lg::set_stdout_level(lg::level::debug);
    lg::set_flush_level(lg::level::debug);
  } else {
    lg::set_file_level(lg::level::debug);
    lg::set_stdout_level(lg::level::warn);
    lg::set_flush_level(lg::level::warn);
  }
  lg::initialize();
}

/*!
 * Entry point for the game.
 */
int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  // TODO - replace with CLI11 and just propagate args through
  // - https://github.com/CLIUtils/CLI11/issues/744

  // Figure out if the CPU has AVX2 to enable higher performance AVX2 versions of functions.
  setup_cpu_info();
  // If the CPU doesn't have AVX, GOAL code won't work and we exit.
  if (!get_cpu_info().has_avx) {
    printf("Your CPU does not support AVX, which is required for OpenGOAL.\n");
    return -1;
  }

  // parse arguments
  bool verbose = false;
  bool disable_avx2 = false;
  std::optional<fs::path> project_path_override = std::nullopt;
  for (int i = 1; i < argc; i++) {
    if (std::string("-v") == argv[i]) {
      verbose = true;
      break;
    }

    if (std::string("-no-avx2") == argv[i]) {
      disable_avx2 = true;
    }

    if (std::string("-proj-path") == argv[i] && i + 1 < argc) {
      project_path_override = std::make_optional(fs::path(argv[i + 1]));
    }
  }

  // set up file paths for resources. This is the full repository when developing, and the data
  // directory (a subset of the full repo) in release versions
  if (!file_util::setup_project_path(project_path_override)) {
    return 1;
  }

  if (disable_avx2) {
    // for debugging the non-avx2 code paths, there's a flag to manually disable.
    printf("Note: AVX2 code has been manually disabled.\n");
    get_cpu_info().has_avx2 = false;
  }

#ifndef __AVX2__
  if (get_cpu_info().has_avx2) {
    // printf("Note: your CPU supports AVX2, but this build was not compiled with AVX2 support\n");
    get_cpu_info().has_avx2 = false;
  }
#endif

  if (get_cpu_info().has_avx2) {
    printf("AVX2 mode enabled\n");
  } else {
    printf("AVX2 mode disabled\n");
  }

  try {
    setup_logging(verbose);
  } catch (const std::exception& e) {
    lg::error("Failed to setup logging: {}", e.what());
    return 1;
  }

  bool force_debug_next_time = false;
  while (true) {
    std::vector<std::string> args;
    for (int i = 0; i < argc; i++) {
      args.push_back(argv[i]);
    }
    if (force_debug_next_time) {
      args.push_back("-boot");
      args.push_back("-debug");
      force_debug_next_time = false;
    }
    std::vector<char*> ptrs;
    for (auto& str : args) {
      ptrs.push_back(str.data());
    }

    // run the runtime in a loop so we can reset the game and have it restart cleanly
    lg::info("OpenGOAL Runtime {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
    try {
      auto exit_status = exec_runtime(ptrs.size(), ptrs.data());
      switch (exit_status) {
        case RuntimeExitStatus::EXIT:
          return 0;
        case RuntimeExitStatus::RESTART_RUNTIME:
        case RuntimeExitStatus::RUNNING:
          break;
        case RuntimeExitStatus::RESTART_IN_DEBUG:
          force_debug_next_time = true;
          break;
      }
    } catch (std::exception& ex) {
      lg::error("Unexpected exception occurred - {}", ex.what());
      throw ex;
    }
  }
  return 0;
}
