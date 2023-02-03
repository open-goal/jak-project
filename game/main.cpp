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

#include "third-party/CLI11.hpp"

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
  lg::set_file(file_util::get_file_path({"log/game.txt"}));
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

  // CLI flags
  bool verbose_logging = false;
  bool disable_avx2 = false;
  fs::path project_path_override;
  std::vector<std::string> game_args;
  CLI::App app{"OpenGOAL Game Runtime"};
  app.add_flag("-v,--verbose", verbose_logging, "Enable verbose logging on stdout");
  app.add_flag("--no-avx2", verbose_logging, "Disable AVX2 for testing");
  app.add_option("--proj-path", project_path_override,
                 "Specify the location of the 'data/' folder");
  // TODO - document the rest and add descriptions
  app.footer(
      "Game Args (passed through after '--'):\n  -boot\n  -debug\n  -nodisplay\n  -vm\n  -novm\n  "
      "-jak2");
  app.add_option("Game Args", game_args,
                 "Remaining arguments (after '--') that are passed-through to the game itself");
  app.allow_extras();
  CLI11_PARSE(app, argc, argv);

  // Figure out if the CPU has AVX2 to enable higher performance AVX2 versions of functions.
  setup_cpu_info();
  // If the CPU doesn't have AVX, GOAL code won't work and we exit.
  if (!get_cpu_info().has_avx) {
    lg::info("Your CPU does not support AVX, which is required for OpenGOAL.");
    return -1;
  }

  // set up file paths for resources. This is the full repository when developing, and the data
  // directory (a subset of the full repo) in release versions
  if (project_path_override.empty()) {
    if (!file_util::setup_project_path({})) {
      return 1;
    } else if (!file_util::setup_project_path(project_path_override)) {
      return 1;
    }
  }

  if (disable_avx2) {
    // for debugging the non-avx2 code paths, there's a flag to manually disable.
    lg::info("Note: AVX2 code has been manually disabled.");
    get_cpu_info().has_avx2 = false;
  }

#ifndef __AVX2__
  if (get_cpu_info().has_avx2) {
    // printf("Note: your CPU supports AVX2, but this build was not compiled with AVX2 support\n");
    get_cpu_info().has_avx2 = false;
  }
#endif

  if (get_cpu_info().has_avx2) {
    lg::info("AVX2 mode enabled");
  } else {
    lg::info("AVX2 mode disabled");
  }

  setup_logging(verbose_logging);

  bool force_debug_next_time = false;
  std::vector<char*> arg_ptrs;
  for (auto& str : game_args) {
    arg_ptrs.push_back(str.data());
  }
  while (true) {
    if (force_debug_next_time) {
      // NOTE - I presume the game doesn't care if there are duplicates?
      game_args.push_back("-boot");
      game_args.push_back("-debug");
      force_debug_next_time = false;
      arg_ptrs.clear();
      for (auto& str : game_args) {
        arg_ptrs.push_back(str.data());
      }
    }

    // run the runtime in a loop so we can reset the game and have it restart cleanly
    lg::info("OpenGOAL Runtime {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
    try {
      auto exit_status = exec_runtime(arg_ptrs.size(), arg_ptrs.data());
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
