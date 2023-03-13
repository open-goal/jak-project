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

#include "game/common/game_common_types.h"

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

std::string game_arg_documentation() {
  // clang-format off
  std::string output = fmt::format(fmt::emphasis::bold, "Game Args (passed through to the game runtime after '--')\n");
  output += fmt::format(fmt::fg(fmt::color::gray), "Order matters, some args will negate others (see kmachine.cpp for details)\n");
  output += fmt::format(fmt::fg(fmt::color::gray), "Args with `*` are not well supported\n\n");
  // Common args
  output += fmt::format(fmt::emphasis::bold, "Common:\n");
  output += "  -cd          * Use the DVD drive for everything. This is how the game runs in retail\n";
  output += "  -cddata      * Use the DVD drive for everything but IOP modules\n";
  output += "  -deviso      * One of two modes for testing without the need for DVDs\n";
  output += "  -fakeiso       The other of two modes for testing without the need for DVDs\n";
  output += "  -boot          Used to set GOAL up for running the game in retail mode\n";
  output += "  -debug         Used to set GOAL up for debugging/development\n";
  output += "  -debug-mem     Used to set up GOAL in debug mode, but not to load debug-segments\n";
  output += "  -nokernel      An added mode to allow booting without a KERNEL.CGO for testing\n";
  output += "  -nosound       An added mode to allow booting without sound for testing\n";
  output += "  -level [name]  Used to inform the game to boot a specific level the default level is `#f`\n";
  // Jak 1 Related
  output += fmt::format(fmt::emphasis::bold | fmt::fg(fmt::color::orange), "Jak 1:\n");
  output += "  -demo          Used to pass the message `demo` to the gkernel in the DebugBootMessage (instead of play)\n";
  // Jak 2 only
  output += fmt::format(fmt::emphasis::bold | fmt::fg(fmt::color::purple), "Jak 2:\n");
  output += "  -kiosk         A demo mode, TODO on specifics\n";
  output += "  -preview       A demo mode, TODO on specifics\n";
  output += "  -debug-boot    Used to boot the game in retail mode, but with debug segments\n";
  output += "  -user [name]   Specify the debugging username, the default is `unknown`\n";
  output += "  -art [name]    Specify the art-group name to set `DebugBootArtGroup`, there is no default\n";
  // clang-format on
  return output;
}

/*!
 * Entry point for the game.
 */
int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  // TODO - this is a temporary shim to convert the old arg format
  // into the new
  //
  // This is needed to avoid a coupled release with the launcher and
  // can be removed after one release cycle
  //
  // Normal users launch gk with _no_ args
  //
  // Only handling args the launcher provides, all others can be changed
  // in this repo at the time of merge.
  std::vector<char*> adjusted_argv_vals;
  std::vector<char*> adjusted_argv_vals_passthru;
  for (int i = 0; i < argc; i++) {
    const auto& val = std::string(argv[i]);
    // Handle all args that aren't passed through
    if (val == "-proj-path") {
      adjusted_argv_vals.push_back("--proj-path");
      i++;
      if (i > argc) {
        return 1;
      }
      adjusted_argv_vals.push_back(argv[i]);
    } else if (val == "--") {
      // if we hit a '--' then break out, args will be matched but they are already in the new
      // format
      break;
    } else if (val == "-boot") {
      // now handle all the ones that get passed to the game
      adjusted_argv_vals_passthru.push_back("-boot");
    } else if (val == "-fakeiso") {
      adjusted_argv_vals_passthru.push_back("-fakeiso");
    } else if (val == "-debug") {
      adjusted_argv_vals_passthru.push_back("-debug");
    }
  }

  std::vector<char*> new_argv;
  if (!adjusted_argv_vals.empty() || !adjusted_argv_vals_passthru.empty()) {
    new_argv.push_back(argv[0]);
    for (const auto& arg : adjusted_argv_vals) {
      new_argv.push_back(arg);
    }
    if (!adjusted_argv_vals_passthru.empty()) {
      new_argv.push_back("--");
      for (const auto& arg : adjusted_argv_vals_passthru) {
        new_argv.push_back(arg);
      }
    }
    argv = new_argv.data();
    argc = new_argv.size();
  }
  // --- END temporary shim

  // CLI flags
  std::string game_name = "jak1";
  bool verbose_logging = false;
  bool disable_avx2 = false;
  bool disable_display = false;
  bool disable_debug_vm = false;
  fs::path project_path_override;
  std::vector<std::string> game_args;
  CLI::App app{"OpenGOAL Game Runtime"};
  app.add_option("-g,--game", game_name, "The game name: 'jak1' or 'jak2'");
  app.add_flag("-v,--verbose", verbose_logging, "Enable verbose logging on stdout");
  app.add_flag("--no-avx2", verbose_logging, "Disable AVX2 for testing");
  app.add_flag("--no-display", disable_display, "Disable video display");
  app.add_flag("--no-vm", disable_debug_vm, "Disable debug PS2 VM (defaulted to on)");
  app.add_option("--proj-path", project_path_override,
                 "Specify the location of the 'data/' folder");
  app.footer(game_arg_documentation());
  app.add_option("Game Args", game_args,
                 "Remaining arguments (after '--') that are passed-through to the game itself");
  app.allow_extras();
  CLI11_PARSE(app, argc, argv);

  // Create struct with all non-kmachine handled args to pass to the runtime
  GameLaunchOptions game_options;
  game_options.disable_debug_vm = disable_debug_vm;
  game_options.disable_display = disable_display;
  game_options.game_version = game_name_to_version(game_name);

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
    lg::info("No project path provided, looking for data/ folder in current directory");
    if (!file_util::setup_project_path({})) {
      return 1;
    }
  } else if (!file_util::setup_project_path(project_path_override)) {
    return 1;
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

  try {
    setup_logging(verbose_logging);
  } catch (const std::exception& e) {
    lg::error("Failed to setup logging: {}", e.what());
    return 1;
  }

  bool force_debug_next_time = false;
  // always start with an empty arg, as internally kmachine starts at `1` not `0`
  std::vector<char*> arg_ptrs = {""};
  for (auto& str : game_args) {
    arg_ptrs.push_back(str.data());
  }

  while (true) {
    if (force_debug_next_time) {
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
      auto exit_status = exec_runtime(game_options, arg_ptrs.size(), arg_ptrs.data());
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
