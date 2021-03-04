#include <cstdio>
#include "goalc/compiler/Compiler.h"
#include "common/versions.h"
#include "common/util/FileUtil.h"
#include "common/log/log.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/color.h"

#include "common/goos/ReplHistory.h"

void setup_logging(bool verbose) {
  lg::set_file(file_util::get_file_path({"log/compiler.txt"}));
  if (verbose) {
    lg::set_file_level(lg::level::info);
    lg::set_stdout_level(lg::level::info);
    lg::set_flush_level(lg::level::info);
  } else {
    lg::set_file_level(lg::level::warn);
    lg::set_stdout_level(lg::level::warn);
    lg::set_flush_level(lg::level::warn);
  }
  lg::initialize();
}

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;

  std::string argument;
  bool verbose = false;
  for (int i = 1; i < argc; i++) {
    if (std::string("-v") == argv[i]) {
      verbose = true;
      break;
    }

    if (std::string("-cmd") == argv[i] && i < argc - 1) {
      argument = argv[++i];
    }
  }
  setup_logging(verbose);

  lg::info("OpenGOAL Compiler {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  std::unique_ptr<Compiler> compiler = std::make_unique<Compiler>();

  // Welcome message / brief intro for documentation
  std::string ascii;
  ascii += " _____             _____ _____ _____ __    \n";
  ascii += "|     |___ ___ ___|   __|     |  _  |  |   \n";
  ascii += "|  |  | . | -_|   |  |  |  |  |     |  |__ \n";
  ascii += "|_____|  _|___|_|_|_____|_____|__|__|_____|\n";
  ascii += "      |_|                                  \n";
  fmt::print(fmt::emphasis::bold | fg(fmt::color::orange), ascii);

  fmt::print("Welcome to OpenGOAL {}.{}!\n", versions::GOAL_VERSION_MAJOR,
             versions::GOAL_VERSION_MINOR);
  fmt::print("Run ");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(repl-help)");
  fmt::print(" for help with common commands and REPL usage.\n");
  fmt::print("Run ");
  fmt::print(fmt::emphasis::bold | fg(fmt::color::cyan), "(lt)");
  fmt::print(" to connect to the local target.\n");

  ReplHistory::repl_load_history();
  if (argument.empty()) {
    ReplStatus status = ReplStatus::WANT_RELOAD;
    while (status == ReplStatus::WANT_RELOAD) {
      compiler = std::make_unique<Compiler>();
      status = compiler->execute_repl();
      if (status == ReplStatus::WANT_RELOAD) {
        fmt::print("Reloading compiler...\n");
      }
    }
  } else {
    compiler->run_front_end_on_string(argument);
  }

  return 0;
}
