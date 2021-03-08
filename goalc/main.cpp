#include <cstdio>
#include "goalc/compiler/Compiler.h"
#include "common/versions.h"
#include "common/util/FileUtil.h"
#include "common/log/log.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/color.h"

#include "common/goos/ReplUtils.h"

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

  // Init REPL
  std::unique_ptr<Compiler> compiler = std::make_unique<Compiler>();

  if (argument.empty()) {
    ReplStatus status = ReplStatus::WANT_RELOAD;
    while (status == ReplStatus::WANT_RELOAD) {
      compiler = std::make_unique<Compiler>(std::make_unique<ReplWrapper>());
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
