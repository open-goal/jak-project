#include <cstdio>
#include "goalc/compiler/Compiler.h"
#include "common/versions.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"
#include "third-party/spdlog/include/spdlog/sinks/basic_file_sink.h"
#include "third-party/spdlog/include/spdlog/sinks/stdout_color_sinks.h"

void setup_logging(bool verbose) {
  spdlog::set_level(spdlog::level::debug);
  if (verbose) {
    auto game_logger = spdlog::stdout_color_mt("GOAL Compiler");
    spdlog::set_default_logger(game_logger);
    spdlog::flush_on(spdlog::level::info);
    spdlog::set_pattern("%v");
    spdlog::info("Verbose logging enabled");
  } else {
    auto game_logger = spdlog::basic_logger_mt("GOAL Compiler", "logs/compiler.log");
    spdlog::set_default_logger(game_logger);
    spdlog::flush_on(spdlog::level::debug);
    printf("OpenGOAL Compiler %d.%d\n", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);
  }
}

int main(int argc, char** argv) {
  (void)argc;
  (void)argv;

  bool verbose = false;
  for (int i = 1; i < argc; i++) {
    if (std::string("-v") == argv[i]) {
      verbose = true;
      break;
    }
  }
  setup_logging(verbose);

  spdlog::info("OpenGOAL Compiler {}.{}", versions::GOAL_VERSION_MAJOR,
               versions::GOAL_VERSION_MINOR);

  Compiler compiler;
  compiler.execute_repl();

  return 0;
}
