#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/versions/versions.h"

#include "goalc/compiler/Compiler.h"

int main(int argc, char** argv) {
  // logging
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();

  // game version
  std::string game = "jak1";
  if (argc > 1) {
    game = argv[1];
  }
  GameVersion game_version = game_name_to_version(game);

  // path
  if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }

  lg::info("OpenGOAL Compiler {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  std::unique_ptr<Compiler> compiler;
  ReplStatus status = ReplStatus::OK;
  try {
    compiler = std::make_unique<Compiler>(game_version, std::nullopt, "",
                                          std::make_unique<REPL::Wrapper>(game_version));
    while (status != ReplStatus::WANT_EXIT) {
      if (status == ReplStatus::WANT_RELOAD) {
        lg::info("Reloading compiler...");
        if (compiler) {
          compiler->save_repl_history();
        }
        compiler = std::make_unique<Compiler>(game_version, std::nullopt, "",
                                              std::make_unique<REPL::Wrapper>(game_version));
        status = ReplStatus::OK;
      }
      std::string input_from_stdin = compiler->get_repl_input();
      if (!input_from_stdin.empty()) {
        status = compiler->handle_repl_string(input_from_stdin);
      }
    }
  } catch (std::exception& e) {
    lg::error("Compiler Fatal Error: {}", e.what());
  }

  return 0;
}
