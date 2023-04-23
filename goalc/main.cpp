#include <cstdio>
#include <regex>

#include "common/log/log.h"
#include "common/repl/nrepl/ReplServer.h"
#include "common/repl/util.h"
#include "common/util/FileUtil.h"
#include "common/util/diff.h"
#include "common/util/string_util.h"
#include "common/util/unicode_util.h"
#include "common/versions/versions.h"

#include "goalc/compiler/Compiler.h"

#include "third-party/CLI11.hpp"
#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"

void setup_logging() {
  lg::set_file(file_util::get_file_path({"log", "compiler.log"}));
  lg::set_file_level(lg::level::info);
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();
}

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  bool auto_find_user = false;
  std::string cmd = "";
  std::string username = "#f";
  std::string game = "jak1";
  int nrepl_port = 8181;
  fs::path project_path_override;

  // TODO - a lot of these flags could be deprecated and moved into `repl-config.json`
  // TODO - auto-find the user if there is only one folder within `user/`
  CLI::App app{"OpenGOAL Compiler / REPL"};
  app.add_option("-c,--cmd", cmd, "Specify a command to run, no REPL is launched in this mode");
  app.add_option("-u,--user", username,
                 "Specify the username to use for your user profile in 'goal_src/user/'");
  app.add_option("-p,--port", nrepl_port, "Specify the nREPL port.  Defaults to 8181");
  app.add_flag("--user-auto", auto_find_user,
               "Attempt to automatically deduce the user, overrides '--user'");
  app.add_option("-g,--game", game, "The game name: 'jak1' or 'jak2'");
  app.add_option("--proj-path", project_path_override,
                 "Specify the location of the 'data/' folder");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  GameVersion game_version = game_name_to_version(game);

  if (!project_path_override.empty()) {
    if (!fs::exists(project_path_override)) {
      lg::error("Error: project path override '{}' does not exist", project_path_override.string());
      return 1;
    }
    if (!file_util::setup_project_path(project_path_override)) {
      lg::error("Could not setup project path!");
      return 1;
    }
  } else if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }

  try {
    setup_logging();
  } catch (const std::exception& e) {
    lg::error("Failed to setup logging: {}", e.what());
    return 1;
  }

  lg::info("OpenGOAL Compiler {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  // Figure out the username
  if (auto_find_user) {
    username = REPL::find_repl_username();
  }
  // Load the user's startup file
  auto startup_file = REPL::load_user_startup_file(username, game_version);
  // Load the user's REPL config
  auto repl_config = REPL::load_repl_config(username, game_version);

  // Init Compiler
  std::unique_ptr<Compiler> compiler;
  std::mutex compiler_mutex;
  // if a command is provided on the command line, no REPL just run the compiler on it
  try {
    if (!cmd.empty()) {
      compiler = std::make_unique<Compiler>(game_version);
      compiler->run_front_end_on_string(cmd);
      return 0;
    }
  } catch (std::exception& e) {
    lg::error("Compiler Fatal Error: {}", e.what());
    return 1;
  }

  // Otherwise, start the REPL normally
  ReplStatus status = ReplStatus::OK;
  std::function<void()> repl_startup_func = [&]() {
    // Run automatic forms if applicable
    std::lock_guard<std::mutex> lock(compiler_mutex);
    for (const auto& cmd : startup_file.run_before_listen) {
      status = compiler->handle_repl_string(cmd);
    }
  };

  // Initialize nREPL server socket
  std::function<bool()> shutdown_callback = [&]() { return status == ReplStatus::WANT_EXIT; };
  ReplServer repl_server(shutdown_callback, nrepl_port);
  bool repl_server_ok = repl_server.init_server();
  std::thread nrepl_thread;
  // the compiler may throw an exception if it fails to load its standard library.
  try {
    compiler = std::make_unique<Compiler>(
        game_version, std::make_optional(repl_config), username,
        std::make_unique<REPL::Wrapper>(username, repl_config, startup_file));
    // Start nREPL Server if it spun up successfully
    if (repl_server_ok) {
      nrepl_thread = std::thread([&]() {
        while (!shutdown_callback()) {
          auto resp = repl_server.get_msg();
          if (resp) {
            std::lock_guard<std::mutex> lock(compiler_mutex);
            status = compiler->handle_repl_string(resp.value());
            // Print out the prompt, just for better UX
            compiler->print_to_repl(compiler->get_prompt());
          }
          std::this_thread::sleep_for(std::chrono::microseconds(50000));
        }
      });
    }
    repl_startup_func();

    // Poll Terminal
    while (status != ReplStatus::WANT_EXIT) {
      if (status == ReplStatus::WANT_RELOAD) {
        lg::info("Reloading compiler...");
        std::lock_guard<std::mutex> lock(compiler_mutex);
        if (compiler) {
          compiler->save_repl_history();
        }
        compiler = std::make_unique<Compiler>(
            game_version, std::make_optional(repl_config), username,
            std::make_unique<REPL::Wrapper>(username, repl_config, startup_file));
        status = ReplStatus::OK;
      }
      // process user input
      std::string input_from_stdin = compiler->get_repl_input();
      if (!input_from_stdin.empty()) {
        // lock, while we compile
        std::lock_guard<std::mutex> lock(compiler_mutex);
        status = compiler->handle_repl_string(input_from_stdin);
      }
    }
  } catch (std::exception& e) {
    lg::error("Compiler Fatal Error: {}", e.what());
    status = ReplStatus::WANT_EXIT;
  }

  // TODO - investigate why there is such a delay when exitting

  // Cleanup
  if (repl_server_ok) {
    repl_server.shutdown_server();
    nrepl_thread.join();
  }
  return 0;
}
