#include <cstdio>
#include <regex>

#include "common/goos/ReplUtils.h"
#include "common/log/log.h"
#include "common/nrepl/ReplServer.h"
#include "common/util/FileUtil.h"
#include "common/util/diff.h"
#include "common/versions.h"

#include "goalc/compiler/Compiler.h"

#include "third-party/CLI11.hpp"
#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"

void setup_logging() {
  lg::set_file(file_util::get_file_path({"log/compiler.txt"}));
  lg::set_file_level(lg::level::info);
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  lg::initialize();
}

int main(int argc, char** argv) {
  bool auto_listen = false;
  bool auto_debug = false;
  bool auto_find_user = false;
  std::string cmd = "";
  std::string startup_cmd = "";
  std::string username = "#f";
  std::string game = "jak1";
  int nrepl_port = 8181;
  fs::path project_path_override;

  // TODO - a lot of these flags could be deprecated and moved into `repl-config.json`
  // TODO - auto-find the user if there is only one folder within `user/`
  CLI::App app{"OpenGOAL Compiler / REPL"};
  app.add_option("-c,--cmd", cmd, "Specify a command to run");
  app.add_option("--startup-cmd", startup_cmd,
                 "Specify a command to run and keep the REPL open afterwards");
  app.add_option("-u,--user", username,
                 "Specify the username to use for your user profile in 'goal_src/user/'");
  app.add_option("-p,--port", nrepl_port, "Specify the nREPL port.  Defaults to 8181");
  app.add_flag("--auto-lt", auto_listen,
               "Attempt to automatically connect to the listener on startup");
  app.add_flag("--auto-dbg", auto_debug,
               "Attempt to automatically connect to the debugger on startup");
  app.add_flag("--user-auto", auto_find_user,
               "Attempt to automatically deduce the user, overrides '-user'");
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

  std::vector<std::string> user_startup_commands = {};
  std::optional<std::string> repl_config = {};

  if (auto_find_user) {
    username = "#f";
    std::regex allowed_chars("[0-9a-zA-Z\\-\\.\\!\\?<>]");
    try {
      auto text = std::make_shared<goos::FileText>(
          file_util::get_file_path({"goal_src", "user", "user.txt"}), "goal_src/user/user.txt");
      goos::TextStream ts(text);
      ts.seek_past_whitespace_and_comments();
      std::string found_username;
      while (ts.text_remains()) {
        auto character = ts.read();
        if (!std::regex_match(std::string(1, character), allowed_chars)) {
          break;
        }
        found_username.push_back(character);
      }
      if (!found_username.empty()) {
        username = found_username;
        // Check for a `startup.gc` file, each line will be executed on the REPL on startup
        auto startup_file_path =
            file_util::get_file_path({"goal_src", "user", username, "startup.gc"});
        if (file_util::file_exists(startup_file_path)) {
          auto data = file_util::read_text_file(startup_file_path);
          auto startup_cmds = split_string(data);
          for (const auto& cmd : startup_cmds) {
            user_startup_commands.push_back(cmd);
          }
        }
        // Check for a `repl-config.json` file, so things can be configured without tons of flags
        auto repl_config_path =
            file_util::get_file_path({"goal_src", "user", username, "repl-config.json"});
        if (file_util::file_exists(repl_config_path)) {
          repl_config = file_util::read_text_file(repl_config_path);
        }
      }
    } catch (std::exception& e) {
      printf("error opening user desc file: %s\n", e.what());
    }
  }

  setup_logging();

  lg::info("OpenGOAL Compiler {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  // Init REPL
  ReplStatus status = ReplStatus::WANT_RELOAD;
  std::function<bool()> shutdown_callback = [&]() { return status == ReplStatus::WANT_EXIT; };
  ReplServer repl_server(shutdown_callback, nrepl_port);
  bool repl_server_ok = repl_server.init_server();
  std::thread nrepl_thread;
  // the compiler may throw an exception if it fails to load its standard library.
  try {
    std::unique_ptr<Compiler> compiler;
    std::mutex compiler_mutex;
    // if a command is provided on the command line, no REPL just run the compiler on it
    if (!cmd.empty()) {
      compiler = std::make_unique<Compiler>(game_version);
      compiler->run_front_end_on_string(cmd);
      return 0;
    }
    compiler = std::make_unique<Compiler>(game_version, username, std::make_unique<ReplWrapper>());
    if (repl_config) {
      compiler->update_via_config_file(repl_config.value(), game);
    }
    // Start nREPL Server
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
    // Run automatic forms if applicable
    // - this should probably be deprecated in favor of the `startup.gc` file
    if (user_startup_commands.empty() && (auto_debug || auto_listen)) {
      user_startup_commands.push_back("(lt)");
    }
    if (user_startup_commands.empty() && auto_debug) {
      std::lock_guard<std::mutex> lock(compiler_mutex);
      status = compiler->handle_repl_string("(dbgc)");
      user_startup_commands.push_back("(dbgc)");
    }

    if (!user_startup_commands.empty()) {
      std::lock_guard<std::mutex> lock(compiler_mutex);
      for (const auto& cmd : user_startup_commands) {
        status = compiler->handle_repl_string(cmd);
      }
    }

    // Poll Terminal
    while (status != ReplStatus::WANT_EXIT) {
      if (status == ReplStatus::WANT_RELOAD) {
        lg::info("Reloading compiler...");
        std::lock_guard<std::mutex> lock(compiler_mutex);
        if (compiler) {
          compiler->save_repl_history();
        }
        compiler =
            std::make_unique<Compiler>(game_version, username, std::make_unique<ReplWrapper>());
        if (repl_config) {
          compiler->update_via_config_file(repl_config.value(), game);
        }
        if (!startup_cmd.empty()) {
          compiler->handle_repl_string(startup_cmd);
          // reset to prevent re-executing on manual reload
          startup_cmd = "";
        }
        status = ReplStatus::OK;
      }
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

  // Cleanup
  if (repl_server_ok) {
    repl_server.shutdown_server();
    nrepl_thread.join();
  }
  return 0;
}
