#include <cstdio>
#include "goalc/compiler/Compiler.h"
#include "common/versions.h"
#include "common/util/FileUtil.h"
#include "common/log/log.h"

#include "third-party/CLI11.hpp"
#include "third-party/fmt/core.h"
#include "third-party/fmt/color.h"

#include "common/goos/ReplUtils.h"
#include <regex>

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
  bool verbose = false;
  bool auto_listen = false;
  bool auto_debug = false;
  bool auto_find_user = false;
  std::string cmd = "";
  std::string username = "#f";
  int nrepl_port = 8181;

  CLI::App app{"OpenGOAL Compiler / REPL"};
  app.add_option("-c,--cmd", cmd, "Specify a command to run");
  app.add_option("-u,--user", username,
                 "Specify the username to use for your user profile in 'goal_src/user/'");
  app.add_option("-p,--port", nrepl_port, "Specify the nREPL port.  Defaults to 8181");
  app.add_flag("-v,--verbose", verbose, "Enable verbose output");
  app.add_flag("--auto-lt", auto_listen,
               "Attempt to automatically connect to the listener on startup");
  app.add_flag("--auto-dbg", auto_debug,
               "Attempt to automatically connect to the debugger on startup");
  app.add_flag("--user-auto", auto_find_user,
               "Attempt to automatically deduce the user, overrides '-user'");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  if (!file_util::setup_project_path(std::nullopt)) {
    return 1;
  }

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
        auto character = std::string(1, ts.read());
        if (std::regex_match(character, allowed_chars)) {
          found_username.push_back(ts.read());
        } else {
          break;
        }
      }
      if (!found_username.empty()) {
        username = found_username;
      }
    } catch (std::exception& e) {
      printf("error opening user desc file: %s\n", e.what());
    }
  }

  setup_logging(verbose);

  lg::info("OpenGOAL Compiler {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  // Init REPL
  // the compiler may throw an exception if it fails to load its standard library.
  try {
    std::unique_ptr<Compiler> compiler;
    if (!cmd.empty()) {
      compiler = std::make_unique<Compiler>();
      compiler->run_front_end_on_string(cmd);
    } else {
      ReplStatus status = ReplStatus::WANT_RELOAD;
      while (status == ReplStatus::WANT_RELOAD) {
        compiler = std::make_unique<Compiler>(nrepl_port, username, std::make_unique<ReplWrapper>());
        status = compiler->execute_repl(auto_listen, auto_debug);
        if (status == ReplStatus::WANT_RELOAD) {
          fmt::print("Reloading compiler...\n");
        }
      }
    }
  } catch (std::exception& e) {
    fmt::print("Compiler Fatal Error: {}\n", e.what());
  }

  return 0;
}
