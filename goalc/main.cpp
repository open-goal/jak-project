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
  if (!file_util::setup_project_path()) {
    return 1;
  }
  std::string argument;
  std::string username = "#f";
  bool verbose = false;
  bool auto_listen = false;
  bool auto_debug = false;
  for (int i = 1; i < argc; i++) {
    if (std::string("-v") == argv[i]) {
      verbose = true;
    } else if (std::string("-cmd") == argv[i] && i + 1 < argc) {
      argument = argv[++i];
    } else if (std::string("-auto-lt") == argv[i]) {
      auto_listen = true;
    } else if (std::string("-auto-dbg") == argv[i]) {
      auto_debug = true;
    } else if (std::string("-user") == argv[i] && i + 1 < argc) {
      username = argv[++i];
    } else if (std::string("-user-auto") == argv[i]) {
      try {
        auto text = std::make_shared<goos::FileText>(
            file_util::get_file_path({"goal_src", "user", "user.txt"}), "goal_src/user/user.txt");
        goos::TextStream ts(text);
        ts.seek_past_whitespace_and_comments();
        username.clear();
        while (ts.text_remains()) {
          char c = ts.read();
          if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
              c == '-' || c == '.' || c == '!' || c == '?' || c == '<' || c == '>') {
            username.push_back(c);
          } else {
            break;
          }
        }
        if (username.empty()) {
          username = "#f";
        }
      } catch (std::exception& e) {
        printf("error opening user desc file: %s\n", e.what());
        username = "#f";
      }
    }
  }
  setup_logging(verbose);

  lg::info("OpenGOAL Compiler {}.{}", versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR);

  // Init REPL
  // the compiler may throw an exception if it fails to load its standard library.
  try {
    std::unique_ptr<Compiler> compiler;
    if (argument.empty()) {
      ReplStatus status = ReplStatus::WANT_RELOAD;
      while (status == ReplStatus::WANT_RELOAD) {
        compiler = std::make_unique<Compiler>(username, std::make_unique<ReplWrapper>());
        status = compiler->execute_repl(auto_listen, auto_debug);
        if (status == ReplStatus::WANT_RELOAD) {
          fmt::print("Reloading compiler...\n");
        }
      }
    } else {
      compiler = std::make_unique<Compiler>();
      compiler->run_front_end_on_string(argument);
    }
  } catch (std::exception& e) {
    fmt::print("Compiler Fatal Error: {}\n", e.what());
  }

  return 0;
}
