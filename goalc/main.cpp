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
  if (!file_util::setup_project_path(std::nullopt)) {
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

  std::string auto_input;
  if (auto_debug || auto_listen) {
    auto_input.append("(lt)");
  }
  if (auto_debug) {
    auto_input.append("(dbg) (:cont)");
  }

  // Init REPL
  // the compiler may throw an exception if it fails to load its standard library.
  try {
    std::unique_ptr<Compiler> compiler;
    std::mutex compiler_mutex;
    if (argument.empty()) {

      // for example, start a separate thread
      // std::thread server_thread([&](){
      //   while (!want_exit) {
      //     std::string msg = server.get_msg();
      //     {
      //       // only locked inside this scope
      //       std::lock_guard<std::mutex> lock(compiler_mutex);
      //       status = compiler->handle_repl_string(msg);
      //     }
      //   }
      // });

      ReplStatus status = ReplStatus::WANT_RELOAD;
      // loop, until the user requests an exit
      while (status != ReplStatus::WANT_EXIT) {
        // if we want to reload the compiler, reconstruct it
        if (status == ReplStatus::WANT_RELOAD) {
          // lock, in case something else is using it
          std::lock_guard<std::mutex> lock(compiler_mutex);
          compiler = std::make_unique<Compiler>(username, std::make_unique<ReplWrapper>());
          status = ReplStatus::OK;
        }

        std::string input_from_stdin = compiler->get_repl_input();
        if (!input_from_stdin.empty()) {
          // lock, while we compile
          std::lock_guard<std::mutex> lock(compiler_mutex);
          status = compiler->handle_repl_string(input_from_stdin);
        }

        if (!auto_input.empty()) {
          // lock, while we compile
          std::lock_guard<std::mutex> lock(compiler_mutex);
          status = compiler->handle_repl_string(auto_input);
          auto_input.clear();
        }
      }

      // server_thread.join();
    } else {
      compiler = std::make_unique<Compiler>();
      compiler->run_front_end_on_string(argument);
    }
  } catch (std::exception& e) {
    fmt::print("Compiler Fatal Error: {}\n", e.what());
  }

  return 0;
}
