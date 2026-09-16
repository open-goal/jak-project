#include <cstdio>
#include <regex>

#include "common/log/log.h"
#include "common/repl/nrepl/ReplServer.h"
#include "common/repl/repl_wrapper.h"
#include "common/util/FileUtil.h"
#include "common/util/diff.h"
#include "common/util/font/font_utils_korean.h"
#include "common/util/string_util.h"
#include "common/util/term_util.h"
#include "common/util/unicode_util.h"
#include "common/versions/versions.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/debugger/DebugServer.h"

#include "fmt/color.h"
#include "fmt/format.h"
#include "third-party/CLI11.hpp"

void setup_logging(const bool disable_ansi_colors) {
  lg::set_file_level(lg::level::info);
  lg::set_stdout_level(lg::level::info);
  lg::set_flush_level(lg::level::info);
  if (disable_ansi_colors) {
    lg::disable_ansi_colors();
  }
  lg::set_file("compiler");
  lg::initialize();
}

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  bool auto_find_user = false;
  std::string cmd = "";
  std::string username = "#f";
  std::string game = "jak1";
  int nrepl_port = -1;
  int debug_port = -1;
  fs::path project_path_override;
  fs::path iso_path_override;
  std::string instr_set_name = "x86";

  // TODO - a lot of these flags could be deprecated and moved into `repl-config.json`
  CLI::App app{"OpenGOAL Compiler / REPL"};
  app.add_option("-c,--cmd", cmd, "Specify a command to run, no REPL is launched in this mode");
  app.add_option("-u,--user", username,
                 "Specify the username to use for your user profile in 'goal_src/user/'");
  app.add_option("-p,--port", nrepl_port, "Specify the nREPL port.  Defaults to 8181");
  app.add_option("--debug-port", debug_port,
                 "Specify the port for the JSON debug server that external debuggers (such as the "
                 "VS Code extension) connect to.  Defaults to 8128 for jak1, 8129 for jak2, 8130 "
                 "for jak3");
  app.add_flag("--user-auto", auto_find_user,
               "Attempt to automatically deduce the user, overrides '--user'");
  app.add_option("-g,--game", game, "The game name: 'jak1' or 'jak2'");
  app.add_option("--proj-path", project_path_override,
                 "Specify the location of the 'data/' folder");
  app.add_option("--iso-path", iso_path_override, "Specify the location of the 'iso_data/' folder");
  app.add_option("--instruction-set", instr_set_name,
                 "Select the x86 or ARM64 code generation backend.");
  define_common_cli_arguments(app);
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  GameVersion game_version = game_name_to_version(game);

  emitter::InstructionSet instr_set;
  if (instr_set_name == "x86") {
    instr_set = emitter::InstructionSet::X86;
  } else if (instr_set_name == "arm64") {
    instr_set = emitter::InstructionSet::ARM64;
  } else {
    lg::error("Instruction set '{}' must be 'x86' or 'arm64'", instr_set_name);
    return 1;
  }

  if (!project_path_override.empty()) {
    if (!fs::exists(project_path_override)) {
      lg::error("Error: project path override '{}' does not exist", project_path_override.string());
      return 1;
    }
    if (!file_util::setup_project_path(project_path_override, true)) {
      lg::error("Could not setup project path!");
      return 1;
    }
  } else if (!file_util::setup_project_path(std::nullopt, true)) {
    return 1;
  }

  try {
    setup_logging(_cli_flag_disable_ansi);
  } catch (const std::exception& e) {
    lg::error("Failed to setup logging: {}", e.what());
    return 1;
  }

  // Figure out the username
  if (auto_find_user) {
    username = REPL::find_repl_username();
  }
  // Load the user's startup file
  auto startup_file = REPL::load_user_startup_file(username, game_version);
  // Load the user's REPL config
  auto repl_config = REPL::load_repl_config(username, game_version, nrepl_port);
  repl_config.temp_debug_port = debug_port;

  // Check for a custom ISO path before we instantiate the compiler.
  if (!iso_path_override.empty()) {
    if (!fs::exists(iso_path_override)) {
      lg::error("Error: iso path override '{}' does not exist", iso_path_override.string());
      return 1;
    }
    file_util::set_iso_data_dir(iso_path_override);
    repl_config.iso_path = iso_path_override.string();
  }

  // Init Compiler
  std::unique_ptr<Compiler> compiler;
  std::mutex compiler_mutex;
  // if a command is provided on the command line, no REPL just run the compiler on it
  try {
    if (!cmd.empty()) {
      compiler = std::make_unique<Compiler>(game_version, instr_set);
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
  ReplServer repl_server(shutdown_callback, repl_config.get_nrepl_port());
  bool nrepl_server_ok = repl_server.init_server(true);
  std::thread nrepl_thread;

  // Initialize the debug server socket for external debuggers to connect to
  DebugServer debug_server(shutdown_callback, repl_config.get_debug_port());
  bool debug_server_ok = debug_server.init_server(true);
  std::thread debug_thread;

  // the compiler may throw an exception if it fails to load its standard library.
  try {
    compiler = std::make_unique<Compiler>(
        game_version, instr_set, std::make_optional(repl_config), username,
        std::make_unique<REPL::Wrapper>(username, repl_config, startup_file, nrepl_server_ok));

    if (debug_server_ok) {
      debug_server.set_compiler(compiler.get(), &compiler_mutex);
      debug_thread = std::thread([&]() {
        while (!shutdown_callback()) {
          debug_server.run_once();
        }
      });
    }

    // Start nREPL Server if it spun up successfully
    if (nrepl_server_ok) {
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
            game_version, instr_set, std::make_optional(repl_config), username,
            std::make_unique<REPL::Wrapper>(username, repl_config, startup_file, nrepl_server_ok));
        if (debug_server_ok) {
          debug_server.set_compiler(compiler.get(), &compiler_mutex);
        }
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
  if (nrepl_server_ok) {
    repl_server.shutdown_server();
    nrepl_thread.join();
  }
  if (debug_server_ok) {
    debug_server.shutdown_server();
    debug_thread.join();
  }
  return 0;
}
