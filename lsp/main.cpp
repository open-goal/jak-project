// clang-format off

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <optional>
#include <vector>
#include <regex>

#include "common/log/log.h"
#include "common/util/unicode_util.h"

#include "lsp/handlers/lsp_router.h"
#include "lsp/state/workspace.h"
#include "lsp/transport/stdio.h"
#include "lsp/state/app.h"

#include "third-party/CLI11.hpp"

// clang-format on

// NOTE - if we ever add HTTP support to the LSP
/*
  What needs to be understood is that for connection timing issues the server is actually a client
  and the client is the server in terms of opening the ports.

  When you specify a socket transport the client is listening on that port for a connection. The
  socket port number is passed as --socket=${port} to the server process started.
*/

void setup_logging(bool verbose, std::string log_file) {
  if (!log_file.empty()) {
    lg::set_file(log_file);
  }
  if (verbose) {
    lg::set_file_level(lg::level::debug);
    lg::set_flush_level(lg::level::debug);
  } else {
    lg::set_file_level(lg::level::info);
    lg::set_flush_level(lg::level::info);
  }

  // We use stdout to communicate with the client, so don't use it at all!
  lg::set_stdout_level(lg::level::off);
  lg::initialize();
}

int main(int argc, char** argv) {
  ArgumentGuard u8_guard(argc, argv);

  CLI::App app{"OpenGOAL Language Server"};

  bool use_stdin = true;
  bool verbose = false;
  std::string logfile;
  app.add_flag("--stdio", use_stdin,
               "Don't launch an HTTP server and instead accept input on stdin");
  app.add_flag("-v,--verbose", verbose, "Enable verbose logging");
  app.add_option("-l,--log", logfile, "Log file path");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  AppState appstate;
  LSPRouter lsp_router;
  appstate.verbose = verbose;
  setup_logging(appstate.verbose, logfile);
  lsp_router.init_routes();

  lg::info("OpenGOAL LSP Initialized, ready for requests");

#ifdef _WIN32
  _setmode(_fileno(stdout), _O_BINARY);
  _setmode(_fileno(stdin), _O_BINARY);
#endif

  char c;
  MessageBuffer message_buffer;
  while (std::cin.get(c)) {
    message_buffer.handle_char(c);

    if (message_buffer.message_completed()) {
      json body = message_buffer.body();
      auto method_name = body["method"].get<std::string>();
      lg::info(">>> Received message of method '{}'", method_name);
      auto responses = lsp_router.route_message(message_buffer, appstate);
      if (responses) {
        for (const auto& response : responses.value()) {
          std::cout << response.c_str() << std::flush;
          if (appstate.verbose) {
            lg::debug("<<< Sending message: {}", response);
          } else {
            lg::info("<<< Sending message of method '{}'", method_name);
          }
        }
      }
      message_buffer.clear();
    }
  }

  return 0;
}
