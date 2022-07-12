#include <chrono>
#include <fcntl.h>
#include <io.h>
#include <iostream>
#include <optional>
#include <stdio.h>
#include <thread>
#include <vector>
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <regex>
#include <windows.h>

#include <common/log/log.h>

#include "handlers/lsp_router.h"
#include "state/workspace.h"
#include "transport/stdio.h"
#include <state/app.h>

#include "third-party/CLI11.hpp"

// TODO - look into replacing our xsocket with cpphttplib eventually

// NOTE - if we ever add HTTP support to the LSP
/*
  What needs to be understood is that for connection timing issues the server is actually a client
  and the client is the server in terms of opening the ports.

  When you specify a socket transport the client is listening on that port for a connection. The
  socket port number is passed as --socket=${port} to the server process started.
*/

void setup_logging(std::string log_file) {
  lg::set_file(log_file);
  lg::set_file_level(lg::level::debug);
  // We use stdout to communicate with the client, so don't use it at all!
  lg::set_stdout_level(lg::level::off);
  lg::set_flush_level(lg::level::debug);
  lg::initialize();
}

int main(int argc, char** argv) {
  // TODO - do the utf-8 thing!
  CLI::App app{"OpenGOAL Language Server"};

  bool use_stdin = true;
  bool verbose = false;
  std::string logfile;
  auto stdin_option = app.add_flag("--stdio", use_stdin,
                                   "Don't launch an HTTP server and instead accept input on stdin");
  app.add_flag("-v,--verbose", verbose, "Enable verbose logging");
  app.add_option("-l,--log", logfile, "Log file path");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  AppState appstate;
  LSPRouter lsp_router;
  appstate.verbose = verbose;
  if (!logfile.empty()) {
    setup_logging(logfile);
  }
  lsp_router.init_routes();

  // Decompiling
  // Read in all-types files
  // TODO - hard-coded for now, but need to enumerate the directory eventually i think?
  /*m_dts.parse_type_defs(
      {"C:\\Users\\xtvas\\Repositories\\opengoal\\jak-project\\decompiler\\config\\all-types.gc"});

  auto info = m_dts.symbol_definition_info["vector"];
  lg::info("Loaded DTS");*/

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
      lg::info(">>> Received message of type '{}'", body["method"].get<std::string>());
      if (appstate.verbose) {
        lg::debug("Headers:\n");
        for (auto elem : message_buffer.headers()) {
          auto pretty_header = fmt::format("{}: {}\n", elem.first, elem.second);
          lg::debug("{}", pretty_header);
        }
        lg::debug("Body: \n{}\n", body.dump(2));
        lg::debug("Raw: \n{}\n", message_buffer.raw());
      }

      auto messages = lsp_router.route_message(message_buffer, appstate);
      if (messages.has_value()) {
        for (const auto& message : messages.value()) {
          std::cout << message.c_str() << std::flush;
          if (appstate.verbose) {
            lg::debug("<<< Sending message: \n{}", message);
          } else {
            lg::info("<<< Sending message");
          }
        }
      }
      message_buffer.clear();
    }
  }

  return 0;
}
