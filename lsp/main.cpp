#include <chrono>
#include <iostream>
#include <optional>
#include <thread>
#include <vector>

#include "workspace.h"

#include <common/log/log.h>

#include "transport/stdio.h"

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
  lg::set_stdout_level(lg::level::off);
  lg::set_flush_level(lg::level::debug);
  lg::initialize();
}

struct AppState {
  Workspace workspace;
  bool verbose;
};

std::string make_response(const json& response) {
  json content = response;
  content["jsonrpc"] = "2.0";

  std::string header;
  header.append("Content-Length: " + std::to_string(content.dump().size()) + "\n"); // removed \r here, doesn't seem to matter
  header.append("Content-Type: application/vscode-jsonrpc;charset=utf-8\n"); // removed \r here, doesn't seem to matter
  header.append("\n"); // removed \r here, doesn't seem to matter
  return header + content.dump(); // TODO - i dump minified to get around windows being an idiot - https://stackoverflow.com/questions/16888339/what-is-the-simplest-way-to-write-to-stdout-in-binary-mode
}

std::optional<std::string> handle_message(const MessageBuffer& message_buffer, AppState& appstate) {
  json body = message_buffer.body();

  if (body["method"] == "initialized") {
    return std::nullopt;
  }

  if (body["method"] == "initialize") {
    appstate.workspace.set_initialized(true);

    json text_document_sync{
        {"openClose", true},
        {"change", 1},  // Full sync
        {"willSave", false},
        {"willSaveWaitUntil", false},
        {"save", {{"includeText", false}}},
    };

    json completion_provider{
        {"resolveProvider", false},
        {"triggerCharacters", {}},
    };
    json signature_help_provider{{"triggerCharacters", ""}};
    json code_lens_provider{{"resolveProvider", false}};
    json document_on_type_formatting_provider{
        {"firstTriggerCharacter", ""},
        {"moreTriggerCharacter", ""},
    };
    json document_link_provider{{"resolveProvider", false}};
    json execute_command_provider{{"commands", {}}};
    json result{{"capabilities",
                 {
                     {"textDocumentSync", text_document_sync},
                     {"hoverProvider", false},
                     {"completionProvider", completion_provider},
                     {"signatureHelpProvider", signature_help_provider},
                     {"definitionProvider", false},
                     {"referencesProvider", false},
                     {"documentHighlightProvider", false},
                     {"documentSymbolProvider", false},
                     {"workspaceSymbolProvider", false},
                     {"codeActionProvider", false},
                     {"codeLensProvider", code_lens_provider},
                     {"documentFormattingProvider", false},
                     {"documentRangeFormattingProvider", false},
                     {"documentOnTypeFormattingProvider", document_on_type_formatting_provider},
                     {"renameProvider", false},
                     {"documentLinkProvider", document_link_provider},
                     {"executeCommandProvider", execute_command_provider},
                     {"experimental", {}},
                 }}};

    json result_body{{"id", body["id"]}, {"result", result}};
    return make_response(result_body);
  }
  /*else if (body["method"] == "textDocument/didOpen") {
    auto uri = body["params"]["textDocument"]["uri"];
    auto text = body["params"]["textDocument"]["text"];
    appstate.workspace.add_document(uri, text);

    json diagnostics = get_diagnostics(uri, text, appstate);
    if (diagnostics.empty()) {
      diagnostics = json::array();
    }
    json result_body{{"method", "textDocument/publishDiagnostics"},
                     {"params",
                      {
                          {"uri", uri},
                          {"diagnostics", diagnostics},
                      }}};
    return make_response(result_body);
  } else if (body["method"] == "textDocument/didChange") {
    auto uri = body["params"]["textDocument"]["uri"];
    auto change = body["params"]["contentChanges"][0]["text"];
    appstate.workspace.change_document(uri, change);

    std::string document = appstate.workspace.documents()[uri];
    json diagnostics = get_diagnostics(uri, document, appstate);
    if (diagnostics.empty()) {
      diagnostics = json::array();
    }
    json result_body{{"method", "textDocument/publishDiagnostics"},
                     {"params",
                      {
                          {"uri", uri},
                          {"diagnostics", diagnostics},
                      }}};
    return make_response(result_body);
  }*/

  // If the workspace has not yet been initialized but the client sends a
  // message that doesn't have method "initialize" then we'll return an error
  // as per LSP spec.
  if (body["method"] != "initialize" && !appstate.workspace.is_initialized()) {
    json error{
        {"code", -32002},
        {"message", "Server not yet initialized."},
    };
    json result_body{{"error", error}};
    return make_response(result_body);
  }

  // If we don't know the method requested, we end up here.
  if (body.count("method") == 1) {
    json error{
        {"code", -32601},
        {"message", fmt::format("Method '{}' not supported.", body["method"].get<std::string>())},
    };
    json result_body{{"error", error}};
    return make_response(result_body);
  }

  // If we couldn't parse anything we end up here.
  json error{
      {"code", -32700},
      {"message", "Couldn't parse message."},
  };
  json result_body{{"error", error}};
  return make_response(result_body);
}

int main(int argc, char** argv) {
  CLI::App app{"OpenGOAL Language Server"};

  bool use_stdin = true;
  bool verbose = false;
  std::string logfile;
  auto stdin_option = app.add_flag("--stdio", use_stdin,
                                   "Don't launch an HTTP server and instead accept input on stdin");
  app.add_flag("-v,--verbose", verbose, "Enable verbose logging");
  app.add_option("-l,--log", logfile, "Log file");
  app.validate_positionals();
  CLI11_PARSE(app, argc, argv);

  AppState appstate;
  appstate.verbose = verbose;
  setup_logging(logfile);

  // Decompiling
  // Read in all-types files
  // TODO - hard-coded for now, but need to enumerate the directory eventually i think?
  /*m_dts.parse_type_defs(
      {"C:\\Users\\xtvas\\Repositories\\opengoal\\jak-project\\decompiler\\config\\all-types.gc"});

  auto info = m_dts.symbol_definition_info["vector"];
  lg::info("Loaded DTS");*/

  lg::info("OpenGOAL LSP Initialized, ready for requests");

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

      auto message = handle_message(message_buffer, appstate);
      if (message.has_value()) {
        std::cout << message.value() << std::flush;

        if (appstate.verbose) {
          lg::debug("<<< Sending message: \n{}", message.value());
        } else {
          lg::info("<<< Sending message");
        }
      }
      message_buffer.clear();
    }
  }

  return 0;
}
