#include "lsp_router.h"

#include "initialize.hpp"

#include "common/log/log.h"

#include "protocol/error_codes.h"

#include "third-party/fmt/core.h"

void LspRouter::init_routes() {
  m_routes["initialize"] = initialize_handler;
}

json error_resp(ErrorCodes error_code, std::string error_message) {
  lg::debug("A");
  json error{
      {"code", static_cast<int>(error_code)},
      {"message", error_message},
  };
  lg::debug("B");
  return json{{"error", error}};
}

std::string LspRouter::make_response(const json& result) {
  json content = result;
  content["jsonrpc"] = "2.0";
  lg::debug("C");

  std::string header;
  header.append("Content-Length: " + std::to_string(content.dump().size()) +
                "\r\n");  // removed \r here, doesn't seem to matter
  header.append(
      "Content-Type: application/vscode-jsonrpc;charset=utf-8\r\n");  // removed \r here, doesn't seem
                                                                    // to matter
  header.append("\r\n");  // removed \r here, doesn't seem to matter
  return header +
         content
             .dump();  // TODO - i dump minified to get around windows being an idiot -
                       // https://stackoverflow.com/questions/16888339/what-is-the-simplest-way-to-write-to-stdout-in-binary-mode
}

std::optional<std::string> LspRouter::route_message(const MessageBuffer& message_buffer,
                                                    AppState& appstate) {
  json body = message_buffer.body();
  auto method = body["method"];
  lg::info(method);

  // 'initialized' request payload is special in that it doesn't have any of the parameters we'd
  // normally expect
  // TODO - maybe have a map of noop requests?
  if (method == "initialized") {
    return {};
  }

  // If the workspace has not yet been initialized but the client sends a
  // message that doesn't have method "initialize" then we'll return an error
  // as per LSP spec.
  if (method != "initialize" && !appstate.workspace.is_initialized()) {
    return make_response(
        error_resp(ErrorCodes::ServerNotInitialized, "Server not yet initialized."));
  }

  if (m_routes.count(method) == 0) {
    lg::warn("Method not supported '{}'", method);
    return make_response(
        error_resp(ErrorCodes::MethodNotFound, fmt::format("Method '{}' not supported", method)));
  } else {
    auto result = m_routes[method](body["id"], body["params"]);
    if (result.has_value()) {
      json response;
      response["id"] = body["id"];
      response["result"] = result.value();
      if (method == "initialize") {
        appstate.workspace.set_initialized(true);
        lg::info("initialized!");
      }
      return std::make_optional(make_response(response));
    } else {
      return {};
    }
  }
}
