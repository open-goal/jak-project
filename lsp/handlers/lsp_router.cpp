#include "lsp_router.h"

#include "common/log/log.h"

#include "lsp/handlers/initialize.h"
#include "lsp/handlers/text_document/type_hierarchy.h"
#include "lsp/protocol/error_codes.h"
#include "text_document/completion.h"
#include "text_document/document_color.h"
#include "text_document/document_symbol.h"
#include "text_document/document_synchronization.h"
#include "text_document/formatting.h"
#include "text_document/go_to.h"
#include "text_document/hover.h"

#include "fmt/core.h"

json error_resp(ErrorCodes error_code, const std::string& error_message) {
  json error{
      {"code", static_cast<int>(error_code)},
      {"message", error_message},
  };
  return json{{"error", error}};
}

LSPRoute::LSPRoute() : m_route_type(LSPRouteType::NOOP) {}

LSPRoute::LSPRoute(std::function<void(Workspace&, json)> notification_handler)
    : m_route_type(LSPRouteType::NOTIFICATION), m_notification_handler(notification_handler) {}

LSPRoute::LSPRoute(std::function<void(Workspace&, json)> notification_handler,
                   std::function<std::optional<json>(Workspace&, json)> post_notification_publish)
    : m_route_type(LSPRouteType::NOTIFICATION),
      m_notification_handler(notification_handler),
      m_post_notification_publish(post_notification_publish) {}

LSPRoute::LSPRoute(std::function<std::optional<json>(Workspace&, int, json)> request_handler)
    : m_route_type(LSPRouteType::REQUEST_RESPONSE), m_request_handler(request_handler) {}

void LSPRouter::init_routes() {
  m_routes["exit"] = LSPRoute([](Workspace& /*workspace*/, nlohmann::json /*params*/) {
    lg::info("Shutting down LSP due to explicit request");
    exit(0);
  });
  m_routes["shutdown"] = LSPRoute(
      [](Workspace& /*workspace*/, int /*id*/, nlohmann::json /*params*/) -> std::optional<json> {
        lg::info("Received shutdown request");
        return error_resp(ErrorCodes::UnknownErrorCode, "Problem occurred while existing");
      });
  m_routes["initialize"] = LSPRoute(lsp_handlers::initialize);
  m_routes["initialize"].m_generic_post_action = [](Workspace& workspace) {
    workspace.set_initialized(true);
  };
  m_routes["initialized"] = LSPRoute();
  m_routes["textDocument/documentSymbol"] = LSPRoute(lsp_handlers::document_symbols);
  m_routes["textDocument/didOpen"] =
      LSPRoute(lsp_handlers::did_open, lsp_handlers::did_open_push_diagnostics);
  m_routes["textDocument/didChange"] =
      LSPRoute(lsp_handlers::did_change, lsp_handlers::did_change_push_diagnostics);
  m_routes["textDocument/didClose"] = LSPRoute(lsp_handlers::did_close);
  m_routes["textDocument/willSave"] = LSPRoute(lsp_handlers::will_save);
  m_routes["textDocument/hover"] = LSPRoute(lsp_handlers::hover);
  m_routes["textDocument/definition"] = LSPRoute(lsp_handlers::go_to_definition);
  m_routes["textDocument/completion"] = LSPRoute(lsp_handlers::get_completions);
  m_routes["textDocument/documentColor"] = LSPRoute(lsp_handlers::document_color);
  m_routes["textDocument/formatting"] = LSPRoute(lsp_handlers::formatting);
  m_routes["textDocument/prepareTypeHierarchy"] = LSPRoute(lsp_handlers::prepare_type_hierarchy);
  m_routes["typeHierarchy/supertypes"] = LSPRoute(lsp_handlers::supertypes_type_hierarchy);
  m_routes["typeHierarchy/subtypes"] = LSPRoute(lsp_handlers::subtypes_type_hierarchy);
  // TODO - m_routes["textDocument/signatureHelp"] = LSPRoute(get_completions_handler);
  // Not Supported Routes, noops
  m_routes["$/cancelRequest"] = LSPRoute();
  m_routes["textDocument/documentLink"] = LSPRoute();
  m_routes["textDocument/codeLens"] = LSPRoute();
  m_routes["textDocument/colorPresentation"] = LSPRoute();
}

std::string LSPRouter::make_response(const json& result) {
  json content = result;
  content["jsonrpc"] = "2.0";

  std::string header;
  header.append("Content-Length: " + std::to_string(content.dump().size()) + "\r\n");
  header.append("Content-Type: application/vscode-jsonrpc;charset=utf-8\r\n");
  header.append("\r\n");
  return header + content.dump();
}

std::optional<std::vector<std::string>> LSPRouter::route_message(
    const MessageBuffer& message_buffer,
    AppState& appstate) {
  const json& body = message_buffer.body();
  const auto method = body.at("method").get<std::string>();

  // If the workspace has not yet been initialized but the client sends a
  // message that doesn't have method "initialize" then we'll return an error
  // as per the LSP spec.
  if (method != "initialize" && !appstate.workspace.is_initialized()) {
    auto error = {
        make_response(error_resp(ErrorCodes::ServerNotInitialized, "Server not yet initialized."))};
    return std::make_optional(error);
  }

  // Exit early if we can't handle the route
  if (m_routes.find(method) == m_routes.end()) {
    lg::warn("Method not supported '{}'", method);
    auto error = {make_response(
        error_resp(ErrorCodes::MethodNotFound, fmt::format("Method '{}' not supported", method)))};
    return std::make_optional(error);
  }

  try {
    auto& route = m_routes.at(method);
    std::vector<json> resp_bodies;
    // Handle the request/notificiation
    switch (route.m_route_type) {
      case LSPRouteType::NOOP:
        break;
      case LSPRouteType::NOTIFICATION:
        route.m_notification_handler(appstate.workspace, body["params"]);
        break;
      case LSPRouteType::REQUEST_RESPONSE:
        auto resp_body = route.m_request_handler(appstate.workspace, body["id"], body["params"]);
        json resp;
        resp["id"] = body["id"];
        if (resp_body) {
          resp["result"] = resp_body.value();
        } else {
          resp["result"] = nullptr;
        }
        resp_bodies.push_back(resp);
        break;
    }

    // Run any publish we need to do after the fact
    if (route.m_post_notification_publish) {
      auto resp = route.m_post_notification_publish.value()(appstate.workspace, body["params"]);
      if (resp) {
        resp_bodies.push_back(resp.value());
      }
    }

    // Run any generic post action
    if (route.m_generic_post_action) {
      route.m_generic_post_action.value()(appstate.workspace);
    }

    // Serialize all payloads with headers
    std::vector<std::string> resps;
    for (const auto& body : resp_bodies) {
      resps.push_back(make_response(body));
    }

    // Return
    if (resps.empty()) {
      return {};
    }
    return resps;
  } catch (std::exception& e) {
    lg::error("Unexpected exception occurred - {} | {}", e.what(), body.dump());
    // TODO - return an error with the message
    return {};
  }
}
