#pragma once

#include <optional>
#include <string>

#include "common/util/json_util.h"

#include "lsp/state/app.h"
#include "lsp/state/workspace.h"
#include "lsp/transport/stdio.h"

enum class LSPRouteType { NOOP = 0, NOTIFICATION = 1, REQUEST_RESPONSE = 2 };

class LSPRoute {
 public:
  LSPRoute();
  LSPRoute(std::function<void(Workspace&, json)> notification_handler);
  LSPRoute(std::function<void(Workspace&, json)> notification_handler,
           std::function<std::optional<json>(Workspace&, json)> post_notification_publish);
  LSPRoute(std::function<std::optional<json>(Workspace&, int, json)> request_handler);

  LSPRouteType m_route_type;

  /// @brief Handle a notification -- this requires no response to the client
  std::function<void(Workspace&, json)> m_notification_handler;
  /// @brief Handle a new request from the client that expects a response
  std::function<std::optional<json>(Workspace&, int, json)> m_request_handler;
  /// @brief Prepares a notification response body to be published _after_ the main handler is
  /// processed
  std::optional<std::function<std::optional<json>(Workspace&, json)>> m_post_notification_publish;
  /// @brief Generic function to perform some action after processing
  std::optional<std::function<void(Workspace&)>> m_generic_post_action;
};

class LSPRouter {
 public:
  void init_routes();
  std::optional<std::vector<std::string>> route_message(const MessageBuffer& message_buffer,
                                                        AppState& appstate);
  std::string make_response(const json& result);

 private:
  std::unordered_map<std::string, LSPRoute> m_routes;
  ;
};
