#pragma once

#include <optional>
#include <string>

#include "state/app.h"
#include "state/workspace.h"
#include "transport/stdio.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

class LspRouter {
 public:
  void init_routes();
  std::optional<std::string> route_message(const MessageBuffer& message_buffer, AppState& appstate);
  std::string make_response(const json& result);

 private:
  // method-name => json function(id, params)
  std::unordered_map<std::string, std::function<std::optional<json>(Workspace&, int, json)>>
      m_request_routes;
  std::unordered_map<std::string, std::function<void(Workspace&, json)>>
      m_notification_routes;
};
