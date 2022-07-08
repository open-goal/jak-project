#pragma once

#include <optional>
#include <string>

#include "state/workspace.h"

#include "transport/stdio.h"

#include "third-party/json.hpp"
#include "state/app.h"

using json = nlohmann::json;

class LspRouter {
 public:
  void init_routes();
  std::optional<std::string> route_message(const MessageBuffer& message_buffer, AppState& appstate);
  std::string make_response(const json& result);

 private:
  // method-name => json function(id, params)
  // TODO - probably need to accept a Workspace
  std::unordered_map<std::string, std::function<std::optional<json>(int, json)>> m_routes;
};
