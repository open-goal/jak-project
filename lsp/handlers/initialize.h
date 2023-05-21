#pragma once

#include "common/log/log.h"

#include "lsp/protocol/initialize_result.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

std::optional<json> initialize_handler(Workspace& workspace, int id, json params) {
  InitializeResult result;
  return result.to_json();
}
