#include <lsp/protocol/initialize_result.hpp>

#include "third-party/json.hpp"
#include "common/log/log.h"

using json = nlohmann::json;

std::optional<json> initialize_handler(Workspace& workspace, int id, json params) {
  InitializeResult result;
  return result.to_json();
}
