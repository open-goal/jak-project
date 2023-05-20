#pragma once

#include <optional>

#include "lsp/protocol/common_types.h"
#include "lsp/state/workspace.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

std::optional<json> document_symbols_handler(Workspace& workspace, int id, json params) {
  auto converted_params = params.get<LSPSpec::DocumentSymbolParams>();
  auto tracked_file = workspace.get_tracked_ir_file(converted_params.m_textDocument.m_uri);

  if (!tracked_file) {
    return {};
  }

  // TODO - convert to type!

  json arr = json::array();
  for (const auto& symbol : tracked_file.value().m_symbols) {
    arr.push_back(symbol);
  }
  return arr;
}
