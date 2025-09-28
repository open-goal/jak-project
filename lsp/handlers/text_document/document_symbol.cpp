#include "document_symbol.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

std::optional<json> ir_symbols(Workspace& workspace, LSPSpec::DocumentSymbolParams params) {
  json symbols = json::array();
  auto maybe_tracked_file = workspace.get_tracked_ir_file(params.m_textDocument.m_uri);
  if (!maybe_tracked_file) {
    return symbols;
  }

  const auto& tracked_file = maybe_tracked_file.value().get();
  for (const auto& symbol : tracked_file.m_symbols) {
    symbols.push_back(symbol);
  }

  return symbols;
}

std::optional<json> og_symbols(Workspace& workspace, LSPSpec::DocumentSymbolParams params) {
  json symbols = json::array();
  auto maybe_tracked_file = workspace.get_tracked_og_file(params.m_textDocument.m_uri);
  if (!maybe_tracked_file) {
    return symbols;
  }

  const auto& tracked_file = maybe_tracked_file.value().get();
  for (const auto& symbol : tracked_file.m_symbols) {
    symbols.push_back(symbol);
  }

  return symbols;
}

namespace lsp_handlers {

std::optional<json> document_symbols(Workspace& workspace, int /*id*/, json params) {
  auto converted_params = params.get<LSPSpec::DocumentSymbolParams>();
  const auto file_type =
      workspace.determine_filetype_from_uri(converted_params.m_textDocument.m_uri);

  if (file_type == Workspace::FileType::OpenGOALIR) {
    return ir_symbols(workspace, converted_params);
  } else if (file_type == Workspace::FileType::OpenGOAL) {
    return og_symbols(workspace, converted_params);
  }

  return json::array();
}

}  // namespace lsp_handlers
