#include <optional>

#include "protocol/document_synchronization.h"
#include "state/workspace.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

std::optional<json> did_open_handler(Workspace& workspace, json params) {
  auto converted_params = params.get<LSPSpec::DidOpenTextDocumentParams>();
  workspace.update_ir_file(converted_params.m_textDocument.m_uri,
                           converted_params.m_textDocument.m_text);
  return {};
}
