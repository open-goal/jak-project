#include <optional>

#include "protocol/document_diagnostics.h"
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

std::optional<json> did_open_push_diagnostics(Workspace& workspace, json params) {
  auto converted_params = params.get<LSPSpec::DidOpenTextDocumentParams>();
  auto tracked_file = workspace.get_tracked_ir_file(converted_params.m_textDocument.m_uri);

  if (!tracked_file) {
    lg::info("diags - couldn't find file!");
    return {};
  }

  LSPSpec::PublishDiagnosticParams publish_params;
  publish_params.m_uri = converted_params.m_textDocument.m_uri;
  publish_params.m_diagnostics = tracked_file.value().m_diagnostics;
  publish_params.m_version = converted_params.m_textDocument.m_version;

  json response;
  response["method"] = "textDocument/publishDiagnostics";
  response["params"] = publish_params;

  lg::info("finalized response! - {}", response.dump());

  return response;
}
