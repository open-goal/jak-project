#include "document_synchronization.h"

namespace lsp_handlers {

void did_open(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidOpenTextDocumentParams>();
  workspace.start_tracking_file(params.m_textDocument.m_uri, params.m_textDocument.m_languageId,
                                params.m_textDocument.m_text);
}

void did_change(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidChangeTextDocumentParams>();
  for (const auto& change : params.m_contentChanges) {
    workspace.update_tracked_file(params.m_textDocument.m_uri, change.m_text);
  }
}

void did_close(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidCloseTextDocumentParams>();
  workspace.stop_tracking_file(params.m_textDocument.m_uri);
}

void will_save(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::WillSaveTextDocumentParams>();
  workspace.tracked_file_will_save(params.textDocument.m_uri);
}

std::optional<json> did_open_push_diagnostics(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidOpenTextDocumentParams>();
  const auto file_type =
      workspace.determine_filetype_from_languageid(params.m_textDocument.m_languageId);

  LSPSpec::PublishDiagnosticParams publish_params;
  publish_params.m_uri = params.m_textDocument.m_uri;
  publish_params.m_version = params.m_textDocument.m_version;

  if (file_type == Workspace::FileType::OpenGOALIR) {
    auto maybe_tracked_file = workspace.get_tracked_ir_file(params.m_textDocument.m_uri);
    if (!maybe_tracked_file) {
      return {};
    }
    const auto& tracked_file = maybe_tracked_file.value().get();
    publish_params.m_diagnostics = tracked_file.m_diagnostics;
  }

  json response;
  response["method"] = "textDocument/publishDiagnostics";
  response["params"] = publish_params;

  return response;
}

std::optional<json> did_change_push_diagnostics(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidChangeTextDocumentParams>();
  const auto file_type = workspace.determine_filetype_from_uri(params.m_textDocument.m_uri);

  LSPSpec::PublishDiagnosticParams publish_params;
  publish_params.m_uri = params.m_textDocument.m_uri;
  publish_params.m_version = params.m_textDocument.m_version;

  if (file_type == Workspace::FileType::OpenGOALIR) {
    auto maybe_tracked_file = workspace.get_tracked_ir_file(params.m_textDocument.m_uri);
    if (!maybe_tracked_file) {
      return {};
    }
    const auto& tracked_file = maybe_tracked_file.value().get();
    publish_params.m_diagnostics = tracked_file.m_diagnostics;
  }

  json response;
  response["method"] = "textDocument/publishDiagnostics";
  response["params"] = publish_params;

  return response;
}

}  // namespace lsp_handlers
