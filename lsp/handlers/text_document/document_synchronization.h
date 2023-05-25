#pragma once

#include <optional>

#include "lsp/protocol/document_diagnostics.h"
#include "lsp/protocol/document_synchronization.h"
#include "lsp/state/workspace.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

void did_open_handler(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidOpenTextDocumentParams>();
  workspace.start_tracking_file(params.m_textDocument.m_uri, params.m_textDocument.m_languageId,
                                params.m_textDocument.m_text);
}

void did_change_handler(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidChangeTextDocumentParams>();
  for (const auto& change : params.m_contentChanges) {
    workspace.update_tracked_file(params.m_textDocument.m_uri, change.m_text);
  }
}

void did_close_handler(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidCloseTextDocumentParams>();
  workspace.stop_tracking_file(params.m_textDocument.m_uri);
}

std::optional<json> did_open_push_diagnostics(Workspace& workspace, json raw_params) {
  auto params = raw_params.get<LSPSpec::DidOpenTextDocumentParams>();
  const auto file_type =
      workspace.determine_filetype_from_languageid(params.m_textDocument.m_languageId);

  LSPSpec::PublishDiagnosticParams publish_params;
  publish_params.m_uri = params.m_textDocument.m_uri;
  publish_params.m_version = params.m_textDocument.m_version;

  if (file_type == Workspace::FileType::OpenGOALIR) {
    auto tracked_file = workspace.get_tracked_ir_file(params.m_textDocument.m_uri);
    if (!tracked_file) {
      return {};
    }
    publish_params.m_diagnostics = tracked_file.value().m_diagnostics;
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
    auto tracked_file = workspace.get_tracked_ir_file(params.m_textDocument.m_uri);

    if (!tracked_file) {
      return {};
    }
    publish_params.m_diagnostics = tracked_file.value().m_diagnostics;
  }

  json response;
  response["method"] = "textDocument/publishDiagnostics";
  response["params"] = publish_params;

  return response;
}
