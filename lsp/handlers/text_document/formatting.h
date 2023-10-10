#pragma once

#include <optional>

#include "common/formatter/formatter.h"

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/formatting.h"
#include "lsp/state/data/mips_instructions.h"
#include "lsp/state/workspace.h"

std::optional<json> formatting_handler(Workspace& workspace, int /*id*/, json raw_params) {
  auto params = raw_params.get<LSPSpec::DocumentFormattingParams>();
  const auto file_type = workspace.determine_filetype_from_uri(params.textDocument.m_uri);

  if (file_type == Workspace::FileType::OpenGOALIR) {
    return nullptr;
  } else if (file_type == Workspace::FileType::OpenGOAL) {
    auto tracked_file = workspace.get_tracked_og_file(params.textDocument.m_uri);
    if (!tracked_file) {
      return nullptr;
    }
    // TODO move away from holding the content directly
    const auto result = formatter::format_code(tracked_file->m_content);
    if (!result) {
      return nullptr;
    }
    json edits = json::array();
    auto format_edit = LSPSpec::TextEdit();
    format_edit.range = {{0, 0}, {(uint32_t)tracked_file->m_lines.size(), 0}};
    format_edit.newText = result.value();
    edits.push_back(format_edit);
    return edits;
  }

  return nullptr;
}
