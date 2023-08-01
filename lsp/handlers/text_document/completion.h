#pragma once

#include <optional>

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/completion.h"
#include "lsp/state/data/mips_instructions.h"
#include "lsp/state/workspace.h"

std::optional<json> get_completions_handler(Workspace& workspace, int id, json params) {
  auto converted_params = params.get<LSPSpec::CompletionParams>();

  // TODO - these need to be cached,

  // TODO - implement response object

  return json::array();
}
