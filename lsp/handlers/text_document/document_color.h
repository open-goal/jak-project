#pragma once

#include <optional>

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/document_color.h"
#include "lsp/state/workspace.h"

#include "common/util/json_util.h"

namespace lsp_handlers {

std::optional<json> document_color(Workspace& workspace, int id, json raw_params);

}  // namespace lsp_handlers
