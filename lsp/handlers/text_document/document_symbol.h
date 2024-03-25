#pragma once

#include <optional>

#include "lsp/protocol/common_types.h"
#include "lsp/state/workspace.h"

#include "common/util/json_util.h"

namespace lsp_handlers {

std::optional<json> document_symbols(Workspace& workspace, int id, json params);

}  // namespace lsp_handlers
