#pragma once

#include <optional>

#include "common/util/json_util.h"

#include "lsp/state/workspace.h"

namespace lsp_handlers {

std::optional<json> formatting(Workspace& workspace, int id, json raw_params);

}  // namespace lsp_handlers
