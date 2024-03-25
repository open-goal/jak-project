#pragma once

#include "common/log/log.h"
#include "common/util/json_util.h"

#include "lsp/state/workspace.h"

namespace lsp_handlers {
std::optional<json> initialize(Workspace& workspace, int id, json params);
}
