#pragma once

#include <optional>

#include "common/util/json_util.h"

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/completion.h"
#include "lsp/state/data/mips_instructions.h"
#include "lsp/state/workspace.h"

namespace lsp_handlers {
std::optional<json> get_completions(Workspace& workspace, int id, json params);
}
