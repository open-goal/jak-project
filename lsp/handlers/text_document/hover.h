#pragma once

#include <optional>
#include <regex>

#include "common/util/json_util.h"
#include "common/util/string_util.h"

#include "goalc/compiler/docs/DocTypes.h"
#include "lsp/protocol/common_types.h"
#include "lsp/protocol/hover.h"
#include "lsp/state/data/mips_instructions.h"
#include "lsp/state/workspace.h"

namespace lsp_handlers {
std::optional<json> hover(Workspace& workspace, int id, json raw_params);

}  // namespace lsp_handlers
