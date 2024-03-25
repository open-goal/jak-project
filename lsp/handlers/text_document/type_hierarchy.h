#pragma once

#include <optional>

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/type_hierarchy.h"
#include "lsp/state/workspace.h"
#include "common/util/json_util.h"

namespace lsp_handlers {

std::optional<json> prepare_type_hierarchy(Workspace& workspace, int id, json raw_params);

std::optional<json> supertypes_type_hierarchy(Workspace& workspace, int id, json raw_params);

std::optional<json> subtypes_type_hierarchy(Workspace& workspace, int id, json raw_params);
}  // namespace lsp_handlers
