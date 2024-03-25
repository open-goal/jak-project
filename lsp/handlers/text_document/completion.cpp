#include "completion.h"

namespace lsp_handlers {

std::optional<json> get_completions(Workspace& /*workspace*/, int /*id*/, json params) {
  auto converted_params = params.get<LSPSpec::CompletionParams>();

  // TODO - these need to be cached,

  // TODO - implement response object

  return json::array();
}

}  // namespace lsp_handlers
