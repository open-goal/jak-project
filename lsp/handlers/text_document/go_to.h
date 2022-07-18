#include <optional>

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/hover.h"
#include "lsp/state/data/mips_instructions.h"
#include "lsp/state/workspace.h"

std::optional<json> go_to_definition_handler(Workspace& workspace, int id, json params) {
  auto converted_params = params.get<LSPSpec::TextDocumentPositionParams>();
  auto tracked_file = workspace.get_tracked_ir_file(converted_params.m_textDocument.m_uri);

  if (!tracked_file) {
    lg::debug("GOTODEF - no file");
    return {};
  }

  json locations = json::array();
  auto symbol_name = tracked_file->get_symbol_at_position(converted_params.m_position);

  if (!symbol_name) {
    lg::debug("GOTODEF - no symbol");
    return locations;
  }

  lg::debug("GOTODEF - symbol - {}", symbol_name.value());

  auto symbol_info =
      workspace.get_symbol_info_from_all_types(symbol_name.value(), tracked_file->m_all_types_uri);

  if (!symbol_info) {
    lg::debug("GOTODEF - no symbol info");
    return locations;
  }

  LSPSpec::Location location;
  location.m_uri = tracked_file->m_all_types_uri;
  location.m_range.m_start = {(uint32_t)symbol_info->line_idx_to_display,
                              (uint32_t)symbol_info->pos_in_line};
  location.m_range.m_end = {(uint32_t)symbol_info->line_idx_to_display,
                            (uint32_t)symbol_info->pos_in_line};
  locations.push_back(location);

  return locations;
}
