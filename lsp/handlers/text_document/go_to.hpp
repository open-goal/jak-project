#include <optional>

#include "protocol/common_types.h"
#include "protocol/hover.h"
#include "state/data/mips_instructions.h"
#include "state/workspace.h"

std::optional<json> go_to_definition_handler(Workspace& workspace, int id, json params) {
  auto converted_params = params.get<LSPSpec::TextDocumentPositionParams>();
  auto tracked_file = workspace.get_tracked_ir_file(converted_params.m_textDocument.m_uri);

  lg::debug("definition - A");

  if (!tracked_file) {
    lg::debug("definition - no file");
    return {};
  }

  json locations = json::array();
  auto symbol_name = tracked_file->get_symbol_at_position(converted_params.m_position);

  if (!symbol_name) {
    lg::debug("definition - no symbol");
    return locations;
  }

  lg::debug("definition - symbol - {}", symbol_name.value());

  auto symbol_info =
      workspace.get_symbol_info_from_all_types(symbol_name.value(), tracked_file->m_all_types_file.string());

  if (!symbol_info) {
    lg::debug("definition - no symbol info");
    return locations;
  }

  LSPSpec::Location location;
  // TODO - construct the all-types uri
  location.m_uri =
      "file:///C:/Users/xtvas/Repositories/opengoal/jak-project/decompiler/config/all-types.gc";
  location.m_range.m_start = {(uint32_t)symbol_info->line_idx_to_display,
                              (uint32_t)symbol_info->pos_in_line};
  location.m_range.m_end = {(uint32_t)symbol_info->line_idx_to_display,
                            (uint32_t)symbol_info->pos_in_line};
  lg::debug("path from symbol info - {}", symbol_info->filename);
  locations.push_back(location);

  lg::debug("definition - resp - {}", locations.dump());

  return locations;
}
