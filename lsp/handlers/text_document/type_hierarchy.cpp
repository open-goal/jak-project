#include "type_hierarchy.h"

#include "lsp/lsp_util.h"

namespace lsp_handlers {
std::optional<json> prepare_type_hierarchy(Workspace& workspace, int /*id*/, json raw_params) {
  auto params = raw_params.get<LSPSpec::TypeHierarchyPrepareParams>();
  const auto file_type = workspace.determine_filetype_from_uri(params.m_textDocument.m_uri);

  if (file_type != Workspace::FileType::OpenGOAL) {
    return nullptr;
  }
  auto maybe_tracked_file = workspace.get_tracked_og_file(params.m_textDocument.m_uri);
  if (!maybe_tracked_file) {
    return nullptr;
  }
  const auto& tracked_file = maybe_tracked_file.value().get();
  const auto symbol = tracked_file.get_symbol_at_position(params.m_position);
  if (!symbol) {
    lg::debug("prepare_type_hierarchy - no symbol");
    return nullptr;
  }

  const auto& symbol_info = workspace.get_global_symbol_info(tracked_file, symbol.value());
  if (!symbol_info) {
    lg::debug("prepare_type_hierarchy - no symbol info - {}", symbol.value());
    return nullptr;
  }

  const auto& def_loc = workspace.get_symbol_def_location(tracked_file, symbol_info.value());
  if (!def_loc) {
    return nullptr;
  }

  auto type_item = LSPSpec::TypeHierarchyItem();
  type_item.name = symbol.value();
  // TODO - differentiate between struct and class perhaps
  type_item.kind = LSPSpec::SymbolKind::Class;
  if (symbol_info && !symbol_info.value()->m_docstring.empty()) {
    type_item.detail = symbol_info.value()->m_docstring;
  }
  type_item.uri = lsp_util::uri_from_path(def_loc->file_path);
  // TODO - this range is technically not entirely correct, we'd have to parse the defining file
  // with an AST to get the true extent of the deftype.  But for this purpose, its not really needed
  //
  // HACK - the definition that our compiler stores is the form itself, so we will add
  // the width of the prefix `(deftype ` to the char_index
  // TODO - A better way would be to use the AST
  type_item.range.m_start = {(uint32_t)def_loc->line_idx, (uint32_t)(def_loc->char_idx + 9)};
  type_item.range.m_end = {(uint32_t)def_loc->line_idx,
                           (uint32_t)(def_loc->char_idx + 9 + symbol.value().length())};
  type_item.selectionRange.m_start = {(uint32_t)def_loc->line_idx,
                                      (uint32_t)(def_loc->char_idx + 8)};
  type_item.selectionRange.m_end = {(uint32_t)def_loc->line_idx,
                                    (uint32_t)(def_loc->char_idx + 8 + symbol.value().length())};

  json items = json::array();
  items.push_back(type_item);
  return items;
}

std::optional<json> supertypes_type_hierarchy(Workspace& workspace, int /*id*/, json raw_params) {
  auto params = raw_params.get<LSPSpec::TypeHierarchySupertypesParams>();
  const std::optional<GameVersion> game_version =
      workspace.determine_game_version_from_uri(params.item.uri);
  if (!game_version) {
    return nullptr;
  }
  const auto& parent_type_path =
      workspace.get_symbols_parent_type_path(params.item.name, game_version.value());
  json items = json::array();
  for (const auto& parent_type : parent_type_path) {
    if (std::get<0>(parent_type) == params.item.name) {
      continue;  // skip the item itself
    }
    auto type_item = LSPSpec::TypeHierarchyItem();
    type_item.name = std::get<0>(parent_type);
    // TODO - differentiate between struct and class perhaps
    type_item.kind = LSPSpec::SymbolKind::Class;
    if (!std::get<1>(parent_type).empty()) {
      type_item.detail = std::get<1>(parent_type);
    }
    const auto& def_loc = std::get<2>(parent_type);
    type_item.uri = def_loc.filename;
    // TODO - this range is technically not entirely correct, we'd have to parse the defining file
    // with an AST to get the true entent of the deftype.  But for this purpose, its not really
    // needed
    //
    // HACK - the definition that our compiler stores is the form itself, so we will add
    // the width of the prefix `(deftype ` to the char_index
    // TODO - A better way would be to use the AST
    type_item.range.m_start = {(uint32_t)def_loc.line_idx, (uint32_t)(def_loc.char_idx + 9)};
    type_item.range.m_end = {(uint32_t)def_loc.line_idx,
                             (uint32_t)(def_loc.char_idx + 9 + std::get<0>(parent_type).length())};
    type_item.selectionRange.m_start = {(uint32_t)def_loc.line_idx,
                                        (uint32_t)(def_loc.char_idx + 8)};
    type_item.selectionRange.m_end = {
        (uint32_t)def_loc.line_idx,
        (uint32_t)(def_loc.char_idx + 8 + std::get<0>(parent_type).length())};
    items.push_back(type_item);
  }
  return items;
}

std::optional<json> subtypes_type_hierarchy(Workspace& workspace, int /*id*/, json raw_params) {
  auto params = raw_params.get<LSPSpec::TypeHierarchySupertypesParams>();
  const std::optional<GameVersion> game_version =
      workspace.determine_game_version_from_uri(params.item.uri);
  if (!game_version) {
    return nullptr;
  }
  const auto& parent_type_path =
      workspace.get_types_subtypes(params.item.name, game_version.value());
  json items = json::array();
  for (const auto& parent_type : parent_type_path) {
    auto type_item = LSPSpec::TypeHierarchyItem();
    type_item.name = std::get<0>(parent_type);
    // TODO - differentiate between struct and class perhaps
    type_item.kind = LSPSpec::SymbolKind::Class;
    if (!std::get<1>(parent_type).empty()) {
      type_item.detail = std::get<1>(parent_type);
    }
    const auto& def_loc = std::get<2>(parent_type);
    type_item.uri = def_loc.filename;
    // TODO - this range is technically not entirely correct, we'd have to parse the defining file
    // with an AST to get the true entent of the deftype.  But for this purpose, its not really
    // needed
    //
    // HACK - the definition that our compiler stores is the form itself, so we will add
    // the width of the prefix `(deftype ` to the char_index
    // TODO - A better way would be to use the AST
    type_item.range.m_start = {(uint32_t)def_loc.line_idx, (uint32_t)(def_loc.char_idx + 9)};
    type_item.range.m_end = {(uint32_t)def_loc.line_idx,
                             (uint32_t)(def_loc.char_idx + 9 + std::get<0>(parent_type).length())};
    type_item.selectionRange.m_start = {(uint32_t)def_loc.line_idx,
                                        (uint32_t)(def_loc.char_idx + 8)};
    type_item.selectionRange.m_end = {
        (uint32_t)def_loc.line_idx,
        (uint32_t)(def_loc.char_idx + 8 + std::get<0>(parent_type).length())};
    items.push_back(type_item);
  }
  return items;
}
}  // namespace lsp_handlers
