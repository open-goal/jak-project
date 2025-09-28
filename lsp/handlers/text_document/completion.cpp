#include "completion.h"

namespace lsp_handlers {

std::unordered_map<symbol_info::Kind, LSPSpec::CompletionItemKind> completion_item_kind_map = {
    {symbol_info::Kind::CONSTANT, LSPSpec::CompletionItemKind::Constant},
    {symbol_info::Kind::FUNCTION, LSPSpec::CompletionItemKind::Function},
    {symbol_info::Kind::FWD_DECLARED_SYM, LSPSpec::CompletionItemKind::Reference},
    {symbol_info::Kind::GLOBAL_VAR, LSPSpec::CompletionItemKind::Variable},
    {symbol_info::Kind::INVALID, LSPSpec::CompletionItemKind::Text},
    {symbol_info::Kind::LANGUAGE_BUILTIN, LSPSpec::CompletionItemKind::Function},
    {symbol_info::Kind::MACRO, LSPSpec::CompletionItemKind::Operator},
    {symbol_info::Kind::METHOD, LSPSpec::CompletionItemKind::Method},
    {symbol_info::Kind::TYPE, LSPSpec::CompletionItemKind::Class},
};

std::optional<json> get_completions(Workspace& workspace, int /*id*/, json params) {
  auto converted_params = params.get<LSPSpec::CompletionParams>();
  const auto file_type = workspace.determine_filetype_from_uri(converted_params.textDocument.m_uri);

  if (file_type != Workspace::FileType::OpenGOAL) {
    return nullptr;
  }
  auto maybe_tracked_file = workspace.get_tracked_og_file(converted_params.textDocument.m_uri);
  if (!maybe_tracked_file) {
    return nullptr;
  }
  std::vector<LSPSpec::CompletionItem> items;
  const auto& tracked_file = maybe_tracked_file.value().get();
  // The cursor position in the context of completions is always 1 character ahead of the text, we
  // move it back 1 spot so we can actually detect what the user has typed so far
  LSPSpec::Position new_position = converted_params.position;
  if (new_position.m_character > 0) {
    new_position.m_character--;
  }
  const auto symbol = tracked_file.get_symbol_at_position(new_position);
  if (!symbol) {
    lg::debug("get_completions - no symbol to work from");
  } else {
    const auto matching_symbols =
        workspace.get_symbols_starting_with(tracked_file.m_game_version, symbol.value());
    lg::debug("get_completions - found {} symbols", matching_symbols.size());

    for (const auto& symbol : matching_symbols) {
      LSPSpec::CompletionItem item;
      item.label = symbol->m_name;
      item.kind = completion_item_kind_map.at(symbol->m_kind);
      // TODO - flesh out this more fully when auto-complete with non-globals works as well
      items.push_back(item);
    }
  }
  LSPSpec::CompletionList list_result;
  list_result.isIncomplete = false;  // we want further typing to re-evaluate the list
  list_result.items = items;
  return list_result;
}

}  // namespace lsp_handlers
