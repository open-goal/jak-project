#include <optional>
#include <regex>

#include "lsp/protocol/common_types.h"
#include "lsp/protocol/hover.h"
#include "lsp/state/data/mips_instructions.h"
#include "lsp/state/workspace.h"

std::optional<json> hover_handler(Workspace& workspace, int id, json params) {
  auto converted_params = params.get<LSPSpec::TextDocumentPositionParams>();
  auto tracked_file = workspace.get_tracked_ir_file(converted_params.m_textDocument.m_uri);

  if (!tracked_file) {
    return {};
  }

  // See if it's an OpenGOAL symbol or a MIPS mnemonic
  auto symbol_name = tracked_file->get_symbol_at_position(converted_params.m_position);
  auto token_at_pos = tracked_file->get_mips_instruction_at_position(converted_params.m_position);
  if (!symbol_name && !token_at_pos) {
    return {};
  }

  // TODO - try specifying the range so it highlights everything, ie. `c.lt.s`

  LSPSpec::MarkupContent markup;
  markup.m_kind = "markdown";

  // Prefer symbols
  if (symbol_name) {
    lg::debug("hover - symbol match - {}", symbol_name.value());
    auto symbol_info = workspace.get_definition_info_from_all_types(symbol_name.value(),
                                                                    tracked_file->m_all_types_uri);
    if (symbol_info && symbol_info.value().docstring.has_value()) {
      std::string docstring = symbol_info.value().docstring.value();
      lg::debug("hover - symbol has docstring - {}", docstring);
      // A docstring exists, print it!
      // By convention, docstrings are assumed to be markdown, they support code-blocks everything
      // the only thing extra we do, is replace [[<symbol>]] with links if available
      std::unordered_map<std::string, std::string> symbol_replacements = {};
      std::smatch match;

      std::string::const_iterator searchStart(docstring.cbegin());
      while (
          std::regex_search(searchStart, docstring.cend(), match, std::regex("\\[{2}(.*)\\]{2}"))) {
        // Have we already accounted for this symbol?
        const auto& name = match[1].str();
        if (symbol_replacements.count(name) != 0) {
          continue;
        }
        // Get this symbols info
        auto symbol_info =
            workspace.get_definition_info_from_all_types(name, tracked_file->m_all_types_uri);
        if (!symbol_info) {
          symbol_replacements[name] = fmt::format("_{}_", name);
        } else {
          // Construct path
          auto symbol_uri = fmt::format("{}#L{}%2C{}", tracked_file->m_all_types_uri,
                                        symbol_info.value().definition_info->line_idx_to_display + 1,
                                        symbol_info.value().definition_info->pos_in_line);
          symbol_replacements[name] = fmt::format("[{}]({})", name, symbol_uri);
        }
        searchStart = match.suffix().first;
      }
      // Replace all symbol occurences
      for (const auto& [key, val] : symbol_replacements) {
        docstring = std::regex_replace(docstring, std::regex("\\[{2}" + key + "\\]{2}"), val);
      }

      markup.m_value = docstring;
      LSPSpec::Hover hover_resp;
      hover_resp.m_contents = markup;
      return hover_resp;
    }
  }

  // Otherwise, maybe it's a MIPS instruction
  if (token_at_pos) {
    lg::debug("hover - token match - {}", token_at_pos.value());
    auto& token = token_at_pos.value();
    std::transform(token.begin(), token.end(), token.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    // Find the instruction, there are some edge-cases here where they could be multiple
    // TODO - havn't addressed `bc` and such instructions!  Those need to be prefixed matched
    std::vector<std::string> ee_instructions = {};
    std::vector<std::string> vu_instructions = {};
    for (const auto& instr : LSPData::MIPS_INSTRUCTION_LIST) {
      auto mnemonic_lower = instr.mnemonic;
      std::transform(mnemonic_lower.begin(), mnemonic_lower.end(), mnemonic_lower.begin(),
                     [](unsigned char c) { return std::tolower(c); });
      if (mnemonic_lower == token) {
        if (instr.type == "ee") {
          ee_instructions.push_back(fmt::format("- _{}_\n\n", instr.description));
        }
        else {
          vu_instructions.push_back(fmt::format("- _{}_\n\n", instr.description));
        }
      }
    }

    if (ee_instructions.empty() && vu_instructions.empty()) {
      return {};
    }

    // Construct the body
    std::string body = "";
    if (!ee_instructions.empty()) {
      body += "**EE Instructions**\n\n";
      for (const auto& instr : ee_instructions) {
        body += instr;
      }
      body += "___\n\n";
    }

    if (!vu_instructions.empty()) {
      body += "**VU Instructions**\n\n";
      for (const auto& instr : vu_instructions) {
        body += instr;
      }
      body += "___\n\n";
    }
    markup.m_value = body;
    LSPSpec::Hover hover_resp;
    hover_resp.m_contents = markup;
    return hover_resp;
  }

  return {};
}
