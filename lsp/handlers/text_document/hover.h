#include <optional>

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

  auto token_at_pos = tracked_file->get_mips_instruction_at_position(converted_params.m_position);

  LSPSpec::MarkupContent markup;
  markup.m_kind = "markdown";
  if (token_at_pos) {
    auto token = token_at_pos.value();
    std::transform(token.begin(), token.end(), token.begin(),
                   [](unsigned char c) { return std::tolower(c); });
    // Find the instruction, there are some edge-cases here where they could be multiple
    // TODO - handle those (print both is an easy fix?)
    // TODO - havn't addressed `bc` and such instructions!  Those need to be prefixed matched
    for (const auto& instr : LSPData::MIPS_INSTRUCTION_LIST) {
      auto mnemonic_lower = instr.mnemonic;
      std::transform(mnemonic_lower.begin(), mnemonic_lower.end(), mnemonic_lower.begin(),
                     [](unsigned char c) { return std::tolower(c); });
      if (mnemonic_lower == token) {
        markup.m_value = fmt::format("### {}\n{}", instr.mnemonic, instr.description);
        break;
      }
    }
  } else {
    return {};
  }
  LSPSpec::Hover hover_resp;
  hover_resp.m_contents = markup;
  // TODO - try specifying the range so it highlights everything, ie. `c.lt.s`

  return hover_resp;
}
