#include "hover.h"

bool is_number(const std::string& s) {
  return !s.empty() && std::find_if(s.begin(), s.end(),
                                    [](unsigned char c) { return !std::isdigit(c); }) == s.end();
}

std::vector<std::string> og_method_names = {"new",      "delete", "print",    "inspect",  "length",
                                            "asize-of", "copy",   "relocate", "mem-usage"};

std::optional<LSPSpec::Hover> hover_handler_ir(Workspace& workspace,
                                               const LSPSpec::TextDocumentPositionParams& params,
                                               const WorkspaceIRFile& tracked_file) {
  // See if it's an OpenGOAL symbol or a MIPS mnemonic
  auto symbol_name = tracked_file.get_symbol_at_position(params.m_position);
  auto token_at_pos = tracked_file.get_mips_instruction_at_position(params.m_position);
  if (!symbol_name && !token_at_pos) {
    return {};
  }

  LSPSpec::MarkupContent markup;
  markup.m_kind = "markdown";

  // TODO - try specifying the range so it highlights everything, ie. `c.lt.s`
  // Prefer symbols
  if (symbol_name) {
    lg::debug("hover - symbol match - {}", symbol_name.value());
    auto symbol_info = workspace.get_definition_info_from_all_types(symbol_name.value(),
                                                                    tracked_file.m_all_types_uri);
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
            workspace.get_definition_info_from_all_types(name, tracked_file.m_all_types_uri);
        if (!symbol_info) {
          symbol_replacements[name] = fmt::format("_{}_", name);
        } else {
          // Construct path
          auto symbol_uri =
              fmt::format("{}#L{}%2C{}", tracked_file.m_all_types_uri,
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
    } else if (!token_at_pos) {
      // Check if it's a number, and if so we'll do some numeric conversions
      if (!is_number(symbol_name.value())) {
        return {};
      }
      lg::debug("hover - numeric match - {}", symbol_name.value());
      // Construct the body
      std::string body = "";
      uint32_t num = std::atoi(symbol_name.value().data());
      // Assuming it comes in as Decimal
      body += "| Base    | Value |\n";
      body += "|---------|-------|\n";
      body += fmt::format("| Decimal        | `{:d}` |\n", num);
      body += fmt::format("| Hex            | `{:X}` |\n", num);
      // TODO - would be nice to format as groups of 4
      body += fmt::format("| Binary         | `{:b}` |\n", num);
      if (num >= 16 && (num - 16) % 4 == 0) {
        uint32_t method_id = (num - 16) / 4;
        std::string method_name = "not built-in";
        if (method_id <= 8) {
          method_name = og_method_names.at(method_id);
        }
        body += fmt::format("| Method ID   | `{}` - `{}` |\n", method_id, method_name);
      }
      body += fmt::format("| Octal          | `{:o}` |\n", num);

      markup.m_value = body;
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
        } else {
          vu_instructions.push_back(fmt::format("- _{}_\n\n", instr.description));
        }
      }
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

std::string truncate_docstring(const std::string& docstring) {
  std::string truncated = "";
  const auto lines = str_util::split(docstring);
  for (const auto& line : lines) {
    const auto trimmed_line = str_util::ltrim(line);
    if (str_util::starts_with(trimmed_line, "@")) {
      break;
    }
    truncated += trimmed_line + "\n";
  }
  return truncated;
}

namespace lsp_handlers {
std::optional<json> hover(Workspace& workspace, int /*id*/, json raw_params) {
  auto params = raw_params.get<LSPSpec::TextDocumentPositionParams>();
  auto file_type = workspace.determine_filetype_from_uri(params.m_textDocument.m_uri);

  if (file_type == Workspace::FileType::OpenGOALIR) {
    auto tracked_file = workspace.get_tracked_ir_file(params.m_textDocument.m_uri);
    if (!tracked_file) {
      return {};
    }
    return hover_handler_ir(workspace, params, tracked_file.value());
  } else if (file_type == Workspace::FileType::OpenGOAL) {
    auto maybe_tracked_file = workspace.get_tracked_og_file(params.m_textDocument.m_uri);
    if (!maybe_tracked_file) {
      return {};
    }
    const auto& tracked_file = maybe_tracked_file.value().get();
    const auto symbol = tracked_file.get_symbol_at_position(params.m_position);
    if (!symbol) {
      lg::debug("hover - no symbol");
      return {};
    }
    // TODO - there is an issue with docstrings and overridden methods
    const auto& symbol_info = workspace.get_global_symbol_info(tracked_file, symbol.value());
    if (!symbol_info) {
      lg::debug("hover - no symbol info - {}", symbol.value());
      return {};
    }
    LSPSpec::MarkupContent markup;
    markup.m_kind = "markdown";

    const auto args = Docs::get_args_from_docstring(symbol_info.value()->m_args,
                                                    symbol_info.value()->m_docstring);
    std::string signature = "";
    bool takes_args = true;
    if (symbol_info.value()->m_kind == symbol_info::Kind::FUNCTION) {
      signature += "function ";
    } else if (symbol_info.value()->m_kind == symbol_info::Kind::METHOD) {
      signature += "method ";
    } else if (symbol_info.value()->m_kind == symbol_info::Kind::MACRO) {
      signature += "macro ";
    } else {
      takes_args = false;
    }
    // TODO - others useful, probably states?
    auto type_info = workspace.get_symbol_typeinfo(tracked_file, symbol.value());
    signature += symbol.value();
    if (takes_args) {
      signature += "(";
      for (int i = 0; i < (int)args.size(); i++) {
        const auto& arg = args.at(i);
        if (i == (int)args.size() - 1) {
          signature += fmt::format("{}: {}", arg.name, arg.type);
        } else {
          signature += fmt::format("{}: {}, ", arg.name, arg.type);
        }
      }
      signature += ")";
      if (symbol_info.value()->m_kind == symbol_info::Kind::FUNCTION && type_info) {
        signature += fmt::format(": {}", type_info->first.last_arg().base_type());
      } else if (symbol_info.value()->m_kind == symbol_info::Kind::METHOD) {
        signature +=
            fmt::format(": {}", symbol_info.value()->m_method_info.type.last_arg().base_type());
      }
    } else if (type_info) {
      signature += fmt::format(": {}", type_info->second->get_parent());
    }

    std::string body = fmt::format("```opengoal\n{}\n```\n\n", signature);
    body += "___\n\n";
    if (!symbol_info.value()->m_docstring.empty()) {
      body += truncate_docstring(symbol_info.value()->m_docstring) + "\n\n";
    }

    // TODO - support @see/@returns/[[reference]]
    for (const auto& arg : args) {
      std::string param_line = "";
      if (arg.is_mutated) {
        param_line += fmt::format("*@param!* `{}: {}`", arg.name, arg.type);
      } else if (arg.is_optional) {
        param_line += fmt::format("*@param?* `{}: {}`", arg.name, arg.type);
      } else if (arg.is_unused) {
        param_line += fmt::format("*@param_* `{}: {}`", arg.name, arg.type);
      } else {
        param_line += fmt::format("*@param* `{}: {}`", arg.name, arg.type);
      }
      if (!arg.description.empty()) {
        param_line += fmt::format(" - {}\n\n", arg.description);
      } else {
        param_line += "\n\n";
      }
      body += param_line;
    }

    markup.m_value = body;
    LSPSpec::Hover hover_resp;
    hover_resp.m_contents = markup;
    return hover_resp;
  }

  return {};
}
}  // namespace lsp_handlers
