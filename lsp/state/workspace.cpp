#include "workspace.h"

#include <regex>

#include "common/log/log.h"

#include "lsp/protocol/common_types.h"

LSPSpec::DocumentUri uri_from_path(fs::path path) {
  // Replace slash type on windows
  std::string path_str = path.string();
#ifdef _WIN32
  std::replace(path_str.begin(), path_str.end(), '\\', '/');
#endif
  return fmt::format("file:///{}", path_str);
}

Workspace::Workspace(){};
Workspace::~Workspace(){};

bool Workspace::is_initialized() {
  return m_initialized;
};

void Workspace::set_initialized(bool new_value) {
  m_initialized = new_value;
}

std::optional<WorkspaceIRFile> Workspace::get_tracked_ir_file(const LSPSpec::URI& file_uri) {
  if (m_tracked_ir_files.count(file_uri) == 0) {
    return {};
  }
  return m_tracked_ir_files[file_uri];
}

std::optional<goos::TextDb::ShortInfo> Workspace::get_symbol_info_from_all_types(
    const std::string& symbol_name,
    const LSPSpec::DocumentUri& all_types_uri) {
  if (m_tracked_all_types_files.count(all_types_uri) == 0) {
    return {};
  }
  const auto& dts = m_tracked_all_types_files[all_types_uri].m_dts;
  if (dts.symbol_metadata_map.count(symbol_name) == 0) {
    return {};
  }
  return dts.symbol_metadata_map.at(symbol_name).definition_info;
}

void Workspace::start_tracking_file(const LSPSpec::DocumentUri& file_uri,
                                    const std::string& language_id,
                                    const std::string& content) {
  if (language_id == "opengoal-ir") {
    lg::debug("new ir file - {}", file_uri);
    WorkspaceIRFile file(content);
    m_tracked_ir_files[file_uri] = file;
    if (!file.m_all_types_uri.empty()) {
      if (m_tracked_all_types_files.count(file.m_all_types_uri) == 0) {
        lg::debug("new all-types file - {}", file_uri);
        m_tracked_all_types_files[file.m_all_types_uri] = WorkspaceAllTypesFile(
            file.m_all_types_uri, file.m_game_version, file.m_all_types_file_path);
        m_tracked_all_types_files[file.m_all_types_uri].parse_type_system();
      }
    }
  }
  // TODO - only supporting IR files currently!
}

void Workspace::update_tracked_file(const LSPSpec::DocumentUri& file_uri,
                                    const std::string& content) {
  lg::debug("potentially updating - {}", file_uri);
  // Check if the file is already tracked or not, this is done because change events don't give
  // language details it's assumed you are keeping track of that!
  if (m_tracked_ir_files.count(file_uri) != 0) {
    lg::debug("updating tracked IR file - {}", file_uri);
    WorkspaceIRFile file(content);
    m_tracked_ir_files[file_uri] = file;
    // There is the potential for the all-types to have changed, albeit this is probably never going
    // to happen
    if (!file.m_all_types_uri.empty() &&
        m_tracked_all_types_files.count(file.m_all_types_uri) == 0) {
      auto& all_types_file = m_tracked_all_types_files[file.m_all_types_uri];
      all_types_file.m_file_path = file.m_all_types_file_path;
      all_types_file.m_uri = file.m_all_types_uri;
      all_types_file.m_game_version = file.m_game_version;
      all_types_file.update_type_system();
    }
  }

  if (m_tracked_all_types_files.count(file_uri) != 0) {
    lg::debug("updating tracked all types file - {}", file_uri);
    // If the all-types file has changed, re-parse it
    // NOTE - this assumes its still for the same game version!
    m_tracked_all_types_files[file_uri].update_type_system();
  }
};

void Workspace::stop_tracking_file(const LSPSpec::DocumentUri& file_uri) {
  if (m_tracked_ir_files.count(file_uri) != 0) {
    m_tracked_ir_files.erase(file_uri);
  }
  if (m_tracked_all_types_files.count(file_uri) != 0) {
    m_tracked_all_types_files.erase(file_uri);
  }
}

WorkspaceIRFile::WorkspaceIRFile(const std::string& content) {
  // Get all lines of file
  std::string::size_type pos = 0;
  std::string::size_type prev = 0;

  // TODO - i hate this assignment inside a conditional, get rid of it
  while ((pos = content.find('\r\n', prev)) != std::string::npos) {
    std::string line = content.substr(prev, pos - prev);
    m_lines.push_back(line);
    // Run any checks on that line
    find_all_types_path(line);
    find_function_symbol(m_lines.size() - 1, line);
    identify_diagnostics(m_lines.size() - 1, line);
    prev = pos + 1;
  }
  std::string line = content.substr(prev);
  m_lines.push_back(line);
  find_function_symbol(m_lines.size() - 1, line);
  identify_diagnostics(m_lines.size() - 1, line);

  lg::info("Added new file. {} lines with {} symbols and {} diagnostics", m_lines.size(),
           m_symbols.size(), m_diagnostics.size());
}

// This is kind of a hack, but to ensure consistency.  The file will reference the all-types.gc
// file it was generated with, this lets us accurately jump to the definition properly!
void WorkspaceIRFile::find_all_types_path(const std::string& line) {
  std::regex regex("; ALL_TYPES=(.*)=(.*)");
  std::smatch matches;

  if (std::regex_search(line, matches, regex)) {
    if (matches.size() == 3) {
      auto game_version = matches[1];
      auto all_types_path = matches[2];
      lg::debug("Found DTS Path - {} : {}", game_version.str(), all_types_path.str());
      auto all_types_uri = uri_from_path(fs::path(all_types_path.str()));
      lg::debug("DTS URI - {}", all_types_uri);

      if (valid_game_version(game_version.str())) {
        m_game_version = game_name_to_version(game_version.str());
        m_all_types_uri = all_types_uri;
        m_all_types_file_path = fs::path(all_types_path.str());
      } else {
        lg::error("Invalid game version, ignoring - {}", game_version.str());
      }
    }
  }
}

void WorkspaceIRFile::find_function_symbol(const uint32_t line_num_zero_based,
                                           const std::string& line) {
  std::regex regex("; \\.function (.*)");
  std::smatch matches;

  if (std::regex_search(line, matches, regex)) {
    // NOTE - assumes we can only find 1 function per line
    if (matches.size() == 2) {
      auto match = matches[1];
      lg::info("Adding Symbol - {}", match.str());
      LSPSpec::DocumentSymbol new_symbol;
      new_symbol.m_name = match.str();
      // TODO - function doc-string
      // new_symbol.m_detail = ...
      new_symbol.m_kind = LSPSpec::SymbolKind::Function;
      LSPSpec::Range symbol_range;
      symbol_range.m_start = {line_num_zero_based, 0};
      symbol_range.m_end = {line_num_zero_based, 0};  // NOTE - set on the next function
      new_symbol.m_range = symbol_range;
      LSPSpec::Range symbol_selection_range;
      symbol_selection_range.m_start = {line_num_zero_based, 0};
      symbol_selection_range.m_end = {line_num_zero_based, (uint32_t)line.length() - 1};
      new_symbol.m_selectionRange = symbol_selection_range;
      m_symbols.push_back(new_symbol);
    }
  }

  std::regex end_function("^;; \\.endfunction\\s*$");
  if (std::regex_match(line, end_function)) {
    lg::debug("Found end of previous function on line - {}", line);
    // Set the previous symbols end-line
    if (!m_symbols.empty()) {
      m_symbols[m_symbols.size() - 1].m_range.m_end.m_line = line_num_zero_based - 1;
    }
  }
}

void WorkspaceIRFile::identify_diagnostics(const uint32_t line_num_zero_based,
                                           const std::string& line) {
  std::regex info_regex(";; INFO: (.*)");
  std::regex warn_regex(";; WARN: (.*)");
  std::regex error_regex(";; ERROR: (.*)");
  std::smatch info_matches;
  std::smatch warn_matches;
  std::smatch error_matches;

  LSPSpec::Range diag_range;
  diag_range.m_start = {line_num_zero_based, 0};
  diag_range.m_end = {line_num_zero_based, (uint32_t)line.length() - 1};

  // Check for an info-level warnings
  if (std::regex_search(line, info_matches, info_regex)) {
    // NOTE - assumes we can only find 1 function per line
    if (info_matches.size() == 2) {
      auto match = info_matches[1];
      lg::debug("Found info-level diagnostic - {}", match.str());
      LSPSpec::Diagnostic new_diag;
      new_diag.m_severity = LSPSpec::DiagnosticSeverity::Information;
      new_diag.m_message = match.str();
      new_diag.m_range = diag_range;
      new_diag.m_source = "OpenGOAL LSP";
      m_diagnostics.push_back(new_diag);
      return;
    }
  }
  // Check for a warn level warnings
  if (std::regex_search(line, warn_matches, warn_regex)) {
    // NOTE - assumes we can only find 1 function per line
    if (warn_matches.size() == 2) {
      auto match = warn_matches[1];
      lg::debug("Found warn-level diagnostic - {}", match.str());
      LSPSpec::Diagnostic new_diag;
      new_diag.m_severity = LSPSpec::DiagnosticSeverity::Warning;
      new_diag.m_message = match.str();
      new_diag.m_range = diag_range;
      new_diag.m_source = "OpenGOAL LSP";
      m_diagnostics.push_back(new_diag);
      return;
    }
  }
  // Check for a error level warnings
  if (std::regex_search(line, error_matches, error_regex)) {
    // NOTE - assumes we can only find 1 function per line
    if (error_matches.size() == 2) {
      auto match = error_matches[1];
      lg::debug("Found error-level diagnostic - {}", match.str());
      LSPSpec::Diagnostic new_diag;
      new_diag.m_severity = LSPSpec::DiagnosticSeverity::Error;
      new_diag.m_message = match.str();
      new_diag.m_range = diag_range;
      new_diag.m_source = "OpenGOAL LSP";
      m_diagnostics.push_back(new_diag);
      return;
    }
  }
}

std::optional<std::string> WorkspaceIRFile::get_mips_instruction_at_position(
    const LSPSpec::Position position) {
  // Split the line on typical word boundaries
  std::string line = m_lines.at(position.m_line);
  std::smatch matches;
  std::regex regex("[\\w\\.]+");

  if (std::regex_search(line, matches, regex)) {
    auto match = matches[0];
    lg::info("hover first match - {}", match.str());
    auto match_start = matches.position(0);
    auto match_end = match_start + match.length();
    if (position.m_character >= match_start && position.m_character <= match_end) {
      return match;
    }
  }

  return {};
}

std::optional<std::string> WorkspaceIRFile::get_symbol_at_position(
    const LSPSpec::Position position) {
  // Split the line on typical word boundaries
  std::string line = m_lines.at(position.m_line);
  lg::debug("symbol checking - '{}'", line);
  std::smatch matches;
  std::regex regex("[\\w\\.\\-_!<>*]+");
  std::regex_token_iterator<std::string::iterator> rend;

  std::regex_token_iterator<std::string::iterator> match(line.begin(), line.end(), regex);
  while (match != rend) {
    lg::debug("match - '{}'", match->str());
    auto match_start = std::distance(line.begin(), match->first);
    auto match_end = match_start + match->length();
    if (position.m_character >= match_start && position.m_character <= match_end) {
      lg::debug("returning");
      return match->str();
    }
    match++;
  }

  return {};
}

void WorkspaceAllTypesFile::parse_type_system() {
  lg::debug("DTS Loading - '{}'", m_file_path.string());
  m_dts.parse_type_defs({m_file_path.string()});
  lg::debug("DTS Loaded At - '{}'", m_file_path.string());
}

void WorkspaceAllTypesFile::update_type_system() {
  m_dts = decompiler::DecompilerTypeSystem(m_game_version);
  parse_type_system();
}
