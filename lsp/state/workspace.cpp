#include "workspace.h"

#include <regex>

#include "common/log/log.h"

#include "protocol/common_types.h"

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
void Workspace::update_ir_file(const LSPSpec::URI& file_uri, const std::string& content) {
  WorkspaceIRFile file(content);
  m_tracked_ir_files[file_uri] = file;
};

WorkspaceIRFile::WorkspaceIRFile(const std::string& content) {
  lg::info("WS File A");
  // Get all lines of file
  std::string::size_type pos = 0;
  std::string::size_type prev = 0;

  while ((pos = content.find('\r\n', prev)) != std::string::npos) {
    std::string line = content.substr(prev, pos - prev);
    m_lines.push_back(line);
    // Run any checks on that line
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
    lg::info("Found end of previous function on line - {}", line);
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
  std::smatch info_matches;
  std::smatch warn_matches;

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
      new_diag.m_severity = LSPSpec::DiagnosticSeverity::Error;
      new_diag.m_message = match.str();
      new_diag.m_range = diag_range;
      new_diag.m_source = "OpenGOAL LSP";
      m_diagnostics.push_back(new_diag);
      return;
    }
  }
}

std::optional<std::string> WorkspaceIRFile::get_word_at_position(const LSPSpec::Position position) {
  // Split the line on typical word boundaries
  std::string line = m_lines.at(position.m_line);
  std::smatch matches;
  std::regex regex("[\\w\\.]+");

  if (std::regex_search(line, matches, regex)) {
    // TODO - hard-coded to only care about the first match, only hovering for asm instructions
    // currently
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
