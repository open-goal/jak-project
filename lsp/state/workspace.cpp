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
    prev = pos + 1;
  }
  std::string line = content.substr(prev);
  m_lines.push_back(line);
  find_function_symbol(m_lines.size() - 1, line);

  lg::info("Added new file. {} lines with {} symbols", m_lines.size(), m_symbols.size());
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
  } else {
    lg::info("NOPE! - {}", line);
  }
}
