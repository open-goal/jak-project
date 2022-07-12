#pragma once

#include <map>
#include <optional>
#include <string>
#include <utility>

#include "common/util/FileUtil.h"

#include "protocol/common_types.h"
#include "protocol/document_diagnostics.h"
#include "protocol/document_symbols.h"

// TODO - will need ideas to support multiple languages in one LSP
// - perhaps a separate workspace per language?  or some sort of directory
// - when you get the file contents, it comes with language identifier so this isn;t _that_ hard to

class WorkspaceIRFile {
 public:
  WorkspaceIRFile(){};
  WorkspaceIRFile(const std::string& content);
  // TODO - make private
  int32_t version;
  std::vector<std::string> m_lines;
  std::vector<LSPSpec::DocumentSymbol> m_symbols;
  std::vector<LSPSpec::Diagnostic> m_diagnostics;

 private:
  void find_function_symbol(const uint32_t line_num_zero_based, const std::string& line);
  /// @brief Make any relevant diagnostics on the IR line.
  /// It's assumed each line in an IR can have atmost one diagnostic, and they are contained to just that line!
  /// @param line_num_zero_based 
  /// @param line 
  void identify_diagnostics(const uint32_t line_num_zero_based, const std::string& line);
};

class Workspace {
 public:
  Workspace();
  virtual ~Workspace();

  bool is_initialized();
  void set_initialized(bool new_value);

  void update_ir_file(const LSPSpec::URI& file_uri, const std::string& content);
  std::optional<WorkspaceIRFile> get_tracked_ir_file(const LSPSpec::URI& file_uri);

 private:
  bool m_initialized = false;
  std::map<LSPSpec::URI, WorkspaceIRFile> m_tracked_ir_files = {};
};
