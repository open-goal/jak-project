#pragma once

#include <optional>
#include <string>
#include <unordered_map>

#include "common/util/FileUtil.h"

#include "decompiler/util/DecompilerTypeSystem.h"
#include "lsp/protocol/common_types.h"
#include "lsp/protocol/document_diagnostics.h"
#include "lsp/protocol/document_symbols.h"

class WorkspaceIRFile {
 public:
  WorkspaceIRFile(){};
  WorkspaceIRFile(const std::string& content);
  // TODO - make private
  int32_t version;
  std::vector<std::string> m_lines;
  std::vector<LSPSpec::DocumentSymbol> m_symbols;
  std::vector<LSPSpec::Diagnostic> m_diagnostics;
  GameVersion m_game_version;
  LSPSpec::DocumentUri m_all_types_uri = "";
  fs::path m_all_types_file_path;

  std::optional<std::string> get_mips_instruction_at_position(const LSPSpec::Position position);
  std::optional<std::string> get_symbol_at_position(const LSPSpec::Position position);

 private:
  void find_all_types_path(const std::string& line);
  void find_function_symbol(const uint32_t line_num_zero_based, const std::string& line);
  /// @brief Make any relevant diagnostics on the IR line.
  /// It's assumed each line in an IR can have atmost one diagnostic, and they are contained to just
  /// that line!
  /// @param line_num_zero_based
  /// @param line
  void identify_diagnostics(const uint32_t line_num_zero_based, const std::string& line);
};

class WorkspaceAllTypesFile {
 public:
  WorkspaceAllTypesFile() : m_dts(GameVersion::Jak1){};
  WorkspaceAllTypesFile(const LSPSpec::DocumentUri& uri,
                        const GameVersion version,
                        const fs::path file_path)
      : m_uri(uri), m_game_version(version), m_dts(m_game_version), m_file_path(file_path){};

  GameVersion m_game_version;
  LSPSpec::DocumentUri m_uri;
  decompiler::DecompilerTypeSystem m_dts;
  fs::path m_file_path;

  void parse_type_system();
  void update_type_system();
};

class Workspace {
 public:
  Workspace();
  virtual ~Workspace();

  bool is_initialized();
  void set_initialized(bool new_value);

  void start_tracking_file(const LSPSpec::DocumentUri& file_uri,
                           const std::string& language_id,
                           const std::string& content);
  void update_tracked_file(const LSPSpec::DocumentUri& file_uri, const std::string& content);
  void stop_tracking_file(const LSPSpec::DocumentUri& file_uri);
  std::optional<WorkspaceIRFile> get_tracked_ir_file(const LSPSpec::URI& file_uri);
  std::optional<goos::TextDb::ShortInfo> get_symbol_info_from_all_types(
      const std::string& symbol_name,
      const std::string& all_types_uri);

 private:
  bool m_initialized = false;
  std::unordered_map<LSPSpec::DocumentUri, WorkspaceIRFile> m_tracked_ir_files = {};
  std::unordered_map<LSPSpec::DocumentUri, WorkspaceAllTypesFile> m_tracked_all_types_files = {};
};
