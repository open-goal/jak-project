#pragma once

#include <optional>
#include <string>
#include <unordered_map>

#include "common/util/FileUtil.h"

#include "decompiler/util/DecompilerTypeSystem.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/docs/DocTypes.h"
#include "lsp/protocol/common_types.h"
#include "lsp/protocol/document_diagnostics.h"
#include "lsp/protocol/document_symbols.h"
#include "lsp/state/lsp_requester.h"

class WorkspaceOGFile {
 public:
  WorkspaceOGFile(){};
  WorkspaceOGFile(const std::string& content, const GameVersion& game_version);
  // TODO - make private
  int32_t version;
  // TODO - keep an AST of the file instead
  std::string m_content;
  std::vector<std::string> m_lines;
  std::vector<LSPSpec::DocumentSymbol> m_symbols;
  std::vector<LSPSpec::Diagnostic> m_diagnostics;
  GameVersion m_game_version;

  std::optional<std::string> get_symbol_at_position(const LSPSpec::Position position) const;
};

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

  std::optional<std::string> get_mips_instruction_at_position(
      const LSPSpec::Position position) const;
  std::optional<std::string> get_symbol_at_position(const LSPSpec::Position position) const;

 private:
  void find_all_types_path(const std::string& line);
  void find_function_symbol(const uint32_t line_num_zero_based, const std::string& line);
  /// Make any relevant diagnostics on the IR line.
  /// It's assumed each line in an IR can have atmost one diagnostic, and they are contained to just
  /// that line!
  void identify_diagnostics(const uint32_t line_num_zero_based, const std::string& line);
};

class WorkspaceAllTypesFile {
 public:
  WorkspaceAllTypesFile() : m_dts(GameVersion::Jak1){};
  WorkspaceAllTypesFile(const LSPSpec::DocumentUri& uri,
                        const GameVersion version,
                        const fs::path file_path)
      : m_game_version(version), m_uri(uri), m_dts(m_game_version), m_file_path(file_path){};

  GameVersion m_game_version;
  LSPSpec::DocumentUri m_uri;
  decompiler::DecompilerTypeSystem m_dts;
  fs::path m_file_path;

  void parse_type_system();
  void update_type_system();
};

class Workspace {
 public:
  enum class FileType { OpenGOAL, OpenGOALIR, Unsupported };
  Workspace();
  virtual ~Workspace();

  bool is_initialized();
  void set_initialized(bool new_value);

  // Even though when a file is initially opened it has the language id
  // many subsequent requests only provide a uri to the file
  // and it's a lot faster to check the end of a string, then multiple tracked file maps
  FileType determine_filetype_from_languageid(const std::string& language_id);
  FileType determine_filetype_from_uri(const LSPSpec::DocumentUri& file_uri);

  void start_tracking_file(const LSPSpec::DocumentUri& file_uri,
                           const std::string& language_id,
                           const std::string& content);
  void update_tracked_file(const LSPSpec::DocumentUri& file_uri, const std::string& content);
  void stop_tracking_file(const LSPSpec::DocumentUri& file_uri);
  std::optional<WorkspaceOGFile> get_tracked_og_file(const LSPSpec::URI& file_uri);
  std::optional<WorkspaceIRFile> get_tracked_ir_file(const LSPSpec::URI& file_uri);
  std::optional<DefinitionMetadata> get_definition_info_from_all_types(
      const std::string& symbol_name,
      const LSPSpec::DocumentUri& all_types_uri);
  std::optional<SymbolInfo> get_global_symbol_info(const WorkspaceOGFile& file,
                                                   const std::string& symbol_name);
  std::optional<TypeSpec> get_symbol_typespec(const WorkspaceOGFile& file,
                                              const std::string& symbol_name);
  std::optional<Docs::DefinitionLocation> get_symbol_def_location(const WorkspaceOGFile& file,
                                                                  const SymbolInfo& symbol_info);

 private:
  LSPRequester m_requester;
  bool m_initialized = false;
  std::unordered_map<LSPSpec::DocumentUri, WorkspaceOGFile> m_tracked_og_files = {};
  std::unordered_map<LSPSpec::DocumentUri, WorkspaceIRFile> m_tracked_ir_files = {};
  std::unordered_map<LSPSpec::DocumentUri, WorkspaceAllTypesFile> m_tracked_all_types_files = {};

  // TODO:
  // OpenGOAL is still incredibly tightly coupled to the jak projects as a language
  //
  // In the future, information like GameVersion should just be within the project file
  // and then we can track projects instead of games
  //
  // Until that decoupling happens, things like this will remain fairly clunky.
  std::unordered_map<GameVersion, std::unique_ptr<Compiler>> m_compiler_instances;
};
