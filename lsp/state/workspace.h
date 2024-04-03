#pragma once

#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>

#include "common/util/FileUtil.h"

#include "decompiler/util/DecompilerTypeSystem.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/docs/DocTypes.h"
#include "lsp/protocol/common_types.h"
#include "lsp/protocol/document_diagnostics.h"
#include "lsp/protocol/document_symbols.h"
#include "lsp/state/lsp_requester.h"

#include "third-party/tree-sitter/tree-sitter/lib/src/tree.h"

// TODO -
// https://sourcegraph.com/github.com/ensisoft/detonator@36f626caf957d0734865a8f5641be6170d997f45/-/blob/editor/app/lua-tools.cpp?L116:15-116:30

struct TreeSitterTreeDeleter {
  void operator()(TSTree* ptr) const { ts_tree_delete(ptr); }
};

struct OpenGOALFormResult {
  std::vector<std::string> tokens;
  std::pair<int, int> start_point;
  std::pair<int, int> end_point;
};

struct OGGlobalIndex {
  std::unordered_map<std::string, Docs::SymbolDocumentation> global_symbols = {};
  std::unordered_map<std::string, Docs::FileDocumentation> per_file_symbols = {};
};

class WorkspaceOGFile {
 public:
  WorkspaceOGFile(){};
  WorkspaceOGFile(const LSPSpec::DocumentUri& uri,
                  const std::string& content,
                  const GameVersion& game_version);
  LSPSpec::DocumentUri m_uri;
  std::string m_content;
  int m_line_count = 0;
  std::string m_line_ending;
  GameVersion m_game_version;
  std::vector<LSPSpec::DocumentSymbol> m_symbols;
  std::vector<LSPSpec::Diagnostic> m_diagnostics;

  void parse_content(const std::string& new_content);
  void update_symbols(const std::vector<symbol_info::SymbolInfo*>& symbol_infos);
  std::optional<std::string> get_symbol_at_position(const LSPSpec::Position position) const;
  std::vector<OpenGOALFormResult> search_for_forms_that_begin_with(
      std::vector<std::string> prefix) const;

 private:
  int32_t version;
  std::shared_ptr<TSTree> m_ast;
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
  void identify_diagnostics(const uint32_t line_num_zero_based,
                            const std::string& line,
                            const bool in_opengoal_block);
};

class WorkspaceAllTypesFile {
 public:
  WorkspaceAllTypesFile()
      : m_dts(std::make_unique<decompiler::DecompilerTypeSystem>(GameVersion::Jak1)){};
  WorkspaceAllTypesFile(const LSPSpec::DocumentUri& uri,
                        const GameVersion version,
                        const fs::path file_path)
      : m_game_version(version),
        m_uri(uri),
        m_dts(std::make_unique<decompiler::DecompilerTypeSystem>(m_game_version)),
        m_file_path(file_path){};

  GameVersion m_game_version;
  LSPSpec::DocumentUri m_uri;
  std::unique_ptr<decompiler::DecompilerTypeSystem> m_dts;
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
  std::optional<GameVersion> determine_game_version_from_uri(const LSPSpec::DocumentUri& uri);

  void start_tracking_file(const LSPSpec::DocumentUri& file_uri,
                           const std::string& language_id,
                           const std::string& content);
  void update_tracked_file(const LSPSpec::DocumentUri& file_uri, const std::string& content);
  void tracked_file_will_save(const LSPSpec::DocumentUri& file_uri);
  void update_global_index(const GameVersion game_version);
  void stop_tracking_file(const LSPSpec::DocumentUri& file_uri);
  std::optional<std::reference_wrapper<WorkspaceOGFile>> get_tracked_og_file(
      const LSPSpec::URI& file_uri);
  std::optional<std::reference_wrapper<WorkspaceIRFile>> get_tracked_ir_file(
      const LSPSpec::URI& file_uri);
  std::optional<DefinitionMetadata> get_definition_info_from_all_types(
      const std::string& symbol_name,
      const LSPSpec::DocumentUri& all_types_uri);
  std::vector<symbol_info::SymbolInfo*> get_symbols_starting_with(const GameVersion game_version,
                                                                  const std::string& symbol_prefix);
  std::optional<symbol_info::SymbolInfo*> get_global_symbol_info(const WorkspaceOGFile& file,
                                                                 const std::string& symbol_name);
  std::optional<std::pair<TypeSpec, Type*>> get_symbol_typeinfo(const WorkspaceOGFile& file,
                                                                const std::string& symbol_name);
  std::optional<symbol_info::DefinitionLocation> get_symbol_def_location(
      const WorkspaceOGFile& file,
      const symbol_info::SymbolInfo* symbol_info);
  std::vector<std::tuple<std::string, std::string, Docs::DefinitionLocation>>
  get_symbols_parent_type_path(const std::string& symbol_name, const GameVersion game_version);
  std::vector<std::tuple<std::string, std::string, Docs::DefinitionLocation>> get_types_subtypes(
      const std::string& symbol_name,
      const GameVersion game_version);
  std::unordered_map<std::string, s64> get_enum_entries(const std::string& enum_name,
                                                        const GameVersion game_version);

 private:
  LSPRequester m_requester;
  bool m_initialized = false;
  std::unordered_map<LSPSpec::DocumentUri, WorkspaceOGFile> m_tracked_og_files = {};
  std::unordered_map<LSPSpec::DocumentUri, WorkspaceIRFile> m_tracked_ir_files = {};
  std::unordered_map<LSPSpec::DocumentUri, std::unique_ptr<WorkspaceAllTypesFile>>
      m_tracked_all_types_files = {};

  // TODO:
  // OpenGOAL is still incredibly tightly coupled to the jak projects as a language
  //
  // In the future, information like GameVersion should just be within the project file
  // and then we can track projects instead of games
  //
  // Until that decoupling happens, things like this will remain fairly clunky.
  // TODO - change this to a shared_ptr so it can more easily be passed around functions
  std::unordered_map<GameVersion, std::unique_ptr<Compiler>> m_compiler_instances;
  std::unordered_map<GameVersion, OGGlobalIndex> m_global_indicies;
};
