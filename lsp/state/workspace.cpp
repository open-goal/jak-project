#include "workspace.h"

#include <iomanip>
#include <regex>
#include <sstream>

#include "common/log/log.h"
#include "common/util/string_util.h"

#include "lsp/protocol/common_types.h"

std::string url_encode(const std::string& value) {
  std::ostringstream escaped;
  escaped.fill('0');
  escaped << std::hex;

  for (std::string::const_iterator i = value.begin(), n = value.end(); i != n; ++i) {
    std::string::value_type c = (*i);

    // Keep alphanumeric and other accepted characters intact
    if (isalnum(c) || c == '-' || c == '_' || c == '.' || c == '~' || c == '/') {
      escaped << c;
      continue;
    }

    // Any other characters are percent-encoded
    escaped << std::uppercase;
    escaped << '%' << std::setw(2) << int((unsigned char)c);
    escaped << std::nouppercase;
  }

  return escaped.str();
}

std::string url_decode(const std::string& input) {
  std::ostringstream decoded;

  for (std::size_t i = 0; i < input.length(); ++i) {
    if (input[i] == '%') {
      // Check if there are enough characters remaining
      if (i + 2 < input.length()) {
        // Convert the next two characters after '%' into an integer value
        std::istringstream hexStream(input.substr(i + 1, 2));
        int hexValue = 0;
        hexStream >> std::hex >> hexValue;

        // Append the decoded character to the result
        decoded << static_cast<char>(hexValue);

        // Skip the next two characters
        i += 2;
      }
    } else if (input[i] == '+') {
      // Replace '+' with space character ' '
      decoded << ' ';
    } else {
      // Append the character as is
      decoded << input[i];
    }
  }

  return decoded.str();
}

LSPSpec::DocumentUri uri_from_path(fs::path path) {
  auto path_str = file_util::convert_to_unix_path_separators(path.string());
  // vscode works with proper URL encoded URIs for file paths
  // which means we have to roll our own...
  path_str = url_encode(path_str);
  return fmt::format("file:///{}", path_str);
}

std::string uri_to_path(LSPSpec::DocumentUri uri) {
  auto decoded_uri = url_decode(uri);
  if (str_util::starts_with(decoded_uri, "file:///")) {
#ifdef _WIN32
    decoded_uri = decoded_uri.substr(8);
#else
    decoded_uri = decoded_uri.substr(7);
#endif
  }
  return decoded_uri;
}

Workspace::Workspace(){};
Workspace::~Workspace(){};

bool Workspace::is_initialized() {
  return m_initialized;
};

void Workspace::set_initialized(bool new_value) {
  m_initialized = new_value;
}

Workspace::FileType Workspace::determine_filetype_from_languageid(const std::string& language_id) {
  if (language_id == "opengoal") {
    return FileType::OpenGOAL;
  } else if (language_id == "opengoal-ir") {
    return FileType::OpenGOALIR;
  }
  return FileType::Unsupported;
}

Workspace::FileType Workspace::determine_filetype_from_uri(const LSPSpec::DocumentUri& file_uri) {
  if (str_util::ends_with(file_uri, ".gc")) {
    return FileType::OpenGOAL;
  } else if (str_util::ends_with(file_uri, "ir2.asm")) {
    return FileType::OpenGOALIR;
  }
  return FileType::Unsupported;
}

std::optional<WorkspaceOGFile> Workspace::get_tracked_og_file(const LSPSpec::URI& file_uri) {
  if (m_tracked_og_files.find(file_uri) == m_tracked_og_files.end()) {
    return {};
  }
  return m_tracked_og_files[file_uri];
}

std::optional<WorkspaceIRFile> Workspace::get_tracked_ir_file(const LSPSpec::URI& file_uri) {
  if (m_tracked_ir_files.count(file_uri) == 0) {
    return {};
  }
  return m_tracked_ir_files[file_uri];
}

std::optional<DefinitionMetadata> Workspace::get_definition_info_from_all_types(
    const std::string& symbol_name,
    const LSPSpec::DocumentUri& all_types_uri) {
  if (m_tracked_all_types_files.count(all_types_uri) == 0) {
    return {};
  }
  const auto& dts = m_tracked_all_types_files[all_types_uri]->m_dts;
  if (dts->symbol_metadata_map.count(symbol_name) == 0) {
    return {};
  }
  return dts->symbol_metadata_map.at(symbol_name);
}

// TODO - a gross hack that should go away when the language isn't so tightly coupled to the jak
// games
//
// This is bad because jak 2 now uses some code from the jak1 folder, and also wouldn't be able to
// be determined (jak1 or jak2?) if we had a proper 'common' folder(s).
std::optional<GameVersion> determine_game_version_from_uri(const LSPSpec::DocumentUri& uri) {
  const auto path = uri_to_path(uri);
  if (str_util::contains(path, "goal_src/jak1")) {
    return GameVersion::Jak1;
  } else if (str_util::contains(path, "goal_src/jak2")) {
    return GameVersion::Jak2;
  }
  return {};
}

std::optional<SymbolInfo> Workspace::get_global_symbol_info(const WorkspaceOGFile& file,
                                                            const std::string& symbol_name) {
  if (m_compiler_instances.find(file.m_game_version) == m_compiler_instances.end()) {
    lg::debug("Compiler not instantiated for game version - {}",
              version_to_game_name(file.m_game_version));
    return {};
  }
  const auto& compiler = m_compiler_instances[file.m_game_version].get();
  const auto symbol_infos = compiler->lookup_exact_name_info(symbol_name);
  if (!symbol_infos || symbol_infos->empty()) {
    return {};
  } else if (symbol_infos->size() > 1) {
    // TODO - handle this (overriden methods is the main issue here)
    lg::debug("Found symbol info, but found multiple infos - {}", symbol_infos->size());
    return {};
  }
  const auto& symbol = symbol_infos->at(0);
  return symbol;
}

std::optional<TypeSpec> Workspace::get_symbol_typespec(const WorkspaceOGFile& file,
                                                       const std::string& symbol_name) {
  if (m_compiler_instances.find(file.m_game_version) == m_compiler_instances.end()) {
    lg::debug("Compiler not instantiated for game version - {}",
              version_to_game_name(file.m_game_version));
    return {};
  }
  const auto& compiler = m_compiler_instances[file.m_game_version].get();
  const auto typespec = compiler->lookup_typespec(symbol_name);
  if (typespec) {
    return typespec;
  }
  return {};
}

std::optional<Docs::DefinitionLocation> Workspace::get_symbol_def_location(
    const WorkspaceOGFile& file,
    const SymbolInfo& symbol_info) {
  if (m_compiler_instances.find(file.m_game_version) == m_compiler_instances.end()) {
    lg::debug("Compiler not instantiated for game version - {}",
              version_to_game_name(file.m_game_version));
    return {};
  }
  const auto& compiler = m_compiler_instances[file.m_game_version].get();
  std::optional<Docs::DefinitionLocation> def_loc;
  const auto& goos_info = compiler->get_goos().reader.db.get_short_info_for(symbol_info.src_form());
  if (goos_info) {
    Docs::DefinitionLocation new_def_loc;
    new_def_loc.filename = uri_from_path(goos_info->filename);
    new_def_loc.line_idx = goos_info->line_idx_to_display;
    new_def_loc.char_idx = goos_info->pos_in_line;
    def_loc = new_def_loc;
  }
  return def_loc;
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
        lg::debug("new all-types file - {}", file.m_all_types_uri);
        m_tracked_all_types_files[file.m_all_types_uri] = std::make_unique<WorkspaceAllTypesFile>(
            file.m_all_types_uri, file.m_game_version, file.m_all_types_file_path);
        m_tracked_all_types_files[file.m_all_types_uri]->parse_type_system();
      }
    }
  } else if (language_id == "opengoal") {
    auto game_version = determine_game_version_from_uri(file_uri);
    if (!game_version) {
      lg::debug("Could not determine game version from path - {}", file_uri);
      return;
    }
    // TODO - this should happen on a separate thread so the LSP is not blocking during this lengthy
    // step
    if (m_compiler_instances.find(*game_version) == m_compiler_instances.end()) {
      lg::debug(
          "first time encountering a OpenGOAL file for game version - {}, initializing a compiler",
          version_to_game_name(*game_version));
      const auto project_path = file_util::try_get_project_path_from_path(uri_to_path(file_uri));
      lg::debug("Detected project path - {}", project_path.value());
      if (!file_util::setup_project_path(project_path)) {
        lg::debug("unable to setup project path, not initializing a compiler");
        return;
      }
      m_requester.send_progress_create_request("indexing-jak2", "Indexing - Jak 2");
      m_compiler_instances.emplace(game_version.value(),
                                   std::make_unique<Compiler>(game_version.value()));
      // TODO - if this fails, annotate some errors, adjust progress
      m_compiler_instances.at(*game_version)->run_front_end_on_string("(make-group \"all-code\")");
      m_requester.send_progress_finish_request("indexing-jak2", "Indexed - Jak 2");
    }
    //  TODO - otherwise, just `ml` the file instead of rebuilding the entire thing
    //  TODO - if the file fails to `ml`, annotate some errors
    m_tracked_og_files[file_uri] = WorkspaceOGFile(content, *game_version);
  }
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
      all_types_file->m_file_path = file.m_all_types_file_path;
      all_types_file->m_uri = file.m_all_types_uri;
      all_types_file->m_game_version = file.m_game_version;
      all_types_file->update_type_system();
    }
  }

  if (m_tracked_all_types_files.count(file_uri) != 0) {
    lg::debug("updating tracked all types file - {}", file_uri);
    // If the all-types file has changed, re-parse it
    // NOTE - this assumes its still for the same game version!
    m_tracked_all_types_files[file_uri]->update_type_system();
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

WorkspaceOGFile::WorkspaceOGFile(const std::string& content, const GameVersion& game_version)
    : m_content(content), m_game_version(game_version) {
  m_lines = str_util::split(content);
  lg::info("Added new OG file. {} lines with {} symbols and {} diagnostics", m_lines.size(),
           m_symbols.size(), m_diagnostics.size());
}

std::optional<std::string> WorkspaceOGFile::get_symbol_at_position(
    const LSPSpec::Position position) const {
  // Split the line on typical word boundaries
  std::string line = m_lines.at(position.m_line);
  std::smatch matches;
  std::regex regex("[\\w\\.\\-_!<>*?]+");
  std::regex_token_iterator<std::string::iterator> rend;

  std::regex_token_iterator<std::string::iterator> match(line.begin(), line.end(), regex);
  while (match != rend) {
    auto match_start = std::distance(line.begin(), match->first);
    auto match_end = match_start + match->length();
    if (position.m_character >= match_start && position.m_character <= match_end) {
      return match->str();
    }
    match++;
  }

  return {};
}

WorkspaceIRFile::WorkspaceIRFile(const std::string& content) {
  // Get all lines of file
  std::string::size_type pos = 0;
  std::string::size_type prev = 0;

  // TODO - i hate this assignment inside a conditional, get rid of it
  while ((pos = content.find("\r\n", prev)) != std::string::npos) {
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

  lg::info("Added new IR file. {} lines with {} symbols and {} diagnostics", m_lines.size(),
           m_symbols.size(), m_diagnostics.size());
}

// This is kind of a hack, but to ensure consistency.  The file will reference the all-types.gc
// file it was generated with, this lets us accurately jump to the definition properly!
void WorkspaceIRFile::find_all_types_path(const std::string& line) {
  std::regex regex("; ALL_TYPES=(.*)=(.*)");
  std::smatch matches;

  if (std::regex_search(line, matches, regex)) {
    if (matches.size() == 3) {
      const auto& game_version = matches[1];
      const auto& all_types_path = matches[2];
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
      const auto& match = matches[1];
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
      const auto& match = info_matches[1];
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
      const auto& match = warn_matches[1];
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
      const auto& match = error_matches[1];
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
    const LSPSpec::Position position) const {
  // Split the line on typical word boundaries
  std::string line = m_lines.at(position.m_line);
  std::smatch matches;
  std::regex regex("[\\w\\.]+");

  if (std::regex_search(line, matches, regex)) {
    const auto& match = matches[0];
    auto match_start = matches.position(0);
    auto match_end = match_start + match.length();
    if (position.m_character >= match_start && position.m_character <= match_end) {
      return match;
    }
  }

  return {};
}

std::optional<std::string> WorkspaceIRFile::get_symbol_at_position(
    const LSPSpec::Position position) const {
  // Split the line on typical word boundaries
  std::string line = m_lines.at(position.m_line);
  std::smatch matches;
  std::regex regex("[\\w\\.\\-_!<>*]+");
  std::regex_token_iterator<std::string::iterator> rend;

  std::regex_token_iterator<std::string::iterator> match(line.begin(), line.end(), regex);
  while (match != rend) {
    auto match_start = std::distance(line.begin(), match->first);
    auto match_end = match_start + match->length();
    if (position.m_character >= match_start && position.m_character <= match_end) {
      return match->str();
    }
    match++;
  }

  return {};
}

void WorkspaceAllTypesFile::parse_type_system() {
  lg::debug("DTS Loading - '{}'", m_file_path.string());
  m_dts->parse_type_defs({m_file_path.string()});
  lg::debug("DTS Loaded At - '{}'", m_file_path.string());
}

void WorkspaceAllTypesFile::update_type_system() {
  m_dts = std::make_unique<decompiler::DecompilerTypeSystem>(m_game_version);
  parse_type_system();
}
