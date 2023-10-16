#include "Tools.h"

#include "common/goos/ParseHelpers.h"
#include "common/util/DgoWriter.h"
#include "common/util/FileUtil.h"

#include "goalc/build_level/jak1/build_level.h"
#include "goalc/build_level/jak2/build_level.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/data_compiler/dir_tpages.h"
#include "goalc/data_compiler/game_count.h"
#include "goalc/data_compiler/game_text_common.h"

#include "third-party/fmt/core.h"

CompilerTool::CompilerTool(Compiler* compiler) : Tool("goalc"), m_compiler(compiler) {}

bool CompilerTool::needs_run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }

  if (!m_compiler->knows_object_file(fs::path(task.input.at(0)).stem().u8string())) {
    return true;
  }
  return Tool::needs_run(task, path_map);
}

bool CompilerTool::run(const ToolInput& task, const PathMap& /*path_map*/) {
  // todo check inputs
  try {
    CompilationOptions options;
    options.filename = task.input.at(0);
    options.color = true;
    options.write = true;
    m_compiler->asm_file(options);
  } catch (std::exception& e) {
    lg::print("Compilation failed: {}\n", e.what());
    return false;
  }
  return true;
}

namespace {
DgoDescription parse_desc_file(const std::string& filename, goos::Reader& reader) {
  auto& dgo_desc = reader.read_from_file({filename}).as_pair()->cdr;
  if (goos::list_length(dgo_desc) != 1) {
    throw std::runtime_error("Invalid DGO description - got too many lists");
  }
  auto& dgo = dgo_desc.as_pair()->car;

  DgoDescription desc;
  auto& first = dgo.as_pair()->car;
  desc.dgo_name = first.as_string()->data;
  auto& dgo_rest = dgo.as_pair()->cdr.as_pair()->car;

  for_each_in_list(dgo_rest, [&](const goos::Object& entry) {
    if (!entry.is_string()) {
      throw std::runtime_error(fmt::format("Invalid file name for DGO: {}\n", entry.print()));
    }

    DgoDescription::DgoEntry o;
    const auto& file_name = entry.as_string()->data;
    // automatically deduce dgo name
    // (not really a fan of how this is written...)
    if (file_name.length() > 2 && file_name.substr(file_name.length() - 2, 2) == ".o") {
      // ends with .o so it's a code file
      o.name_in_dgo = file_name.substr(0, file_name.length() - 2);
    } else if (file_name.length() > 6 && file_name.substr(file_name.length() - 6, 6) == "-ag.go") {
      // ends with -ag.go so it's an art group file
      o.name_in_dgo = file_name.substr(0, file_name.length() - 6);
    } else if (file_name.length() > 3 && file_name.substr(file_name.length() - 3, 3) == ".go") {
      // ends with .go so it's a generic data file
      o.name_in_dgo = file_name.substr(0, file_name.length() - 3);
    }
    o.file_name = file_name;
    desc.entries.push_back(o);
  });
  return desc;
}
}  // namespace

DgoTool::DgoTool() : Tool("dgo") {}

bool DgoTool::run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  auto desc = parse_desc_file(task.input.at(0), m_reader);
  build_dgo(desc, path_map.output_prefix);
  return true;
}

std::vector<std::string> DgoTool::get_additional_dependencies(const ToolInput& task,
                                                              const PathMap& path_map) {
  std::vector<std::string> result;
  auto desc = parse_desc_file(task.input.at(0), m_reader);
  for (auto& x : desc.entries) {
    // todo out
    result.push_back(fmt::format("out/{}obj/{}", path_map.output_prefix, x.file_name));
  }
  return result;
}

TpageDirTool::TpageDirTool() : Tool("tpage-dir") {}

bool TpageDirTool::run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  compile_dir_tpages(task.input.at(0), path_map.output_prefix);
  return true;
}

CopyTool::CopyTool() : Tool("copy") {}

bool CopyTool::run(const ToolInput& task, const PathMap& /*path_map*/) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  for (auto& out : task.output) {
    fs::copy(fs::path(file_util::get_file_path({task.input.at(0)})),
             fs::path(file_util::get_file_path({out})), fs::copy_options::overwrite_existing);
  }
  return true;
}

GameCntTool::GameCntTool() : Tool("game-cnt") {}

bool GameCntTool::run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  compile_game_count(task.input.at(0), path_map.output_prefix);
  return true;
}

TextTool::TextTool() : Tool("text") {}

bool TextTool::needs_run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }

  std::vector<std::string> deps;
  std::vector<GameTextDefinitionFile> files;
  open_text_project("text", task.input.at(0), files);
  for (auto& file : files) {
    deps.push_back(path_map.apply_remaps(file.file_path));
  }
  return Tool::needs_run({task.input, deps, task.output, task.arg}, path_map);
}

bool TextTool::run(const ToolInput& task, const PathMap& path_map) {
  GameTextDB db;
  std::vector<GameTextDefinitionFile> files;
  open_text_project("text", task.input.at(0), files);
  for (auto& file : files) {
    file.file_path = path_map.apply_remaps(file.file_path);
  }
  compile_game_text(files, db, path_map.output_prefix);
  return true;
}

GroupTool::GroupTool() : Tool("group") {}

bool GroupTool::run(const ToolInput&, const PathMap& /*path_map*/) {
  return true;
}

void enumerate_subtitle_project_files(const std::string& tool_name,
                                      const std::string& file_path,
                                      const PathMap& path_map,
                                      std::vector<GameSubtitleDefinitionFile>& files,
                                      std::vector<std::string>& deps) {
  open_subtitle_project(tool_name, file_path, files);
  for (auto& file : files) {
    deps.push_back(path_map.apply_remaps(file.lines_path));
    deps.push_back(path_map.apply_remaps(file.meta_path));
    if (file.lines_base_path) {
      deps.push_back(path_map.apply_remaps(file.lines_base_path.value()));
    }
    if (file.meta_base_path) {
      deps.push_back(path_map.apply_remaps(file.meta_base_path.value()));
    }
  }
}

void run_subtitle_project_files(const std::string& tool_name,
                                const std::string& file_path,
                                const PathMap& path_map,
                                std::vector<GameSubtitleDefinitionFile>& files) {
  open_subtitle_project(tool_name, file_path, files);
  for (auto& file : files) {
    file.lines_path = path_map.apply_remaps(file.lines_path);
    file.meta_path = path_map.apply_remaps(file.meta_path);
    if (file.lines_base_path) {
      file.lines_base_path = path_map.apply_remaps(file.lines_base_path.value());
    }
    if (file.meta_base_path) {
      file.meta_base_path = path_map.apply_remaps(file.meta_base_path.value());
    }
  }
}

SubtitleTool::SubtitleTool() : Tool("subtitle") {}

bool SubtitleTool::needs_run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  std::vector<GameSubtitleDefinitionFile> files;
  std::vector<std::string> deps;
  enumerate_subtitle_project_files(name(), task.input.at(0), path_map, files, deps);
  return Tool::needs_run({task.input, deps, task.output, task.arg}, path_map);
}

bool SubtitleTool::run(const ToolInput& task, const PathMap& path_map) {
  GameSubtitleDB db;
  db.m_subtitle_version = GameSubtitleDB::SubtitleFormat::V1;
  std::vector<GameSubtitleDefinitionFile> files;
  run_subtitle_project_files(name(), task.input.at(0), path_map, files);
  compile_game_subtitles(files, db, path_map.output_prefix);
  return true;
}

SubtitleV2Tool::SubtitleV2Tool() : Tool("subtitle-v2") {}

bool SubtitleV2Tool::needs_run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  std::vector<GameSubtitleDefinitionFile> files;
  std::vector<std::string> deps;
  enumerate_subtitle_project_files(name(), task.input.at(0), path_map, files, deps);
  return Tool::needs_run({task.input, deps, task.output, task.arg}, path_map);
}

bool SubtitleV2Tool::run(const ToolInput& task, const PathMap& path_map) {
  GameSubtitleDB db;
  db.m_subtitle_version = GameSubtitleDB::SubtitleFormat::V2;
  std::vector<GameSubtitleDefinitionFile> files;
  run_subtitle_project_files(name(), task.input.at(0), path_map, files);
  compile_game_subtitles(files, db, path_map.output_prefix);
  return true;
}

BuildLevelTool::BuildLevelTool() : Tool("build-level") {}

bool BuildLevelTool::needs_run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  auto deps = get_build_level_deps(task.input.at(0));
  return Tool::needs_run({task.input, deps, task.output, task.arg}, path_map);
}

bool BuildLevelTool::run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  return jak1::run_build_level(task.input.at(0), task.output.at(0), path_map.output_prefix);
}

BuildLevel2Tool::BuildLevel2Tool() : Tool("build-level2") {}

bool BuildLevel2Tool::needs_run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  auto deps = get_build_level_deps(task.input.at(0));
  return Tool::needs_run({task.input, deps, task.output, task.arg}, path_map);
}

bool BuildLevel2Tool::run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }
  return jak2::run_build_level(task.input.at(0), task.output.at(0), path_map.output_prefix);
}
