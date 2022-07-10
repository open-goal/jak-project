#include "Tools.h"

#include "common/goos/ParseHelpers.h"
#include "common/util/DgoWriter.h"
#include "common/util/FileUtil.h"

#include "goalc/build_level/build_level.h"
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
    fmt::print("Compilation failed: {}\n", e.what());
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
  auto& dgo_rest = dgo.as_pair()->cdr;

  for_each_in_list(dgo_rest, [&](const goos::Object& entry) {
    goos::Arguments e_arg;
    std::string err;
    if (!goos::get_va(entry, &err, &e_arg)) {
      throw std::runtime_error(fmt::format("Invalid DGO description: {}\n", err));
    }

    if (!goos::va_check(e_arg, {goos::ObjectType::STRING, goos::ObjectType::STRING}, {}, &err)) {
      throw std::runtime_error(fmt::format("Invalid DGO description: {}\n", err));
    }
    DgoDescription::DgoEntry o;
    o.file_name = e_arg.unnamed.at(0).as_string()->data;
    o.name_in_dgo = e_arg.unnamed.at(1).as_string()->data;
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
  open_text_project("text", task.input.at(0), deps);
  for (auto& dep : deps) {
    dep = path_map.apply_remaps(dep);
  }
  return Tool::needs_run({task.input, deps, task.output, task.arg}, path_map);
}

bool TextTool::run(const ToolInput& task, const PathMap& path_map) {
  GameTextDB db;
  std::vector<std::string> inputs;
  open_text_project("text", task.input.at(0), inputs);
  for (auto& in : inputs) {
    in = path_map.apply_remaps(in);
  }
  compile_game_text(inputs, db, path_map.output_prefix);
  return true;
}

GroupTool::GroupTool() : Tool("group") {}

bool GroupTool::run(const ToolInput&, const PathMap& /*path_map*/) {
  return true;
}

SubtitleTool::SubtitleTool() : Tool("subtitle") {}

bool SubtitleTool::needs_run(const ToolInput& task, const PathMap& path_map) {
  if (task.input.size() != 1) {
    throw std::runtime_error(fmt::format("Invalid amount of inputs to {} tool", name()));
  }

  std::vector<std::string> deps;
  open_text_project("subtitle", task.input.at(0), deps);
  for (auto& dep : deps) {
    dep = path_map.apply_remaps(dep);
  }
  return Tool::needs_run({task.input, deps, task.output, task.arg}, path_map);
}

bool SubtitleTool::run(const ToolInput& task, const PathMap& path_map) {
  GameSubtitleDB db;
  db.m_subtitle_groups = std::make_unique<GameSubtitleGroups>();
  db.m_subtitle_groups->hydrate_from_asset_file();
  std::vector<std::string> inputs;
  open_text_project("subtitle", task.input.at(0), inputs);
  for (auto& in : inputs) {
    in = path_map.apply_remaps(in);
  }
  compile_game_subtitle(inputs, db, path_map.output_prefix);
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
  return run_build_level(task.input.at(0), task.output.at(0), path_map.output_prefix);
}
