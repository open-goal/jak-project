
#include <chrono>
#include <filesystem>

#include "third-party/fmt/core.h"

#include "Tool.h"
#include "common/util/FileUtil.h"

Tool::Tool(const std::string& name) : m_name(name) {}

bool Tool::needs_run(const ToolInput& task) {
  // for this to return false, all outputs need to be newer than all inputs.

  auto in_file = std::filesystem::path(file_util::get_file_path({task.input}));

  if (!std::filesystem::exists(in_file)) {
    throw std::runtime_error(fmt::format("Input file {} does not exist.", task.input));
  }

  auto newest_input = std::filesystem::last_write_time(in_file);
  for (auto& dep : task.deps) {
    auto dep_path = std::filesystem::path(file_util::get_file_path({dep}));
    if (std::filesystem::exists(dep_path)) {
      auto dep_time = std::filesystem::last_write_time(dep_path);
      if (dep_time > newest_input) {
        newest_input = dep_time;
      }
    } else {
      return true;  // don't have a dep.
    }
  }

  for (auto& dep : get_additional_dependencies(task)) {
    auto dep_path = std::filesystem::path(file_util::get_file_path({dep}));
    if (std::filesystem::exists(dep_path)) {
      auto dep_time = std::filesystem::last_write_time(dep_path);
      if (dep_time > newest_input) {
        newest_input = dep_time;
      }
    } else {
      return true;  // don't have a dep.
    }
  }

  for (auto& out : task.output) {
    auto out_path = std::filesystem::path(file_util::get_file_path({out}));
    if (std::filesystem::exists(out_path)) {
      auto out_time = std::filesystem::last_write_time(out_path);
      if (out_time < newest_input) {
        return true;
      }
    } else {
      return true;  // don't have a dep.
    }
  }

  return false;
}
