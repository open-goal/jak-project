
#include <chrono>
#include <filesystem>

#include "third-party/fmt/core.h"

#include "Tool.h"
#include "common/util/FileUtil.h"

Tool::Tool(const std::string& name) : m_name(name) {}

namespace {
bool invalid(const std::filesystem::path& path, const std::filesystem::file_time_type& in_time) {
  if (std::filesystem::exists(path)) {
    auto dep_time = std::filesystem::last_write_time(path);
    if (dep_time < in_time) {
      return true;
    }
  } else {
    return true;
  }
  return false;
}
}  // namespace

bool Tool::needs_run(const ToolInput& task) {
  auto in_time = std::filesystem::last_write_time(
      std::filesystem::path(file_util::get_file_path({task.input})));

  for (auto& dep : task.deps) {
    if (invalid(file_util::get_file_path({dep}), in_time)) {
      return true;
    }
  }

  for (auto& out : task.output) {
    if (invalid(file_util::get_file_path({out}), in_time)) {
      return true;
    }
  }

  for (auto& dep : get_additional_dependencies(task)) {
    if (invalid(file_util::get_file_path({dep}), in_time)) {
      return true;
    }
  }

  return false;
}

bool Tool::are_deps_out_of_date(const std::string& in, const std::vector<std::string>& deps) {
  auto in_time =
      std::filesystem::last_write_time(std::filesystem::path(file_util::get_file_path({in})));

  for (auto& dep : deps) {
    auto dep_time =
        std::filesystem::last_write_time(std::filesystem::path(file_util::get_file_path({dep})));
    if (dep_time < in_time) {
      return true;
    }
  }

  return false;
}