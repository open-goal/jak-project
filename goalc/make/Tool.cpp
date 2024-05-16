
#include "Tool.h"

#include <chrono>

#include "common/util/FileUtil.h"

#include "fmt/core.h"

Tool::Tool(const std::string& name) : m_name(name) {}

bool Tool::needs_run(const ToolInput& task, const PathMap& path_map) {
  // for this to return false, all outputs need to be newer than all inputs.

  for (auto& in : task.input) {
    auto in_file = fs::path(file_util::get_file_path({in}));

    if (!fs::exists(in_file)) {
      throw std::runtime_error(fmt::format("Input file {} does not exist.", in));
    }

    auto newest_input = fs::last_write_time(in_file);
    for (auto& dep : task.deps) {
      auto dep_path = fs::path(file_util::get_file_path({dep}));
      if (fs::exists(dep_path)) {
        auto dep_time = fs::last_write_time(dep_path);
        if (dep_time > newest_input) {
          newest_input = dep_time;
        }
      } else {
        return true;  // don't have a dep.
      }
    }

    for (auto& dep : get_additional_dependencies(task, path_map)) {
      auto dep_path = fs::path(file_util::get_file_path({dep}));
      if (fs::exists(dep_path)) {
        auto dep_time = fs::last_write_time(dep_path);
        if (dep_time > newest_input) {
          newest_input = dep_time;
        }
      } else {
        return true;  // don't have a dep.
      }
    }

    for (auto& out : task.output) {
      auto out_path = fs::path(file_util::get_file_path({out}));
      if (fs::exists(out_path)) {
        auto out_time = fs::last_write_time(out_path);
        if (out_time < newest_input) {
          return true;
        }
      } else {
        return true;  // don't have a dep.
      }
    }
  }

  return false;
}

std::string PathMap::apply_remaps(const std::string& input) const {
  if (!input.empty() && input[0] == '$') {
    std::string prefix = "$";
    size_t i = 1;
    for (; i < input.size(); i++) {
      char c = input[i];
      if (c == '/') {
        break;
      } else {
        prefix.push_back(c);
      }
    }
    const auto& it = path_remap.find(prefix);
    if (it == path_remap.end()) {
      return input;
    } else {
      std::string result = it->second;
      while (!result.empty() && result.back() == '/') {
        result.pop_back();
      }
      result.insert(result.end(), input.begin() + i, input.end());
      return result;
    }
  } else {
    return input;
  }
}
