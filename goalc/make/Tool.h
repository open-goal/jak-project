#pragma once

#include <optional>
#include <string>
#include <vector>

#include "common/goos/Object.h"

struct PathMap {
  std::string output_prefix;
  std::unordered_map<std::string, std::string> path_remap;
  std::string apply_remaps(const std::string& input) const;
};

struct ToolInput {
  std::vector<std::string>& input;   // the input file
  std::vector<std::string>& deps;    // explicit dependencies
  std::vector<std::string>& output;  // produced output files.
  goos::Object arg;                  // optional argument
};

class Tool {
 public:
  Tool(const std::string& name);
  virtual bool run(const ToolInput& task, const PathMap& path_map) = 0;
  virtual std::vector<std::string> get_additional_dependencies(const ToolInput&,
                                                               const PathMap& /*path_map*/) {
    return {};
  }
  virtual bool needs_run(const ToolInput& task, const PathMap& path_map);
  virtual ~Tool() = default;

  const std::string& name() const { return m_name; }

 private:
  std::string m_name;
};
