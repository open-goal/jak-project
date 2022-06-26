#pragma once

#include <optional>
#include <string>
#include <vector>

#include "common/goos/Object.h"

struct ToolInput {
  std::vector<std::string>& input;   // the input file
  std::vector<std::string>& deps;    // explicit dependencies
  std::vector<std::string>& output;  // produced output files.
  goos::Object arg;                  // optional argument
};

class Tool {
 public:
  Tool(const std::string& name);
  virtual bool run(const ToolInput& task) = 0;
  virtual std::vector<std::string> get_additional_dependencies(const ToolInput&) { return {}; }
  virtual bool needs_run(const ToolInput& task);
  virtual ~Tool() = default;

  const std::string& name() const { return m_name; }

 private:
  std::string m_name;
};
