#pragma once

#include <optional>
#include <string>
#include <vector>

struct ToolInput {
  std::string& input;                // the input file
  std::vector<std::string>& deps;    // explicit dependencies
  std::vector<std::string>& output;  // produced output files.
};

class Tool {
 public:
  Tool(const std::string& name);
  virtual bool run(const ToolInput& task) = 0;
  virtual std::vector<std::string> get_additional_dependencies(const ToolInput&) { return {}; }
  virtual bool needs_run(const ToolInput& task);
  virtual ~Tool() = default;

  const std::string& name() const { return m_name; }

 protected:
  bool are_deps_out_of_date(const std::string& in, const std::vector<std::string>& deps);

 private:
  std::string m_name;
};