#pragma once

#include "common/goos/Reader.h"
#include "goalc/make/Tool.h"

class DgoTool : public Tool {
 public:
  DgoTool();
  bool run(const ToolInput& task) override;
  std::vector<std::string> get_additional_dependencies(const ToolInput&) override;

 private:
  goos::Reader m_reader;
};
