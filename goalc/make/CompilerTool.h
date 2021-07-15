#pragma once

#include "goalc/make/Tool.h"

class Compiler;

class CompilerTool : public Tool {
 public:
  CompilerTool(Compiler* compiler);

  bool run(const ToolInput& task) override;
  bool needs_run(const ToolInput& task) override;

 private:
  Compiler* m_compiler = nullptr;
};