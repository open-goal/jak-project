#include "CompilerTool.h"
#include <filesystem>
#include "goalc/compiler/Compiler.h"

CompilerTool::CompilerTool(Compiler* compiler) : Tool("goalc"), m_compiler(compiler) {}

bool CompilerTool::needs_run(const ToolInput& task) {
  if (!m_compiler->knows_object_file(std::filesystem::path(task.input).stem())) {
    return true;
  }
  return Tool::needs_run(task);
}

bool CompilerTool::run(const ToolInput& task) {
  // todo check inputs
  try {
    m_compiler->run_front_end_on_string(
        fmt::format("(asm-file \"{}\" :no-time-prints :color :write)", task.input));
  } catch (std::exception& e) {
    fmt::print("Compilation failed: {}\n", e.what());
    return false;
  }
  return true;
}
