#pragma once

#include "goalc/make/Tool.h"
#include "common/goos/Reader.h"
#include "goalc/data_compiler/game_subtitle.h"

class Compiler;

class CompilerTool : public Tool {
 public:
  CompilerTool(Compiler* compiler);
  bool run(const ToolInput& task) override;
  bool needs_run(const ToolInput& task) override;

 private:
  Compiler* m_compiler = nullptr;
};

class DgoTool : public Tool {
 public:
  DgoTool();
  bool run(const ToolInput& task) override;
  std::vector<std::string> get_additional_dependencies(const ToolInput&) override;

 private:
  goos::Reader m_reader;
};

class TpageDirTool : public Tool {
 public:
  TpageDirTool();
  bool run(const ToolInput& task) override;
};

class CopyTool : public Tool {
 public:
  CopyTool();
  bool run(const ToolInput& task) override;
};

class GameCntTool : public Tool {
 public:
  GameCntTool();
  bool run(const ToolInput& task) override;
};

class TextTool : public Tool {
 public:
  TextTool();
  bool run(const ToolInput& task) override;
};

class GroupTool : public Tool {
 public:
  GroupTool();
  bool run(const ToolInput& task) override;
};

class SubtitleTool : public Tool {
 public:
  SubtitleTool(Compiler* compiler);
  bool run(const ToolInput& task) override;
  bool needs_run(const ToolInput& task) override;

 private:
  Compiler* m_compiler;
};
