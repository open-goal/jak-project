#pragma once

#include "common/goos/Reader.h"

#include "goalc/make/Tool.h"

class Compiler;

class CompilerTool : public Tool {
 public:
  CompilerTool(Compiler* compiler);
  bool run(const ToolInput& task, const PathMap& path_map) override;
  bool needs_run(const ToolInput& task, const PathMap& path_map) override;

 private:
  Compiler* m_compiler = nullptr;
};

class DgoTool : public Tool {
 public:
  DgoTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
  std::vector<std::string> get_additional_dependencies(const ToolInput&,
                                                       const PathMap& path_map) override;

 private:
  goos::Reader m_reader;
};

class TpageDirTool : public Tool {
 public:
  TpageDirTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
};

class CopyTool : public Tool {
 public:
  CopyTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
};

class GameCntTool : public Tool {
 public:
  GameCntTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
};

class TextTool : public Tool {
 public:
  TextTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
  bool needs_run(const ToolInput& task, const PathMap& path_map) override;
};

class GroupTool : public Tool {
 public:
  GroupTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
};

class SubtitleTool : public Tool {
 public:
  SubtitleTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
  bool needs_run(const ToolInput& task, const PathMap& path_map) override;
};

class SubtitleV2Tool : public Tool {
 public:
  SubtitleV2Tool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
  bool needs_run(const ToolInput& task, const PathMap& path_map) override;
};

class BuildLevelTool : public Tool {
 public:
  BuildLevelTool();
  bool run(const ToolInput& task, const PathMap& path_map) override;
  bool needs_run(const ToolInput& task, const PathMap& path_map) override;
};
