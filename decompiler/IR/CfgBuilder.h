#pragma once

#include <memory>

namespace decompiler {
class IR;
class Function;
class LinkedObjectFile;
class ControlFlowGraph;

std::shared_ptr<IR> build_cfg_ir(Function& function, ControlFlowGraph& cfg, LinkedObjectFile& file);
}  // namespace decompiler