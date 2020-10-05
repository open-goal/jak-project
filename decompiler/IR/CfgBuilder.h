#pragma once

#include <memory>

class IR;
class Function;
class LinkedObjectFile;
class ControlFlowGraph;

std::shared_ptr<IR> build_cfg_ir(Function& function, ControlFlowGraph& cfg, LinkedObjectFile& file);