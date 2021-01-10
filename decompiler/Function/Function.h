#pragma once

#ifndef NEXT_FUNCTION_H
#define NEXT_FUNCTION_H

#include <string>
#include <vector>
#include <unordered_map>
#include <stdexcept>
#include <unordered_set>
#include "decompiler/IR2/AtomicOpBuilder.h"
#include "decompiler/Disasm/Instruction.h"
#include "decompiler/Disasm/Register.h"
#include "BasicBlocks.h"
#include "CfgVtx.h"
#include "common/type_system/TypeSpec.h"
#include "decompiler/config.h"

namespace decompiler {
class DecompilerTypeSystem;
class IR_Atomic;
class IR;

struct FunctionName {
  enum class FunctionKind {
    UNIDENTIFIED,  // hasn't been identified yet.
    GLOBAL,        // global named function
    METHOD,
    TOP_LEVEL_INIT,
  } kind = FunctionKind::UNIDENTIFIED;

  std::string function_name;  // only applicable for GLOBAL
  std::string type_name;      // only applicable for METHOD
  int method_id = -1;         // only applicable for METHOD
  int unique_id = -1;

  int id_in_object = -1;
  std::string object_name;

  std::string to_string() const {
    switch (kind) {
      case FunctionKind::GLOBAL:
        return function_name;
      case FunctionKind::METHOD:
        return "(method " + std::to_string(method_id) + " " + type_name + ")";
      case FunctionKind::TOP_LEVEL_INIT:
        return "(top-level-login)";
      case FunctionKind::UNIDENTIFIED:
        return "(anon-function " + std::to_string(id_in_object) + " " + object_name + ")";
      default:
        throw std::runtime_error("Unsupported FunctionKind");
    }
  }

  int get_anon_id() const {
    assert(kind == FunctionKind::UNIDENTIFIED);
    return id_in_object;
  }

  bool empty() const { return kind == FunctionKind::UNIDENTIFIED; }

  void set_as_top_level() { kind = FunctionKind::TOP_LEVEL_INIT; }

  void set_as_global(std::string name) {
    kind = FunctionKind::GLOBAL;
    function_name = std::move(name);
  }

  void set_as_method(std::string tn, int id) {
    kind = FunctionKind::METHOD;
    type_name = std::move(tn);
    method_id = id;
  }
};

class Function {
 public:
  Function(int _start_word, int _end_word);
  void analyze_prologue(const LinkedObjectFile& file);
  void find_global_function_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts);
  void find_method_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts);
  void find_type_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts);
  void add_basic_op(std::shared_ptr<IR_Atomic> op, int start_instr, int end_instr);
  bool has_basic_ops() { return !basic_ops.empty(); }
  bool instr_starts_basic_op(int idx);
  std::shared_ptr<IR_Atomic> get_basic_op_at_instr(int idx);
  bool instr_starts_atomic_op(int idx);
  const AtomicOp& get_atomic_op_at_instr(int idx);
  int get_basic_op_count();
  int get_failed_basic_op_count();
  int get_reginfo_basic_op_count();
  bool run_type_analysis(const TypeSpec& my_type,
                         DecompilerTypeSystem& dts,
                         LinkedObjectFile& file,
                         const std::unordered_map<int, std::vector<TypeHint>>& hints);
  bool run_type_analysis_ir2(const TypeSpec& my_type,
                             DecompilerTypeSystem& dts,
                             LinkedObjectFile& file,
                             const std::unordered_map<int, std::vector<TypeHint>>& hints);
  void run_reg_usage();
  bool build_expression(LinkedObjectFile& file);
  BlockTopologicalSort bb_topo_sort();

  TypeSpec type;

  std::shared_ptr<IR> ir = nullptr;

  int segment = -1;
  int start_word = -1;
  int end_word = -1;  // not inclusive, but does include padding.

  FunctionName guessed_name;

  bool suspected_asm = false;
  bool is_inspect_method = false;
  std::string method_of_type;

  std::vector<Instruction> instructions;
  std::vector<BasicBlock> basic_blocks;
  std::shared_ptr<ControlFlowGraph> cfg = nullptr;

  int prologue_start = -1;
  int prologue_end = -1;

  int epilogue_start = -1;
  int epilogue_end = -1;

  std::string warnings;
  bool contains_asm_ops = false;

  bool attempted_type_analysis = false;

  struct Prologue {
    bool decoded = false;  // have we removed the prologue from basic blocks?
    int total_stack_usage = -1;

    // ra/fp are treated differently from other register backups
    bool ra_backed_up = false;
    int ra_backup_offset = -1;

    bool fp_backed_up = false;
    int fp_backup_offset = -1;

    bool fp_set = false;

    int n_gpr_backup = 0;
    int gpr_backup_offset = -1;

    int n_fpr_backup = 0;
    int fpr_backup_offset = -1;

    int n_stack_var_bytes = 0;
    int stack_var_offset = -1;

    bool epilogue_ok = false;

    std::string to_string(int indent = 0) const;

  } prologue;

  bool uses_fp_register = false;
  std::vector<std::shared_ptr<IR_Atomic>> basic_ops;

  struct {
    bool atomic_ops_attempted = false;
    bool atomic_ops_succeeded = false;
    std::shared_ptr<FunctionAtomicOps> atomic_ops = nullptr;
    Env env;
  } ir2;

 private:
  void check_epilogue(const LinkedObjectFile& file);
  std::unordered_map<int, int> instruction_to_basic_op;
  std::unordered_map<int, int> basic_op_to_instruction;
};
}  // namespace decompiler
#endif  // NEXT_FUNCTION_H
