#pragma once

#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "BasicBlocks.h"
#include "CfgVtx.h"
#include "Warnings.h"

#include "common/type_system/TypeSpec.h"
#include "common/type_system/state.h"

#include "decompiler/Disasm/Instruction.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/analysis/atomic_op_builder.h"
#include "decompiler/config.h"

namespace decompiler {
class DecompilerTypeSystem;

struct FunctionName {
  enum class FunctionKind {
    UNIDENTIFIED,  // hasn't been identified yet.
    GLOBAL,        // global named function
    METHOD,
    NV_STATE,
    V_STATE,
    TOP_LEVEL_INIT
  } kind = FunctionKind::UNIDENTIFIED;

  std::string function_name;  // only applicable for GLOBAL
  std::string type_name;      // only applicable for METHOD or v state
  std::string state_name;     // for nv state or v state
  StateHandler handler_kind;  // for nv state or v state
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
        return "(top-level-login " + object_name + ")";
      case FunctionKind::UNIDENTIFIED:
        return "(anon-function " + std::to_string(id_in_object) + " " + object_name + ")";
      case FunctionKind::NV_STATE:
        return fmt::format("({} {})", handler_kind_to_name(handler_kind), state_name);
      case FunctionKind::V_STATE:
        return fmt::format("({} {} {})", handler_kind_to_name(handler_kind), state_name, type_name);
      default:
        throw std::runtime_error("Unsupported FunctionKind");
    }
  }

  int get_anon_id() const {
    ASSERT(kind == FunctionKind::UNIDENTIFIED);
    return id_in_object;
  }

  bool empty() const { return kind == FunctionKind::UNIDENTIFIED; }

  bool is_handler() const {
    return kind == FunctionKind::NV_STATE || kind == FunctionKind::V_STATE;
  }
  bool is_handler(StateHandler shk) const { return is_handler() && handler_kind == shk; }

  bool is_event_handler() const { return is_handler(StateHandler::EVENT); }

  void set_as_top_level(const std::string& object_file_name) {
    kind = FunctionKind::TOP_LEVEL_INIT;
    object_name = object_file_name;
  }

  void set_as_global(std::string name) {
    kind = FunctionKind::GLOBAL;
    function_name = std::move(name);
  }

  void set_as_method(std::string tn, int id) {
    kind = FunctionKind::METHOD;
    type_name = std::move(tn);
    method_id = id;
  }

  void set_as_nv_state(const std::string& state, StateHandler hk) {
    state_name = state;
    handler_kind = hk;
    kind = FunctionKind::NV_STATE;
  }

  void set_as_v_state(const std::string& type, const std::string& state, StateHandler hk) {
    state_name = state;
    handler_kind = hk;
    kind = FunctionKind::V_STATE;
    type_name = type;
  }
};

class Function {
 public:
  Function(int _start_word, int _end_word, GameVersion version);
  ~Function();
  void analyze_prologue(const LinkedObjectFile& file);
  void find_global_function_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts);
  void find_method_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts);
  void find_type_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts);
  bool instr_starts_basic_op(int idx);
  bool instr_starts_atomic_op(int idx);
  const AtomicOp& get_atomic_op_at_instr(int idx);
  BlockTopologicalSort bb_topo_sort();
  std::string name() const;

  TypeSpec type;

  int segment = -1;
  int start_word = -1;
  int end_word = -1;  // not inclusive, but does include padding.

  FunctionName guessed_name;
  std::string state_handler_as_anon_func;

  bool suspected_asm = false;
  bool is_inspect_method = false;
  std::string method_of_type;

  std::vector<Instruction> instructions;
  std::vector<BasicBlock> basic_blocks;
  std::shared_ptr<ControlFlowGraph> cfg = nullptr;
  bool cfg_ok = false;

  int prologue_start = -1;
  int prologue_end = -1;

  int epilogue_start = -1;
  int epilogue_end = -1;

  DecompWarnings warnings;

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

  struct {
    bool atomic_ops_attempted = false;
    bool atomic_ops_succeeded = false;
    std::shared_ptr<FunctionAtomicOps> atomic_ops = nullptr;
    Env env;
    std::shared_ptr<FormPool> form_pool = nullptr;
    Form* top_form = nullptr;
    std::string debug_form_string;
    bool print_debug_forms = false;
    bool expressions_succeeded = false;
  } ir2;

  std::optional<std::string> mips2c_output;

  std::vector<std::string> types_defined;

 private:
  void check_epilogue(const LinkedObjectFile& file);
  void resize_first_block(int new_start, const LinkedObjectFile& file);
  std::unordered_map<int, int> instruction_to_basic_op;
  std::unordered_map<int, int> basic_op_to_instruction;
};
}  // namespace decompiler
