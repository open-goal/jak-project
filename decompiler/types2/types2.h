#pragma once

#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "decompiler/Function/Function.h"
#include "decompiler/config.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/util/TP_Type.h"

namespace decompiler::types2 {

// Backprop tag types:
//  these classes are "tags" that can be added to types to give a path to propagate constraints
//  backward. For example, if we encounter a function call, and we know the expected argument types,
//  the type pass will use these tags to propagate information backward.

/*!
 * Represents a case where there are multiple possible fields that could be accessed.
 * For example, (&-> matrix vector 0), (&-> matrix data 0). Or any case with overlapping fields.
 */
struct AmbiguousFieldAccess {
  struct Possibility {
    TypeSpec type;
    // TODO: probably stash more info here.
  };
  std::vector<Possibility> possibilities;
  int selected_possibility = -1;  // -1 if not selected.
};

/*!
 * Tag to link an unknown type back to a label with unknown type.
 */
struct UnknownLabel {
  int label_idx = -1;
  std::string label_name;  // just for debug prints
  std::optional<TypeSpec> selected_type;
};

/*!
 * Tag to link an unknown type back to a stack structure with unknown type.
 */
struct UnknownStackStructure {
  int stack_offset = -1;
  std::optional<TypeSpec> selected_type;
};

/*!
 * Tag to link a type back to something outside the block.
 */
struct BlockEntryType {
  Register reg;
  bool is_reg = true;
  int stack_slot = -1;
  std::optional<TP_Type> selected_type;
  bool updated = false;
  std::optional<TP_Type>* type_to_clear = nullptr;
};

struct AmbiguousIntOrFloatConstant {
  std::optional<bool> is_float;
};

/*!
 * Union of all tag types.
 */
struct Tag {
  bool has_tag() { return kind != NONE; }
  enum Kind {
    FIELD_ACCESS,
    UNKNOWN_LABEL,
    UNKNOWN_STACK_STRUCTURE,
    BLOCK_ENTRY,
    INT_OR_FLOAT,
    NONE
  } kind = NONE;

  union {
    AmbiguousFieldAccess* field_access;
    BlockEntryType* block_entry;
    UnknownLabel* unknown_label;
    UnknownStackStructure* unknown_stack_structure;
    AmbiguousIntOrFloatConstant* int_or_float;
  };
};

/*!
 * The basic "type" that we're trying to figure out for each register on each instruction.
 */
struct Type {
  Tag tag;                      // may have type "none"
  std::optional<TP_Type> type;  // may be unknown
};

struct RegType {
  Type type;
  Register reg;
};

struct StackSlotType {
  Type type;
  int slot = -1;
};

struct TypeState {
  Type* gpr_types[32];
  Type* fpr_types[32];
  Type* next_state_type = nullptr;

  Type*& operator[](const Register& reg) {
    switch (reg.get_kind()) {
      case Reg::FPR:
        return fpr_types[reg.get_fpr()];
      case Reg::GPR:
        return gpr_types[reg.get_gpr()];
      default:
        ASSERT(false);
    }
  }

  const Type* operator[](const Register& reg) const {
    switch (reg.get_kind()) {
      case Reg::FPR:
        return fpr_types[reg.get_fpr()];
      case Reg::GPR:
        return gpr_types[reg.get_gpr()];
      default:
        ASSERT(false);
    }
  }

  Type* try_find_stack_spill_slot(int slot) {
    for (auto ss : stack_slot_types) {
      if (ss->slot == slot) {
        return &ss->type;
      }
    }
    return nullptr;
  }

  const Type* try_find_stack_spill_slot(int slot) const {
    for (auto& ss : stack_slot_types) {
      if (ss->slot == slot) {
        return &ss->type;
      }
    }
    return nullptr;
  }

  template <typename T>
  void for_each_type(T&& f) {
    for (auto gpr_type : gpr_types) {
      f(*gpr_type);
    }
    for (auto fpr_type : fpr_types) {
      f(*fpr_type);
    }
    for (auto spill : stack_slot_types) {
      f(spill->type);
    }
    f(*next_state_type);
  }

  std::vector<StackSlotType*> stack_slot_types;
};

struct Instruction {
  TypeState types;
  size_t aop_idx = -1;
  std::vector<RegType> written_reg_types;
  std::optional<StackSlotType> written_stack_slot_type;
  std::optional<Type> written_next_state_type;
  std::unique_ptr<AmbiguousFieldAccess> field_access_tag;
  std::unique_ptr<UnknownLabel> unknown_label_tag;
  std::unique_ptr<UnknownStackStructure> unknown_stack_structure_tag;
  std::unique_ptr<AmbiguousIntOrFloatConstant> int_or_float;
};

struct BlockStartTypes {
  Type gpr_types[32];
  Type fpr_types[32];
  Type next_state_type;
  std::vector<StackSlotType> stack_slot_types;

  StackSlotType* try_find_stack_spill_slot(int slot) {
    for (auto& ss : stack_slot_types) {
      if (ss.slot == slot) {
        return &ss;
      }
    }
    return nullptr;
  }

  Type& operator[](const Register& reg) {
    switch (reg.get_kind()) {
      case Reg::FPR:
        return fpr_types[reg.get_fpr()];
      case Reg::GPR:
        return gpr_types[reg.get_gpr()];
      default:
        ASSERT(false);
    }
  }
};

struct Block {
  bool needs_run = false;
  BlockStartTypes start_types;
  TypeState start_type_state;
  std::vector<Instruction*> instructions;
  std::vector<std::shared_ptr<BlockEntryType>> block_entry_tags;
};

struct FunctionCache {
  std::vector<Block> blocks;
  std::vector<Instruction> instructions;
  std::vector<RegType> reg_type_casts;
  std::vector<StackSlotType> stack_slot_casts;
  std::vector<int> block_visit_order;
};

struct Output {
  std::vector<::decompiler::TypeState> block_init_types;
  std::vector<::decompiler::TypeState> op_end_types;
  std::vector<StackStructureHint> stack_structure_hints;
  bool succeeded = false;
};

struct Input {
  TypeSpec function_type;
  DecompilerTypeSystem* dts;
  Function* func;
};

struct TypePropExtras {
  bool needs_rerun = false;
  bool tags_locked = false;
};

void run(Output& out, const Input& input);

bool backprop_tagged_type(const TP_Type& expected_type,
                          types2::Type& actual_type,
                          const DecompilerTypeSystem& dts);

}  // namespace decompiler::types2