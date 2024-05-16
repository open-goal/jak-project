#include "analyze_inspect_method.h"

#include "common/log/log.h"

#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

// types with duplicate inspects
static const std::vector<std::string> g_duplicate_inspects_jak3 = {
    "sky-vertex",   "shadow-edge",      "hfrag-poly4",     "hfrag-poly9",
    "hfrag-poly25", "hfrag-mip-packet", "sprite-aux-list", "game-save"};

bool is_set_reg_to_int(AtomicOp* op, Register dst, s64 value) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return false;
  }

  // destination should be a register
  auto dest = set->dst();
  if (dst != dest.reg()) {
    return false;
  }

  auto math = set->src();
  if (SimpleExpression::Kind::IDENTITY != math.kind()) {
    return false;
  }

  auto arg = math.get_arg(0);

  if (!arg.is_int() || value != arg.get_int()) {
    return false;
  }

  return true;
}

bool is_set_reg_to_symbol_value(AtomicOp* op,
                                std::optional<Register> dst,
                                const std::string& value) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return false;
  }

  // destination should be a register
  if (dst) {
    auto dest = set->dst();
    if (dst != dest.reg()) {
      return false;
    }
  }

  auto math = set->src();
  if (SimpleExpression::Kind::IDENTITY != math.kind()) {
    return false;
  }

  auto arg = math.get_arg(0);

  if (!arg.is_sym_val(value)) {
    return false;
  }

  return true;
}

bool is_set_reg_to_symbol_ptr(AtomicOp* op, std::optional<Register> dst, const std::string& value) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return false;
  }

  // destination should be a register
  if (dst) {
    auto dest = set->dst();
    if (dst != dest.reg()) {
      return false;
    }
  }

  auto math = set->src();
  if (SimpleExpression::Kind::IDENTITY != math.kind()) {
    return false;
  }

  auto arg = math.get_arg(0);

  if (!arg.is_sym_ptr(value)) {
    return false;
  }

  return true;
}

std::optional<std::string> get_set_reg_to_symbol_ptr(AtomicOp* op, std::optional<Register> dst) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return {};
  }

  // destination should be a register
  if (dst) {
    auto dest = set->dst();
    if (dst != dest.reg()) {
      return {};
    }
  }

  auto math = set->src();
  if (SimpleExpression::Kind::IDENTITY != math.kind()) {
    return {};
  }

  auto arg = math.get_arg(0);

  if (!arg.is_sym_ptr()) {
    return {};
  }

  return arg.get_str();
}

std::optional<std::string> get_set_reg_to_symbol_value(AtomicOp* op, std::optional<Register> dst) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return {};
  }

  // destination should be a register
  if (dst) {
    auto dest = set->dst();
    if (dst != dest.reg()) {
      return {};
    }
  }

  auto math = set->src();
  if (SimpleExpression::Kind::IDENTITY != math.kind()) {
    return {};
  }

  auto arg = math.get_arg(0);

  if (!arg.is_sym_val()) {
    return {};
  }

  return arg.get_str();
}

bool is_set_reg_to_load(AtomicOp* op, Register dst, int offset) {
  auto lvo = dynamic_cast<LoadVarOp*>(op);
  if (!lvo) {
    return false;
  }

  // destination should be a register
  auto dest = lvo->get_set_destination();
  if (dst != dest.reg()) {
    return false;
  }

  if (lvo->kind() != LoadVarOp::Kind::UNSIGNED) {
    return false;
  }

  if (lvo->size() != 4) {
    return false;
  }

  IR2_RegOffset ro;
  if (!get_as_reg_offset(lvo->src(), &ro)) {
    return false;
  }
  if (ro.offset != offset) {
    return false;
  }

  return true;
}

std::optional<u64> get_set_reg_to_u64_load(AtomicOp* op,
                                           Register dst,
                                           const LinkedObjectFile& file) {
  auto lvo = dynamic_cast<LoadVarOp*>(op);
  if (!lvo) {
    return std::nullopt;
  }

  // destination should be a register
  auto dest = lvo->get_set_destination();
  if (dst != dest.reg()) {
    return std::nullopt;
  }

  if (lvo->src().kind() != SimpleExpression::Kind::IDENTITY) {
    return std::nullopt;
  }

  if (lvo->size() != 8) {
    return std::nullopt;
  }

  const auto& s = lvo->src().get_arg(0);
  if (!s.is_label()) {
    return std::nullopt;
  }
  auto lab = file.labels.at(s.label());

  auto& low = file.words_by_seg.at(lab.target_segment).at(lab.offset / 4);
  auto& hi = file.words_by_seg.at(lab.target_segment).at((lab.offset / 4) + 1);
  if (low.kind() != LinkedWord::PLAIN_DATA || hi.kind() != LinkedWord::PLAIN_DATA) {
    return std::nullopt;
  }
  return ((u64)low.data) | (((u64)hi.data) << 32);
}

std::optional<u64> get_set_reg_to_lui(AtomicOp* op, Register dst, const LinkedObjectFile& file) {
  auto lvo = dynamic_cast<SetVarOp*>(op);
  if (!lvo) {
    return std::nullopt;
  }

  // destination should be a register
  auto dest = lvo->get_set_destination();
  if (dst != dest.reg()) {
    return std::nullopt;
  }

  if (lvo->src().kind() != SimpleExpression::Kind::IDENTITY) {
    return std::nullopt;
  }

  const auto& s = lvo->src().get_arg(0);
  if (!s.is_label()) {
    return std::nullopt;
  }
  auto lab = file.labels.at(s.label());

  auto& low = file.words_by_seg.at(lab.target_segment).at(lab.offset / 4);
  auto& hi = file.words_by_seg.at(lab.target_segment).at((lab.offset / 4) + 1);
  if (low.kind() != LinkedWord::PLAIN_DATA || hi.kind() != LinkedWord::PLAIN_DATA) {
    return std::nullopt;
  }
  return ((u64)low.data) | (((u64)hi.data) << 32);
}

std::optional<std::string> get_string_loaded_to_reg(AtomicOp* op,
                                                    Register reg,
                                                    LinkedObjectFile& file) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return {};
  }

  // destination should be a register
  auto dest = set->dst();
  if (reg != dest.reg()) {
    return {};
  }

  auto math = set->src();
  if (SimpleExpression::Kind::IDENTITY != math.kind()) {
    return {};
  }

  auto& src_atom = set->src().get_arg(0);
  if (!src_atom.is_label()) {
    return {};
  }

  return file.get_goal_string_by_label(src_atom.label());
}

struct FieldPrint {
  static constexpr int NO_ARR = -1;
  static constexpr int DYNAMIC_ARRAY = -2;
  static constexpr int UNKNOWN_ARR_SIZE = -3;

  char format = '\0';
  std::string field_name;
  std::string field_type_name;
  bool has_array = false;
  int array_size = NO_ARR;
};

// if a field has a weird inspect, just return the FieldPrint instead of asserting,
// there's too many edge cases in custom prints to account for all of them
FieldPrint handle_custom_prints(FieldPrint& fp, const std::string& /*str*/) {
  return fp;
}

FieldPrint get_field_print(const std::string& str) {
  int idx = 0;
  auto next = [&]() { return str.at(idx++); };

  auto peek = [&](int off) { return str.at(idx + off); };

  FieldPrint field_print;

  // first is ~T
  char c0 = next();
  ASSERT(c0 == '~');
  char c1 = next();
  if (c1 == '1' || c1 == '2') {
    c1 = next();
  }
  ASSERT(c1 == 'T');

  // next the name:
  char name_char = next();
  if (name_char == '~') {
    return handle_custom_prints(field_print, str);
  }
  while (name_char != ':' && name_char != '[' && name_char != ' ') {
    field_print.field_name.push_back(name_char);
    name_char = next();
  }

  // possibly array thing
  if (name_char == '[') {
    int size = 0;
    char num_char = next();

    // dynamic array using a ~D print
    // (format "~Tstack[~D] @ #x~X~%" (-> obj allocated-length) (-> obj stack))
    if (num_char == '~') {
      num_char = next();
      ASSERT(num_char == 'D');
      num_char = next();
      ASSERT(num_char == ']');
      // distinguish from dynamic arrays that are set to size 0
      field_print.array_size = size = FieldPrint::DYNAMIC_ARRAY;
    }

    while (num_char >= '0' && num_char <= '9') {
      size = size * 10 + (num_char - '0');
      num_char = next();
    }
    field_print.has_array = true;
    field_print.array_size = size;

    ASSERT(num_char == ']');
    char c = next();
    // (method 3 array) and some others have a colon instead of a space here
    if (c == ':') {
      c = next();
    }
    if (c != ' ') {
      return handle_custom_prints(field_print, str);
    }
    c = next();
    if (c != '@') {
      return handle_custom_prints(field_print, str);
    }
    c = next();
    if (c != ' ') {
      return handle_custom_prints(field_print, str);
    }
    c = next();
    if (c != '#') {
      return handle_custom_prints(field_print, str);
    }
    c = next();
    if (c != 'x') {
      return handle_custom_prints(field_print, str);
    }
  } else {
    // next a space
    char space_char = next();
    if (space_char != ' ') {
      return handle_custom_prints(field_print, str);
    }
  }

  // next the format
  char fmt1 = next();
  // if there are extra spaces
  if (fmt1 == ' ') {
    while (fmt1 == ' ') {
      fmt1 = next();
    }
  }
  if (fmt1 == '~' && peek(0) != '`') {  // normal ~_~%
    char fmt_code = next();
    field_print.format = fmt_code;
    char end1 = next();
    if (end1 != '~') {
      return handle_custom_prints(field_print, str);
    }
    char end2 = next();
    if (end2 != '%') {
      return handle_custom_prints(field_print, str);
    }
    ASSERT(idx == (int)str.size());
  } else if (fmt1 == '~' && (peek(0) == 'g' || peek(0) == 'G')) {  // ~g~%
    char fmt_code = next();
    field_print.format = fmt_code;
    char end1 = next();
    if (end1 != '~') {
      return handle_custom_prints(field_print, str);
    }
    char end2 = next();
    if (end2 != '%') {
      return handle_custom_prints(field_print, str);
    }
    ASSERT(idx == (int)str.size());
  } else if (fmt1 == '#' && peek(0) == '<') {  // struct #<my-struct @ #x~X>~%
    next();
    char type_name_c = next();
    while (type_name_c != ' ') {
      field_print.field_type_name += type_name_c;
      type_name_c = next();
    }

    std::string expect_end = "@ #x~X>~%";
    for (char i : expect_end) {
      char c = next();
      ASSERT(i == c);
    }
    field_print.format = 'X';

    ASSERT(idx == (int)str.size());
  } else if (fmt1 == '#' && peek(4) == ':') {  // #x~X : (enum-name
                                               // OR
                                               // #x~X : ~S~%
    if (peek(6) != '(' && peek(7) == 'S') {
      next();
      std::string expect_end = "~X : ~S~%";
      for (char i : expect_end) {
        char c = next();
        ASSERT(i == c);
      }
      field_print.format = 'X';
    } else {
      // skip to paren
      for (int i = 0; i < 7; i++) {
        next();
      }
      auto name = str.substr(idx);
      // some of these don't have the enum name
      if (!name.empty()) {
        name.pop_back();
        field_print.field_type_name = name;
      }
      field_print.format = 'X';
    }
  } else if (fmt1 == '#' && peek(0) == 'x') {  // #x~X~%
    next();
    std::string expect_end = "~X~%";
    for (char i : expect_end) {
      char c = next();
      ASSERT(i == c);
    }
    field_print.format = 'X';
  } else if (fmt1 == '~' && peek(0) == '`') {  // ~`my-type-with-overriden-print`P~%
    next();
    char type_name_c = next();
    while (type_name_c != '`') {
      field_print.field_type_name += type_name_c;
      type_name_c = next();
    }

    std::string expect_end = "P~%";
    for (char i : expect_end) {
      char c = next();
      ASSERT(i == c);
    }
    field_print.format = 'P';

    ASSERT(idx == (int)str.size());
  } else if (str.substr(idx - 1) == "(meters ~m)~%") {
    field_print.format = 'm';
  } else if (str.substr(idx - 1) == "(deg ~r)~%") {
    field_print.format = 'r';
  } else if (str.substr(idx - 1) == "(seconds ~e)~%") {
    field_print.format = 'e';
  }

  else {
    // throw std::runtime_error("other format nyi in get_field_print " + str.substr(idx));
    lg::print("other format nyi in get_field_print {}\n", str.substr(idx));
  }

  return field_print;
}

int get_start_idx_process(Function& function,
                          const std::string& parent_type,
                          Env& env,
                          TypeInspectorResult* result) {
  // hack
  if (function.name() == "(method 3 process-tree)" || function.name() == "(method 3 process)") {
    result->is_basic = true;
    return 7;
  }

  if (parent_type == "process-focusable") {
    result->is_basic = true;
  }

  if (function.basic_blocks.size() != 5) {
    lg::print("[iim] inspect {} had {} basic blocks, expected 5\n", function.name(),
              function.basic_blocks.size());
    return 1;
  }

  if (!function.ir2.atomic_ops) {
    lg::print("[iim] no atomic ops in {}\n", function.name());
    return -1;
  }
  auto& aos = *function.ir2.atomic_ops;

  int op_idx = 0;
  // block 0:
  /*
   *   (set! gp a0)
   *   (b! (truthy gp) L370 (set! v1 #f))
   */

  if (aos.block_id_to_end_atomic_op.at(0) != 2) {
    lg::print("[iim] block 0 had the wrong number of ops: {} for {}\n",
              aos.block_id_to_end_atomic_op.at(0), function.name());
    return -1;
  }

  if (!is_op_2(aos.ops.at(op_idx).get(), SimpleExpression::Kind::IDENTITY,
               Register(Reg::GPR, Reg::GP), Register(Reg::GPR, Reg::A0))) {
    lg::print("[iim] block 0 op 0 bad in {}: {}\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  auto br = dynamic_cast<BranchOp*>(aos.ops.at(op_idx).get());
  if (!br) {
    lg::print("[iim] block 0 op 1 bad in {}: {} (not branch)\n", aos.ops.at(1)->to_string(env),
              function.name());
    return -1;
  }

  if (br->likely() || br->condition().kind() != IR2_Condition::Kind::TRUTHY ||
      !br->condition().src(0).is_var() ||
      br->condition().src(0).var().reg() != Register(Reg::GPR, Reg::GP) ||
      br->branch_delay().kind() != IR2_BranchDelay::Kind::SET_REG_FALSE ||
      br->branch_delay().var(0).reg() != Register(Reg::GPR, Reg::V1)) {
    lg::print("[iim] block 0 op 1 bad in {}: {} (bad branch)\n", aos.ops.at(1)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  // block 1:
  /*
   *  (set! gp gp)
   *  (b! #t L371 (nop!))
   */
  if (aos.block_id_to_end_atomic_op.at(1) != 4) {
    lg::print("[iim] block 1 had the wrong number of ops: {} for {}\n",
              aos.block_id_to_end_atomic_op.at(1), function.name());
    return -1;
  }

  if (!is_op_2(aos.ops.at(op_idx).get(), SimpleExpression::Kind ::IDENTITY,
               Register(Reg::GPR, Reg::GP), Register(Reg::GPR, Reg::GP))) {
    lg::print("[iim] op 2 bad in {}: {}\n", aos.ops.at(op_idx)->to_string(env), function.name());
    return -1;
  }
  op_idx++;

  auto br2 = dynamic_cast<BranchOp*>(aos.ops.at(op_idx).get());
  if (!br2) {
    lg::print("[iim] op 3 bad in {}: {} (not branch)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }

  if (br2->likely() || br2->condition().kind() != IR2_Condition::Kind::ALWAYS ||
      br2->branch_delay().kind() != IR2_BranchDelay::Kind::NOP) {
    lg::print("[iim] op3 bad in {}: {} (bad branch)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  /*


B2:
    or v1, r0, r0             ;; [  4] (set! v1 0)
B3:
L2:
    lw v1, process(s7)        ;; [  5] (set! v1 process)
    lwu t9, 28(v1)            ;; [  6] (set! t9 (l.wu (+ v1 28)))
    or a0, gp, r0             ;; [  7] (set! a0 gp)
    jalr ra, t9               ;; [  8] (call!)
    sll v0, ra, 0
*/
  if (!is_set_reg_to_int(aos.ops.at(op_idx).get(), Register(Reg::GPR, Reg::V1), 0)) {
    lg::print("[iim] op4 bad in {}: {} (bad set 0)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  // try to determine parent type for far labels
  if (parent_type == "UNKNOWN") {
    auto parent_op_str = aos.ops.at(op_idx)->to_string(env);
    auto parent_type_str = parent_op_str.substr(9);
    parent_type_str.pop_back();
    result->parent_type_name = parent_type_str;
  } else {
    if (!is_set_reg_to_symbol_value(aos.ops.at(op_idx).get(), Register(Reg::GPR, Reg::V1),
                                    parent_type)) {
      lg::print("[iim] op5 bad in {}: {} (bad set parent type)\n",
                aos.ops.at(op_idx)->to_string(env), function.name());
      return -1;
    }
  }

  // TODO check if this catches all cases or if there are false positives
  // hack to get correct field offsets for children of process
  if (result->parent_type_name != "structure") {
    result->is_basic = true;
  }

  op_idx++;

  if (aos.ops.at(op_idx).get()->to_string(env) != "(set! t9 (l.wu (+ v1 28)))") {
    lg::print("[iim] op6 bad in {}: {} (bad load inspect)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  if (aos.ops.at(op_idx).get()->to_string(env) != "(set! a0 gp)") {
    lg::print("[iim] op7 bad in {}: {} (bad set arg)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  if (aos.ops.at(op_idx).get()->to_string(env) != "(call!)") {
    lg::print("[iim] op8 bad in {}: {} (bad call)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;
  /*
    lw t9, format(s7)         ;; [  9] (set! t9 format)
    daddiu a0, s7, #t         ;; [ 10] (set! a0 #t)
    daddiu a1, fp, L16        ;; [ 11] (set! a1 L16) "~2Tformation: ~A~%"
    lwu a2, 124(gp)           ;; [ 12] (set! a2 (l.wu (+ gp 124)))
    jalr ra, t9               ;; [ 13] (call!)
    sll v0, ra, 0

    lw t9, format(s7)         ;; [ 14] (set! t9 format)
    daddiu a0, s7, #t         ;; [ 15] (set! a0 #t)
    daddiu a1, fp, L15        ;; [ 16] (set! a1 L15) "~2Tpath: ~A~%"
    lwu a2, 128(gp)           ;; [ 17] (set! a2 (l.wu (+ gp 128)))
    jalr ra, t9               ;; [ 18] (call!)
    sll v0, ra, 0

    lw t9, format(s7)         ;; [ 19] (set! t9 format)
    daddiu a0, s7, #t         ;; [ 20] (set! a0 #t)
    daddiu a1, fp, L14        ;; [ 21] (set! a1 L14) "~2Tformation-timer: ~D~%"
    ld a2, 132(gp)            ;; [ 22] (set! a2 (l.d (+ gp 132)))
    jalr ra, t9               ;; [ 23] (call!)
    sll v0, ra, 0

B4:
L3:
    or v0, gp, r0             ;; [ 24] (set! v0 gp)
    ld ra, 0(sp)
   */
  return op_idx;
}

int get_start_idx(Function& function,
                  LinkedObjectFile& file,
                  TypeInspectorResult* result,
                  const std::string& parent_type,
                  const std::string& type_name,
                  Env& env) {
  if (function.basic_blocks.size() != 5) {
    lg::print("[iim] inspect {} had {} basic blocks, expected 5\n", function.name(),
              function.basic_blocks.size());
    if (parent_type == "basic") {
      result->is_basic = true;
    }
    return -1;
  }

  if (!function.ir2.atomic_ops) {
    lg::print("[iim] no atomic ops in {}\n", function.name());
    return -1;
  }
  auto& aos = *function.ir2.atomic_ops;

  int op_idx = 0;
  // block 0:
  /*
   *   (set! gp a0)
   *   (b! (truthy gp) L370 (set! v1 #f))
   */

  if (aos.block_id_to_end_atomic_op.at(0) != 2) {
    lg::print("[iim] block 0 had the wrong number of ops: {} for {}\n",
              aos.block_id_to_end_atomic_op.at(0), function.name());
    return -1;
  }

  if (!is_op_2(aos.ops.at(op_idx).get(), SimpleExpression::Kind::IDENTITY,
               Register(Reg::GPR, Reg::GP), Register(Reg::GPR, Reg::A0))) {
    lg::print("[iim] block 0 op 0 bad in {}: {}\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  auto br = dynamic_cast<BranchOp*>(aos.ops.at(op_idx).get());
  if (!br) {
    lg::print("[iim] block 0 op 1 bad in {}: {} (not branch)\n", aos.ops.at(1)->to_string(env),
              function.name());
    return -1;
  }

  if (br->likely() || br->condition().kind() != IR2_Condition::Kind::TRUTHY ||
      !br->condition().src(0).is_var() ||
      br->condition().src(0).var().reg() != Register(Reg::GPR, Reg::GP) ||
      br->branch_delay().kind() != IR2_BranchDelay::Kind::SET_REG_FALSE ||
      br->branch_delay().var(0).reg() != Register(Reg::GPR, Reg::V1)) {
    lg::print("[iim] block 0 op 1 bad in {}: {} (bad branch)\n", aos.ops.at(1)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  // block 1:
  /*
   *  (set! gp gp)
   *  (b! #t L371 (nop!))
   */
  if (aos.block_id_to_end_atomic_op.at(1) != 4) {
    lg::print("[iim] block 1 had the wrong number of ops: {} for {}\n",
              aos.block_id_to_end_atomic_op.at(1), function.name());
    return -1;
  }

  if (!is_op_2(aos.ops.at(op_idx).get(), SimpleExpression::Kind ::IDENTITY,
               Register(Reg::GPR, Reg::GP), Register(Reg::GPR, Reg::GP))) {
    lg::print("[iim] op 2 bad in {}: {}\n", aos.ops.at(op_idx)->to_string(env), function.name());
    return -1;
  }
  op_idx++;

  auto br2 = dynamic_cast<BranchOp*>(aos.ops.at(op_idx).get());
  if (!br2) {
    lg::print("[iim] op 3 bad in {}: {} (not branch)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }

  if (br2->likely() || br2->condition().kind() != IR2_Condition::Kind::ALWAYS ||
      br2->branch_delay().kind() != IR2_BranchDelay::Kind::NOP) {
    lg::print("[iim] op3 bad in {}: {} (bad branch)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
    return -1;
  }
  op_idx++;

  // setup
  /*
    (set! v1 0)
    (set! t9 format)
    (set! a0 #t)
    (set! a1 L386)
    (set! a2 gp)
    (set! a3 'vector) (also can be (set! a3 (l.wu (+ gp -4))))
    (call!)
   */

  if (!is_set_reg_to_int(aos.ops.at(op_idx).get(), Register(Reg::GPR, Reg::V1), 0)) {
    lg::print("[iim] op4 bad in {}: {} (bad set 0)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
  }
  op_idx++;

  if (!is_set_reg_to_symbol_value(aos.ops.at(op_idx).get(), Register(Reg::GPR, Reg::T9),
                                  "format")) {
    lg::print("[iim] op5 bad in {}: {} (bad set format)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
  }
  op_idx++;

  if (!is_set_reg_to_symbol_ptr(aos.ops.at(op_idx).get(), Register(Reg::GPR, Reg::A0), "#t")) {
    lg::print("[iim] op6 bad in {}: {} (bad set #t)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
  }
  op_idx++;

  auto type_name_str =
      get_string_loaded_to_reg(aos.ops.at(op_idx).get(), Register(Reg::GPR, Reg::A1), file);
  if (!type_name_str) {
    lg::print("[iim] op7 bad in {}: {} (bad string)\n", aos.ops.at(op_idx)->to_string(env),
              function.name());
  } else if (type_name_str != "[~8x] ~A~%") {
    lg::print("[iim] op7 bad in {}: {} (bad string: {})\n", aos.ops.at(op_idx)->to_string(env),
              function.name(), *type_name_str);
  }
  op_idx++;

  if (!is_op_2(aos.ops.at(op_idx).get(), SimpleExpression::Kind ::IDENTITY,
               Register(Reg::GPR, Reg::A2), Register(Reg::GPR, Reg::GP))) {
    lg::print("[iim] op 8 bad in {}: {}\n", aos.ops.at(op_idx)->to_string(env), function.name());
    return -1;
  }
  op_idx++;

  if (is_set_reg_to_symbol_ptr(aos.ops.at(op_idx).get(), Register(Reg::GPR, Reg::A3), type_name)) {
    result->is_basic = false;
  } else if (aos.ops.at(op_idx)->to_string(env) == "(set! a3 (l.wu (+ gp -4)))" ||
             aos.ops.at(op_idx)->to_string(env) == "(set! a3-0 (l.wu (+ a0-0 -4)))" ||
             aos.ops.at(op_idx)->to_string(env) == "(set! a3-0 (l.wu (+ obj -4)))") {
    result->is_basic = true;
  } else {
    lg::print("[iim] op 9 bad in {}: {}\n", aos.ops.at(op_idx)->to_string(env), function.name());
    return -1;
  }
  op_idx++;

  if (!dynamic_cast<CallOp*>(aos.ops.at(op_idx).get())) {
    lg::print("[iim] op 10 bad in {}: {}\n", aos.ops.at(op_idx)->to_string(env), function.name());
    return -1;
  }
  op_idx++;

  return op_idx;
}

std::pair<Register, int> get_base_of_load(const SimpleExpression& load_addr) {
  if (load_addr.kind() == SimpleExpression::Kind::IDENTITY) {
    const auto& src = load_addr.get_arg(0);
    if (src.is_var()) {
      return {src.var().reg(), 0};
    }
  }

  if (load_addr.kind() == SimpleExpression::Kind::ADD) {
    const auto& src0 = load_addr.get_arg(0);
    const auto& src1 = load_addr.get_arg(1);
    if (src1.get_kind() == SimpleAtom::Kind::INTEGER_CONSTANT &&
        src0.get_kind() == SimpleAtom::Kind::VARIABLE) {
      return {src0.var().reg(), src1.get_int()};
    }
  }
  ASSERT(false);
}

bool is_load_with_base(const SimpleExpression& expr, Register base) {
  return get_base_of_load(expr).first == base;
}

bool is_get_load(AtomicOp* ir, Register dst, Register base) {
  auto as_set = dynamic_cast<LoadVarOp*>(ir);
  return as_set && as_set->get_set_destination().reg() == dst &&
         is_load_with_base(as_set->src(), base);
}

struct LoadInfo {
  int offset = 0;
  int size = 0;
  LoadVarOp::Kind kind;
};

LoadInfo get_load_info_from_set(AtomicOp* load) {
  auto as_load = dynamic_cast<LoadVarOp*>(load);
  ASSERT(as_load);
  LoadInfo info;
  info.kind = as_load->kind();
  info.size = as_load->size();
  auto base = get_base_of_load(as_load->src());
  info.offset = base.second;
  return info;
}

int identify_int_field(int idx,
                       Function& function,
                       TypeInspectorResult* result,
                       FieldPrint& print_info) {
  auto load_info = get_load_info_from_set(function.ir2.atomic_ops->ops.at(idx++).get());

  std::string field_type_name;
  if (load_info.kind == LoadVarOp::Kind::UNSIGNED) {
    field_type_name += "u";
  } else if (load_info.kind == LoadVarOp::Kind::FLOAT) {
    ASSERT(false);  // ...
  }
  field_type_name += "int";

  switch (load_info.size) {
    case 1:
      field_type_name += "8";
      break;
    case 2:
      field_type_name += "16";
      break;
    case 4:
      field_type_name += "32";
      break;
    case 8:
      field_type_name += "64";
      break;
    case 16:
      field_type_name += "128";
      break;
    default:
      throw std::runtime_error("unknown load op size in identify int field " +
                               std::to_string((int)load_info.size));
  }

  if (print_info.format == 'e') {
    field_type_name = "seconds";
    ASSERT(load_info.size == 8);
  }

  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec(field_type_name), offset);
  result->fields_of_type.push_back(field);

  return idx;
}
int identify_float_field(int idx,
                         Function& function,
                         TypeInspectorResult* result,
                         FieldPrint& print_info) {
  auto load_info = get_load_info_from_set(function.ir2.atomic_ops->ops.at(idx++).get());
  ASSERT(load_info.size == 4);
  ASSERT(load_info.kind == LoadVarOp::Kind::FLOAT);

  auto& float_move = function.ir2.atomic_ops->ops.at(idx++);
  if (!is_op_2(float_move.get(), SimpleExpression::Kind::FPR_TO_GPR, make_gpr(Reg::A2),
               make_fpr(0))) {
    printf("bad float move: %s\n", float_move->to_string(function.ir2.env).c_str());
    ASSERT(false);
  }

  std::string type;
  switch (print_info.format) {
    case 'f':
      type = "float";
      break;
    case 'm':
      type = "meters";
      break;
    case 'r':
      type = "deg";
      break;
    case 'X':
      type = "float";
      result->warnings += "field " + print_info.field_name + " is a float printed as hex? ";
      break;
    default:
      ASSERT(false);
  }
  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec(type), offset);
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_pointer_field(int idx,
                           Function& function,
                           TypeInspectorResult* result,
                           FieldPrint& print_info) {
  auto load_info = get_load_info_from_set(function.ir2.atomic_ops->ops.at(idx++).get());
  ASSERT(load_info.size == 4);
  ASSERT(load_info.kind == LoadVarOp::Kind::UNSIGNED);

  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec("pointer"), offset);
  result->fields_of_type.push_back(field);
  return idx;
}

bool get_ptr_offset_constant_nonzero(const SimpleExpression& math, Register base, int* result) {
  // if (!is_reg(math->arg0.get(), base)) {
  if (!math.get_arg(0).is_var() || math.get_arg(0).var().reg() != base) {
    return false;
  }

  if (!math.get_arg(1).is_int()) {
    return false;
  }

  *result = math.get_arg(1).get_int();
  return true;
}

bool get_ptr_offset(AtomicOp* ir, Register dst, Register base, int* result) {
  auto as_set = dynamic_cast<SetVarOp*>(ir);
  if (!as_set) {
    return false;
  }
  if (as_set->dst().reg() != dst) {
    return false;
  }

  return get_ptr_offset_constant_nonzero(as_set->src(), base, result);
}

int identify_array_field(int idx,
                         Function& function,
                         TypeInspectorResult* result,
                         FieldPrint& print_info) {
  AtomicOp* get_op;
  // dynamic array with ~D inspect print
  if (print_info.array_size == FieldPrint::DYNAMIC_ARRAY) {
    idx++;
    get_op = function.ir2.atomic_ops->ops.at(idx).get();
  } else {
    get_op = function.ir2.atomic_ops->ops.at(idx++).get();
  }
  int offset = 0;

  bool ptr;
  if (print_info.array_size == FieldPrint::DYNAMIC_ARRAY) {
    ptr = get_ptr_offset(get_op, make_gpr(Reg::A3), make_gpr(Reg::GP), &offset);
  } else {
    ptr = get_ptr_offset(get_op, make_gpr(Reg::A2), make_gpr(Reg::GP), &offset);
  }

  if (!ptr) {
    printf("bad get ptr offset %s\n", get_op->to_string(function.ir2.env).c_str());
    ASSERT(false);
  }
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec("UNKNOWN"), offset);
  if (print_info.array_size > 0) {
    field.set_array(print_info.array_size);
  } else {
    field.set_dynamic();
  }
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_struct_not_inline_field(int idx,
                                     Function& function,
                                     TypeInspectorResult* result,
                                     FieldPrint& print_info) {
  auto load_info = get_load_info_from_set(function.ir2.atomic_ops->ops.at(idx++).get());

  if (!(load_info.size == 4 && load_info.kind == LoadVarOp::Kind::UNSIGNED)) {
    result->warnings += "field " + print_info.field_type_name + " is likely a value type. ";
  }
  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec(print_info.field_type_name), offset);
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_struct_inline_field(int idx,
                                 Function& function,
                                 TypeInspectorResult* result,
                                 FieldPrint& print_info) {
  auto& get_op = function.ir2.atomic_ops->ops.at(idx++);
  int offset = 0;
  if (!get_ptr_offset(get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP), &offset)) {
    printf("bad get ptr offset %s\n", get_op->to_string(function.ir2.env).c_str());
    // ASSERT(false);
  }
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec(print_info.field_type_name), offset);
  field.set_inline();
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_basic_field(int idx,
                         Function& function,
                         LinkedObjectFile& file,
                         TypeInspectorResult* result,
                         FieldPrint& print_info) {
  (void)file;
  auto load_info = get_load_info_from_set(function.ir2.atomic_ops->ops.at(idx++).get());
  ASSERT(load_info.kind == LoadVarOp::Kind::UNSIGNED || load_info.kind == LoadVarOp::Kind::SIGNED);
  TypeSpec field_type("basic");
  if (load_info.size == 8) {
    result->warnings += "field " + print_info.field_name + " uses ~A with a 64-bit load. ";
    field_type = TypeSpec("uint64");
  } else if (load_info.size == 4) {
    // I wonder if this actually "object", or some other type? It seems to be
    if (load_info.kind == LoadVarOp::Kind::SIGNED) {
      result->warnings += "field " + print_info.field_name + " uses ~A with a signed load. ";
    }
  } else {
    ASSERT(false);
  }

  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, field_type, offset);
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_string_field(int idx,
                          Function& function,
                          LinkedObjectFile& file,
                          TypeInspectorResult* result,
                          FieldPrint& print_info) {
  (void)file;
  auto load_info = get_load_info_from_set(function.ir2.atomic_ops->ops.at(idx++).get());
  ASSERT(load_info.kind == LoadVarOp::Kind::UNSIGNED || load_info.kind == LoadVarOp::Kind::SIGNED);
  TypeSpec field_type("string");
  if (load_info.size == 8) {
    result->warnings += "field " + print_info.field_name + " uses ~S with a 64-bit load. ";
    field_type = TypeSpec("uint64");
  } else if (load_info.size == 4) {
    // I wonder if this actually "object", or some other type? It seems to be
    if (load_info.kind == LoadVarOp::Kind::SIGNED) {
      result->warnings += "field " + print_info.field_name + " uses ~S with a signed load. ";
    }
  } else {
    ASSERT(false);
  }

  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, field_type, offset);
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_cstring_field(int idx,
                           Function& function,
                           LinkedObjectFile& file,
                           TypeInspectorResult* result,
                           FieldPrint& print_info) {
  (void)file;
  auto& get_op = function.ir2.atomic_ops->ops.at(idx++);
  // assuming unknown array size at first
  int size = FieldPrint::UNKNOWN_ARR_SIZE;
  int offset = 0;
  std::string comment;

  // usually either a daddiu or lq
  if (!get_ptr_offset(get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP), &offset)) {
    // daddiu failed, try lq
    auto load_info = get_load_info_from_set(get_op.get());
    if (load_info.size == 16) {
      size = 16;
      offset = load_info.offset;
      comment = "field uses ~g print with a quadword load!";
    } else {
      printf("bad get ptr offset %s\n", get_op->to_string(function.ir2.env).c_str());
      ASSERT(false);
    }
  }
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec("uint8"), offset);
  field.set_array(size);
  if (!comment.empty()) {
    field.set_comment(comment);
  }
  result->fields_of_type.push_back(field);
  return idx;
}

int detect(int idx, Function& function, LinkedObjectFile& file, TypeInspectorResult* result) {
  auto& get_format_op = function.ir2.atomic_ops->ops.at(idx++);
  if (!is_set_reg_to_symbol_value(get_format_op.get(), make_gpr(Reg::T9), "format")) {
    return idx;
    ASSERT_MSG(false,
               fmt::format("bad get format: {}\n", get_format_op->to_string(function.ir2.env)));
  }

  auto& get_true = function.ir2.atomic_ops->ops.at(idx++);
  if (!is_set_reg_to_symbol_ptr(get_true.get(), make_gpr(Reg::A0), "#t")) {
    ASSERT_MSG(false, "bad get true");
  }

  auto sstr = get_string_loaded_to_reg(function.ir2.atomic_ops->ops.at(idx++).get(),
                                       make_gpr(Reg::A1), file);
  if (!sstr) {
    ASSERT_MSG(false, "bad sstr");
  }

  // hack to ignore format print from enum->string and other unexpected stuff
  if (sstr->substr(0, 1) != "~" || sstr == "~T  [~D]~2Tactor-group: ~`actor-group`P~%" ||
      sstr == "~T  [~D]~2Tbuffer: ~A~%") {
    return idx;
  }

  auto info = get_field_print(*sstr);

  auto& first_get_op = function.ir2.atomic_ops->ops.at(idx);

  // v1 load (process pointer):
  // lw t9, format(s7)
  // daddiu a0, s7, #t
  // daddiu a1, fp, L389
  // lwu v1, 12(gp)
  // beq s7, v1, L281
  // or a2, s7, r0
  // B1:
  // lwu v1, 0(v1)
  // lwu a2, 28(v1)
  // B2:
  // L281:
  // jalr ra, t9
  auto load_a2_gp = is_get_load(first_get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP));
  auto load_v1_gp = is_get_load(first_get_op.get(), make_gpr(Reg::V1), make_gpr(Reg::GP));

  if ((load_a2_gp || load_v1_gp) &&
      (info.format == 'D' || info.format == 'd' || info.format == 'X' || info.format == 'e') &&
      !info.has_array && info.field_type_name.empty()) {
    idx = identify_int_field(idx, function, result, info);
    // it's a load!
  } else if (is_get_load(first_get_op.get(), make_fpr(0), make_gpr(Reg::GP)) &&
             (info.format == 'f' || info.format == 'm' || info.format == 'r' ||
              info.format == 'X') &&
             !info.has_array && info.field_type_name.empty()) {
    idx = identify_float_field(idx, function, result, info);
  } else if ((load_a2_gp || load_v1_gp) && info.format == 'A' && !info.has_array &&
             info.field_type_name.empty()) {
    idx = identify_basic_field(idx, function, file, result, info);
  } else if ((load_a2_gp || load_v1_gp) && info.format == 'S' && !info.has_array &&
             info.field_type_name.empty()) {
    idx = identify_string_field(idx, function, file, result, info);
  } else if ((load_a2_gp || load_v1_gp) && (info.format == 'G' || info.format == 'g') &&
             !info.has_array && info.field_type_name.empty()) {
    idx = identify_cstring_field(idx, function, file, result, info);
  } else if ((load_a2_gp || load_v1_gp) && info.format == 'X' && !info.has_array &&
             info.field_type_name.empty()) {
    idx = identify_pointer_field(idx, function, result, info);
  } else if (info.has_array && (info.format == 'X' || info.format == 'P') &&
             info.field_type_name.empty()) {
    idx = identify_array_field(idx, function, result, info);
  } else if (!info.has_array && (info.format == 'X' || info.format == 'P') &&
             !info.field_type_name.empty()) {
    // structure.
    if (is_get_load(first_get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP))) {
      // not inline
      idx = identify_struct_not_inline_field(idx, function, result, info);
    } else {
      idx = identify_struct_inline_field(idx, function, result, info);
    }
  }

  else {
    printf("couldn't do %s, %s, adding unknown field\n", sstr->c_str(),
           first_get_op->to_string(function.ir2.env).c_str());
    // if all else fails, create an unknown field so the rest of the inspect can pass.
    Field unknown("UNKNOWN", TypeSpec("UNKNOWN"), -1);
    unknown.set_comment("field could not be read.");
    result->fields_of_type.push_back(unknown);
    return idx;
  }

  CallOp* call_op;
  if (load_v1_gp) {
    call_op = dynamic_cast<CallOp*>(function.ir2.atomic_ops->ops.at(idx = idx + 3).get());
  } else {
    // dynamic array with ~D inspect print
    if (info.array_size == FieldPrint::DYNAMIC_ARRAY) {
      idx++;
      call_op = dynamic_cast<CallOp*>(function.ir2.atomic_ops->ops.at(idx++).get());
    } else {
      call_op = dynamic_cast<CallOp*>(function.ir2.atomic_ops->ops.at(idx++).get());
    }
  }

  // inspect strings like "#x~X : ~S~%" load the field twice, once into a2, then into v1,
  // then have a bunch of string branches, so we just skip this, since we already have the field.
  //
  // lw t9, format(s7)         ;; [218] (set! t9 format)
  // daddiu a0, s7, #t         ;; [219] (set! a0 #t)
  // daddiu a1, fp, L503       ;; [220] (set! a1 L503) "~1Tclass: #x~X : ~S~%"
  // lhu a2, 26(gp)            ;; [221] (set! a2 (l.hu (+ gp 26)))
  // lhu v1, 26(gp)            ;; [222] (set! v1 (l.hu (+ gp 26)))
  // addiu a3, r0, 149         ;; [223] (set! a3 149)
  // bne v1, a3, L70           ;; [224] (b! (!= v1 a3) L70 (nop!))
  // sll r0, r0, 0

  if (load_a2_gp) {
    auto load_v1_gp_again = is_get_load(function.ir2.atomic_ops->ops.at(idx - 1).get(),
                                        make_gpr(Reg::V1), make_gpr(Reg::GP));
    if (load_v1_gp_again) {
      return idx;
    }
  }

  if (!call_op) {
    printf("bad call\n");
    // ASSERT(false);
    return -1;
  }

  return idx;
}

std::string inspect_inspect_method(Function& inspect_method,
                                   const std::string& type_name,
                                   DecompilerTypeSystem& dts,
                                   LinkedObjectFile& file,
                                   DecompilerTypeSystem& previous_game_ts,
                                   TypeInspectorCache& ti_cache,
                                   ObjectFileDB::PerObjectAllTypeInfo& object_file_meta) {
  lg::print(" iim: {}\n", inspect_method.name());
  TypeInspectorResult result;
  ASSERT(type_name == inspect_method.guessed_name.type_name);
  TypeFlags flags;
  flags.flag = 0;
  result.found_flags = dts.lookup_flags(type_name, &flags.flag);
  result.type_name = type_name;
  result.parent_type_name = dts.lookup_parent_from_inspects(type_name);
  result.flags = flags.flag;
  result.type_size = flags.size;
  result.type_method_count = flags.methods;

  // ignore duplicate inspects
  if (ti_cache.previous_results.find(type_name) != ti_cache.previous_results.end() &&
      !(std::find(g_duplicate_inspects_jak3.begin(), g_duplicate_inspects_jak3.end(), type_name) !=
        g_duplicate_inspects_jak3.end())) {
    return fmt::format(";; {} is already defined!\n", type_name);
  }

  // Only set heap-base if it's different from the automatic one
  // A child (or child of a child) of process ALWAYS has heap-base set.
  if (flags.heap_base > 0) {
    auto process_type = dts.ts.get_type_of_type<BasicType>("process");
    auto auto_hb = (flags.size - process_type->size() + 0xf) & ~0xf;

    if (auto_hb != flags.heap_base) {
      result.type_heap_base = std::make_optional(flags.heap_base);
    }
  }

  {
    TypeFlags parent_flags;
    parent_flags.flag = 0;
    if (result.parent_type_name != "UNKNOWN" &&
        dts.lookup_flags(result.parent_type_name, &parent_flags.flag)) {
      result.parent_method_count = parent_flags.methods;
    }
  }
  if (!result.found_flags) {
    lg::print("[iim] no flags found for {}, maybe defined in the kernel\n", type_name);
  }

  result.parent_type_name = dts.lookup_parent_from_inspects(type_name);
  int idx = get_start_idx(inspect_method, file, &result, result.parent_type_name, type_name,
                          inspect_method.ir2.env);

  if (idx < 0) {
    idx = get_start_idx_process(inspect_method, result.parent_type_name, inspect_method.ir2.env,
                                &result);
  }
  StructureType* old_game_type = nullptr;
  if (previous_game_ts.ts.fully_defined_type_exists(type_name)) {
    old_game_type = dynamic_cast<StructureType*>(previous_game_ts.ts.lookup_type(type_name));
  }
  if (idx <= 0) {
    // can't get any field...
    result.warnings += "Failed to read fields.";
    idx = -2;
    ti_cache.previous_results[type_name] = result;
    return result.print_as_deftype(old_game_type, ti_cache.previous_results, previous_game_ts,
                                   object_file_meta);
  }
  while (idx < int(inspect_method.ir2.atomic_ops->ops.size()) - 2 && idx != -1) {
    // skip over non-format calls in inspects
    auto sstr = inspect_method.ir2.atomic_ops->ops.at(idx)->to_string(inspect_method.ir2.env);
    if (sstr.substr(sstr.size() - 7) != "format)") {
      idx++;
      continue;
    }
    idx = detect(idx, inspect_method, file, &result);
  }

  if (idx == -1) {
    result.warnings += "Failed to read some fields.";
  }
  ti_cache.previous_results[type_name] = result;
  return result.print_as_deftype(old_game_type, ti_cache.previous_results, previous_game_ts,
                                 object_file_meta);
}

std::string old_method_string(const MethodInfo& info, const bool omit_comment = false) {
  if (info.type.arg_count() > 0) {
    if (info.type.base_type() == "function" || info.type.base_type() == "state") {
      std::string result;
      if (omit_comment) {
        result = fmt::format(" ({} (", info.name);
      } else {
        result = fmt::format(" ;; ({} (", info.name);
      }
      bool add = false;
      for (int i = 0; i < (int)info.type.arg_count() - 1; i++) {
        result += info.type.get_arg(i).print();
        result += ' ';
        add = true;
      }
      if (add) {
        result.pop_back();
      }
      result += ") ";
      result += info.type.get_arg(info.type.arg_count() - 1).print();
      if (info.type.base_type() == "state") {
        result += " :state";
      }
      result += ")";
      return result;
    }
  }

  if (omit_comment) {
    return fmt::format(" ({} {}) weird method", info.name, info.type.print());
  }
  return fmt::format(" ;; ({} {}) weird method", info.name, info.type.print());
}

bool allow_guess(const Field& field) {
  // allow anything UNKNOWN because we have no idea
  if (field.type().base_type() == "UNKNOWN") {
    return true;
  }

  // don't allow known inline's because we get that right.
  if (field.is_inline()) {
    return false;
  }

  auto typ = field.type().print();

  if (typ == "basic" || typ == "uint32") {
    return true;
  }

  return false;
}
/*
 * old_game_type may be null
 */
std::string TypeInspectorResult::print_as_deftype(
    StructureType* old_game_type,
    std::unordered_map<std::string, TypeInspectorResult>& previous_results,
    DecompilerTypeSystem& previous_game_ts,
    ObjectFileDB::PerObjectAllTypeInfo& object_file_meta) {
  std::string result;

  result += "#|\n";
  result += fmt::format("(deftype {} ({})\n  (", type_name, parent_type_name);

  int longest_field_name = 0;
  int longest_type_name = 0;
  int longest_mods = 0;

  std::string inline_string = ":inline";
  std::string dynamic_string = ":dynamic";

  std::vector<bool> needed, was_guess;
  {
    const auto& prev_it = previous_results.find(parent_type_name);
    if (prev_it != previous_results.end()) {
      auto& prev_fields = prev_it->second.fields_of_type;
      for (auto& field : fields_of_type) {
        auto field_it = std::find(prev_fields.begin(), prev_fields.end(), field);
        needed.push_back(field_it == prev_fields.end());
      }
    } else {
      needed.resize(fields_of_type.size(), true);
    }
  }

  for (auto& field : fields_of_type) {
    if (!allow_guess(field)) {
      was_guess.push_back(false);
      continue;
    }
    if (old_game_type) {
      Field old_field;
      if (old_game_type->lookup_field(field.name(), &old_field) &&
          field.type() != old_field.type()) {
        field.type() = old_field.type();
        was_guess.push_back(true);
      } else {
        was_guess.push_back(false);
      }
    } else {
      was_guess.push_back(false);
    }
  }

  for (size_t field_idx = 0; field_idx < fields_of_type.size(); field_idx++) {
    if (!needed[field_idx]) {
      continue;
    }
    auto& field = fields_of_type[field_idx];
    longest_field_name = std::max(longest_field_name, int(field.name().size()));
    longest_type_name = std::max(longest_type_name, int(field.type().print().size()));

    int mods = 0;
    // mods are array size, :inline, :dynamic
    if (field.is_array() && !field.is_dynamic()) {
      // "??" for unknown array size
      if (field.array_size() == FieldPrint::UNKNOWN_ARR_SIZE) {
        mods += 2;
      } else {
        mods += std::to_string(field.array_size()).size();
      }
    }

    if (field.is_inline()) {
      if (mods) {
        mods++;  // space
      }
      mods += inline_string.size();
    }

    if (field.is_dynamic()) {
      if (mods) {
        mods++;  // space
      }
      mods += dynamic_string.size();
    }
    longest_mods = std::max(longest_mods, mods);
  }

  for (size_t field_idx = 0; field_idx < fields_of_type.size(); field_idx++) {
    if (!needed[field_idx]) {
      continue;
    }
    auto& field = fields_of_type[field_idx];
    result += "(";
    result += field.name();
    result.append(1 + (longest_field_name - int(field.name().size())), ' ');
    result += field.type().print();
    result.append(1 + (longest_type_name - int(field.type().print().size())), ' ');

    std::string mods;
    if (field.is_array() && !field.is_dynamic()) {
      if (field.array_size() == FieldPrint::UNKNOWN_ARR_SIZE) {
        mods += "??";
        mods += " ";
      } else {
        mods += std::to_string(field.array_size());
        mods += " ";
      }
    }

    if (field.is_inline()) {
      mods += inline_string;
      mods += " ";
    }

    if (field.is_dynamic()) {
      mods += dynamic_string;
      mods += " ";
    }
    result.append(mods);
    result.append(longest_mods - int(mods.size() - 1), ' ');

    result.append(":offset-assert ");
    result.append(std::to_string(field.offset()));
    result.append(")");
    if (old_game_type) {
      Field old_field;
      if (old_game_type->lookup_field(field.name(), &old_field)) {
        if (old_field.type() != field.type()) {
          result += fmt::format(" ;; {}", old_field.type().print());
          if (old_field.is_array() && !old_field.is_dynamic()) {
            result += fmt::format(" {}", old_field.array_size());
          }
          if (old_field.is_inline()) {
            result += " :inline";
          }
          if (old_field.is_dynamic()) {
            result += " :dynamic";
          }
        }
      }
    }
    if (field.has_comment()) {
      result += fmt::format(" ;; {}", field.comment());
    }

    if (was_guess[field_idx]) {
      result += " ;; guessed by decompiler";
    }
    result.append("\n   ");
  }
  result.append(")\n");

  result.append(fmt::format("  :method-count-assert {}\n", type_method_count));
  result.append(fmt::format("  :size-assert         #x{:x}\n", type_size));
  if (type_heap_base.has_value()) {
    result.append(fmt::format("  :heap-base           #x{:x}\n", type_heap_base.value()));
  }
  result.append(fmt::format("  :flag-assert         #x{:x}\n  ", flags));
  if (!warnings.empty()) {
    result.append(";; ");
    result.append(warnings);
    result.append("\n  ");
  }

  std::string state_methods_list;
  std::unordered_map<int, std::string> method_states = {};
  if (object_file_meta.state_methods.count(type_name) != 0) {
    method_states = object_file_meta.state_methods.at(type_name);
    for (const auto& [method_id, state_name] : method_states) {
      MethodInfo info;
      state_methods_list += fmt::format("    {} ;; {}", state_name, method_id);
      if (old_game_type && old_game_type->get_my_method(method_id, &info)) {
        state_methods_list += ", old:" + old_method_string(info, true);
      }
      state_methods_list += "\n";
    }
  }

  std::string methods_list;
  if (type_method_count > 9) {
    MethodInfo old_new_method;
    if (old_game_type && old_game_type->get_my_new_method(&old_new_method)) {
      methods_list.append("    (new (symbol type) _type_) ;; 0");
      methods_list.append(old_method_string(old_new_method));
      methods_list.push_back('\n');
    }
    for (int i = parent_method_count; i < type_method_count; i++) {
      // If it's a state-method (virtual state) skip it
      if (method_states.find(i) != method_states.end()) {
        continue;
      }

      methods_list.append(fmt::format("    ({}-method-{} () none) ;; {}", type_name, i, i));
      if (old_game_type) {
        MethodInfo info;
        if (old_game_type->get_my_method(i, &info)) {
          methods_list += old_method_string(info);
        }
      }
      methods_list.push_back('\n');
    }
  }

  // non-virtual states
  std::string non_virtual_states_list;
  for (const auto& [state_name, guessed_type_name] : object_file_meta.non_virtual_state_guesses) {
    if (type_name == guessed_type_name) {
      std::string line;
      line += fmt::format("    {}", state_name);
      auto it = previous_game_ts.symbol_types.find(state_name);
      if (it != previous_game_ts.symbol_types.end()) {
        line += fmt::format(" ;; associated process guessed by decompiler, old: {}",
                            it->second.print());
      }
      non_virtual_states_list.append(line + "\n");
    }
  }
  // methods and virtual states
  if (!methods_list.empty()) {
    result.append("(:methods\n");
    result.append(methods_list);
    result.append("    )\n  ");
  }
  if (!state_methods_list.empty()) {
    result.append("(:state-methods\n");
    result.append(state_methods_list);
    result.append("    )\n  ");
  }
  // non-virtual states
  if (!non_virtual_states_list.empty()) {
    result.append("(:states\n");
    result.append(non_virtual_states_list);
    result.append("    )\n  ");
  }

  result.append(")\n");
  result += "|#\n";

  return result;
}

std::string get_regex_match(const std::string& form, const std::regex& regex) {
  std::smatch matches;
  if (std::regex_search(form, matches, regex)) {
    if (matches.size() == 2) {
      return matches[1];
    }
  }
  return "";
}

std::string get_state_symbol_name(LinkedObjectFile& file, const std::string& label_name) {
  try {
    auto& label = file.get_label_by_name(label_name);
    auto& label_words = file.words_by_seg.at(label.target_segment);
    int start_word_idx = (label.offset / 4) - 1;

    auto& first_word = label_words.at(start_word_idx);
    if (first_word.kind() != LinkedWord::TYPE_PTR || first_word.symbol_name() != "state") {
      return "";
    }

    auto& name_word = label_words.at(start_word_idx + 1);
    if (name_word.kind() != LinkedWord::SYM_PTR) {
      return "";
    }

    return name_word.symbol_name();
  } catch (std::exception& e) {
    return "";
  }
}

std::string get_label_type_name(LinkedObjectFile& file, const std::string& label_name) {
  try {
    auto& label = file.get_label_by_name(label_name);
    auto& label_words = file.words_by_seg.at(label.target_segment);
    int start_word_idx = (label.offset / 4) - 1;

    auto& first_word = label_words.at(start_word_idx);
    if (first_word.kind() != LinkedWord::TYPE_PTR) {
      return "";
    }
    return first_word.symbol_name();
  } catch (std::exception& e) {
    return "";
  }
}

void inspect_top_level_for_metadata(Function& top_level,
                                    LinkedObjectFile& file,
                                    DecompilerTypeSystem& /*dts*/,
                                    DecompilerTypeSystem& /*previous_game_ts*/,
                                    ObjectFileDB::PerObjectAllTypeInfo& objectFile) {
  // State as a method:
  /*
  lui v1, L267              ;; [ 77] (set! gp-0 L267) [] -> [gp: <uninitialized> ]
  ori gp, v1, L267
  lw t9, method-set!(s7)    ;; [ 78] (set! t9-12 method-set!) [] -> [t9: <uninitialized> ]
  lw a0, com-airlock(s7)    ;; [ 79] (set! a0-12 com-airlock) [] -> [a0: <uninitialized> ]
  addiu a1, r0, 21          ;; [ 80] (set! a1-10 21) [] -> [a1: <uninitialized> ]
  or a2, gp, r0             ;; [ 81] (set! a2-10 gp-0) [gp: <uninitialized> ] -> [a2:
  <uninitialized> ]
  */
  // State as symbol:
  /*
  lui v1, L753              ;; [354] (set! v1-38 L753) [] -> [v1: <uninitialized> ]
  ori v1, v1, L753
  sw v1, target-roll(s7)    ;; [355] (s.w! target-roll v1-38) [v1: <uninitialized> ] -> []
  */
  if (!top_level.ir2.atomic_ops) {
    return;
  }

  // Check for non-method states
  std::string last_seen_label;
  // TODO - safely increment op number
  for (int i = 0; i < (int)top_level.ir2.atomic_ops->ops.size(); i++) {
    const auto& aop = top_level.ir2.atomic_ops->ops.at(i);
    const std::string as_str = aop.get()->to_string(top_level.ir2.env);

    // Keep track of the last seen label so we can easily reference it if a later operation uses
    // it
    auto label_match = get_regex_match(as_str, std::regex("\\(set!\\s[^\\s]*\\s(L.*)\\)"));
    if (!label_match.empty()) {
      last_seen_label = label_match;

      // Check if the next operation is storing the label
      std::string curr_op =
          top_level.ir2.atomic_ops->ops.at(i + 1).get()->to_string(top_level.ir2.env);
      auto symbol_name = get_regex_match(curr_op, std::regex("\\(s\\.w!\\s([^\\(\\)\\s]*)\\s"));
      if (symbol_name.empty()) {
        continue;
      }

      // Check that the label is a state
      auto label_type_name = get_label_type_name(file, last_seen_label);
      if (label_type_name.empty()) {
        continue;
      }
      objectFile.symbol_types[symbol_name] = label_type_name;
    }

    if (as_str.find("method-set!") != std::string::npos) {
      // The next operation should have the type name
      i++;
      std::string curr_op = top_level.ir2.atomic_ops->ops.at(i).get()->to_string(top_level.ir2.env);
      auto type_match = get_regex_match(curr_op, std::regex("\\(set!\\s[^\\s]*\\s(.*)\\)"));
      if (type_match.empty()) {
        continue;
      }
      i++;
      // The next operation should have the method id
      curr_op = top_level.ir2.atomic_ops->ops.at(i).get()->to_string(top_level.ir2.env);
      auto method_id_match = get_regex_match(curr_op, std::regex("\\(set!\\s[^\\s]*\\s(\\d*)\\)"));
      if (method_id_match.empty()) {
        continue;
      }
      int method_id = std::stoi(method_id_match);

      // Now check the last seen label to see if it's a state
      auto state_name = get_state_symbol_name(file, last_seen_label);
      if (state_name.empty()) {
        continue;
      }
      // Ensure there are no labels between now and when the `method-set!` is actually called
      bool was_another_label = false;
      for (int j = i; j < (int)top_level.ir2.atomic_ops->ops.size(); j++) {
        const auto& temp_aop = top_level.ir2.atomic_ops->ops.at(j);
        const std::string temp_as_str = temp_aop.get()->to_string(top_level.ir2.env);
        if (temp_as_str.find("call!") != std::string::npos) {
          break;
        }
        auto temp_label_match =
            get_regex_match(temp_as_str, std::regex("\\(set!\\s[^\\s]*\\s(L.*)\\)"));
        if (!temp_label_match.empty()) {
          was_another_label = true;
          break;
        }
      }
      if (!was_another_label) {
        objectFile.state_methods[type_match][method_id] = state_name;
      }
    }
  }

  // Check for types
  // if there's no inspect method, we can use just use the call to the type's new method
  // to find the type
  for (int i = 0; i < ((int)top_level.ir2.atomic_ops->ops.size()) - 5; i++) {
    // lw v1, type(s7)           ;; [ 20] (set! v1-10 type) [] -> [v1: <the etype type> ]
    const auto& aop_0 = top_level.ir2.atomic_ops->ops.at(i);
    if (!is_set_reg_to_symbol_value(aop_0.get(), {}, "type")) {
      continue;
    }

    // lwu t9, 16(v1)            ;; [ 21] (set! t9-0 (l.wu (+ v1-10 16)))
    //                           ;; [v1: <the etype type> ] -> [t9: (function symbol type int
    //                           type)
    const auto& aop_1 = top_level.ir2.atomic_ops->ops.at(i + 1);
    if (!is_set_reg_to_load(aop_1.get(), Register(Reg::GPR, Reg::T9), 16)) {
      continue;
    }

    // daddiu a0, s7, float-type ;; [ 22] (set! a0-0 'float-type) [] -> [a0: symbol ]
    const auto& aop_2 = top_level.ir2.atomic_ops->ops.at(i + 2);
    auto type_name = get_set_reg_to_symbol_ptr(aop_2.get(), Register(Reg::GPR, Reg::A0));
    if (!type_name) {
      continue;
    }

    // lw a1, uint32(s7)         ;; [ 23] (set! a1-0 uint32) [] -> [a1: <the etype uint32> ]
    const auto& aop_3 = top_level.ir2.atomic_ops->ops.at(i + 3);
    auto parent_name = get_set_reg_to_symbol_value(aop_3.get(), Register(Reg::GPR, Reg::A1));
    if (!parent_name) {
      continue;
    }

    // ld a2, L117(fp)           ;; [ 24] (set! a2-0 (l.d L117)) [] -> [a2: uint ]
    const auto& aop_4 = top_level.ir2.atomic_ops->ops.at(i + 4);
    auto flags = get_set_reg_to_u64_load(aop_4.get(), Register(Reg::GPR, Reg::A2), file);
    if (!flags) {
      // far label load
      // lui v1, L1352           ;; [ 24] (set! v1-10 L1352)
      // ori v1, v1, L1352
      // addu v1, fp, v1
      // ld a2, 0(v1)            ;; [ 25] (set! a2-0 (l.d v1-10))
      flags = get_set_reg_to_lui(aop_4.get(), Register(Reg::GPR, Reg::V1), file);
      if (!flags) {
        continue;
      }
    }

    // jalr ra, t9               ;; [ 25] (call! a0-0 a1-0 a2-0)
    const auto& aop_5 = top_level.ir2.atomic_ops->ops.at(i + 5);
    if (!dynamic_cast<CallOp*>(aop_5.get())) {
      // far labels
      const auto& aop_6 = top_level.ir2.atomic_ops->ops.at(i + 6);
      if (!dynamic_cast<CallOp*>(aop_6.get())) {
        continue;
      }
    }

    if (objectFile.type_info.count(*type_name) == 0) {
      objectFile.type_names_in_order.push_back(*type_name);
    }
    auto& info = objectFile.type_info[*type_name];
    if (!info.from_inspect_method) {
      // no inspect method! generate a deftype.
      info.type_definition = fmt::format(
          ";; (deftype {} ({})\n"
          ";;   ()\n"
          ";;   :flag-assert #x{:x}\n"
          ";;   )\n",
          *type_name, *parent_name, *flags);
    }
    info.parent = *parent_name;
    info.flags = *flags;
  }
}

std::string inspect_top_level_symbol_defines(Function& top_level,
                                             LinkedObjectFile& /*file*/,
                                             DecompilerTypeSystem& dts,
                                             DecompilerTypeSystem& previous_game_ts,
                                             ObjectFileDB::PerObjectAllTypeInfo& object_file_meta) {
  if (!top_level.ir2.atomic_ops) {
    return {};
  }
  std::string result;
  for (auto& aop : top_level.ir2.atomic_ops->ops) {
    auto* as_store = dynamic_cast<StoreOp*>(aop.get());
    if (as_store && as_store->addr().kind() == SimpleExpression::Kind::IDENTITY &&
        as_store->addr().get_arg(0).is_sym_val()) {
      auto& sym_name = as_store->addr().get_arg(0).get_str();
      const auto sym_already_seen = object_file_meta.already_seen_symbols.find(sym_name) !=
                                    object_file_meta.already_seen_symbols.end();
      if (!sym_already_seen) {
        object_file_meta.already_seen_symbols.insert(sym_name);
        if (dts.ts.partially_defined_type_exists(sym_name)) {
          continue;
        }
        std::string type_name = "object";
        // Look to see if we know the type name
        if (object_file_meta.symbol_types.count(sym_name) != 0) {
          type_name = object_file_meta.symbol_types.at(sym_name);
        }
        result += fmt::format(";; (define-extern {} {})", sym_name, type_name);
        auto it = previous_game_ts.symbol_types.find(sym_name);
        if (it != previous_game_ts.symbol_types.end()) {
          result += fmt::format(" ;; {}", it->second.print());
        }
        result += '\n';
      }
    }
  }
  return result;
}
}  // namespace decompiler
