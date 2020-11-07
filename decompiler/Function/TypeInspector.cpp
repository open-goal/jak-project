#include "decompiler/config.h"
#include "decompiler/Disasm/InstructionMatching.h"
#include "TypeInspector.h"
#include "Function.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "third-party/fmt/core.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "common/type_system/deftype.h"
#include "decompiler/IR/IR.h"

namespace {
struct FieldPrint {
  char format = '\0';
  std::string field_name;
  std::string field_type_name;
  bool has_array = false;
  int array_size = -1;
};

FieldPrint get_field_print(const std::string& str) {
  int idx = 0;
  auto next = [&]() { return str.at(idx++); };

  auto peek = [&](int off) { return str.at(idx + off); };

  FieldPrint field_print;

  // first is ~T
  char c0 = next();
  assert(c0 == '~');
  char c1 = next();
  assert(c1 == 'T');

  // next the name:
  char name_char = next();
  while (name_char != ':' && name_char != '[') {
    field_print.field_name.push_back(name_char);
    name_char = next();
  }

  // possibly array thing
  if (name_char == '[') {
    int size = 0;
    char num_char = next();
    while (num_char >= '0' && num_char <= '9') {
      size = size * 10 + (num_char - '0');
      num_char = next();
    }
    field_print.has_array = true;
    field_print.array_size = size;

    assert(num_char == ']');
    char c = next();
    assert(c == ' ');
    c = next();
    assert(c == '@');
    c = next();
    assert(c == ' ');
    c = next();
    assert(c == '#');
    c = next();
    assert(c == 'x');
  } else {
    // next a space
    char space_char = next();
    assert(space_char == ' ');
  }

  // next the format
  char fmt1 = next();
  if (fmt1 == '~' && peek(0) != '`') {  // normal ~_~%
    char fmt_code = next();
    field_print.format = fmt_code;
    char end1 = next();
    assert(end1 == '~');
    char end2 = next();
    assert(end2 == '%');
    assert(idx == (int)str.size());
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
      assert(i == c);
    }
    field_print.format = 'X';

    assert(idx == (int)str.size());
  } else if (fmt1 == '#' && peek(0) == 'x') {  // #x~X~%
    next();
    std::string expect_end = "~X~%";
    for (char i : expect_end) {
      char c = next();
      assert(i == c);
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
      assert(i == c);
    }
    field_print.format = 'P';

    assert(idx == (int)str.size());
  } else if (str.substr(idx - 1) == "(meters ~m)~%") {
    field_print.format = 'm';
  } else if (str.substr(idx - 1) == "(deg ~r)~%") {
    field_print.format = 'r';
  } else if (str.substr(idx - 1) == "(seconds ~e)~%") {
    field_print.format = 'e';
  }

  else {
    throw std::runtime_error("other format nyi in get_field_print " + str.substr(idx));
  }

  return field_print;
}

bool is_int(IR* ir, s64 value) {
  auto as_int = dynamic_cast<IR_IntegerConstant*>(ir);
  return as_int && as_int->value == value;
}

bool is_reg(IR* ir, Register reg) {
  auto as_reg = dynamic_cast<IR_Register*>(ir);
  return as_reg && as_reg->reg == reg;
}

bool is_math_reg_constant(IR* ir, IR_IntMath2::Kind kind, Register src0, s64 src1) {
  auto as_math = dynamic_cast<IR_IntMath2*>(ir);
  return as_math && as_math->kind == kind && is_reg(as_math->arg0.get(), src0) &&
         is_int(as_math->arg1.get(), src1);
}

bool is_load_with_offset(IR* ir, IR_Load::Kind kind, int load_size, Register base, s64 offset) {
  auto as_load = dynamic_cast<IR_Load*>(ir);
  return as_load && as_load->kind == kind && as_load->size == load_size &&
         is_math_reg_constant(as_load->location.get(), IR_IntMath2::ADD, base, offset);
}

bool is_get_load_with_offset(IR* ir,
                             Register dst,
                             IR_Load::Kind kind,
                             int load_size,
                             Register base,
                             s64 offset) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  return as_set && is_reg(as_set->dst.get(), dst) &&
         is_load_with_offset(as_set->src.get(), kind, load_size, base, offset);
}

struct LoadInfo {
  int offset = 0;
  int size = 0;
  IR_Load::Kind kind;
};

LoadInfo get_load_info_from_set(IR* load) {
  auto as_set = dynamic_cast<IR_Set*>(load);
  assert(as_set);
  auto as_load = dynamic_cast<IR_Load*>(as_set->src.get());
  assert(as_load);
  LoadInfo info;
  info.kind = as_load->kind;
  info.size = as_load->size;
  if (dynamic_cast<IR_Register*>(as_load->location.get())) {
    info.offset = 0;
    return info;
  }

  auto as_math = dynamic_cast<IR_IntMath2*>(as_load->location.get());
  assert(as_math);
  assert(as_math->kind == IR_IntMath2::ADD);
  auto as_int = dynamic_cast<IR_IntegerConstant*>(as_math->arg1.get());
  assert(as_int);
  info.offset = as_int->value;
  return info;
}

Register get_base_of_load(IR_Load* load) {
  auto as_reg = dynamic_cast<IR_Register*>(load->location.get());
  if (as_reg) {
    return as_reg->reg;
  }

  auto as_math = dynamic_cast<IR_IntMath2*>(load->location.get());
  assert(as_math->kind == IR_IntMath2::ADD);
  assert(dynamic_cast<IR_IntegerConstant*>(as_math->arg1.get()));
  auto math_reg = dynamic_cast<IR_Register*>(as_math->arg0.get());
  if (math_reg) {
    return math_reg->reg;
  } else {
    assert(false);
  }
  return {};
}

bool is_load_with_base(IR* ir, Register base) {
  auto as_load = dynamic_cast<IR_Load*>(ir);
  return as_load && base == get_base_of_load(as_load);
}

bool is_get_load(IR* ir, Register dst, Register base) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  return as_set && is_reg(as_set->dst.get(), dst) && is_load_with_base(as_set->src.get(), base);
}

bool is_reg_reg_move(IR* ir, Register dst, Register src) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  return as_set && is_reg(as_set->dst.get(), dst) && is_reg(as_set->src.get(), src);
}

bool is_sym_value(IR* ir, const std::string& sym_name) {
  auto as_sym_value = dynamic_cast<IR_SymbolValue*>(ir);
  return as_sym_value && as_sym_value->name == sym_name;
}

bool is_sym(IR* ir, const std::string& sym_name) {
  auto as_sym = dynamic_cast<IR_Symbol*>(ir);
  return as_sym && as_sym->name == sym_name;
}

bool is_get_sym_value(IR* ir, Register dst, const std::string& sym_name) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  return as_set && is_reg(as_set->dst.get(), dst) && is_sym_value(as_set->src.get(), sym_name);
}

bool is_get_sym(IR* ir, Register dst, const std::string& sym_name) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  return as_set && is_reg(as_set->dst.get(), dst) && is_sym(as_set->src.get(), sym_name);
}

bool is_label(IR* ir) {
  return dynamic_cast<IR_StaticAddress*>(ir);
}

bool is_get_label(IR* ir, Register dst) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  return as_set && is_reg(as_set->dst.get(), dst) && is_label(as_set->src.get());
}

int get_label_id_of_set(IR* ir) {
  return dynamic_cast<IR_StaticAddress*>(dynamic_cast<IR_Set*>(ir)->src.get())->label_id;
}

bool is_set_shift(IR* ir) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  if (as_set) {
    auto as_math = dynamic_cast<IR_IntMath2*>(as_set->src.get());
    if (as_math && (as_math->kind == IR_IntMath2::LEFT_SHIFT ||
                    as_math->kind == IR_IntMath2::RIGHT_SHIFT_LOGIC ||
                    as_math->kind == IR_IntMath2::RIGHT_SHIFT_ARITH)) {
      return true;
    }
  }

  auto as_asm = dynamic_cast<IR_AsmOp*>(ir);
  return as_asm && as_asm->name == "sllv";
}

bool get_ptr_offset_constant_nonzero(IR_IntMath2* math, Register base, int* result) {
  if (!is_reg(math->arg0.get(), base)) {
    return false;
  }

  auto as_int = dynamic_cast<IR_IntegerConstant*>(math->arg1.get());
  if (!as_int) {
    return false;
  }

  *result = as_int->value;
  return true;
}

bool get_ptr_offset_zero(IR_IntMath2* math, Register base, int* result) {
  if (!is_reg(math->arg0.get(), make_gpr(Reg::R0)) || !is_reg(math->arg1.get(), base)) {
    return false;
  }
  *result = 0;
  return true;
}

bool get_ptr_offset(IR* ir, Register dst, Register base, int* result) {
  auto as_set = dynamic_cast<IR_Set*>(ir);
  if (!as_set) {
    return false;
  }

  if (!is_reg(as_set->dst.get(), dst)) {
    return false;
  }

  auto as_math = dynamic_cast<IR_IntMath2*>(as_set->src.get());
  if (!as_math) {
    return false;
  }
  return get_ptr_offset_constant_nonzero(as_math, base, result) ||
         get_ptr_offset_zero(as_math, base, result);
}

int get_start_idx(Function& function,
                  LinkedObjectFile& file,
                  TypeInspectorResult* result,
                  const std::string& parent_type) {
  if (function.basic_blocks.size() > 1) {
    result->warnings += " too many basic blocks";
    return 0;
  }

  /*
   ;; for a basic
    or gp, a0, r0             ;; (set! gp a0)
    lw t9, format(s7)         ;; (set! t9 format)
    daddiu a0, s7, #t         ;; (set! a0 '#t)
    daddiu a1, fp, L362       ;; (set! a1 L362) "[~8x] ~A~%"
    or a2, gp, r0             ;; (set! a2 gp)
    lwu a3, -4(gp)            ;; (set! a3 (l.wu (+.i gp -4)))
    jalr ra, t9               ;; (call!)
    sll v0, ra, 0
   ;; for a struct
    or gp, a0, r0             ;; (set! gp a0)
    lw t9, format(s7)         ;; (set! t9 format)
    daddiu a0, s7, #t         ;; (set! a0 '#t)
    daddiu a1, fp, L79        ;; (set! a1 L79) "[~8x] ~A~%"
    or a2, gp, r0             ;; (set! a2 gp)
    daddiu a3, s7, dead-pool-heap-rec;; (set! a3 'dead-pool-heap-rec)
    jalr ra, t9               ;; (call!)
   */

  // check size
  if (function.basic_ops.size() < 7) {
    result->warnings += " not enough basic ops";
    return 0;
  }

  auto& move_op = function.basic_ops.at(0);
  if (!is_reg_reg_move(move_op.get(), make_gpr(Reg::GP), make_gpr(Reg::A0))) {
    result->warnings += "bad first move";
    return 0;
  }

  auto& get_format_op = function.basic_ops.at(1);

  if (is_get_sym_value(get_format_op.get(), make_gpr(Reg::T9), "format")) {
    auto& get_true = function.basic_ops.at(2);
    if (!is_get_sym(get_true.get(), make_gpr(Reg::A0), "#t")) {
      result->warnings += "bad get true";
      return 0;
    }

    auto& get_str = function.basic_ops.at(3);
    if (!is_get_label(get_str.get(), make_gpr(Reg::A1))) {
      result->warnings += "bad get label";
      return 0;
    }

    auto str = file.get_goal_string_by_label(file.labels.at(get_label_id_of_set(get_str.get())));
    if (str != "[~8x] ~A~%") {
      result->warnings += "bad type dec string: " + str;
      return 0;
    }

    auto& move2_op = function.basic_ops.at(4);
    if (!is_reg_reg_move(move2_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP))) {
      result->warnings += "bad second move";
      return 0;
    }

    auto& load_op = function.basic_ops.at(5);
    bool is_basic_load = is_get_load_with_offset(load_op.get(), make_gpr(Reg::A3),
                                                 IR_Load::UNSIGNED, 4, make_gpr(Reg::GP), -4);
    result->is_basic = is_basic_load;

    bool is_struct_load = is_get_sym(load_op.get(), make_gpr(Reg::A3), function.method_of_type);

    if (!is_basic_load && !is_struct_load) {
      result->warnings += "bad load";
      return 0;
    }

    auto& call = function.basic_ops.at(6);
    if (!dynamic_cast<IR_Call*>(call.get())) {
      result->warnings += "bad call";
      return 0;
    }

    // okay!
    return 7;
  } else {
    if (is_get_sym_value(get_format_op.get(), make_gpr(Reg::V1), parent_type)) {
      // now get the inspect method.
      auto& get_method_op = function.basic_ops.at(2);
      if (!is_get_load_with_offset(get_method_op.get(), make_gpr(Reg::T9), IR_Load::UNSIGNED, 4,
                                   make_gpr(Reg::V1), 28)) {
        result->warnings += "bad get method op " + get_method_op->print(file);
        return 0;
      }

      auto& move2_op = function.basic_ops.at(3);
      if (!is_reg_reg_move(move2_op.get(), make_gpr(Reg::A0), make_gpr(Reg::GP))) {
        result->warnings += "bad move2 op " + move2_op->print(file);
        return 0;
      }

      auto& call_op = function.basic_ops.at(4);
      if (!dynamic_cast<IR_Call*>(call_op.get())) {
        result->warnings += "bad call op " + call_op->print(file);
        return 0;
      }

      result->warnings += "inherited inpspect of " + parent_type;
      result->is_basic = true;
      return 5;

    } else {
      result->warnings +=
          "unrecognized get op: " + get_format_op->print(file) + " parent was " + parent_type;
      return 0;
    }
  }
}

int identify_basic_field(int idx,
                         Function& function,
                         LinkedObjectFile& file,
                         TypeInspectorResult* result,
                         FieldPrint& print_info) {
  (void)file;
  auto load_info = get_load_info_from_set(function.basic_ops.at(idx++).get());
  assert(load_info.size == 4);
  assert(load_info.kind == IR_Load::UNSIGNED || load_info.kind == IR_Load::SIGNED);

  if (load_info.kind == IR_Load::SIGNED) {
    result->warnings += "field " + print_info.field_name + " is a basic loaded with a signed load ";
  }

  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec("basic"), offset);
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_pointer_field(int idx,
                           Function& function,
                           LinkedObjectFile& file,
                           TypeInspectorResult* result,
                           FieldPrint& print_info) {
  (void)file;
  auto load_info = get_load_info_from_set(function.basic_ops.at(idx++).get());
  assert(load_info.size == 4);
  assert(load_info.kind == IR_Load::UNSIGNED);

  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec("pointer"), offset);
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_array_field(int idx,
                         Function& function,
                         LinkedObjectFile& file,
                         TypeInspectorResult* result,
                         FieldPrint& print_info) {
  auto& get_op = function.basic_ops.at(idx++);
  int offset = 0;
  if (!get_ptr_offset(get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP), &offset)) {
    printf("bad get ptr offset %s\n", get_op->print(file).c_str());
    assert(false);
  }
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec("UNKNOWN"), offset);
  if (print_info.array_size) {
    field.set_array(print_info.array_size);
  } else {
    field.set_dynamic();
  }
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_float_field(int idx,
                         Function& function,
                         LinkedObjectFile& file,
                         TypeInspectorResult* result,
                         FieldPrint& print_info) {
  auto load_info = get_load_info_from_set(function.basic_ops.at(idx++).get());
  assert(load_info.size == 4);
  assert(load_info.kind == IR_Load::FLOAT);

  auto& float_move = function.basic_ops.at(idx++);
  if (!is_reg_reg_move(float_move.get(), make_gpr(Reg::A2), make_fpr(0))) {
    printf("bad float move: %s\n", float_move->print(file).c_str());
    assert(false);
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
      assert(false);
  }
  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec(type), offset);
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_struct_not_inline_field(int idx,
                                     Function& function,
                                     LinkedObjectFile& file,
                                     TypeInspectorResult* result,
                                     FieldPrint& print_info) {
  (void)file;
  auto load_info = get_load_info_from_set(function.basic_ops.at(idx++).get());

  if (!(load_info.size == 4 && load_info.kind == IR_Load::UNSIGNED)) {
    result->warnings += "field " + print_info.field_type_name + " is likely a value type";
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
                                 LinkedObjectFile& file,
                                 TypeInspectorResult* result,
                                 FieldPrint& print_info) {
  auto& get_op = function.basic_ops.at(idx++);
  int offset = 0;
  if (!get_ptr_offset(get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP), &offset)) {
    printf("bad get ptr offset %s\n", get_op->print(file).c_str());
    assert(false);
  }
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec(print_info.field_type_name), offset);
  field.set_inline();
  result->fields_of_type.push_back(field);
  return idx;
}

int identify_int_field(int idx,
                       Function& function,
                       LinkedObjectFile& file,
                       TypeInspectorResult* result,
                       FieldPrint& print_info) {
  (void)file;
  auto load_info = get_load_info_from_set(function.basic_ops.at(idx++).get());

  std::string field_type_name;
  if (load_info.kind == IR_Load::UNSIGNED) {
    field_type_name += "u";
  } else if (load_info.kind == IR_Load::FLOAT) {
    assert(false);  // ...
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
    switch (load_info.kind) {
      case IR_Load::SIGNED:
        field_type_name = "sseconds";
        break;
      case IR_Load::UNSIGNED:
        field_type_name = "useconds";
        break;
      default:
        assert(false);
    }
    assert(load_info.size == 8);
  }

  int offset = load_info.offset;
  if (result->is_basic) {
    offset += BASIC_OFFSET;
  }

  Field field(print_info.field_name, TypeSpec(field_type_name), offset);
  result->fields_of_type.push_back(field);

  return idx;
}

int detect(int idx, Function& function, LinkedObjectFile& file, TypeInspectorResult* result) {
  auto& get_format_op = function.basic_ops.at(idx++);
  if (!is_get_sym_value(get_format_op.get(), make_gpr(Reg::T9), "format")) {
    printf("bad get format");
    assert(false);
  }

  auto& get_true = function.basic_ops.at(idx++);
  if (!is_get_sym(get_true.get(), make_gpr(Reg::A0), "#t")) {
    printf("bad get true");
    assert(false);
  }

  auto& get_str = function.basic_ops.at(idx++);
  if (!is_get_label(get_str.get(), make_gpr(Reg::A1))) {
    result->warnings += "bad get label";
    return true;
  }

  auto str = file.get_goal_string_by_label(file.labels.at(get_label_id_of_set(get_str.get())));
  auto info = get_field_print(str);

  auto& first_get_op = function.basic_ops.at(idx);

  if (is_get_load(first_get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP)) &&
      (info.format == 'D' || info.format == 'X' || info.format == 'e') && !info.has_array &&
      info.field_type_name.empty()) {
    idx = identify_int_field(idx, function, file, result, info);
    // it's a load!
  } else if (is_get_load(first_get_op.get(), make_fpr(0), make_gpr(Reg::GP)) &&
             (info.format == 'f' || info.format == 'm' || info.format == 'r' ||
              info.format == 'X') &&
             !info.has_array && info.field_type_name.empty()) {
    idx = identify_float_field(idx, function, file, result, info);
  } else if (is_get_load(first_get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP)) &&
             info.format == 'A' && !info.has_array && info.field_type_name.empty()) {
    idx = identify_basic_field(idx, function, file, result, info);
  } else if (is_get_load(first_get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP)) &&
             info.format == 'X' && !info.has_array && info.field_type_name.empty()) {
    idx = identify_pointer_field(idx, function, file, result, info);
  } else if (info.has_array && (info.format == 'X' || info.format == 'P') &&
             info.field_type_name.empty()) {
    idx = identify_array_field(idx, function, file, result, info);
  } else if (!info.has_array && (info.format == 'X' || info.format == 'P') &&
             !info.field_type_name.empty()) {
    // structure.
    if (is_get_load(first_get_op.get(), make_gpr(Reg::A2), make_gpr(Reg::GP))) {
      // not inline
      idx = identify_struct_not_inline_field(idx, function, file, result, info);
    } else {
      idx = identify_struct_inline_field(idx, function, file, result, info);
    }
  }

  else if (is_set_shift(first_get_op.get())) {
    result->warnings += "likely a bitfield type";
    return -1;
  } else {
    printf("couldn't do %s, %s\n", str.c_str(), first_get_op->print(file).c_str());
    return -1;
  }

  auto& call_op = function.basic_ops.at(idx++);
  if (!dynamic_cast<IR_Call*>(call_op.get())) {
    printf("bad call\n");
    assert(false);
  }

  return idx;
}
}  // namespace

TypeInspectorResult inspect_inspect_method(Function& inspect,
                                           const std::string& type_name,
                                           DecompilerTypeSystem& dts,
                                           LinkedObjectFile& file) {
  TypeInspectorResult result;
  TypeFlags flags;
  flags.flag = 0;
  dts.lookup_flags(type_name, &flags.flag);
  result.type_name = type_name;
  result.parent_type_name = dts.lookup_parent_from_inspects(type_name);
  result.flags = flags.flag;
  result.type_size = flags.size;
  result.type_method_count = flags.methods;
  result.type_heap_base = flags.heap_base;
  assert(flags.pad == 0);

  auto& bad_set = get_config().bad_inspect_types;
  int idx = get_start_idx(inspect, file, &result, result.parent_type_name);
  if (idx == 0 || bad_set.find(type_name) != bad_set.end()) {
    // printf("was weird: %s\n", result.warnings.c_str());
    return result;
  }
  while (idx < int(inspect.basic_ops.size()) - 1 && idx != -1) {
    idx = detect(idx, inspect, file, &result);
  }

  // todo, continue to identify fields, then identify the return.

  result.success = true;
  return result;
}

std::string TypeInspectorResult::print_as_deftype() {
  std::string result;

  result += fmt::format("(deftype {} ({})\n  (", type_name, parent_type_name);

  int longest_field_name = 0;
  int longest_type_name = 0;
  int longest_mods = 0;

  std::string inline_string = ":inline";
  std::string dynamic_string = ":dynamic";

  for (auto& field : fields_of_type) {
    longest_field_name = std::max(longest_field_name, int(field.name().size()));
    longest_type_name = std::max(longest_type_name, int(field.type().print().size()));

    int mods = 0;
    // mods are array size, :inline, :dynamic
    if (field.is_array() && !field.is_dynamic()) {
      mods += std::to_string(field.array_size()).size();
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

  for (auto& field : fields_of_type) {
    result += "(";
    result += field.name();
    result.append(1 + (longest_field_name - int(field.name().size())), ' ');
    result += field.type().print();
    result.append(1 + (longest_type_name - int(field.type().print().size())), ' ');

    std::string mods;
    if (field.is_array() && !field.is_dynamic()) {
      mods += std::to_string(field.array_size());
      mods += " ";
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
    result.append(")\n   ");
  }
  result.append(")\n");

  result.append(fmt::format("  :method-count-assert {}\n", type_method_count));
  result.append(fmt::format("  :size-assert         #x{:x}\n", type_size));
  result.append(fmt::format("  :flag-assert         #x{:x}\n  ", flags));
  if (!warnings.empty()) {
    result.append(";; ");
    result.append(warnings);
    result.append("\n  ");
  }

  if (type_method_count > 9) {
    result.append("(:methods\n    ");
    for (int i = 9; i < type_method_count; i++) {
      result.append(fmt::format("(dummy-{} () none {})\n    ", i, i));
    }
    result.append(")\n  ");
  }
  result.append(")\n");

  return result;
}