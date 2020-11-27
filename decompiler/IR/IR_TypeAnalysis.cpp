#include "IR.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "third-party/fmt/core.h"
#include "common/goos/Object.h"

namespace {
bool is_plain_type(const TP_Type& type, const TypeSpec& ts) {
  return type.kind == TP_Type::OBJECT_OF_TYPE && type.ts == ts;
}

bool is_integer_type(const TP_Type& type) {
  return is_plain_type(type, TypeSpec("int")) || is_plain_type(type, TypeSpec("uint"));
}
}  // namespace

void IR_Atomic::propagate_types(const TypeState& input,
                                const LinkedObjectFile& file,
                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  throw std::runtime_error(
      fmt::format("Could not propagate types for {}, not yet implemented", print(file)));
}

TP_Type IR::get_expression_type(const TypeState& input,
                                const LinkedObjectFile& file,
                                DecompilerTypeSystem& dts) {
  (void)input;
  (void)dts;
  throw std::runtime_error(
      fmt::format("Could not get expression types for {}, not yet implemented", print(file)));
}

void IR_Set_Atomic::propagate_types(const TypeState& input,
                                    const LinkedObjectFile& file,
                                    DecompilerTypeSystem& dts) {
  // pass through types
  end_types = input;
  // modify as needed
  switch (kind) {
    case IR_Set::REG_64:
    case IR_Set::LOAD:
    case IR_Set::GPR_TO_FPR:
    case IR_Set::FPR_TO_GPR64:
    case IR_Set::REG_FLT: {
      auto as_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(as_reg);
      end_types.get(as_reg->reg) = src->get_expression_type(input, file, dts);
    } break;
    default:
      throw std::runtime_error(fmt::format(
          "Could not propagate types through IR_Set_Atomic, kind not handled {}", print(file)));
  }
}

TP_Type IR_Register::get_expression_type(const TypeState& input,
                                         const LinkedObjectFile& file,
                                         DecompilerTypeSystem& dts) {
  (void)file;
  (void)dts;
  return input.get(reg);
}

TP_Type IR_Load::get_expression_type(const TypeState& input,
                                     const LinkedObjectFile& file,
                                     DecompilerTypeSystem& dts) {
  (void)input;
  auto as_static = dynamic_cast<IR_StaticAddress*>(location.get());
  if (as_static && kind == FLOAT) {
    // loading static data with a FLOAT kind load (lwc1), assume result is a float.
    return TP_Type(dts.ts.make_typespec("float"));
  }

  throw std::runtime_error(
      fmt::format("Could not get type of load: {}. Not handled.", print(file)));
}

TP_Type IR_FloatMath2::get_expression_type(const TypeState& input,
                                           const LinkedObjectFile& file,
                                           DecompilerTypeSystem& dts) {
  (void)input;
  (void)file;

  // regardless of input types, the output is going to be a float.
  switch (kind) {
    case DIV:
    case MUL:
    case ADD:
    case SUB:
    case MIN:
    case MAX:
      return TP_Type(dts.ts.make_typespec("float"));
    default:
      assert(false);
  }
}

TP_Type IR_IntMath2::get_expression_type(const TypeState& input,
                                         const LinkedObjectFile& file,
                                         DecompilerTypeSystem& dts) {
  auto arg0_type = arg0->get_expression_type(input, file, dts);
  auto arg1_type = arg1->get_expression_type(input, file, dts);

  if (is_integer_type(arg0_type) && is_integer_type(arg1_type)) {
    // case where both arguments are integers.
    // in this case we assume we're actually doing math.
    switch (kind) {
      case ADD:
      case SUB:
        // we don't know if we're signed or unsigned. so let's just go with the first type.
        return TP_Type(arg0_type.ts);
      case MUL_SIGNED:
      case DIV_SIGNED:
        // result is going to be signed, regardless of inputs.
        return TP_Type(TypeSpec("int"));
      default:
        break;
    }
  }

  throw std::runtime_error(
      fmt::format("Can't get_expression_type on this IR_IntMath2: {}", print(file)));
}

void BranchDelay::type_prop(TypeState& output,
                            const LinkedObjectFile& file,
                            DecompilerTypeSystem& dts) {
  (void)dts;
  switch (kind) {
    case DSLLV: {
      // I think this is only used in ash, in which case the output should be an int/uint
      // welll
      auto dst = dynamic_cast<IR_Register*>(destination.get());
      assert(dst);
      auto src = dynamic_cast<IR_Register*>(source.get());
      assert(src);
      if (is_plain_type(output.get(src->reg), TypeSpec("uint"))) {
        // todo, this won't catch child uint types. I think this doesn't matter though.
        output.get(dst->reg) = TP_Type(TypeSpec("uint"));
      }
      output.get(dst->reg) = TP_Type(TypeSpec("int"));
    }

    break;
    default:
      throw std::runtime_error("Unhandled branch delay in type_prop: " + to_form(file).print());
  }
}

void IR_Branch_Atomic::propagate_types(const TypeState& input,
                                       const LinkedObjectFile& file,
                                       DecompilerTypeSystem& dts) {
  // pass through types
  end_types = input;
  branch_delay.type_prop(end_types, file, dts);
  // todo clobbers.
}