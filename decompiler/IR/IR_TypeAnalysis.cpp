#include "IR.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "third-party/fmt/core.h"

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
    case IR_Set::REG_64: {
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