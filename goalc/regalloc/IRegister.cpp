#include "third-party/fmt/core.h"
#include "IRegister.h"

std::string IRegister::to_string() const {
  //  if (with_constraints) {
  //    std::string result = fmt::format("i{}-{}\n", emitter::to_string(kind), id);
  //    for (const auto& x : constraints) {
  //      result += fmt::format("  [{:3d] in {}\n", x.instr_idx,
  //                            emitter::gRegInfo.get_info(x.desired_register).name);
  //    }
  //    return result;
  //  } else {
  return fmt::format("i{}-{}", emitter::to_string(kind), id);
  //  }
}

std::string IRegConstraint::to_string() const {
  if (contrain_everywhere) {
    return fmt::format("[all] {} in {}", ireg.to_string(), desired_register.print());
  } else {
    return fmt::format("[{:3d}] {} in {}", instr_idx, ireg.to_string(), desired_register.print());
  }
}