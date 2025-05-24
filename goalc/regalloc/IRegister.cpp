#include "IRegister.h"

#include "common/util/Assert.h"

#include "fmt/core.h"

std::string IRegister::to_string() const {
  //  if (with_constraints) {
  //    std::string result = fmt::format("i{}-{}\n", emitter::to_string(kind), id);
  //    for (const auto& x : constraints) {
  //      result += fmt::format("  [{:3d] in {}\n", x.instr_idx,
  //                            emitter::gRegInfo.get_info(x.desired_register).name);
  //    }
  //    return result;
  //  } else {
  switch (reg_class) {
    case RegClass::GPR_64:
      return fmt::format("igpr-{}", id);
    case RegClass::FLOAT:
      return fmt::format("ifpr-{}", id);
    case RegClass::INT_128:
      return fmt::format("ii128-{}", id);
    case RegClass::VECTOR_FLOAT:
      return fmt::format("ivf-{}", id);
    default:
      ASSERT(false);
      return {};
  }
}

std::string IRegConstraint::to_string() const {
  if (contrain_everywhere) {
    return fmt::format("[all] {} in {}", ireg.to_string(), desired_register.print());
  } else {
    return fmt::format("[{:3d}] {} in {}", instr_idx, ireg.to_string(), desired_register.print());
  }
}
