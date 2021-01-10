#include "third-party/fmt/core.h"
#include "AtomicOp.h"

namespace decompiler {
std::string AtomicOp::reg_type_info_as_string(const TypeState& init_types, const TypeState& end_types) const {
  std::string result;

  auto read_mask = regs_to_gpr_mask(m_read_regs);
  auto write_mask = regs_to_gpr_mask(m_write_regs);
  auto clobber_mask = regs_to_gpr_mask(m_clobber_regs);

  result += fmt::format("[{}] -> [{}]", init_types.print_gpr_masked(read_mask),
                        end_types.print_gpr_masked(write_mask));

  if (clobber_mask) {
    result += "cl: ";
    for (auto& reg : m_clobber_regs) {
      result += reg.to_string();
      result += ' ';
    }
  }

  return result;
}
}