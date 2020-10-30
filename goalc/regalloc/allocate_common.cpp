#include "third-party/fmt/core.h"
#include "allocate_common.h"

std::string StackOp::print() const {
  std::string result;
  bool added = false;
  for (const auto& op : ops) {
    if (op.load) {
      result += fmt::format("{} <- [{:2d}], ", emitter::gRegInfo.get_info(op.reg).name, op.slot);
      added = true;
    }
    if (op.store) {
      result += fmt::format("{} -> [{:2d}], ", emitter::gRegInfo.get_info(op.reg).name, op.slot);
      added = true;
    }
  }

  if (added) {
    result.pop_back();
    result.pop_back();
  }

  return result;
}

std::string Assignment::to_string() const {
  std::string result;
  if (spilled) {
    result += "*";
  }
  switch (kind) {
    case Kind::STACK:
      result += fmt::format("s[{:2d}]", stack_slot);
      break;
    case Kind::REGISTER:
      result += emitter::gRegInfo.get_info(reg).name;
      break;
    case Kind::UNASSIGNED:
      result += "unassigned";
      break;
    default:
      assert(false);
  }

  return result;
}

std::string LiveInfo::print_assignment() {
  std::string result = "Assignment for var " + std::to_string(var) + "\n";
  for (uint32_t i = 0; i < assignment.size(); i++) {
    result += fmt::format("i[{:3d}] {}\n", i + min, assignment.at(i).to_string());
  }
  return result;
}