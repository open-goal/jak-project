#include "config_parsers.h"

#include "common/util/json_util.h"

namespace decompiler {
std::vector<StackStructureHint> parse_stack_structure_hints(const nlohmann::json& json) {
  std::vector<StackStructureHint> result;
  for (auto& stack_structure : json) {
    StackStructureHint hint;
    hint.stack_offset = stack_structure.at(0).get<int>();
    auto& type_info = stack_structure.at(1);
    if (type_info.is_array()) {
      auto container_type = type_info.at(0).get<std::string>();
      if (container_type == "array") {
        hint.container_type = StackStructureHint::ContainerType::ARRAY;
      } else if (container_type == "inline-array") {
        hint.container_type = StackStructureHint::ContainerType::INLINE_ARRAY;
      } else {
        throw std::runtime_error("Container type is invalid: " + container_type);
      }
      hint.element_type = type_info.at(1).get<std::string>();
      hint.container_size = type_info.at(2).get<int>();
    } else if (type_info.is_string()) {
      hint.container_type = StackStructureHint::ContainerType::NONE;
      hint.container_size = -1;
      hint.element_type = type_info.get<std::string>();
    } else {
      throw std::runtime_error("Invalid stack var override");
    }
    result.push_back(hint);
  }
  return result;
}

std::unordered_map<int, std::vector<decompiler::RegisterTypeCast>> parse_cast_hints(
    const nlohmann::json& casts) {
  std::unordered_map<int, std::vector<decompiler::RegisterTypeCast>> out;

  for (auto& cast : casts) {
    auto idx_range = parse_json_optional_integer_range(cast.at(0));
    for (auto idx : idx_range) {
      RegisterTypeCast type_cast;
      type_cast.atomic_op_idx = idx;
      type_cast.reg = Register(cast.at(1).get<std::string>());
      type_cast.type_name = cast.at(2).get<std::string>();
      out[idx].push_back(type_cast);
    }
  }

  return out;
}

}  // namespace decompiler