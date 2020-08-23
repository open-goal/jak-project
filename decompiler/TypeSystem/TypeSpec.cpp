#include "TypeSpec.h"

std::string TypeSpec::to_string() const {
  if (m_args.empty()) {
    return m_base_type;
  } else {
    std::string result = "(";
    result += m_base_type;
    for (const auto& x : m_args) {
      result += " ";
      result += x.to_string();
    }
    result += ")";
    return result;
  }
}

std::shared_ptr<Form> TypeSpec::to_form() const {
  if (m_args.empty()) {
    return toForm(m_base_type);
  } else {
    std::vector<std::shared_ptr<Form>> all;
    all.push_back(toForm(m_base_type));
    for (const auto& x : m_args) {
      all.push_back(x.to_form());
    }
    return buildList(all);
  }
}

bool TypeSpec::operator==(const TypeSpec& other) const {
  if (m_base_type != other.m_base_type) {
    return false;
  }

  if (m_args.size() != other.m_args.size()) {
    return false;
  }

  for (size_t i = 0; i < m_args.size(); i++) {
    if (m_args[i] != other.m_args[i]) {
      return false;
    }
  }

  return true;
}

bool TypeSpec::operator!=(const TypeSpec& other) const {
  return !(*this == other);
}
