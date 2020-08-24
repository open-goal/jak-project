#include "TypeSpec.h"
#include "Type.h"

TypeSpec::TypeSpec(std::string type) : m_type(std::move(type)) {}

TypeSpec::TypeSpec(std::string type, std::vector<TypeSpec> arguments)
    : m_type(std::move(type)), m_arguments(std::move(arguments)) {}

std::string TypeSpec::print() const {
  if (m_arguments.empty()) {
    return m_type;
  } else {
    std::string result = "(" + m_type;
    for (auto& x : m_arguments) {
      result += " " + x.print();
    }
    return result + ")";
  }
}

bool TypeSpec::operator!=(const TypeSpec& other) const {
  return !(other == *this);
}

bool TypeSpec::operator==(const TypeSpec& other) const {
  if (other.m_type != m_type || other.m_arguments.size() != m_arguments.size()) {
    return false;
  }

  for (size_t i = 0; i < m_arguments.size(); i++) {
    if (other.m_arguments[i] != m_arguments[i]) {
      return false;
    }
  }

  return true;
}