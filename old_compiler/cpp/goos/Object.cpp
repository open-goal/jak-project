#include "Object.h"

// EmptyListObject* gEmptyList = nullptr;
std::shared_ptr<EmptyListObject> gEmptyList = nullptr;

template <>
Object Object::make_number(double value) {
  return Object::make_float(value);
}

template <>
Object Object::make_number(int64_t value) {
  return Object::make_integer(value);
}

template <>
Object Object::make_number(char value) {
  return Object::make_char(value);
}