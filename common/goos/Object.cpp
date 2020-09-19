#include "Object.h"
#include "common/util/FileUtil.h"

namespace goos {

std::shared_ptr<EmptyListObject> gEmptyList = nullptr;

/*!
 * Convert type to string (name in brackets)
 */
std::string object_type_to_string(ObjectType type) {
  switch (type) {
    case ObjectType::EMPTY_LIST:
      return "[empty list]";
    case ObjectType::INTEGER:
      return "[integer]";
    case ObjectType::FLOAT:
      return "[float]";
    case ObjectType::CHAR:
      return "[char]";
    case ObjectType::SYMBOL:
      return "[symbol]";
    case ObjectType::STRING:
      return "[string]";
    case ObjectType::PAIR:
      return "[pair]";
    case ObjectType::ARRAY:
      return "[array]";
    case ObjectType::LAMBDA:
      return "[lambda]";
    case ObjectType::MACRO:
      return "[macro]";
    case ObjectType::ENVIRONMENT:
      return "[environment]";
    default:
      throw std::runtime_error("unknown object type in object_type_to_string");
  }
}

/*!
 * Special case to print a float with the %g format specifier
 */
template <>
std::string fixed_to_string(FloatType x) {
  char buff[256];
  sprintf(buff, "%.6f", x);
  return {buff};
}

/*!
 * Special case to print a character and escape the weird ones.
 */
template <>
std::string fixed_to_string(char x) {
  char buff[256];
  if (file_util::is_printable_char(x) && x != ' ') {
    // can print directly
    sprintf(buff, "#\\%c", x);
    return {buff};
  }

  // not printable, special case
  switch (x) {
    case '\n':
      sprintf(buff, "#\\\\n");
      break;
    case ' ':
      sprintf(buff, "#\\\\s");
      break;
    case '\t':
      sprintf(buff, "#\\\\t");
      break;

    default:
      sprintf(buff, "#\\{%d}", u8(x));
  }

  return {buff};
}

/*!
 * Create a new symbol object by interning
 */
Object SymbolObject::make_new(SymbolTable& st, const std::string& name) {
  Object obj;
  obj.type = ObjectType::SYMBOL;
  obj.heap_obj = st.intern(name);
  return obj;
}

/*!
 * Build a list of objects from a vector of objects.
 */
Object build_list(const std::vector<Object>& objects) {
  if (objects.empty()) {
    return EmptyListObject::make_new();
  }

  Object empty = EmptyListObject::make_new();
  Object head = PairObject::make_new(objects[0], empty);
  Object last = head;

  for (std::size_t i = 1; i < objects.size(); i++) {
    last.as_pair()->cdr = PairObject::make_new(objects[i], empty);
    last = last.as_pair()->cdr;
  }

  return head;
}

/*!
 * Compare two objects for equality.
 * Does "expensive" checking.
 */
bool Object::operator==(const Object& other) const {
  if (type != other.type)
    return false;

  switch (type) {
    case ObjectType::STRING:
      return as_string()->data == other.as_string()->data;
    case ObjectType::INTEGER:
      return integer_obj == other.integer_obj;
    case ObjectType::FLOAT:
      return float_obj == other.float_obj;
    case ObjectType::CHAR:
      return char_obj == other.char_obj;

    case ObjectType::SYMBOL:
    case ObjectType::ENVIRONMENT:
    case ObjectType::LAMBDA:
    case ObjectType::MACRO:
      return heap_obj == other.heap_obj;

    case ObjectType::EMPTY_LIST:
      return true;
    case ObjectType::PAIR:
      return as_pair()->car == other.as_pair()->car && as_pair()->cdr == other.as_pair()->cdr;
    case ObjectType::ARRAY: {
      auto a = as_array();
      auto b = other.as_array();
      if (a->size() != b->size()) {
        return false;
      }
      for (size_t i = 0; i < a->data.size(); i++) {
        if ((*a)[i] != (*b)[i]) {
          return false;
        }
      }
      return true;
    }

    default:
      throw std::runtime_error("equality not implemented for " + print());
  }
}

template <>
Object Object::make_number(FloatType value) {
  return Object::make_float(value);
}

template <>
Object Object::make_number(IntType value) {
  return Object::make_integer(value);
}

/*!
 * Debug print argument specification.
 */
std::string ArgumentSpec::print() const {
  std::string result = "  unnamed args:\n";
  for (auto& arg : unnamed) {
    result += "    " + arg + "\n";
  }
  result += "  named args:\n";
  for (auto& arg : named) {
    result += "     " + arg.first;
    if (arg.second.has_default) {
      result += " (default " + arg.second.default_value.print() + ")";
    }
    result += "\n";
  }
  if (!rest.empty()) {
    result += "  rest: " + rest + "\n";
  }

  return result;
}

std::string Arguments::print() const {
  std::string result = "  unnamed args:\n";
  for (auto& arg : unnamed) {
    result += "    " + arg.print() + "\n";
  }
  result += "  named args:\n";
  for (auto& arg : named) {
    result += "     " + arg.first + " " + arg.second.print() + "\n";
  }
  if (!rest.empty()) {
    result += "  rest: \n";
    for (auto& x : rest) {
      result += "    " + x.print() + "\n";
    }
  }

  return result;
}

Object Arguments::get_named(const std::string& name, const Object& default_value) {
  Object result = default_value;
  auto kv = named.find(name);
  if (kv != named.end()) {
    result = kv->second;
  }
  return result;
}

Object Arguments::get_named(const std::string& name) {
  return named.at(name);
}

bool Arguments::has_named(const std::string& name) {
  return named.find(name) != named.end();
}

/*!
 * Make an argument specification which accepts any arguments
 */
ArgumentSpec make_varargs() {
  ArgumentSpec as;
  as.varargs = true;
  return as;
}

bool Arguments::only_contains_named(const std::unordered_set<std::string>& names) {
  for (auto& kv : named) {
    if (names.find(kv.first) == names.end()) {
      return false;
    }
  }
  return true;
}

}  // namespace goos
