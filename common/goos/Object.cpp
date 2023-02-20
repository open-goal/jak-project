/*!
 * @file Object.cpp
 * An "Object" represents a scheme object.
 * There are different types of objects, as represented by ObjectType.
 * An "Object" is an efficient wrapper around any of these types.
 * Some types are "heap allocated", and have reference semantics, and others are
 * "fixed" and have value semantics.  Heap allocated objects implement reference counting with
 * std::shared_ptr.
 *
 * To create a new Object for a heap allocated type, use the make_new static method of the type of
 * object you want to make. This will return a correctly setup Object. For fixed objects, use
 * Object::make_<type>
 *
 * To convert an Object into a more specific object, use the as_<type> method of Object.
 * It will throw an exception if you get the type wrong.
 *
 * These are all the types:
 *
 * EMPTY_LIST - a special heap allocated object. There is only one EMPTY_LIST allocated, and
 * EmptyListObject::make_new() will always return an Object which references that one.
 *
 * INTEGER - a fixed type. Use Object::make_integer() to create one. Internally uses int64_t
 * FLOAT - a fixed type. Use Object::make_float() to create one. Internally uses double
 * CHAR - a fixed type. Use Object::make_char() to create one. Internally uses char
 *
 * SYMBOL - a special heap allocated object. SymbolObject::make_new requires a SymbolTable to
 * store the newly allocated symbol in, and will return an existing symbol if there already is one.
 *
 * STRING - a heap allocated object. Create with StringObject::make_new. Uses std::string internally
 *
 * PAIR - a heap allocated object containing two Objects.
 *
 * ARRAY - a heap allocated object containing a std::vector<Object>
 *
 * LAMBDA - a heap allocated object representing a GOOS lambda
 * MACRO - a heap allocated object representing a GOOS macro
 * ENVIRONMENT - a heap allocated object representing a GOOS environment
 *
 */

#include "Object.h"

#include <cinttypes>

#include "common/util/FileUtil.h"
#include "common/util/print_float.h"

#include "third-party/fmt/core.h"

namespace goos {

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
    case ObjectType::STRING_HASH_TABLE:
      return "[string-hash-table]";
    default:
      throw std::runtime_error("unknown object type in object_type_to_string");
  }
}

/*!
 * Special case to print a float
 */
template <>
std::string fixed_to_string(FloatType x) {
  auto result = float_to_string(x);
  ASSERT((float)x == (float)std::stod(result));
  return result;
}

/*!
 * Special case to print an integer
 */
template <>
std::string fixed_to_string(IntType x) {
  if (x > 10000) {
    return fmt::format("#x{:x}", x);
  } else {
    return fmt::format("{}", x);
  }
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
    return Object::make_empty_list();
  }

  // this is by far the most expensive part of parsing, so this is done a bit carefully.
  // we maintain a std::shared_ptr<PairObject> that represents the list, built from back to front.
  std::shared_ptr<PairObject> head =
      std::make_shared<PairObject>(objects.back(), Object::make_empty_list());

  s64 idx = ((s64)objects.size()) - 2;
  while (idx >= 0) {
    Object next;
    next.type = ObjectType::PAIR;
    next.heap_obj = std::move(head);

    head = std::make_shared<PairObject>();
    head->car = objects[idx];
    head->cdr = std::move(next);

    idx--;
  }

  Object result;
  result.type = ObjectType::PAIR;
  result.heap_obj = head;
  return result;
}

Object build_list(std::vector<Object>&& objects) {
  if (objects.empty()) {
    return Object::make_empty_list();
  }

  // this is by far the most expensive part of parsing, so this is done a bit carefully.
  // we maintain a std::shared_ptr<PairObject> that represents the list, built from back to front.
  std::shared_ptr<PairObject> head =
      std::make_shared<PairObject>(objects.back(), Object::make_empty_list());

  s64 idx = ((s64)objects.size()) - 2;
  while (idx >= 0) {
    Object next;
    next.type = ObjectType::PAIR;
    next.heap_obj = std::move(head);

    head = std::make_shared<PairObject>();
    head->car = std::move(objects[idx]);
    head->cdr = std::move(next);

    idx--;
  }

  Object result;
  result.type = ObjectType::PAIR;
  result.heap_obj = head;
  return result;
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
    case ObjectType::STRING_HASH_TABLE:
      return as_string_hash_table()->data == other.as_string_hash_table()->data;

    default:
      throw std::runtime_error("equality not implemented for " + print());
  }
}

bool Object::is_symbol(const std::string& name) const {
  return is_symbol() && as_symbol()->name == name;
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

namespace {
std::string escape_string(const std::string& in) {
  std::string result;
  result.reserve(in.size());

  for (char c : in) {
    if (c == '"') {
      result.push_back('\\');
      result.push_back('"');
    } else {
      result.push_back(c);
    }
  }

  return result;
}
}  // namespace

std::string StringObject::print() const {
  return "\"" + escape_string(data) + "\"";
}

std::string StringObject::inspect() const {
  return "[string] \"" + escape_string(data) + "\"\n";
}

}  // namespace goos
