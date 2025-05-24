#pragma once

/*!
 * @file Object.h
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
 * STRING_HASH_TABLE - a heap allocated object representing a key-value table. Currently the keys
 * can only be strings, like make-str-hash-table in MIT Scheme.
 *
 */

#include <cstring>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include "common/common_types.h"
#include "common/util/Assert.h"
#include "common/util/crc32.h"

#include "fmt/core.h"

namespace goos {

/*!
 * All objects have one of these kinds
 */
enum class ObjectType : u8 {
  // special
  EMPTY_LIST,

  // fixed
  INTEGER,
  FLOAT,
  CHAR,
  SYMBOL,

  // allocated
  STRING,
  PAIR,
  ARRAY,
  LAMBDA,
  MACRO,
  ENVIRONMENT,
  STRING_HASH_TABLE,
  INVALID
};

std::string object_type_to_string(ObjectType kind);

// Some objects are "fixed", meaning they are stored inline in the Object, and not heap allocated.
// These use value semantics and are copied on object assignment.

// For int/float, these are simply double/int64_t
using FloatType = double;
using IntType = s64;

// For symbols, we simply store a pointer to the symbol's name string.
// this is wrapped in a type just to avoid confusion with normal const char*.
struct InternedSymbolPtr {
  const char* name_ptr;

  struct hash {
    auto operator()(const InternedSymbolPtr& x) const {
      return std::hash<const void*>()((const void*)x.name_ptr);
    }
  };
  bool operator==(const char* msg) const { return strcmp(msg, name_ptr) == 0; }
  bool operator!=(const char* msg) const { return strcmp(msg, name_ptr) != 0; }
  bool operator==(const std::string& str) const { return str == name_ptr; }
  bool operator!=(const std::string& str) const { return str != name_ptr; }
  bool operator==(const InternedSymbolPtr& other) const { return other.name_ptr == name_ptr; }
  bool operator!=(const InternedSymbolPtr& other) const { return other.name_ptr != name_ptr; }
};

/*!
 * By default, convert a fixed object data to string with std::to_string.
 * This will be used for integers, but float and char define their own.
 */
template <typename T>
std::string fixed_to_string(T x) {
  return std::to_string(x);
}

/*!
 * Special case to print floating point
 */
template <>
std::string fixed_to_string<FloatType>(FloatType);

/*!
 * Special case to print character
 */
template <>
std::string fixed_to_string<char>(char);

/*!
 * Special case to print integer
 */
template <>
std::string fixed_to_string<IntType>(IntType);

template <>
std::string fixed_to_string<InternedSymbolPtr>(InternedSymbolPtr);

class SymbolTable {
 public:
  SymbolTable(const SymbolTable&) = delete;
  SymbolTable& operator=(const SymbolTable&) = delete;
  SymbolTable();
  ~SymbolTable();
  InternedSymbolPtr intern(const char* str);

 private:
  void resize();
  int m_power_of_two_size = 0;
  struct Entry {
    u32 hash = 0;
    const char* name = nullptr;
  };
  std::vector<Entry> m_entries;
  int m_used_entries = 0;
  int m_next_resize = 0;
  u32 m_mask = 0;
  static constexpr float kMaxUsed = 0.7;
};

/*!
 * Common implementation for a fixed object
 */
template <typename T>
class FixedObject {
 public:
  T value;

  explicit FixedObject(T v) : value(v) {}
  FixedObject() = default;
  std::string print() const { return fixed_to_string(value); }
  std::string inspect() const { return type_as_string() + " " + fixed_to_string(value) + "\n"; }
  ~FixedObject() = default;

  bool operator==(const FixedObject<T>& other) const { return value == other.value; }

 private:
  std::string type_as_string() const {
    if (std::is_same<T, FloatType>())
      return object_type_to_string(ObjectType::FLOAT);
    if (std::is_same<T, IntType>())
      return object_type_to_string(ObjectType::INTEGER);
    if (std::is_same<T, char>())
      return object_type_to_string(ObjectType::CHAR);
    if (std::is_same<T, InternedSymbolPtr>())
      return object_type_to_string(ObjectType::SYMBOL);
    throw std::runtime_error("Unsupported FixedObject type");
  }
};

/*!
 * The fixed objects supported by GOOS
 */
using IntegerObject = FixedObject<int64_t>;
using FloatObject = FixedObject<double>;
using CharObject = FixedObject<char>;
using SymbolObject = FixedObject<InternedSymbolPtr>;

// Other objects are separate allocated on the heap. These objects should be HeapObjects.

class HeapObject {
 public:
  virtual std::string print() const = 0;
  virtual std::string inspect() const = 0;
  virtual ~HeapObject() = default;
};

// forward declare all HeapObjects
class PairObject;
class EnvironmentObject;
class StringObject;
class LambdaObject;
class MacroObject;
class ArrayObject;
class StringHashTableObject;

// Wrapper Object class for all objects
class Object {
 public:
  std::shared_ptr<HeapObject> heap_obj = nullptr;
  friend Object build_list(const std::vector<Object>& objects);
  friend Object build_list(std::vector<Object>&& objects);

  union {
    IntegerObject integer_obj;
    FloatObject float_obj;
    CharObject char_obj;
    SymbolObject symbol_obj;
  };

  ObjectType type = ObjectType::INVALID;

 private:
  bool disallow_hex_for_int = false;

 public:
  std::string print() const {
    switch (type) {
      case ObjectType::INTEGER:
        return disallow_hex_for_int ? fmt::format("{}", integer_obj.value) : integer_obj.print();
      case ObjectType::FLOAT:
        return float_obj.print();
      case ObjectType::CHAR:
        return char_obj.print();
      case ObjectType::SYMBOL:
        return symbol_obj.print();
      case ObjectType::EMPTY_LIST:
        return "()";
      default:
        return heap_obj->print();
    }
  }

  std::string inspect() const {
    switch (type) {
      case ObjectType::INTEGER:
        return integer_obj.inspect();
      case ObjectType::FLOAT:
        return float_obj.inspect();
      case ObjectType::CHAR:
        return char_obj.inspect();
      case ObjectType::SYMBOL:
        return symbol_obj.inspect();
      case ObjectType::EMPTY_LIST:
        return "[empty list] ()\n";
      default:
        return heap_obj->inspect();
    }
  }

  template <typename T>
  static Object make_number(T value);

  static Object make_integer(IntType value) {
    Object o;
    o.type = ObjectType::INTEGER;
    o.integer_obj.value = value;
    return o;
  }

  static Object make_integer_no_hex(IntType value) {
    Object o;
    o.type = ObjectType::INTEGER;
    o.integer_obj.value = value;
    o.disallow_hex_for_int = true;
    return o;
  }

  static Object make_float(FloatType value) {
    Object o;
    o.type = ObjectType::FLOAT;
    o.float_obj.value = value;
    return o;
  }

  static Object make_char(char value) {
    Object o;
    o.type = ObjectType::CHAR;
    o.char_obj.value = value;
    return o;
  }

  static Object make_empty_list() {
    Object o;
    o.type = ObjectType::EMPTY_LIST;
    return o;
  }

  static Object make_symbol(SymbolTable* table, const char* name) {
    Object o;
    o.type = ObjectType::SYMBOL;
    o.symbol_obj.value = table->intern(name);
    return o;
  }

  PairObject* as_pair() const;
  EnvironmentObject* as_env() const;
  std::shared_ptr<EnvironmentObject> as_env_ptr() const;
  StringObject* as_string() const;
  LambdaObject* as_lambda() const;
  MacroObject* as_macro() const;
  ArrayObject* as_array() const;
  StringHashTableObject* as_string_hash_table() const;

  const InternedSymbolPtr& as_symbol() const {
    if (type != ObjectType::SYMBOL) {
      throw std::runtime_error("as_symbol called on a " + object_type_to_string(type) + " " +
                               print());
    }
    return symbol_obj.value;
  }

  IntType& as_int() {
    if (type != ObjectType::INTEGER) {
      throw std::runtime_error("as_int called on a " + object_type_to_string(type) + " " + print());
    }
    return integer_obj.value;
  }

  const IntType& as_int() const {
    if (type != ObjectType::INTEGER) {
      throw std::runtime_error("as_int called on a " + object_type_to_string(type) + " " + print());
    }
    return integer_obj.value;
  }

  FloatType& as_float() {
    if (type != ObjectType::FLOAT) {
      throw std::runtime_error("as_float called on a " + object_type_to_string(type) + " " +
                               print());
    }
    return float_obj.value;
  }

  const FloatType& as_float() const {
    if (type != ObjectType::FLOAT) {
      throw std::runtime_error("as_float called on a " + object_type_to_string(type) + " " +
                               print());
    }
    return float_obj.value;
  }

  char& as_char() {
    if (type != ObjectType::CHAR) {
      throw std::runtime_error("as_char called on a " + object_type_to_string(type) + " " +
                               print());
    }
    return char_obj.value;
  }

  bool is_empty_list() const { return type == ObjectType::EMPTY_LIST; }
  bool is_list() const { return type == ObjectType::EMPTY_LIST || type == ObjectType::PAIR; }
  bool is_int() const { return type == ObjectType::INTEGER; }
  bool is_int(int val) const { return type == ObjectType::INTEGER && val == as_int(); }
  bool is_float() const { return type == ObjectType::FLOAT; }
  bool is_float(float val) const { return type == ObjectType::FLOAT && val == as_float(); }
  bool is_char() const { return type == ObjectType::CHAR; }
  bool is_symbol() const { return type == ObjectType::SYMBOL; }
  bool is_symbol(const std::string& name) const;
  bool is_string() const { return type == ObjectType::STRING; }
  bool is_string(const std::string& val) const;
  bool is_pair() const { return type == ObjectType::PAIR; }
  bool is_array() const { return type == ObjectType::ARRAY; }
  bool is_env() const { return type == ObjectType::ENVIRONMENT; }
  bool is_macro() const { return type == ObjectType::MACRO; }
  bool is_string_hash_table() const { return type == ObjectType::STRING_HASH_TABLE; }

  bool is_power_of_2_float() const;

  bool operator==(const Object& other) const;
  bool operator!=(const Object& other) const { return !((*this) == other); }
};

class StringObject : public HeapObject {
 public:
  std::string data;
  explicit StringObject(std::string text) : data(std::move(text)) {}

  static Object make_new(const std::string& text) {
    Object obj;
    obj.type = ObjectType::STRING;
    obj.heap_obj = std::make_shared<StringObject>(text);
    return obj;
  }

  std::string print() const override;
  std::string inspect() const override;

  ~StringObject() override = default;
};

class PairObject : public HeapObject {
 public:
  Object car, cdr;

  PairObject(const Object& car_, const Object& cdr_) : car(car_), cdr(cdr_) {}
  PairObject() = default;

  static Object make_new(const Object& a, const Object& b) {
    Object obj;
    obj.type = ObjectType::PAIR;
    obj.heap_obj = std::make_shared<PairObject>(a, b);
    return obj;
  }

  std::string print() const override {
    std::pair<Object, Object> to_print_pair = std::make_pair(car, cdr);

    std::string result = "(";

    // print first thing:
    result += car.print();

    // print second thing
    Object to_print = cdr;
    if (to_print.type == ObjectType::EMPTY_LIST) {
      result += ")";
      return result;
    } else {
      result += " ";
    }

    for (;;) {
      if (to_print.type == ObjectType::PAIR) {
        Object to_print_car = std::dynamic_pointer_cast<PairObject>(to_print.heap_obj)->car;
        result += to_print_car.print();
        to_print = std::dynamic_pointer_cast<PairObject>(to_print.heap_obj)->cdr;
        if (to_print.type == ObjectType::EMPTY_LIST) {
          result += ")";
          return result;
        } else {
          result += " ";
        }
      } else {
        result += ". ";
        result += to_print.print();
        result += ")";
        return result;
      }
    }
  }

  std::string inspect() const override { return "[pair] " + print() + "\n"; }

  ~PairObject() = default;
};

template <typename T>
class InternedPtrMap {
 public:
  InternedPtrMap(const InternedPtrMap&) = delete;
  InternedPtrMap& operator=(const InternedPtrMap&) = delete;
  InternedPtrMap() { clear(); }

  T* lookup(InternedSymbolPtr str) {
    if (m_entries.size() < 10) {
      for (auto& e : m_entries) {
        if (e.key == str.name_ptr) {
          return &e.value;
        }
      }
      return nullptr;
    }
    u32 hash = crc32((const u8*)&str.name_ptr, sizeof(const char*));

    // probe
    for (u32 i = 0; i < m_entries.size(); i++) {
      u32 slot_addr = (hash + i) & m_mask;
      auto& slot = m_entries[slot_addr];
      if (!slot.key) {
        return nullptr;
      } else {
        if (slot.key != str.name_ptr) {
          continue;  // bad hash
        }
        return &slot.value;
      }
    }

    // should be impossible to reach.
    ASSERT_NOT_REACHED();
  }
  void set(InternedSymbolPtr ptr, const T& obj) {
    u32 hash = crc32((const u8*)&ptr.name_ptr, sizeof(const char*));

    // probe
    for (u32 i = 0; i < m_entries.size(); i++) {
      u32 slot_addr = (hash + i) & m_mask;
      auto& slot = m_entries[slot_addr];
      if (!slot.key) {
        // not found, insert!
        slot.key = ptr.name_ptr;
        slot.value = obj;
        m_used_entries++;

        if (m_used_entries >= m_next_resize) {
          resize();
        }
        return;
      } else {
        if (slot.key == ptr.name_ptr) {
          slot.value = obj;
          return;
        }
      }
    }

    // should be impossible to reach.
    ASSERT_NOT_REACHED();
  }
  void clear() {
    m_entries.clear();
    m_power_of_two_size = 3;  // 2 ^ 3 = 8
    m_entries.resize(8);
    m_used_entries = 0;
    m_next_resize = (m_entries.size() * kMaxUsed);
    m_mask = 0b111;
  }

 private:
  struct Entry {
    const char* key = nullptr;
    T value;
  };
  std::vector<Entry> m_entries;

  void resize() {
    m_power_of_two_size++;
    m_mask = (1U << m_power_of_two_size) - 1;

    std::vector<Entry> new_entries(m_entries.size() * 2);
    for (const auto& old_entry : m_entries) {
      if (old_entry.key) {
        bool done = false;
        u32 hash = crc32((const u8*)&old_entry.key, sizeof(const char*));
        for (u32 i = 0; i < new_entries.size(); i++) {
          u32 slot_addr = (hash + i) & m_mask;
          auto& slot = new_entries[slot_addr];
          if (!slot.key) {
            slot.key = old_entry.key;
            slot.value = std::move(old_entry.value);
            done = true;
            break;
          }
        }
        ASSERT(done);
      }
    }

    m_entries = std::move(new_entries);
    m_next_resize = kMaxUsed * m_entries.size();
  }
  int m_power_of_two_size = 0;
  int m_used_entries = 0;
  int m_next_resize = 0;
  u32 m_mask = 0;
  static constexpr float kMaxUsed = 0.7;
};

using EnvironmentMap = InternedPtrMap<Object>;

class EnvironmentObject : public HeapObject {
 public:
  std::string name;
  std::shared_ptr<EnvironmentObject> parent_env;

  // note: this is keyed on the address in the symbol table.
  EnvironmentMap vars;

  // this find work by any name string
  Object* find(const char* n, SymbolTable* st) { return vars.lookup(st->intern(n)); }
  Object* find(InternedSymbolPtr ptr) { return vars.lookup(ptr); }

  EnvironmentObject() = default;

  static Object make_new() {
    Object obj;
    obj.type = ObjectType::ENVIRONMENT;
    obj.heap_obj = std::make_shared<EnvironmentObject>();
    return obj;
  }

  static Object make_new(std::string name,
                         std::shared_ptr<EnvironmentObject> parent_env = nullptr) {
    Object obj;
    obj.type = ObjectType::ENVIRONMENT;
    auto env = std::make_shared<EnvironmentObject>();
    env->name = std::move(name);
    env->parent_env = std::move(parent_env);
    obj.heap_obj = std::move(env);
    return obj;
  }

  std::string print() const override {
    if (name.empty()) {
      return "<unnamed environment>";
    } else {
      return "<environment \"" + name + "\">";
    }
  }

  std::string inspect() const override {
    std::string result = "[environment]\n  name: " + name +
                         "\n  parent: " + (parent_env ? parent_env->print() : "NONE") + "\n";

    return result;
  }
};

struct NamedArg {
  bool has_default = false;
  Object default_value;
};

struct ArgumentSpec {
  bool varargs = false;
  std::vector<std::string> unnamed;
  std::unordered_map<std::string, NamedArg> named;
  std::string rest;
  std::string print() const;
};

ArgumentSpec make_varargs();

struct Arguments {
  std::vector<Object> unnamed;
  // note that this is _not_ an unordered map so that named arguments have a consistent (but
  // arbitrary) alphabetical evaluation order.
  std::map<std::string, Object> named;
  std::vector<Object> rest;
  bool has_rest = false;
  std::string print() const;

  Object get_named(const std::string& name, const Object& default_value);
  Object get_named(const std::string& name);
  bool has_named(const std::string& name);
  bool only_contains_named(const std::unordered_set<std::string>& names);
};

class LambdaObject : public HeapObject {
 public:
  std::string name;
  std::shared_ptr<EnvironmentObject> parent_env;
  Object body;
  ArgumentSpec args;

  LambdaObject() = default;

  static Object make_new() {
    Object obj;
    obj.type = ObjectType::LAMBDA;
    obj.heap_obj = std::make_shared<LambdaObject>();
    return obj;
  }

  std::string print() const override {
    if (name.empty()) {
      return "<unnamed lambda>";
    } else {
      return "<lambda \"" + name + "\">";
    }
  }

  std::string inspect() const override { return "[lambda]\n  name: " + name + "\n" + args.print(); }
};

class MacroObject : public HeapObject {
 public:
  std::string name;
  std::shared_ptr<EnvironmentObject> parent_env;
  Object body;
  ArgumentSpec args;

  MacroObject() = default;

  static Object make_new() {
    Object obj;
    obj.type = ObjectType::MACRO;
    obj.heap_obj = std::make_shared<MacroObject>();
    return obj;
  }

  std::string print() const override {
    if (name.empty()) {
      return "<unnamed macro>";
    } else {
      return "<macro \"" + name + "\">";
    }
  }

  std::string inspect() const override { return "[macro]\n  name: " + name + "\n" + args.print(); }
};

class ArrayObject : public HeapObject {
 public:
  std::vector<Object> data;
  ArrayObject(std::vector<Object> objects) : data(std::move(objects)) {}
  static Object make_new(std::vector<Object> objects) {
    Object obj;
    obj.type = ObjectType::ARRAY;
    obj.heap_obj = std::make_shared<ArrayObject>(std::move(objects));
    return obj;
  }

  std::string print() const override {
    std::string result = "#(";
    if (data.empty()) {
      return result + ")";
    }
    for (const auto& obj : data) {
      result += obj.print() + " ";
    }
    result.pop_back();  // remove last space
    return result + ")";
  }

  std::string inspect() const override {
    return "[array] size: " + std::to_string(data.size()) + " data: " + print() + "\n";
  }

  std::size_t size() const { return data.size(); }

  const Object& operator[](size_t idx) const { return data.at(idx); }

  Object& operator[](size_t idx) { return data.at(idx); }
};

class StringHashTableObject : public HeapObject {
 public:
  std::unordered_map<std::string, Object> data;
  StringHashTableObject() = default;
  static Object make_new() {
    Object obj;
    obj.type = ObjectType::STRING_HASH_TABLE;
    obj.heap_obj = std::make_shared<StringHashTableObject>();
    return obj;
  }

  std::string print() const override {
    std::string result = "{";
    for (const auto& kv : data) {
      result += '(';
      result += kv.first;
      result += ' ';
      result += kv.second.print();
      result += ')';
      result += ' ';
    }
    if (!data.empty()) {
      result.pop_back();
    }
    result += '}';
    return result;
  }

  std::string inspect() const override {
    return "[string-hash-table] kind: string, data: " + print() + "\n";
  }
};

Object build_list(const std::vector<Object>& objects);
Object build_list(std::vector<Object>&& objects);

inline PairObject* Object::as_pair() const {
  if (type != ObjectType::PAIR) {
    throw std::runtime_error("as_pair called on a " + object_type_to_string(type) + " " + print());
  }
  return static_cast<PairObject*>(heap_obj.get());
}

inline EnvironmentObject* Object::as_env() const {
  if (type != ObjectType::ENVIRONMENT) {
    throw std::runtime_error("as_env called on a " + object_type_to_string(type) + " " + print());
  }
  return static_cast<EnvironmentObject*>(heap_obj.get());
}

inline std::shared_ptr<EnvironmentObject> Object::as_env_ptr() const {
  if (type != ObjectType::ENVIRONMENT) {
    throw std::runtime_error("as_env called on a " + object_type_to_string(type) + " " + print());
  }
  return std::dynamic_pointer_cast<EnvironmentObject>(heap_obj);
}

inline StringObject* Object::as_string() const {
  if (type != ObjectType::STRING) {
    throw std::runtime_error("as_string called on a " + object_type_to_string(type) + " " +
                             print());
  }
  return static_cast<StringObject*>(heap_obj.get());
}

inline LambdaObject* Object::as_lambda() const {
  if (type != ObjectType::LAMBDA) {
    throw std::runtime_error("as_lambda called on a " + object_type_to_string(type) + " " +
                             print());
  }
  return static_cast<LambdaObject*>(heap_obj.get());
}

inline MacroObject* Object::as_macro() const {
  if (type != ObjectType::MACRO) {
    throw std::runtime_error("as_macro called on a " + object_type_to_string(type) + " " + print());
  }
  return static_cast<MacroObject*>(heap_obj.get());
}

inline ArrayObject* Object::as_array() const {
  if (type != ObjectType::ARRAY) {
    throw std::runtime_error("as_array called on a " + object_type_to_string(type) + " " + print());
  }
  return static_cast<ArrayObject*>(heap_obj.get());
}

inline StringHashTableObject* Object::as_string_hash_table() const {
  if (type != ObjectType::STRING_HASH_TABLE) {
    throw std::runtime_error("as_string_hash_table called on a " + object_type_to_string(type) +
                             " " + print());
  }
  return static_cast<StringHashTableObject*>(heap_obj.get());
}
}  // namespace goos
