#ifndef COMPILER_Object_H
#define COMPILER_Object_H

#include <cstdint>
#include <string>
#include <memory>
#include <unordered_map>
#include <vector>
#include <cassert>

class Object;

enum ObjectType : uint8_t {
  INTEGER,
  FLOAT,
  CHAR,
  EMPTY_LIST,
  SYMBOL,
  STRING,
  PAIR,
  // todo ARRAY,
  LAMBDA,
  MACRO,
  ENVIRONMENT,
  INVALID
};

template <typename T>
std::string object_type_to_string(T x) {
  return std::to_string(x);
}

template <>
inline std::string object_type_to_string(double x) {
  char buff[256];
  sprintf(buff, "%g", x);
  return std::string(buff);
}

template <>
inline std::string object_type_to_string(char x) {
  char buff[256];
  // printable
  if (x >= 33 && x <= 126) {
    sprintf(buff, "#\\%c", x);
    return std::string(buff);
  }

  switch (x) {
    case '\n':
      sprintf(buff, "#\\newline");
      break;
    case ' ':
      sprintf(buff, "#\\space");
      break;

    default:
      sprintf(buff, "#\\{%d}", (uint8_t)x);
  }

  // not printable
  return std::string(buff);
}

// non-heap-allocated object
template <typename T>
class FixedTypeObject {
 public:
  T value;

  explicit FixedTypeObject(T v) : value(v) {}

  FixedTypeObject() = default;

  std::string print() { return object_type_to_string(value); }

  std::string inspect() {
    return type_as_string() + "\n  value: " + object_type_to_string(value) + "\n";
  }

  ~FixedTypeObject() = default;

 private:
  std::string type_as_string() {
    if (std::is_same<T, double>())
      return "[Float]";
    if (std::is_same<T, int64_t>())
      return "[Integer]";
    if (std::is_same<T, char>())
      return "[Char]";
    return "[Unknown Fixed Type]";
  }
};

using IntegerObject = FixedTypeObject<int64_t>;
using FloatObject = FixedTypeObject<double>;
using CharObject = FixedTypeObject<char>;

// for objects which are heap allocated (reference semantics
// these use virtual methods to implement print/insepct...
class AllocObject {
 public:
  virtual std::string print() = 0;
  virtual std::string inspect() = 0;
  virtual ~AllocObject() = default;
};

class PairObject;
class EnvironmentObject;
class SymbolObject;
class StringObject;
class LambdaObject;
class MacroObject;

// wrapper for both heap allocated and value objects.
class Object {
 public:
  std::shared_ptr<AllocObject> alloc = nullptr;

  union {
    IntegerObject integer_obj;
    FloatObject float_obj;
    CharObject char_obj;
  };

  ObjectType type = INVALID;

  std::string print() {
    switch (type) {
      case INTEGER:
        return integer_obj.print();
      case FLOAT:
        return float_obj.print();
      case CHAR:
        return char_obj.print();
      default:
        return alloc->print();
    }
  }

  std::string inspect() {
    switch (type) {
      case INTEGER:
        return integer_obj.inspect();
      case FLOAT:
        return float_obj.inspect();
      case CHAR:
        return char_obj.inspect();
      default:
        return alloc->inspect();
    }
  }

  template <typename T>
  static Object make_number(T value);

  static Object make_integer(int64_t value) {
    Object o;
    o.type = INTEGER;
    o.integer_obj.value = value;
    return o;
  }

  static Object make_float(double value) {
    Object o;
    o.type = FLOAT;
    o.float_obj.value = value;
    return o;
  }

  static Object make_char(char value) {
    Object o;
    o.type = CHAR;
    o.char_obj.value = value;
    return o;
  }

  std::shared_ptr<PairObject> as_pair() {
    assert(type == PAIR);
    return std::dynamic_pointer_cast<PairObject>(alloc);
  }

  std::shared_ptr<EnvironmentObject> as_env() {
    assert(type == ENVIRONMENT);
    return std::dynamic_pointer_cast<EnvironmentObject>(alloc);
  }

  std::shared_ptr<SymbolObject> as_symbol() const {
    assert(type == SYMBOL);
    return std::dynamic_pointer_cast<SymbolObject>(alloc);
  }

  std::shared_ptr<StringObject> as_string() const {
    assert(type == STRING);
    return std::dynamic_pointer_cast<StringObject>(alloc);
  }

  std::shared_ptr<LambdaObject> as_lambda() {
    assert(type == LAMBDA);
    return std::dynamic_pointer_cast<LambdaObject>(alloc);
  }

  std::shared_ptr<MacroObject> as_macro() {
    assert(type == MACRO);
    return std::dynamic_pointer_cast<MacroObject>(alloc);
  }
};

// There is a single heap allocated EmptyListObject.
class EmptyListObject;
extern std::shared_ptr<EmptyListObject> gEmptyList;

class EmptyListObject : public AllocObject {
 public:
  EmptyListObject() = default;
  static Object make_new() {
    Object obj;
    obj.type = EMPTY_LIST;
    if (!gEmptyList) {
      gEmptyList = std::make_shared<EmptyListObject>();
    }
    obj.alloc = gEmptyList;
    return obj;
  }

  std::string print() override { return "()"; }

  std::string inspect() override {
    char buff[256];
    sprintf(buff, "[Empty List]\n");
    return std::string(buff);
  }

  ~EmptyListObject() = default;
};

class StringObject : public AllocObject {
 public:
  std::string data;
  explicit StringObject(const std::string& text) : data(text) {}

  static Object make_new(const std::string& text) {
    Object obj;
    obj.type = STRING;
    obj.alloc = std::make_shared<StringObject>(text);
    return obj;
  }

  std::string print() override { return "\"" + data + "\""; }

  std::string inspect() override {
    return "[String]\n  data: " + data + "\n  length: " + std::to_string(data.size()) + "\n";
  }

  ~StringObject() = default;
};

class PairObject : public AllocObject {
 public:
  Object car, cdr;

  PairObject(Object car_, Object cdr_) : car(car_), cdr(cdr_) {}

  static Object make_new(Object a, Object b) {
    Object obj;
    obj.type = PAIR;
    obj.alloc = std::make_shared<PairObject>(a, b);
    return obj;
  }

  std::string print() override {
    std::pair<Object, Object> to_print_pair = std::make_pair(car, cdr);

    std::string result = "(";

    // print first thing:
    result += car.print();

    // print second thing
    Object to_print = cdr;
    if (to_print.type == EMPTY_LIST) {
      result += ")";
      return result;
    } else {
      result += " ";
    }

    for (;;) {
      if (to_print.type == PAIR) {
        Object to_print_car = std::dynamic_pointer_cast<PairObject>(to_print.alloc)->car;
        result += to_print_car.print();
        to_print = std::dynamic_pointer_cast<PairObject>(to_print.alloc)->cdr;
        if (to_print.type == EMPTY_LIST) {
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

  std::string inspect() override { return "[Pair]\n  value: " + print() + "\n"; }

  ~PairObject() = default;
};

class SymbolTable;

class SymbolObject : public AllocObject {
 public:
  std::string name;
  explicit SymbolObject(const std::string& c) : name(c) {}

  static Object make_new(SymbolTable& st, const std::string& name);

  std::string print() override { return name; }

  std::string inspect() override {
    char buff[1024];
    sprintf(buff, "[Symbol]\n  name: %s\n  value: 0x%lx\n", name.c_str(), (uint64_t)(this));
    return std::string(buff);
  }

  ~SymbolObject() = default;
};

class SymbolTable {
 public:
  std::shared_ptr<SymbolObject> intern(const std::string& name) {
    auto kv = table.find(name);
    if (kv == table.end()) {
      auto iter = table.insert({name, std::make_shared<SymbolObject>(name)});
      return (*iter.first).second;
    } else {
      return kv->second;
    }
  }

  ~SymbolTable() = default;

 private:
  std::unordered_map<std::string, std::shared_ptr<SymbolObject>> table;
};

inline Object SymbolObject::make_new(SymbolTable& st, const std::string& name) {
  Object obj;
  obj.type = SYMBOL;
  obj.alloc = st.intern(name);
  return obj;
}

class EnvironmentObject : public AllocObject {
 public:
  std::string name;
  std::shared_ptr<EnvironmentObject> parent_env;
  std::unordered_map<std::shared_ptr<SymbolObject>, Object> vars;

  EnvironmentObject() = default;

  static Object make_new() {
    Object obj;
    obj.type = ENVIRONMENT;
    obj.alloc = std::make_shared<EnvironmentObject>();
    return obj;
  }

  std::string print() override {
    if (name.empty()) {
      return "<unnamed environment>";
    } else {
      return "<environment \"" + name + "\">";
    }
  }

  std::string inspect() override {
    std::string result = "[Environment]\n  name: " + name +
                         "\n  parent: " + (parent_env ? parent_env->print() : "NONE") +
                         "\n  vars:\n";
    for (auto kv : vars) {
      result += "    " + kv.first->print() + ": " + kv.second.print() + "\n";
    }
    return result;
  }
};

class LambdaObject : public AllocObject {
 public:
  std::string name;
  std::shared_ptr<EnvironmentObject> parent_env;
  Object body;
  std::vector<Object> unnamed_args;
  std::unordered_map<std::string, Object> named_args;
  Object rest_args;
  bool has_rest = false;

  LambdaObject() = default;

  static Object make_new() {
    Object obj;
    obj.type = LAMBDA;
    obj.alloc = std::make_shared<LambdaObject>();
    return obj;
  }

  std::string print() override {
    if (name.empty()) {
      return "<unnamed procedure>";
    } else {
      return "<procedure \"" + name + "\">";
    }
  }

  std::string inspect() override {
    std::string result = "[Procedure]\n  name: " + name + "\n  unnamed args:\n";
    for (auto& arg : unnamed_args) {
      result += "    " + arg.print() + "\n";
    }
    result += "  named args:\n";
    for (auto& arg : named_args) {
      result += "     " + arg.first + " : " + arg.second.print() + "\n";
    }
    if (has_rest) {
      result += "  rest: " + rest_args.print() + "\n";
    }

    return result;
  }
};

class MacroObject : public AllocObject {
 public:
  std::string name;
  std::shared_ptr<EnvironmentObject> parent_env;
  Object body;
  std::vector<Object> unnamed_args;
  std::unordered_map<std::string, Object> named_args;
  Object rest_args;
  bool has_rest = false;

  MacroObject() = default;

  static Object make_new() {
    Object obj;
    obj.type = MACRO;
    obj.alloc = std::make_shared<MacroObject>();
    return obj;
  }

  std::string print() override {
    if (name.empty()) {
      return "<unnamed macro>";
    } else {
      return "<macro \"" + name + "\">";
    }
  }

  std::string inspect() override {
    std::string result = "[Macro]\n  name: " + name + "\n  unnamed args:\n";
    for (auto& arg : unnamed_args) {
      result += "    " + arg.print() + "\n";
    }
    result += "  named args:\n";
    for (auto& arg : named_args) {
      result += "     " + arg.first + " : " + arg.second.print() + "\n";
    }
    if (has_rest) {
      result += "  rest: " + rest_args.print() + "\n";
    }

    return result;
  }
};

inline Object build_list(const std::vector<Object>& objects) {
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

inline bool operator==(Object& lhs, Object& rhs) {
  if (lhs.type != rhs.type)
    return false;

  switch (lhs.type) {
    case STRING:
      return lhs.as_string()->data == rhs.as_string()->data;

    case INTEGER:
      return lhs.integer_obj.value == rhs.integer_obj.value;

    case FLOAT:
      return lhs.float_obj.value == rhs.float_obj.value;

    case SYMBOL:
    case ENVIRONMENT:
    case LAMBDA:
    case MACRO:
      return lhs.alloc == rhs.alloc;

      // todo, the rest.

    default:
      throw std::runtime_error("equality not implemented for " + lhs.print());
  }
}

#endif  // COMPILER_Object_H
