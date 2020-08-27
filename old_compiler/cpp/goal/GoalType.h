#ifndef JAK_GOALTYPE_H
#define JAK_GOALTYPE_H

#include <string>
#include <vector>
#include <stdexcept>
#include <unordered_map>
#include <memory>
#include "goos/Goos.h"
#include "util.h"

constexpr int BASIC_OFFSET = 4;
constexpr int PSIZE_ALIGN = 16;

class TypeContainer;

class GoalType {
 public:
  GoalType(int sz, std::shared_ptr<GoalType> p) : size(sz) {
    if (p) {
      parent = p->get_name();
    }
  }
  virtual std::string get_name() = 0;
  //  virtual std::string as_runtime_type() = 0;
  virtual std::string print() = 0;

  bool is_parent_type = false;
  bool is_boxed = false;
  bool is_value_type = false;
  int minimum_alignment = 4;
  int alignment_offset = 0;
  int load_size = 4;
  bool load_signed = true;
  bool load_xmm_32_prefer = false;
  bool pack_structure_type = false;
  int size;

  int get_size_in_inline_array() {
    if (pack_structure_type) {
      return size;
    } else {
      return align(size, 16, 0);
    }
  }

  int get_size_in_non_inline_array() {
    if (is_value_type) {
      return size;
    }
    return 4;
  }

  std::string parent;
};

class TypeSpec {
 public:
  TypeSpec(std::shared_ptr<GoalType> gt) : type(gt) {}

  TypeSpec(std::shared_ptr<GoalType> gt, std::vector<TypeSpec> args) : type(gt), ts_args(args) {}

  TypeSpec() = default;
  TypeSpec(Object& o, TypeContainer& types);

  bool operator!=(const TypeSpec& other) const { return !(other == *this); }

  bool operator==(const TypeSpec& other) const {
    if (other.type != type)
      return false;
    if (other.ts_args.size() != ts_args.size())
      return false;
    for (uint32_t i = 0; i < ts_args.size(); i++) {
      if (other.ts_args[i] != ts_args[i])
        return false;
    }
    return true;
  }

  std::string print();

  bool typecheck_base_only(TypeSpec& more_specific, TypeContainer& types);
  std::shared_ptr<GoalType> type;

  std::vector<TypeSpec> ts_args;
};

struct GoalBitField {
  TypeSpec type;
  std::string name;
  int offset = -1;
  int size = -1;

  GoalBitField() = default;
  GoalBitField(TypeSpec _type, std::string _name, int _offset, int _size)
      : type(_type), name(std::move(_name)), offset(_offset), size(_size) {}

  std::string print();
};

struct GoalField {
  TypeSpec type;
  std::string name;
  int offset;
  bool is_inline;
  bool is_dynamic = false;
  bool is_array = false;
  int array_size = 0;

  GoalField() = default;

  GoalField(const TypeSpec& p,
            const std::string& n,
            int off,
            bool iln = false,
            bool dyn = false,
            int arr_size = 0)
      : type(p), name(n), offset(off), is_inline(iln), is_dynamic(dyn), array_size(arr_size) {
    if (arr_size || is_dynamic) {
      is_array = true;
    }
  }

  std::string print();
};

// a type with just a name, for stuff like object, object64, none...
class SimpleType : public GoalType {
 public:
  SimpleType(int sz,
             std::shared_ptr<GoalType> p,
             const std::string& type_name,
             const std::string& runtime_type_name)
      : GoalType(sz, p), name(type_name), runtime_name(runtime_type_name) {}

  SimpleType(int sz, std::shared_ptr<GoalType> p, const std::string& type_name)
      : GoalType(sz, p), name(type_name), runtime_name(type_name) {}

  std::string name;
  std::string runtime_name;

  std::string get_name() override { return name; }

  //  std::string as_runtime_type() override {
  //    return runtime_name;
  //  }

  std::string print() override {
    auto result = "Simple Type: " + name;
    if (name != runtime_name) {
      result += " (runtime: " + runtime_name + ")";
    }
    return result;
  }
};

class BitfieldType : public GoalType {
 public:
  BitfieldType(const std::shared_ptr<GoalType>& _base, std::string _name)
      : GoalType(_base->size, _base), name(std::move(_name)) {
    assert(_base->is_value_type);
    is_value_type = true;
    minimum_alignment = _base->minimum_alignment;
    load_signed = false;  // todo...
    load_size = _base->load_size;
  }

  std::string name;
  std::vector<GoalBitField> fields;

  std::string get_name() override { return name; }

  std::string print() override {
    auto result = "Bit-Field Type: " + name + "\n";
    result += "align " + std::to_string(minimum_alignment) + " size " + std::to_string(load_size) +
              " " + std::to_string(size);
    for (auto& f : fields) {
      result += "\n field: " + f.print() + "\n";
    }
    return result;
  }

  bool find_field(const std::string& name, GoalBitField* field);
};

class StructureType : public GoalType {
 public:
  StructureType(int sz, std::shared_ptr<GoalType> p, const std::string& type_name)
      : GoalType(sz, p), name(type_name) {
    minimum_alignment = 16;
  }

  std::string name;
  std::vector<GoalField> fields;
  bool dynamic = false;

  std::string get_name() override { return name; }

  //  virtual std::string as_runtime_type() override {
  //    return "structure";
  //  }

  virtual std::string print() override {
    auto result = "Structure Type: " + name + "\n";
    if (dynamic) {
      result += " :dynamic\n";
    }
    for (auto& f : fields) {
      result += " field: " + f.print() + "\n";
    }
    return result;
  }

  void inherit_fields(std::shared_ptr<StructureType> pp) {
    fields.clear();
    for (auto& f : pp->fields) {
      fields.push_back(f);
    }

    minimum_alignment = pp->minimum_alignment;
    dynamic = pp->dynamic;
  }
};

class BasicType : public StructureType {
 public:
  BasicType(int sz,
            std::shared_ptr<GoalType> p,
            const std::string& type_name,
            std::shared_ptr<GoalType> type_of_type)
      : StructureType(sz, p, type_name) {
    fields.emplace_back(type_of_type, "type", 0);
    is_boxed = true;
  }

  std::string get_name() override { return name; }

  //  virtual std::string as_runtime_type() override {
  //    return name;
  //  }

  virtual std::string print() override {
    auto result = "Basic Type: " + name + "\n";
    for (auto& f : fields) {
      result += " field: " + f.print() + "\n";
    }
    return result;
  }
};

// symbol will need its own stuff
class SymbolType : public BasicType {
 public:
  SymbolType(std::shared_ptr<GoalType> p, std::shared_ptr<GoalType> type_of_type)
      : BasicType(8, p, "symbol", type_of_type) {}

  std::string get_name() override { return name; }

  //  virtual std::string as_runtime_type() override {
  //    return name;
  //  }

  virtual std::string print() override {
    auto result = "Symbol Type: " + name + "\n";
    for (auto& f : fields) {
      result += " field: " + f.print() + "\n";
    }
    return result;
  }
};

class BooleanType : public SymbolType {
 public:
  BooleanType(std::shared_ptr<GoalType> p, std::shared_ptr<GoalType> type_of_type)
      : SymbolType(p, type_of_type) {
    name = "boolean";
  }

  virtual std::string print() override {
    auto result = "Boolean Type: " + name + "\n";
    for (auto& f : fields) {
      result += " field: " + f.print() + "\n";
    }
    return result;
  }
};

class FunctionType : public BasicType {
 public:
  FunctionType(std::shared_ptr<GoalType> p, std::shared_ptr<GoalType> type_of_type)
      : BasicType(4, p, "function", type_of_type) {}

  virtual std::string print() override {
    auto result = "Function Type: " + name + "\n";
    for (auto& f : fields) {
      result += " field: " + f.print() + "\n";
    }
    return result;
  }
};

#endif  // JAK_GOALTYPE_H
