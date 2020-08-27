#ifndef JAK_TYPECONTAINER_H
#define JAK_TYPECONTAINER_H

#include "GoalType.h"

// todo get method count, more legit checking of enough room for methods...

struct MethodType {
  uint8_t id;
  std::string method_name;
  TypeSpec type;

  bool operator==(const MethodType& other) {
    return (id == other.id) && (method_name == other.method_name) && (type == other.type);
  }
};

class TypeContainer {
 public:
  TypeContainer() = default;
  void fill_with_default_types();
  std::shared_ptr<GoalType> lookup(const std::string& name) {
    auto kv = types.find(name);
    if (kv == types.end()) {
      throw std::runtime_error("unknown type " + name);
    }
    return kv->second;
  }

  bool try_get_method_info(const std::string& type_name,
                           const std::string& method_name,
                           MethodType* result) {
    auto kv = type_method_types.find(type_name);
    if (kv == type_method_types.end()) {
      throw std::runtime_error("unknown type for method lookup " + type_name);
    }

    for (auto& x : kv->second) {
      if (x.method_name == method_name) {
        *result = x;
        return true;
      }
    }

    return false;
  }

  MethodType get_method_info(const std::string& type_name, const std::string& method_name) {
    MethodType t;

    if (try_get_method_info(type_name, method_name, &t)) {
      return t;
    }

    throw std::runtime_error("unknonw method " + method_name + " for type " + type_name);
  }

  uint8_t add_method(const std::string& type_name, const std::string& method_name, TypeSpec type) {
    type_method_types[type_name];

    auto kv = type_method_types.find(type_name);
    if (kv == type_method_types.end()) {
      throw std::runtime_error("unknown type for method lookup " + type_name);
    }

    for (auto& x : kv->second) {
      if (x.method_name == method_name) {
        x.type = type;
        return x.id;
      }
    }

    auto id = kv->second.size();
    if (id >= 255) {
      throw std::runtime_error("too many methods for " + type_name);
    }

    MethodType mt;
    mt.id = id;
    mt.method_name = method_name;
    mt.type = type;
    kv->second.push_back(mt);
    return id;
  }

  uint8_t add_method(const std::string& type_name,
                     const std::string& method_name,
                     std::vector<std::string> ts_args) {
    std::vector<TypeSpec> args;
    for (auto& a : ts_args) {
      args.emplace_back(lookup(a));
    }
    TypeSpec ts(lookup("function"), args);
    return add_method(type_name, method_name, ts);
  }

  void inherit_methods(const std::string& parent_name, const std::string& child_name) {
    auto kv = type_method_types.find(parent_name);
    if (kv == type_method_types.end()) {
      throw std::runtime_error("unknown type " + parent_name + " in inherit methods");
    }

    //    auto& child = type_method_types[child_name];
    for (auto& m : kv->second) {
      add_method(child_name, m.method_name, m.type);
      //      if(child.size() > m.id) {
      //        if(child.at(m.id) == m) continue;
      //      }
      //      child.push_back(m);
    }
  }

  std::unordered_map<std::string, std::shared_ptr<GoalType>> types;
  std::unordered_map<std::string, std::vector<MethodType>> type_method_types;
};

#endif  // JAK_TYPECONTAINER_H
