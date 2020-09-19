#pragma once

#ifndef JAK_TYPESYSTEM_H
#define JAK_TYPESYSTEM_H

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <memory>

#include "TypeSpec.h"
#include "Type.h"

struct FieldLookupInfo {
  Field field;
  TypeSpec type;
  bool needs_deref = true;
  int array_size = -1;
};

struct DerefInfo {
  bool can_deref = false;
  bool mem_deref = false;
  bool sign_extend = false;
  RegKind reg = RegKind::INVALID;
  int stride = -1;
  int load_size = -1;
  TypeSpec result_type;
};

class TypeSystem {
 public:
  TypeSystem();

  Type* add_type(const std::string& name, std::unique_ptr<Type> type);
  void forward_declare_type(std::string name);
  std::string get_runtime_type(const TypeSpec& ts);

  DerefInfo get_deref_info(const TypeSpec& ts);

  bool fully_defined_type_exists(const std::string& name) const;
  TypeSpec make_typespec(const std::string& name) const;
  TypeSpec make_function_typespec(const std::vector<std::string>& arg_types,
                                  const std::string& return_type);

  TypeSpec make_pointer_typespec(const std::string& type);
  TypeSpec make_pointer_typespec(const TypeSpec& type);
  TypeSpec make_inline_array_typespec(const std::string& type);
  TypeSpec make_inline_array_typespec(const TypeSpec& type);

  Type* lookup_type(const TypeSpec& ts) const;
  Type* lookup_type(const std::string& name) const;

  MethodInfo add_method(const std::string& type_name,
                        const std::string& method_name,
                        const TypeSpec& ts);
  MethodInfo add_method(Type* type, const std::string& method_name, const TypeSpec& ts);
  MethodInfo add_new_method(Type* type, const TypeSpec& ts);
  MethodInfo lookup_method(const std::string& type_name, const std::string& method_name);
  MethodInfo lookup_new_method(const std::string& type_name);
  void assert_method_id(const std::string& type_name, const std::string& method_name, int id);

  FieldLookupInfo lookup_field_info(const std::string& type_name, const std::string& field_name);
  void assert_field_offset(const std::string& type_name, const std::string& field_name, int offset);
  int add_field_to_type(StructureType* type,
                        const std::string& field_name,
                        const TypeSpec& field_type,
                        bool is_inline = false,
                        bool is_dynamic = false,
                        int array_size = -1,
                        int offset_override = -1);

  void add_builtin_types();

  std::string print_all_type_information() const;
  bool typecheck(const TypeSpec& expected,
                 const TypeSpec& actual,
                 const std::string& error_source_name = "",
                 bool print_on_error = true,
                 bool throw_on_error = true) const;
  std::vector<std::string> get_path_up_tree(const std::string& type);
  int get_next_method_id(Type* type);

  /*!
   * Get a type by name and cast to a child class of Type*. Must succeed.
   */
  template <typename T>
  T* get_type_of_type(const std::string& type_name) {
    auto x = lookup_type(type_name);
    T* result = dynamic_cast<T*>(x);
    if (!result) {
      throw std::runtime_error("Failed to get " + type_name + " as the right type");
    }
    return result;
  }

  TypeSpec lowest_common_ancestor(const TypeSpec& a, const TypeSpec& b);
  TypeSpec lowest_common_ancestor(const std::vector<TypeSpec>& types);

 private:
  std::string lca_base(const std::string& a, const std::string& b);
  bool typecheck_base_types(const std::string& expected, const std::string& actual) const;
  int get_size_in_type(const Field& field);
  int get_alignment_in_type(const Field& field);
  Field lookup_field(const std::string& type_name, const std::string& field_name);
  StructureType* add_builtin_structure(const std::string& parent,
                                       const std::string& type_name,
                                       bool boxed = false);
  BasicType* add_builtin_basic(const std::string& parent, const std::string& type_name);
  ValueType* add_builtin_value_type(const std::string& parent,
                                    const std::string& type_name,
                                    int size,
                                    bool boxed = false,
                                    bool sign_extend = false,
                                    RegKind reg = RegKind::GPR_64);
  void builtin_structure_inherit(StructureType* st);

  std::unordered_map<std::string, std::unique_ptr<Type>> m_types;
  std::unordered_set<std::string> m_forward_declared_types;
  std::vector<std::unique_ptr<Type>> m_old_types;

  bool m_allow_redefinition = false;
};

TypeSpec coerce_to_reg_type(const TypeSpec& in);

#endif  // JAK_TYPESYSTEM_H
