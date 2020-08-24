#ifndef JAK_TYPESYSTEM_H
#define JAK_TYPESYSTEM_H

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <memory>

#include "TypeSpec.h"
#include "Type.h"

class TypeSystem {
 public:
  TypeSystem();

  void add_builtin_types();
  void add_builtin_globals();

  void forward_declare_type(std::string name);
  Type* add_type(const std::string& name, std::unique_ptr<Type> type);
  MethodInfo add_method(Type* type, const std::string& method_name, const TypeSpec& ts);
  MethodInfo lookup_method(const std::string& type_name, const std::string& method_name);

  Field lookup_field(const std::string& type_name, const std::string& field_name);

  MethodInfo add_new_method(Type* type, const TypeSpec& ts);
  MethodInfo lookup_new_method(const std::string& type_name);

  int add_field_to_type(StructureType* type,
                        const std::string& field_name,
                        const TypeSpec& field_type,
                        bool is_inline = false,
                        bool is_dynamic = false,
                        int array_size = -1,
                        int offset_override = -1);

  Type* lookup_type(const TypeSpec& ts);
  Type* lookup_type(const std::string& name);
  TypeSpec make_typespec(const std::string& name);
  TypeSpec make_function_typespec(const std::vector<std::string>& arg_types,
                                  const std::string& return_type);

  void assert_method_id(const std::string& type_name, const std::string& method_name, int id);
  void assert_field_offset(const std::string& type_name, const std::string& field_name, int offset);

  std::string print_all_type_information() const;

  template <typename T>
  T* get_type_of_type(const std::string& type_name) {
    auto x = lookup_type(type_name);
    T* result = dynamic_cast<T*>(x);
    if (!result) {
      throw std::runtime_error("Failed to get " + type_name + " as the right type");
    }
    return result;
  }

  int get_size_in_type(const Field& field);
  int get_alignment_in_type(const Field& field);

 private:
  int get_next_method_id(Type* type);
  int manual_add_field_to_type(StructureType* type,
                               const std::string& field_name,
                               const TypeSpec& field_type,
                               int offset,
                               int size,
                               int alignment);

  StructureType* add_builtin_structure(const std::string& parent, const std::string& type_name);
  BasicType* add_builtin_basic(const std::string& parent, const std::string& type_name);
  ValueType* add_builtin_value_type(const std::string& parent,
                                    const std::string& type_name,
                                    int size,
                                    bool boxed = false,
                                    bool sign_extend = false,
                                    RegKind reg = RegKind::GPR_64);
  void builtin_structure_inherit(StructureType* st);

  std::unordered_map<std::string, std::unique_ptr<Type>> m_types;
  std::unordered_map<std::string, Type*> m_global_types;
  std::unordered_set<std::string> m_forward_declared_types;
  std::vector<std::unique_ptr<Type>> m_old_types;

  bool m_allow_redefinition = false;
};

#endif  // JAK_TYPESYSTEM_H
