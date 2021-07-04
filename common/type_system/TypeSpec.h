#pragma once

/*!
 * @file TypeSpec.h
 * A GOAL TypeSpec is a reference to a type or compound type.
 */

#include <vector>
#include <string>
#include <optional>
#include "common/util/assert.h"

/*!
 * A :name value modifier to apply to a type.
 */
struct TypeTag {
  std::string name;
  std::string value;

  bool operator==(const TypeTag& other) const;
};

/*!
 * A TypeSpec is a reference to a Type, or possible a compound type.  This is the best way to
 * refer to a type, as it supports compound types and also will work correctly after a type has been
 * redefined. Doing sane things after types change will make debugging GOAL code more pleasant.
 *
 * A compound type contains a "root type", which must by a Type, and a list of "type
 * arguments", which are TypeSpecs.
 */
class TypeSpec {
 public:
  TypeSpec() = default;
  TypeSpec(const std::string& type) : m_type(type) {}

  TypeSpec(const std::string& type, const std::vector<TypeSpec>& arguments)
      : m_type(type), m_arguments(arguments) {}

  TypeSpec(const std::string& type, const std::vector<TypeTag>& tags)
      : m_type(type), m_tags(tags) {}

  TypeSpec(const std::string type,
           const std::vector<TypeSpec>& arguments,
           const std::vector<TypeTag>& tags)
      : m_type(type), m_arguments(arguments), m_tags(tags) {}

  bool operator!=(const TypeSpec& other) const;
  bool operator==(const TypeSpec& other) const;
  bool is_compatible_child_method(const TypeSpec& implementation,
                                  const std::string& child_type) const;
  std::string print() const;

  void add_arg(const TypeSpec& ts) { m_arguments.push_back(ts); }
  void add_new_tag(const std::string& tag_name, const std::string& tag_value);
  std::optional<std::string> try_get_tag(const std::string& tag_name) const;
  const std::string& get_tag(const std::string& tag_name) const;
  void modify_tag(const std::string& tag_name, const std::string& tag_value);
  void add_or_modify_tag(const std::string& tag_name, const std::string& tag_value);

  const std::string base_type() const { return m_type; }

  bool has_single_arg() const { return m_arguments.size() == 1; }

  const TypeSpec& get_single_arg() const {
    assert(m_arguments.size() == 1);
    return m_arguments.front();
  }

  TypeSpec substitute_for_method_call(const std::string& method_type) const;

  size_t arg_count() const { return m_arguments.size(); }

  const TypeSpec& get_arg(int idx) const { return m_arguments.at(idx); }
  TypeSpec& get_arg(int idx) { return m_arguments.at(idx); }
  const TypeSpec& last_arg() const {
    assert(!m_arguments.empty());
    return m_arguments.back();
  }

  const std::vector<TypeTag>& tags() const { return m_tags; }

 private:
  friend class TypeSystem;
  std::string m_type;
  std::vector<TypeSpec> m_arguments;
  std::vector<TypeTag> m_tags;
};
