#pragma once

/*!
 * @file TypeSpec.h
 * A GOAL TypeSpec is a reference to a type or compound type.
 */

#include <optional>
#include <string>
#include <vector>

#include "common/util/Assert.h"
#include "common/util/SmallVector.h"

/*!
 * A :name value modifier to apply to a type.
 */
struct TypeTag {
  std::string name;
  std::string value;

  bool operator==(const TypeTag& other) const;
  bool operator!=(const TypeTag& other) const { return !((*this) == other); }
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
      : m_type(type), m_arguments(new std::vector<TypeSpec>(arguments)) {}

  TypeSpec(const TypeSpec& other) {
    m_type = other.m_type;
    m_tags = other.m_tags;
    if (other.m_arguments) {
      m_arguments = new std::vector<TypeSpec>(*other.m_arguments);
    }
  }

  TypeSpec& operator=(const TypeSpec& other) {
    if (this == &other) {
      return *this;
    }

    if (m_arguments) {
      delete m_arguments;
      m_arguments = nullptr;
    }

    m_type = other.m_type;
    m_tags = other.m_tags;
    if (other.m_arguments) {
      m_arguments = new std::vector<TypeSpec>(*other.m_arguments);
    }

    return *this;
  }

  ~TypeSpec() { delete m_arguments; }

  //  TypeSpec(const std::string& type, const std::vector<TypeTag>& tags)
  //      : m_type(type), m_tags(tags) {}
  //
  //  TypeSpec(const std::string type,
  //           const std::vector<TypeSpec>& arguments,
  //           const std::vector<TypeTag>& tags)
  //      : m_type(type), m_arguments(arguments), m_tags(tags) {}

  bool operator!=(const TypeSpec& other) const;
  bool operator==(const TypeSpec& other) const;
  bool is_compatible_child_method(const TypeSpec& implementation,
                                  const std::string& child_type,
                                  int* bad_arg_idx_out = nullptr) const;
  std::string print() const;

  void add_arg(const TypeSpec& ts) {
    if (!m_arguments) {
      m_arguments = new std::vector<TypeSpec>();
    }
    m_arguments->push_back(ts);
  }
  void add_new_tag(const std::string& tag_name, const std::string& tag_value);
  std::optional<std::string> try_get_tag(const std::string& tag_name) const;
  const std::string& get_tag(const std::string& tag_name) const;
  void modify_tag(const std::string& tag_name, const std::string& tag_value);
  void add_or_modify_tag(const std::string& tag_name, const std::string& tag_value);
  void delete_tag(const std::string& tag_name);

  const std::string& base_type() const { return m_type; }

  bool has_single_arg() const {
    if (m_arguments) {
      return m_arguments->size() == 1;
    }
    return 0;
  }

  const TypeSpec& get_single_arg() const {
    ASSERT(m_arguments);
    ASSERT(m_arguments->size() == 1);
    return m_arguments->front();
  }

  TypeSpec substitute_for_method_call(const std::string& method_type) const;

  size_t arg_count() const {
    if (!m_arguments) {
      return 0;
    }
    return m_arguments->size();
  }

  const TypeSpec& get_arg(int idx) const {
    ASSERT(m_arguments);
    return m_arguments->at(idx);
  }

  TypeSpec& get_arg(int idx) {
    ASSERT(m_arguments);
    return m_arguments->at(idx);
  }

  const TypeSpec& last_arg() const {
    ASSERT(m_arguments);
    ASSERT(!m_arguments->empty());
    return m_arguments->back();
  }

  TypeSpec& last_arg() {
    ASSERT(m_arguments);
    ASSERT(!m_arguments->empty());
    return m_arguments->back();
  }

  bool empty() const {
    if (!m_arguments) {
      return true;
    } else {
      return m_arguments->empty();
    }
  }

  const std::vector<TypeTag>& tags() const { return m_tags; }

 private:
  friend class TypeSystem;
  std::string m_type;
  // hiding this behind a pointer makes things faster in the case where we have no
  // arguments (most of the time) and makes the type analysis pass in the decompiler 2x faster.
  std::vector<TypeSpec>* m_arguments = nullptr;
  std::vector<TypeTag> m_tags;
};
