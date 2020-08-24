/*!
 * @file TypeSpec.h
 * A TypeSpec is a reference to a Type, or possible a compound type.
 * A compound type contains a "root type", which must by a Type, and a list of "type arguments",
 * which are TypeSpecs.
 */

#ifndef JAK_TYPESPEC_H
#define JAK_TYPESPEC_H

#include <vector>
#include <string>

class Type;
class TypeSpec {
 public:
  // create a typespec for a single type
  TypeSpec() = default;
  TypeSpec(std::string type);
  TypeSpec(std::string type, std::vector<TypeSpec> arguments);

  bool operator!=(const TypeSpec& other) const;
  bool operator==(const TypeSpec& other) const;
  std::string print() const;

  void add_arg(const TypeSpec& ts) { m_arguments.push_back(ts); }

  const std::string base_type() const { return m_type; }

 private:
  std::string m_type;
  std::vector<TypeSpec> m_arguments;
};

#endif  // JAK_TYPESPEC_H
