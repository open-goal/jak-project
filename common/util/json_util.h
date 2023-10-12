#pragma once

#include <optional>
#include <stdexcept>

#include "common/util/Range.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

std::string strip_cpp_style_comments(const std::string& input);
std::optional<nlohmann::json> safe_parse_json(const std::string& input);
nlohmann::json parse_commented_json(const std::string& input, const std::string& source_name);
Range<int> parse_json_optional_integer_range(const nlohmann::json& json);

#define json_serialize(field_name) j[#field_name] = obj.field_name;

#define json_serialize_optional(field_name)  \
  if (obj.field_name) {                      \
    j[#field_name] = obj.field_name.value(); \
  }

#define json_deserialize_if_exists(field_name) \
  if (j.contains(#field_name)) {               \
    j.at(#field_name).get_to(obj.field_name);  \
  }

template <typename T>
void json_get_optional(const nlohmann::json& json,
                       const std::string& key,
                       std::optional<T>& optionalValue) {
  if (json.contains(key) && !json[key].is_null()) {
    optionalValue = json[key].get<T>();
  }
}

#define json_deserialize_optional_if_exists(field_name) \
  if (j.contains(#field_name)) {                        \
    json_get_optional(j, #field_name, obj.field_name);  \
  }
