#pragma once

#include <stdexcept>

#include "common/util/Range.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

std::string strip_cpp_style_comments(const std::string& input);
nlohmann::json parse_commented_json(const std::string& input, const std::string& source_name);
Range<int> parse_json_optional_integer_range(const nlohmann::json& json);

// TODO - does nlohmann have a function to basically do this?
#define json_safe_deserialize(field_name)     \
  if (j.contains(#field_name)) {              \
    j.at(#field_name).get_to(obj.field_name); \
  }
