#pragma once

#include <stdexcept>
#include "common/util/Range.h"
#include "third-party/json.hpp"

std::string strip_cpp_style_comments(const std::string& input);
nlohmann::json parse_commented_json(const std::string& input);
Range<int> parse_json_optional_integer_range(const nlohmann::json& json);
