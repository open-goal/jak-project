#pragma once
#include "third-party/json.hpp"

std::string strip_cpp_style_comments(const std::string& input);
nlohmann::json parse_commented_json(const std::string& input);