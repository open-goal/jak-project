#pragma once

#include <string>
#include "common/common_types.h"

std::string fixed_point_to_string(s64 value, s64 scale, bool append_trailing_decimal = false);
std::string float_to_string(float value, bool append_trailing_decimal = true);
std::string meters_to_string(float value, bool append_trailing_decimal = false);
int float_to_cstr(float value, char* buffer, bool append_trailing_decimal = true);
