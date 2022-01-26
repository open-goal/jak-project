#pragma once

#include <string>

std::string float_to_string(float value, bool append_trailing_decimal = true);
std::string meters_to_string(float value, bool append_trailing_decimal = false);
int float_to_cstr(float value, char* buffer, bool append_trailing_decimal = true);
