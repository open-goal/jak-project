#pragma once

#include <string>

std::string float_to_string(float value, bool append_trailing_decimal = true);
int float_to_cstr(float value, char* buffer, bool append_trailing_decimal = true);