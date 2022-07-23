#pragma once

#include <string>
#include <vector>

#ifdef _WIN32
std::wstring utf8_string_to_wide_string(const std::string_view& str);
bool utf8_string_to_wide_string(std::wstring& dest, const std::string_view& str);
std::string wide_string_to_utf8_string(const std::wstring_view& str);
bool wide_string_to_utf8_string(std::string& dest, const std::wstring_view& str);
#endif

std::string get_env(const std::string& name);
