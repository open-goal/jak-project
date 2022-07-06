#pragma once

#include <string>
#include <vector>

std::string utf8_from_utf16(const wchar_t* utf16_string);
std::vector<std::string> get_widechar_cli_args();
