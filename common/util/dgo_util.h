#pragma once

#include <string>
#include "common/common_types.h"

void assert_string_empty_after(const char* str, int size);
std::string get_object_file_name(const std::string& original_name, u8* data, int size);