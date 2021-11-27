#pragma once


#include <string>
#include <vector>
#include "common/goos/Object.h"
#include "common/goos/Reader.h"
#include "common/goos/Printer.h"

namespace pretty_print {
// main pretty print function
std::string to_string_v2(const goos::Object& obj, int line_length = 110);

}  // namespace pretty_print
