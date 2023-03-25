#pragma once

#include <string>
#include <vector>

#include "common/goos/Object.h"
#include "common/goos/Printer.h"
#include "common/goos/Reader.h"

namespace pretty_print {
// main pretty print function
std::string to_string(const goos::Object& obj, int line_length = 110);

}  // namespace pretty_print
