#pragma once

/*!
 * @file PrettyPrinter.h
 * A Pretty Printer for GOOS.
 * It is not very good, but significantly better than putting everything on one line
 */

#include <string>
#include <vector>

#include "common/goos/Object.h"
#include "common/goos/PrettyPrinter2.h"
#include "common/goos/Printer.h"
#include "common/goos/Reader.h"

namespace pretty_print {
// main pretty print function
std::string to_string_v1(const goos::Object& obj, int line_length = 80);

}  // namespace pretty_print
