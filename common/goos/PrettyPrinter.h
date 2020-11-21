#pragma once

/*!
 * @file PrettyPrinter.h
 * A Pretty Printer for GOOS.
 * It is not very good, but significantly better than putting everything on one line
 */

#include <string>
#include <vector>
#include "common/goos/Object.h"
#include "common/goos/Reader.h"

namespace pretty_print {
// main pretty print function
std::string to_string(const goos::Object& obj, int line_length = 80);

// string -> object (as a symbol)
goos::Object to_symbol(const std::string& str);

// list with a single symbol from a string
goos::Object build_list(const std::string& str);

// wrap an object in a list
goos::Object build_list(const goos::Object& obj);

// build a list out of a vector of forms
goos::Object build_list(const std::vector<goos::Object>& objects);

// build a list out of an array of forms
goos::Object build_list(const goos::Object* objects, int count);

// build a list out of a vector of strings that are converted to symbols
goos::Object build_list(const std::vector<std::string>& symbols);

// fancy wrapper functions.  Due to template magic these can call each other
// and accept mixed arguments!

template <typename... Args>
goos::Object build_list(const std::string& str, Args... rest) {
  return goos::PairObject::make_new(to_symbol(str), build_list(rest...));
}

template <typename... Args>
goos::Object build_list(const goos::Object& car, Args... rest) {
  return goos::PairObject::make_new(car, build_list(rest...));
}

goos::Reader& get_pretty_printer_reader();

}  // namespace pretty_print
