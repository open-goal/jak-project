#include "Printer.h"

#include <cmath>
#include <mutex>

#include "common/goos/Object.h"
#include "common/util/print_float.h"

#include "third-party/fmt/core.h"

namespace pretty_print {
namespace {
// the integer representation is used here instead, wouldn't want really long numbers
const std::unordered_set<u32> banned_floats = {};

// print these floats (shown as ints here) as a named constant instead
const std::unordered_map<u32, std::string> const_floats = {{0x40490fda, "PI"},
                                                           {0xc0490fda, "MINUS_PI"}};
}  // namespace

/*!
 * Print a float in a nice representation if possible, or an exact 32-bit integer constant to
 * be reinterpreted.
 */
goos::Object float_representation(float value) {
  u32 int_value;
  memcpy(&int_value, &value, 4);
  if (!proper_float(value)) {
    // lg::warn("PS2-incompatible float (0x{:08X}) detected! Writing as the-as cast.", int_value);
    return pretty_print::build_list("the-as", "float", fmt::format("#x{:x}", int_value));
  } else if (const_floats.find(int_value) != const_floats.end()) {
    return pretty_print::to_symbol(const_floats.at(int_value));
  } else if (banned_floats.find(int_value) == banned_floats.end()) {
    return goos::Object::make_float(value);
  } else {
    return pretty_print::build_list("the-as", "float", fmt::format("#x{:x}", int_value));
  }
}

std::unique_ptr<goos::Reader> pretty_printer_reader;
std::mutex pretty_printer_reader_mutex;

goos::Reader& get_pretty_printer_reader() {
  if (!pretty_printer_reader) {
    pretty_printer_reader = std::make_unique<goos::Reader>();
  }
  return *pretty_printer_reader;
}

goos::Object to_symbol(const std::string& str) {
  std::lock_guard<std::mutex> guard(pretty_printer_reader_mutex);
  return goos::Object::make_symbol(&get_pretty_printer_reader().symbolTable, str.c_str());
}

goos::Object new_string(const std::string& str) {
  return goos::StringObject::make_new(str);
}

goos::Object build_list(const std::string& str) {
  return build_list(to_symbol(str));
}

goos::Object build_list(const goos::Object& obj) {
  return goos::PairObject::make_new(obj, goos::Object::make_empty_list());
}

goos::Object build_list(const std::vector<goos::Object>& objects) {
  if (objects.empty()) {
    return goos::Object::make_empty_list();
  } else {
    return build_list(objects.data(), objects.size());
  }
}

// build a list out of an array of forms
goos::Object build_list(const goos::Object* objects, int count) {
  ASSERT(count);
  goos::Object result = goos::Object::make_empty_list();
  for (int i = count; i-- > 0;) {
    result = goos::PairObject::make_new(objects[i], result);
  }

  return result;
}

// build a list out of a vector of strings that are converted to symbols
goos::Object build_list(const std::vector<std::string>& symbols) {
  if (symbols.empty()) {
    return goos::Object::make_empty_list();
  }
  std::vector<goos::Object> f;
  f.reserve(symbols.size());
  for (auto& x : symbols) {
    f.push_back(to_symbol(x));
  }
  return build_list(f.data(), f.size());
}

void append(goos::Object& _in, const goos::Object& add) {
  auto* in = &_in;
  while (in->is_pair() && !in->as_pair()->cdr.is_empty_list()) {
    in = &in->as_pair()->cdr;
  }

  if (!in->is_pair()) {
    ASSERT(false);  // invalid list
  }
  in->as_pair()->cdr = add;
}

}  // namespace pretty_print
