#pragma once

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "Object.h"

namespace goos {
bool get_va(const goos::Object& rest, std::string* err_string, goos::Arguments* result);
void get_va_no_named(const goos::Object& rest, goos::Arguments* result);
bool va_check(
    const goos::Arguments& args,
    const std::vector<std::optional<goos::ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, std::optional<goos::ObjectType>>>& named,
    std::string* err_string);

template <typename T>
void for_each_in_list(const goos::Object& list, T f) {
  const goos::Object* iter = &list;
  while (iter->is_pair()) {
    const auto& lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  ASSERT(iter->is_empty_list());
}

int list_length(const goos::Object& list);
}  // namespace goos
