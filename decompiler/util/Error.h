#pragma once

#include <stdexcept>

#include "third-party/fmt/core.h"

class Error : public std::runtime_error {
 public:
  template <typename... Args>
  Error(const std::string& format, Args&&... args)
      : std::runtime_error(fmt::format(format, std::forward<Args>(args)...)) {}
};