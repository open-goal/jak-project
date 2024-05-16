#pragma once

#include <stdexcept>

#include "fmt/core.h"

class Error : public std::runtime_error {
 public:
  template <typename... Args>
  Error(const std::string& format, Args&&... args)
      : std::runtime_error(fmt::format(fmt::runtime(format), std::forward<Args>(args)...)) {}
};
