#pragma once

#include <stdexcept>

class CompilerException : public std::runtime_error {
 public:
  CompilerException(const std::string& err) : std::runtime_error(err) {}
  bool print_err_stack = true;
};