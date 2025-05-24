#pragma once
#include <algorithm>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

#include "common/util/Assert.h"

#include "fmt/core.h"

namespace decompiler {
class DecompWarnings {
 public:
  DecompWarnings() = default;

  template <typename... Args>
  void warning(const std::string& str, Args&&... args) {
    _warning(Warning::Kind::WARN, false, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void unique_warning(const std::string& str, Args&&... args) {
    _warning(Warning::Kind::WARN, true, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void error(const std::string& str, Args&&... args) {
    _warning(Warning::Kind::ERR, false, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void unique_error(const std::string& str, Args&&... args) {
    _warning(Warning::Kind::ERR, true, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void error_and_throw(const std::string& str, Args&&... args) {
    auto text = fmt::format(fmt::runtime(str), std::forward<Args>(args)...);
    _warning(Warning::Kind::ERR, false, text);
    throw std::runtime_error(text);
  }

  template <typename... Args>
  void info(const std::string& str, Args&&... args) {
    _warning(Warning::Kind::INFO, false, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void unique_info(const std::string& str, Args&&... args) {
    _warning(Warning::Kind::INFO, true, str, std::forward<Args>(args)...);
  }

  bool has_warnings() const { return !m_warnings.empty(); }

  bool has_errors() const {
    return !m_warnings.empty() &&
           std::any_of(m_warnings.begin(), m_warnings.end(),
                       [](const Warning& warn) { return warn.warning_kind == Warning::Kind::ERR; });
  }

  std::string get_warning_text(bool as_comment) const {
    std::string result;
    for (auto& w : m_warnings) {
      if (as_comment) {
        result += ";; ";
      }
      result += w.print();
    }
    return result;
  }

 private:
  // Add warnings without thinking about it, if you say they should be unique, only max of 1 will be
  // logged with that same text.
  std::unordered_set<std::string> unique_warnings;

  struct Warning {
    enum class Kind { INFO, WARN, ERR };
    Warning(Kind kind, std::string text) : warning_kind(kind), message(std::move(text)) {}

    std::string print() const {
      switch (warning_kind) {
        case Kind::INFO:
          return fmt::format("INFO: {}\n", message);
        case Kind::WARN:
          return fmt::format("WARN: {}\n", message);
        case Kind::ERR:
          return fmt::format("ERROR: {}\n", message);
        default:
          ASSERT(false);
          return {};
      }
    }

    Kind warning_kind;
    std::string message;
  };

  template <typename... Args>
  void _warning(Warning::Kind kind, bool unique, const std::string& str, Args&&... args) {
    std::string msg = fmt::format(fmt::runtime(str), std::forward<Args>(args)...);
    if (unique) {
      if (unique_warnings.find(msg) != unique_warnings.end()) {
        return;
      }
      unique_warnings.insert(msg);
    }
    Warning warn(kind, msg);
    m_warnings.push_back(warn);
  }

  std::vector<Warning> m_warnings;
};
}  // namespace decompiler
