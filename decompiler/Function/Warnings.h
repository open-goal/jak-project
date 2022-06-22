#pragma once
#include <stdexcept>
#include <string>
#include <vector>

#include "common/util/Assert.h"

#include "third-party/fmt/core.h"

namespace decompiler {
class DecompWarnings {
 public:
  DecompWarnings() = default;

  template <typename... Args>
  void general_warning(const std::string& str, Args&&... args) {
    warning(Warning::Kind::GENERAL, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void warn_and_throw(const std::string& str, Args&&... args) {
    auto text = fmt::format(str, std::forward<Args>(args)...);
    warning(Warning::Kind::GENERAL, text);
    throw std::runtime_error(text);
  }

  template <typename... Args>
  void expression_build_warning(const std::string& str, Args&&... args) {
    warning(Warning::Kind::EXPR_BUILD_FAILED, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void cfg_build_warning(const std::string& str, Args&&... args) {
    warning(Warning::Kind::CFG_FAILED, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void type_prop_warning(const std::string& str, Args&&... args) {
    warning(Warning::Kind::TYPE_PROP_FAILED, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void bad_vf_dependency(const std::string& str, Args&&... args) {
    warning(Warning::Kind::BAD_VF_DEPENDENCY, str, std::forward<Args>(args)...);
  }

  template <typename... Args>
  void info(const std::string& str, Args&&... args) {
    warning(Warning::Kind::INFO, str, std::forward<Args>(args)...);
  }

  bool has_warnings() const { return !m_warnings.empty() || m_used_lq_sq; }
  void warn_sq_lq() { m_used_lq_sq = true; }

  std::string get_warning_text(bool as_comment) const {
    std::string result;
    for (auto& w : m_warnings) {
      if (as_comment) {
        result += ";; ";
      }
      result += w.print();
    }
    if (m_used_lq_sq) {
      result += ";; Used lq/sq\n";
    }
    return result;
  }

 private:
  struct Warning {
    enum class Kind {
      GENERAL,
      EXPR_BUILD_FAILED,
      CFG_FAILED,
      TYPE_PROP_FAILED,
      INFO,
      BAD_VF_DEPENDENCY
    };
    Warning(Kind kind, std::string text) : warning_kind(kind), message(std::move(text)) {}

    std::string print() const {
      switch (warning_kind) {
        case Kind::GENERAL:
          return fmt::format("WARN: {}\n", message);
        case Kind::EXPR_BUILD_FAILED:
          return fmt::format("WARN: Expression building failed: {}\n", message);
        case Kind::CFG_FAILED:
          return fmt::format("WARN: CFG building failed: {}\n", message);
        case Kind::TYPE_PROP_FAILED:
          return fmt::format("WARN: Type Propagation failed: {}\n", message);
        case Kind::BAD_VF_DEPENDENCY:
          return fmt::format("WARN: Bad vector register dependency: {}\n", message);
        case Kind::INFO:
          return fmt::format("INFO: {}\n", message);
        default:
          ASSERT(false);
          return {};
      }
    }

    Kind warning_kind;
    std::string message;
  };

  template <typename... Args>
  void warning(Warning::Kind kind, const std::string& str, Args&&... args) {
    Warning warn(kind, fmt::format(str, std::forward<Args>(args)...));
    m_warnings.push_back(warn);
  }

  std::vector<Warning> m_warnings;
  bool m_used_lq_sq = false;
};
}  // namespace decompiler
