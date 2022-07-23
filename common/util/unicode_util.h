#pragma once

#include <string>
#include <string_view>
#include <vector>

#include "common/util/FileUtil.h"

#ifdef _WIN32
std::wstring utf8_string_to_wide_string(const std::string_view& str);
bool utf8_string_to_wide_string(std::wstring& dest, const std::string_view& str);
std::string wide_string_to_utf8_string(const std::wstring_view& str);
bool wide_string_to_utf8_string(std::string& dest, const std::wstring_view& str);
#endif

std::string get_env(const std::string& name);

/// @brief Windows's command line args are not UTF-8 so they need to be converted
/// ghc::filesystem comes with an u8guard class to help with this, but on linux it calls
/// setlocale
///
/// This is problematic for several annoying reasons, so we are going off the (hopefully sane)
/// assumption that linux is using utf-8 by default and that the args are already proper
///
/// This may need to be updated on linux if that assumption proves wrong.
class ArgumentGuard {
 public:
#ifdef _WIN32
  ArgumentGuard(int& argc, char**& argv) : u8_guard(argc, argv) {
    if (!u8_guard.valid()) {
      exit(EXIT_FAILURE);
    }
  }
  fs::u8arguments u8_guard;
#else
  // if linux
  ArgumentGuard(int&, char**&) {}
#endif
};
