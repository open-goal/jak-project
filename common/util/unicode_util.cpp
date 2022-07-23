#include "unicode_util.h"

// clang-format off
#ifdef _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <Stringapiset.h>
#include <processenv.h>
#include <winbase.h>
#include <shellapi.h>
#undef FALSE
#endif
// clang-format on

#ifdef _WIN32
std::wstring utf8_string_to_wide_string(const std::string_view& str) {
  std::wstring ret;
  if (!utf8_string_to_wide_string(ret, str))
    return {};

  return ret;
}

bool utf8_string_to_wide_string(std::wstring& dest, const std::string_view& str) {
  int wlen =
      MultiByteToWideChar(CP_UTF8, 0, str.data(), static_cast<int>(str.length()), nullptr, 0);
  if (wlen < 0)
    return false;

  dest.resize(wlen);
  if (wlen > 0 && MultiByteToWideChar(CP_UTF8, 0, str.data(), static_cast<int>(str.length()),
                                      dest.data(), wlen) < 0)
    return false;

  return true;
}

std::string wide_string_to_utf8_string(const std::wstring_view& str) {
  std::string ret;
  if (!wide_string_to_utf8_string(ret, str))
    ret.clear();

  return ret;
}

bool wide_string_to_utf8_string(std::string& dest, const std::wstring_view& str) {
  int mblen = WideCharToMultiByte(CP_UTF8, 0, str.data(), static_cast<int>(str.length()), nullptr,
                                  0, nullptr, nullptr);
  if (mblen < 0)
    return false;

  dest.resize(mblen);
  if (mblen > 0 && WideCharToMultiByte(CP_UTF8, 0, str.data(), static_cast<int>(str.length()),
                                       dest.data(), mblen, nullptr, nullptr) < 0) {
    return false;
  }

  return true;
}
#endif

std::string get_env(const std::string& name) {
#ifdef _WIN32
  auto name_wide = utf8_string_to_wide_string(name);
  auto val = _wgetenv(name_wide.data());
  if (!val) {
    return "";
  }
  return wide_string_to_utf8_string(val);
#else
  auto val = std::getenv(name.data());
  if (!val) {
    return "";
  }
  return val;
#endif
}
