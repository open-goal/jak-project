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

std::string utf8_from_utf16(const wchar_t* utf16_string) {
#ifdef _WIN32
  if (utf16_string == nullptr) {
    return std::string();
  }
  int target_length = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, utf16_string, -1, nullptr,
                                          0, nullptr, nullptr);
  if (target_length == 0) {
    return std::string();
  }
  std::string utf8_string;
  utf8_string.resize(target_length);
  int converted_length = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, utf16_string, -1,
                                             utf8_string.data(), target_length, nullptr, nullptr);
  if (converted_length == 0) {
    return std::string();
  }
  return utf8_string;
#else
  return "don't call this on linux";
#endif
}

std::vector<std::string> get_widechar_cli_args() {
#ifdef _WIN32
  // Convert the UTF-16 command line arguments to UTF-8 for the Engine to use.
  int argc;
  wchar_t** argv = CommandLineToArgvW(GetCommandLineW(), &argc);
  if (argv == nullptr) {
    return {};
  }

  std::vector<std::string> command_line_arguments;

  for (int i = 0; i < argc; i++) {
    command_line_arguments.push_back(utf8_from_utf16(argv[i]));
  }

  LocalFree(argv);

  return command_line_arguments;
#else
  return {"don't call this on linux"};
#endif
}
