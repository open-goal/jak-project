#include "Assert.h"

#ifndef NO_ASSERT

#include <cstdio>
#include <cstdlib>
#include <string_view>

#include "common/log/log.h"

void private_assert_failed(const char* expr,
                           const char* file,
                           int line,
                           const char* function,
                           const char* msg) {
  if (!msg || msg[0] == '\0') {
    std::string log = fmt::format("Assertion failed: '{}'\n\tSource: {}:{}\n\tFunction: {}\n", expr,
                                  file, line, function);
    lg::die("{}", log);
  } else {
    std::string log =
        fmt::format("Assertion failed: '{}'\n\tMessage: {}\n\tSource: {}:{}\n\tFunction: {}\n",
                    expr, msg, file, line, function);
    lg::die("{}", log);
  }
#if defined(__SWITCH__)
  // -O3 inlines Ptr<T>::operator*, so return_address(0) is the real offending call site.
  // addr2line these against build-switch/game/gk to name it.
  lg::error("assert call site: {}", __builtin_return_address(0));
#endif
  abort();
}

void private_assert_failed(const char* expr,
                           const char* file,
                           int line,
                           const char* function,
                           const std::string_view& msg) {
  if (msg.empty()) {
    private_assert_failed(expr, file, line, function);
  } else {
    private_assert_failed(expr, file, line, function, msg.data());
  }
}

#endif
