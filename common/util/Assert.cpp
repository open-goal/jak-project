#include <cstdio>
#include <cstdlib>

#include "Assert.h"
#include <string_view>

void private_assert_failed(const char* expr,
                           const char* file,
                           int line,
                           const char* function,
                           const char* msg) {
  if (!msg || msg[0] == '\0') {
    fprintf(stderr, "Assertion failed: '%s'\n\tSource: %s:%d\n\tFunction: %s\n", expr, file, line,
            function);
  } else {
    fprintf(stderr, "Assertion failed: '%s'\n\tMessage: %s\n\tSource: %s:%d\n\tFunction: %s\n",
            expr, msg, file, line, function);
  }
  fflush(stdout);  // ensure any stdout logs are flushed before we terminate
  fflush(stderr);
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
