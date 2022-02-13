#include <cstdio>
#include <cstdlib>

#include "Assert.h"

void private_assert_failed(const char* expr, const char* file, int line, const char* function) {
  fprintf(stderr, "%s:%d: Assertion failed: %s\nFunction: %s\n", file, line, expr, function);
  abort();
}
