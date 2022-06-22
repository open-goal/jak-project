#pragma once

#define SIZEOF_SIZE_T 8
#define SIZEOF_UNSIGNED_LONG_LONG 8

#ifdef __cplusplus
extern "C" {
#endif
#include <assert.h>  // missing in xdelta3
#include "third-party/xdelta3/xdelta3/xdelta3.h"
#ifdef __cplusplus
}
#endif