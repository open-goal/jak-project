/*!
 * @file assert.h
 * Custom ASSERT macro
 */

#include "third-party/fmt/core.h"

#pragma once

[[noreturn]] void private_assert_failed(const char* expr,
                                        const char* file,
                                        int line,
                                        const char* function,
                                        const char* msg = "");

#ifdef _WIN32
#define __PRETTY_FUNCTION__ __FUNCSIG__
#endif

#define ASSERT(EX) \
  (void)((EX) || (private_assert_failed(#EX, __FILE__, __LINE__, __PRETTY_FUNCTION__), 0))

#define ASSERT_MSG(EXPR, STR) \
  (void)((EXPR) ||            \
         (private_assert_failed(#EXPR, __FILE__, __LINE__, __PRETTY_FUNCTION__, STR.c_str()), 0))
