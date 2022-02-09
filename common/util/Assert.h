/*!
 * @file assert.h
 * Custom ASSERT macro
 */

#pragma once

[[noreturn]] void private_assert_failed(const char* expr,
                                        const char* file,
                                        int line,
                                        const char* function);

#ifdef _WIN32
#define __PRETTY_FUNCTION__ __FUNCSIG__
#endif

#define ASSERT(EX) \
  (void)((EX) || (private_assert_failed(#EX, __FILE__, __LINE__, __PRETTY_FUNCTION__), 0))
