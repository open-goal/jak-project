/*!
 * @file assert.h
 * Custom ASSERT macro
 */

#pragma once

#ifndef NO_ASSERT

#include <string_view>

[[noreturn]] void private_assert_failed(const char* expr,
                                        const char* file,
                                        int line,
                                        const char* function,
                                        const char* msg = "");

[[noreturn]] void private_assert_failed(const char* expr,
                                        const char* file,
                                        int line,
                                        const char* function,
                                        const std::string_view& msg);

#ifdef _WIN32
#define __PRETTY_FUNCTION__ __FUNCSIG__
#endif

#define ASSERT(EX) \
  (void)((EX) || (private_assert_failed(#EX, __FILE__, __LINE__, __PRETTY_FUNCTION__), 0))

#define ASSERT_NOT_REACHED() \
  (void)((private_assert_failed("not reached", __FILE__, __LINE__, __PRETTY_FUNCTION__), 0))

#define ASSERT_MSG(EXPR, STR) \
  (void)((EXPR) || (private_assert_failed(#EXPR, __FILE__, __LINE__, __PRETTY_FUNCTION__, STR), 0))

#define ASSERT_NOT_REACHED_MSG(STR) \
  (void)((private_assert_failed("not reached", __FILE__, __LINE__, __PRETTY_FUNCTION__, STR), 0))
#else

#define ASSERT(EX) ((void)0)
#define ASSERT_MSG(EXPR, STR) ((void)0)

#endif
