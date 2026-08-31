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

// __FUNCSIG__ only exists under MSVC-compatible compilers (real MSVC, or clang-cl) -- plain
// clang++ in GNU-driver mode on Windows (e.g. MSYS2's CLANG64) already provides its own
// GCC-compatible __PRETTY_FUNCTION__ and doesn't have __FUNCSIG__ at all, so gating this on
// _MSC_VER (which both MSVC and clang-cl define) rather than bare _WIN32 is required.
#ifdef _MSC_VER
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

#define ASSERT_EQ_IMM(EXPR, EXPECTED) \
  ASSERT_MSG((EXPR) == (EXPECTED), fmt::format("result was {}, expected {}", (EXPR), (EXPECTED)))
#else

#define ASSERT(EX) ((void)0)
#define ASSERT_MSG(EXPR, STR) ((void)0)
#define ASSERT_NOT_REACHED() ((void)0)
#define ASSERT_NOT_REACHED_MSG(STR) ((void)0)
#define ASSERT_EQ_IMM(EXPR, EXPECTED) ((void)0)

#endif
