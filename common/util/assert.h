#pragma once

/*!
 * @file assert.h
 * Wrapper around <cassert>.
 */

#if defined NDEBUG && defined _WIN32

#pragma push_macro("NDEBUG")

#undef NDEBUG
#undef assert
#include <cassert>

#pragma pop_macro("NDEBUG")

#else

#include <cassert>

#endif

#define ASSERT assert
