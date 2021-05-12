#pragma once

/*!
 * @file assert.h
 * Wrapper around <cassert>.
 */

#if defined NDEBUG && defined _WIN32

#define _OLD_NDEBUG NDEBUG

#undef NDEBUG
#include <cassert>
#define NDEBUG _OLD_NDEBUG

#undef _OLD_NDEBUG

#else

#include <cassert>

#endif
