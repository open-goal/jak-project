/*!
 * @file assert.h
 * Wrapper around <cassert>.
 * Make sure this file is always the last one included.
 */

#if defined NDEBUG

#undef NDEBUG
#undef assert
#include <cassert>

#define NDEBUG 1

#else

#include <cassert>

#endif
