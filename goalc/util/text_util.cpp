/*!
 * @file text_util.cpp
 * Utilities for dealing with text.
 */

#include "text_util.h"

namespace util {
/*!
 * Is c printable? Is true for letters, numbers, symbols, space, false for everything else.
 * Note: newline/tab is not considered printable.
 */
bool is_printable_char(char c) {
  return c >= ' ' && c <= '~';
}
}  // namespace util
