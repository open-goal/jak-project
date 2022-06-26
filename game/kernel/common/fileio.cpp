#include "fileio.h"

/*!
 * Copy a string from src to dst. The null terminator is copied too.
 * This is identical to normal strcpy.
 * DONE, EXACT
 */
void kstrcpy(char* dst, const char* src) {
  char* dst_ptr = dst;
  const char* src_ptr = src;

  while (*src_ptr != 0) {
    *dst_ptr = *src_ptr;
    src_ptr++;
    dst_ptr++;
  }
  *dst_ptr = 0;
}

/*!
 * Return pointer to null terminator of string.
 * const is for losers.
 * DONE, EXACT
 */
char* strend(char* str) {
  while (*str)
    str++;
  return str;
}

/*!
 * Get filename from path.
 * This function is renamed to basename_goal so it doesn't conflict with "basename" that is
 * already defined on my computer.
 * For example:
 *   a/b/c.e will return c.e
 *   a\b\c.e will return c.e
 *   asdf.asdf will return asdf.asdf
 *   DONE, EXACT
 */
char* basename_goal(char* s) {
  char* input = s;
  char* pt = s;

  // seek to end
  for (;;) {
    char c = *pt;
    if (c) {
      pt++;
    } else {
      break;
    }
  }

  /* Original code, has memory bug.
  // back up...
  for (;;) {
    if (pt < input) {
      return input;
    }
    pt--;
    char c = *pt;
    // until we hit a slash.
    if (c == '\\' || c == '/') {  // slashes
      return pt + 1;              // and return one past
    }
  }
   */

  // back up...
  for (;;) {
    if (pt <= input) {
      return input;
    }
    pt--;
    char c = *pt;
    // until we hit a slash.
    if (c == '\\' || c == '/') {  // slashes
      return pt + 1;              // and return one past
    }
  }
}