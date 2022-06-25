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