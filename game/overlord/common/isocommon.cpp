#include "isocommon.h"

void MakeISOName(char* dst, const char* src) {
  int i = 0;
  const char* src_ptr = src;
  char* dst_ptr = dst;

  // copy name and upper case
  while ((i < 8) && (*src_ptr) && (*src_ptr != '.')) {
    char c = *src_ptr;
    src_ptr++;
    if (('`' < c) && (c < '{')) {  // lower case
      c -= 0x20;
    }
    *dst_ptr = c;
    dst_ptr++;
    i++;
  }

  // pad out name with spaces
  while (i < 8) {
    *dst_ptr = ' ';
    dst_ptr++;
    i++;
  }

  // increment past period
  if (*src_ptr == '.')
    src_ptr++;

  // same for extension
  while (i < 11 && (*src_ptr)) {
    char c = *src_ptr;
    src_ptr++;
    if (('`' < c) && (c < '{')) {  // lower case
      c -= 0x20;
    }
    *dst_ptr = c;
    dst_ptr++;
    i++;
  }

  while (i < 11) {
    *dst_ptr = ' ';
    dst_ptr++;
    i++;
  }
  *dst_ptr = 0;
}
