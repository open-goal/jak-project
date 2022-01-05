/*!
 * @file isocommon.cpp
 * Common ISO utilities.
 */

#include "common/common_types.h"
#include <cstring>
#include "isocommon.h"
#include "common/util/assert.h"

/*!
 * Convert file name to "ISO Name"
 * ISO names are upper case and 12 bytes long.
 * xxxxxxxxyyy0
 *
 * x - uppercase letter of file name, or space
 * y - uppercase letter of file extension, or space
 * 0 - null terminator (\0, not the character zero)
 */
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

/*!
 * Unmakes an ISO name back to the original name.
 * Keeps it upper case.
 * Not used.
 */
void UnmakeISOName(char* dst, const char* src) {
  int i = 0;
  const char* src_ptr = src;
  char* dst_ptr = dst;

  // copy non-space characters
  while ((i < 8) && (*src != ' ')) {
    *dst_ptr = *src_ptr;
    src_ptr++;
    dst_ptr++;
    i++;
  }

  // skip src to the extension
  src_ptr += 8 - i;

  if (*src_ptr != ' ') {
    // if there's an extension, add the period
    *dst_ptr = '.';
    i = 0;
    // copy extension
    dst_ptr++;
    while (i < 3 && *src_ptr != ' ') {
      *dst_ptr = *src_ptr;
      src_ptr++;
      i++;
    }
  }
  *dst_ptr = 0;
}

/*!
 * Convert an animation name to ISO name.
 * The animation name is a bunch of dash separated words.
 * The resulting ISO name has the same first two chars as the animation name, and one char from each
 * remaining word. Once there are no more words but remaining chars in the ISO name, the ith extra
 * char is the i+1 th char of the last word. A word ending in a number (or just a number) is turned
 * into the number. The word "resolution" becomes z. The word "accept" becomes y. The word "reject"
 * becomes n. Other words become the first char of the word. The result is uppercased and the file
 * extension is STR Examples (animation name and disc file name, not ISO name):
 *  green-sagecage-outro-beat-boss-enough-cells -> GRSOBBEC.STR
 *  swamp-tetherrock-swamprockexplode-4 -> SWTS4.STR
 *  minershort-resolution-1-orbs -> MIZ1ORBS.STR
 * @param dst
 * @param src
 */
void ISONameFromAnimationName(char* dst, const char* src) {
  // The Animation Name is a bunch of words separated by dashes

  // copy first two chars of the first word exactly
  dst[0] = src[0];
  dst[1] = src[1];
  s32 i = 2;  // 2 chars added to dst.

  // skip ahead to the first dash (or \0 if there's no dashes)
  const char* src_ptr = src;
  while (*src_ptr && *src_ptr != '-') {
    src_ptr++;
  }

  // the points to the next dash (or \0 if there's none).
  const char* next_ptr = src_ptr;
  if (*src_ptr) {
    // loop over words (next_ptr points to dash before word, i counts chars in dest)
    while (src_ptr = next_ptr + 1, i < 8) {
      // scan next_ptr forward to next dash
      next_ptr = src_ptr;
      while (*next_ptr && *next_ptr != '-') {
        next_ptr++;
      }

      // there's no next word, so break (the current word will be handled there)
      if (!*next_ptr)
        break;

      // add a char for the current word:
      char char_to_add;
      if (next_ptr[-1] < '0' || next_ptr[-1] > '9') {
        // word doesn't end in a number.

        // some special case words map to special letters (likely to avoid animation name conflicts)
        if (next_ptr - src_ptr == 10 && !memcmp(src_ptr, "resolution", 10)) {
          char_to_add = 'z';
        } else if (next_ptr - src_ptr == 6 && !memcmp(src_ptr, "accept", 6)) {
          char_to_add = 'y';
        } else if (next_ptr - src_ptr == 6 && !memcmp(src_ptr, "reject", 6)) {
          char_to_add = 'n';
        } else {
          // not a special case, just take the first letter.
          char_to_add = *src_ptr;
        }
      } else {
        // the current word ends in a number, just use this number (I think usually the whole word
        // is just a number)
        char_to_add = next_ptr[-1];
      }

      dst[i++] = char_to_add;
    }

    // here we ran out of room in dest, or words in source.
    // if there's still room in dest and chars in source, just add them
    while (*src_ptr && (i < 8)) {
      dst[i] = *src_ptr;
      src_ptr++;
      i++;
    }
  }

  // pad with spaces (for ISO Name)
  while (i < 8) {
    dst[i++] = ' ';
  }

  // upper case
  for (i = 0; i < 8; i++) {
    if (dst[i] > '`' && dst[i] < '{') {
      dst[i] -= 0x20;
    }
  }

  // append file extension
  strcpy(dst + 8, "STR");
}
