#include "isocommon.h"

#include "common/util/Assert.h"

namespace jak3 {
void jak3_overlord_init_globals_isocommon() {}

/*!
 * Convert file name to "ISO Name"
 * ISO names are upper case and 12 bytes long.
 * xxxxxxxxyyy0
 *
 * x - uppercase letter of file name, or space
 * y - uppercase letter of file extension, or space
 * 0 - null terminator (\0, not the character zero)
 */
void MakeISOName(ISOName* dst, const char* src) {
  int i = 0;
  const char* src_ptr = src;
  char* dst_ptr = dst->data;

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

// UnmakeISOName

/*!
 * Pack an 8-character (64-bits) file name into a packed vag file name (32 + 10 = 42 bit).
 */
int PackVAGFileName(u32* out, const char* name) {
  if (!out || !name) {
    return 0;
  }
  int ret = 1;

  // accumulator of up to 4 packed characters
  u32 acc = 0;
  u32 first_four = 0;
  for (int i = 0; i < 8; i++) {
    // start the second word:
    if (i == 4) {
      first_four = acc;
      acc = 0;
    }

    // read character from input
    u32 name_char = *((const u8*)name);
    name++;

    u32 remapped_char;

    if (name_char - 'A' < 26) {              // capital letter
      remapped_char = name_char - 'A' + 1;   // so A becomes 1.
    } else if (name_char - 'a' < 26) {       // lowercase letter
      remapped_char = name_char - 'a' + 1;   // so a becomes 1.
    } else if (name_char - '0' < 10) {       // digit
      remapped_char = name_char - '0' + 27;  // so 0 becomes 27
    } else if (name_char == '-') {
      remapped_char = 37;
    } else if (name_char == ' ' || name_char == '\0') {
      remapped_char = 0;
    } else {
      ASSERT_NOT_REACHED();  // invalid char in input.
    }

    ASSERT((remapped_char & 0xff) == remapped_char);
    acc = acc * 38 + remapped_char;  // (null + alphabet + 10 + dash)
  }

  out[0] = (first_four << 0x15) | acc;
  out[1] = first_four >> 0xb;

  return ret;
}

// UnpackVAGFileName - nobody uses it...

}  // namespace jak3