/*!
 * @file isocommon.cpp
 * Common ISO utilities.
 */

#include "isocommon.h"

#include <cstring>

#include "common/common_types.h"
#include "common/util/Assert.h"

namespace jak1 {

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

}  // namespace jak1