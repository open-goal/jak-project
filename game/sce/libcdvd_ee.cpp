/*!
 * @file libcdvd_ee.cpp
 * Stub implementation of the EE CD/DVD library
 */

#include "libcdvd_ee.h"

#include "common/util/Assert.h"

namespace ee {

namespace {
// CD/DVD media type set by sceCdMMode
int media_mode;
}  // namespace

void LIBRARY_INIT_sceCd() {
  media_mode = -1;
}

/*!
 * Initialize the CD/DVD subsystem.
 * init_mode should be SCECdINIT
 */
int sceCdInit(int init_mode) {
  ASSERT(init_mode == SCECdINIT);
  return 1;  // Initialization was performed normally
}

/*!
 * Tell the library if we are expecting a CD or DVD.
 */
int sceCdMmode(int media) {
  media_mode = media;
  return 1;  // If successful, returns 1
}

/*!
 * Is the drive ready for commands?
 * Mode is a flag for non-blocking, otherwise block until ready.
 */
int sceCdDiskReady(int mode) {
  (void)mode;
  // always ready!
  return SCECdComplete;
}

/*!
 * What type of disk do we have?
 */
int sceCdGetDiskType() {
  // if we set CD or DVD, return the appropriate PS2 game disk type.
  switch (media_mode) {
    case SCECdCD:
      return SCECdPS2CD;
    case SCECdDVD:
      return SCECdPS2DVD;
    default:
      // unset/unknown media mode, so drive won't work.
      return SCECdIllegalMedia;
  }
}
}  // namespace ee
