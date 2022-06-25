#include "kmachine.h"

#include "common/log/log.h"

#include "game/sce/libcdvd_ee.h"

/*!
 * Where does OVERLORD load its data from?
 */
OverlordDataSource isodrv;

// Get IOP modules from DVD or from dsefilesv
u32 modsrc;

// Reboot IOP with IOP kernel from DVD/CD on boot
u32 reboot;

void kmachine_init_globals_common() {
  isodrv = fakeiso;  // changed. fakeiso is the only one that works in opengoal.
  modsrc = 1;
  reboot = 1;
}

/*!
 * Initialize the CD Drive
 * DONE, EXACT
 */
void InitCD() {
  lg::info("Initializing CD drive. This may take a while...");
  ee::sceCdInit(SCECdINIT);
  ee::sceCdMmode(SCECdDVD);
  while (ee::sceCdDiskReady(0) == SCECdNotReady) {
    lg::debug("Drive not ready... insert a disk!");
  }
  lg::debug("Disk type {}\n", ee::sceCdGetDiskType());
}