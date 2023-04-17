#pragma once

/*!
 * @file fake_iso.h
 * This provides an implementation of IsoFs for reading a "fake iso".
 * A "fake iso" is just a map file which maps 8.3 ISO file names to files in the source folder.
 * This way we don't need to actually create an ISO.
 *
 * The game has this compilation unit, but there is nothing in it. Probably it is removed to save
 * IOP memory and was only included on TOOL-only builds.  So this is my interpretation of how it
 * should work.
 */

#include "isocommon.h"

namespace jak1 {
void fake_iso_init_globals();
extern IsoFs fake_iso;
}  // namespace jak1
