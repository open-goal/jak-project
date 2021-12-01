#pragma once

/*!
 * @file libcdvd_ee.h
 * Stub implementation of the EE CD/DVD library
 */

// for sceCdInit
#define SCECdINIT 0x00

// Media modes
#define SCECdCD 1
#define SCECdDVD 2

// Status
#define SCECdComplete 0x02
#define SCECdNotReady 0x06

// Disk Types
#define SCECdIllegalMedia 0xff
#define SCECdDVDV 0xfe
#define SCECdCDDA 0xfd
#define SCECdPS2DVD 0x14
#define SCECdPS2CD 0x12
#define SCECdDETCT 0x01

namespace ee {
void LIBRARY_INIT_sceCd();
int sceCdInit(int init_mode);
int sceCdMmode(int media);
int sceCdDiskReady(int mode);
int sceCdGetDiskType();
}  // namespace ee
