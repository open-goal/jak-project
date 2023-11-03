#pragma once

/*!
 * @file ramdisk_rpc_types.h
 * Types used for the RamDisk Remote Procedure Call between the EE and the IOP
 */

#include "common/common_types.h"
#include "common/versions/versions.h"

// TODO: jak 3 stub
constexpr PerGameVersion<int> RAMDISK_RPC_ID(0xdeb3, 0xfab2, 0x0);
constexpr int RAMDISK_RPC_CHANNEL = 2;
constexpr int RAMDISK_GET_DATA_FNO = 0;
constexpr int RAMDISK_RESET_AND_LOAD_FNO = 1;
constexpr int RAMDISK_BYPASS_LOAD_FILE = 4;

struct RPC_Ramdisk_LoadCmd {
  u32 pad;
  u32 file_id_or_ee_addr;
  u32 offset_into_file;
  u32 size;
  char name[16];  // guess on length?
};
