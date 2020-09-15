#pragma once

/*!
 * @file ramdisk_rpc_types.h
 * Types used for the RamDisk Remote Procedure Call between the EE and the IOP
 */

#ifndef JAK1_RAMDISK_RPC_TYPES_H
#define JAK1_RAMDISK_RPC_TYPES_H

#include "common/common_types.h"

constexpr int RAMDISK_RPC_ID = 0xdeb3;
constexpr int RAMDISK_RPC_CHANNEL = 2;
constexpr int RAMDISK_GET_DATA_FNO = 0;
constexpr int RAMDISK_RESET_AND_LOAD_FNO = 1;
constexpr int RAMDISK_BYPASS_LOAD_FILE = 4;

struct RPC_Ramdisk_LoadCmd {
  char pad[4];
  uint32_t file_id_or_ee_addr;
  uint32_t offset_into_file;
  uint32_t size;
  char name[16];  // guess on length?
};

#endif  // JAK1_RAMDISK_RPC_TYPES_H
