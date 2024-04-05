/*!
 * @file ramdisk.cpp
 * A RAMDISK RPC for storing files in the extra RAM left over on the IOP.
 * Also called "Server".
 */

#include "ramdisk.h"

#include <cstdio>
#include <cstring>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/common/ramdisk_rpc_types.h"
#include "game/overlord/jak1/iso.h"
#include "game/overlord/jak1/iso_api.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

// Note - the RAMDISK code supports having multiple files, but it appears only one file can ever be
// used at a time.

namespace jak1 {
constexpr int RAMDISK_SIZE = 0xcac00;               // Memory size of RAMDISK
constexpr int RAMDISK_MAX_FILES = 16;               // Maximum number of files to store in RAMDISK.
constexpr int RAMDISK_RETURN_BUFFER_SIZE = 0x2000;  // Maximum size of an individual RAMDISK read
constexpr int DEVTOOL_IOP_MEM_ALLOC =
    0x1f5d00;  // Extra memory to waste to compensate for extra RAM in dev kit

u32 gNumFiles;         // Number of files in the RAMDISK
u32 gMemUsed;          // Memory of RAMDISK used
u32 gMemSize;          // Total memory of RAMDISK
u32 gMemFreeAtStart;   // Memory free after allocation of RAMDISK
uint8_t* gMem;         // Allocation for RAMDISK
uint8_t* gRamdiskRAM;  // Also allocation for RAMDISK
constexpr int kRamdiskBufferSize = 40;
uint8_t gRPCBuf[kRamdiskBufferSize];  // Buffer for RAMDISK RPC handler

// Each file stored in the ramdisk has a file record:
struct RamdiskFileRecord {
  uint32_t size;               // size of file in bytes (will be 16-byte aligned)
  uint32_t additional_offset;  // an offset into the memory for the file
  uint32_t file_id;            // an ID number used to identify this file.
};

RamdiskFileRecord gFiles[RAMDISK_MAX_FILES];        // File records
uint8_t gReturnBuffer[RAMDISK_RETURN_BUFFER_SIZE];  // Buffer to hold data requested by EE

using namespace iop;

void ramdisk_init_globals() {
  gNumFiles = 0;
  gMemUsed = 0;
  gMemSize = 0;
  gMemFreeAtStart = 0;
  gMem = nullptr;
  gRamdiskRAM = nullptr;
  memset(gRPCBuf, 0, sizeof(gRPCBuf));
  memset(gFiles, 0, sizeof(gFiles));
  memset(gReturnBuffer, 0, sizeof(gReturnBuffer));
}

/*!
 * Initialze the RAMDISK IOP System.
 * For some reason the name of this function is lost, so this is a guess at the name.
 * DONE, EXACT
 */
void InitRamdisk() {
  gNumFiles = 0;
  gMemUsed = 0;
  gMemSize = RAMDISK_SIZE;

  // some sort of "trick" to allocate memory if we are on a debug system to simulate the memory size
  // of the real PS2.
  if (QueryTotalFreeMemSize() > 0x200000) {
    AllocSysMemory(SMEM_Low, DEVTOOL_IOP_MEM_ALLOC, nullptr);
  }

  // allocate RAMDISK RAM
  gMem = (uint8_t*)AllocSysMemory(SMEM_Low, gMemSize, nullptr);
  if (gMem) {
    gMemFreeAtStart = QueryTotalFreeMemSize();
    gRamdiskRAM = gMem;
  } else {
    printf("[OVERLORD RAMDISK] Failed to allocate memory for RAMDISK!\n");  // added
  }
}

void* RPC_Ramdisk(unsigned int fno, void* data, int size);

/*!
 * The main function for the IOP Ramdisk/Server thread.
 * DONE, EXACT
 */
u32 Thread_Server() {
  sceSifQueueData dq;
  sceSifServeData serve;

  // set up RPC
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, RAMDISK_RPC_ID[g_game_version], RPC_Ramdisk, gRPCBuf,
                    kRamdiskBufferSize, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

/*!
 * Ramdisk RPC Handler.
 * DONE
 * Added some debugging print statements.
 * Returns a pointer to the file contents on successful GET_DATA.
 * Returns nullptr in all other cases.
 */
void* RPC_Ramdisk(unsigned int fno, void* data, int size) {
  (void)size;

  auto cmd = (RPC_Ramdisk_LoadCmd*)data;
  if (fno == RAMDISK_RESET_AND_LOAD_FNO) {
    // reset files and memory
    gNumFiles = 0;
    gMemUsed = 0;

    // locate file to load into ramdisk
    auto file_record = FindISOFile(cmd->name);
    if (!file_record) {
      printf("[OVERLORD RAMDISK] Failed to find ISO file for load (%s).\n", cmd->name);  // added
      return nullptr;
    }

    // if we have available memory and records (we'll always have enough records, we just reset it!)
    // NOTE - there is a bug here where the rounding up to 16-bytes can cause it to overflow!
    auto file_length = GetISOFileLength(file_record);
    if ((file_length + gMemUsed <= gMemSize) && (gNumFiles != RAMDISK_MAX_FILES)) {
      // Create the new file record
      gFiles[gNumFiles].size = (file_length + 0xf) & 0xfffffff0;
      ASSERT(gFiles[gNumFiles].size + gMemUsed <
             gMemSize);  // ADDED! this checks for a real bug in the code.
      gFiles[gNumFiles].additional_offset = 0;
      gFiles[gNumFiles].file_id = cmd->file_id_or_ee_addr;

      // Increment file count
      gNumFiles++;

      // Load file into IOP at the appropriate spot
      printf("[OVERLORD RAMDISK] loading file %s with size %d\n", cmd->name, file_length);
      LoadISOFileToIOP(file_record, gMem + gMemUsed, file_length);
      gMemUsed += gFiles[gNumFiles].size;
    } else {
      printf("[OVERLORD RAMDISK] Failed to load file because RAMDISK is out of memory or files!\n");
      printf("num files %d, mem used %d\n", gNumFiles, gMemUsed);
      printf("file size: %d\n", file_length);
      printf("file name: %s\n", cmd->name);
    }
  } else if (fno == RAMDISK_GET_DATA_FNO) {
    // Copy data into a local IOP buffer

    // Total offset into ramdisk memory
    auto offset = cmd->offset_into_file;

    // find a matching file, and compute its offset
    u32 file_idx = 0;
    while (file_idx < gNumFiles && gFiles[file_idx].file_id != cmd->file_id_or_ee_addr) {
      offset += gFiles[file_idx].size;
      file_idx++;
    }

    if (file_idx == gNumFiles) {
      // didn't find the file
      printf("[OVERLORD RAMDISK] Failed to find ISO file for read.\n");  // added
      return nullptr;
    }

    if (cmd->size > RAMDISK_RETURN_BUFFER_SIZE) {
      printf("[OVERLORD RAMDISK] requested file read size is too large.\n");  // added
      return nullptr;
    }

    // copy to return buffer.  This way RAMDISK data is valid until another GET_DATA.
    memcpy(gReturnBuffer, gMem + offset + gFiles[file_idx].additional_offset, cmd->size);
    return gReturnBuffer;
  } else if (fno == RAMDISK_BYPASS_LOAD_FILE) {
    printf("[OVERLORD RAMDISK] got \"%s\"\n", cmd->name);
    // This is just a normal file load to the EE.
    auto file_record = FindISOFile(cmd->name);
    if (!file_record) {
      printf("[OVERLORD RAMDISK] Failed to open file for bypass load.\n");  // added
      return nullptr;
    }
    LoadISOFileToEE(file_record, cmd->file_id_or_ee_addr, cmd->size);
  } else {
    printf("[OVERLORD RAMDISK] Unsupported fno\n");  // ADDED
  }
  return nullptr;
}
}  // namespace jak1