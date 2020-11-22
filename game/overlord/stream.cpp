/*!
 * @file stream.cpp
 * OVERLORD streaming driver.
 * Supports loading a file directly to the EE, or loading chunks of a chunked file.
 */

#include <cassert>
#include "stream.h"
#include "game/sce/iop.h"
#include "game/common/str_rpc_types.h"
#include "game/overlord/isocommon.h"
#include "game/overlord/iso_api.h"

using namespace iop;

static RPC_Str_Cmd sRPCBuf;
void* RPC_STR(unsigned int fno, void* _cmd, int y);

/*!
 * We cache the chunk file headers so we can avoid seeking to the chunk header each time we
 * need to load another chunk, even if we load chunks out of order.
 */
struct CacheEntry {
  // the record for the chunk file described.
  FileRecord* fr = nullptr;
  // counts down from INT32_MAX - 1 each time we have a cache miss.
  s32 countdown = 0;
  // the actual cached data.
  StrFileHeader header;
};

// the actual header cache.
constexpr int STR_INDEX_CACHE_SIZE = 4;
CacheEntry sCache[STR_INDEX_CACHE_SIZE];

void stream_init_globals() {
  memset(&sRPCBuf, 0, sizeof(RPC_Str_Cmd));
}

/*!
 * Run the STR RPC handler.
 */
u32 STRThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, STR_RPC_ID, RPC_STR, &sRPCBuf, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

u32 PLAYThread() {
  assert(false);
  return 0;
}

/*!
 * The STR RPC handler.
 */
void* RPC_STR(unsigned int fno, void* _cmd, int y) {
  (void)fno;
  (void)y;
  auto* cmd = (RPC_Str_Cmd*)_cmd;
  printf("RPC STR runs!\n");
  if (cmd->chunk_id < 0) {
    // it's _not_ a stream file. So we just treat it like a normal load.

    // find the file with the given name
    auto file_record = isofs->find(cmd->name);
    if (file_record == nullptr) {
      // file not found!
      printf("[OVERLORD STR] Failed to find file %s for loading.\n", cmd->name);
      cmd->result = STR_RPC_RESULT_ERROR;
    } else {
      // load directly to the EE
      cmd->length = LoadISOFileToEE(file_record, cmd->ee_addr, cmd->length);
      if (cmd->length) {
        // successful load!
        cmd->result = STR_RPC_RESULT_DONE;
      } else {
        // there was an error loading.
        cmd->result = STR_RPC_RESULT_ERROR;
      }
    }
  } else {
    // it's a chunked file. These are only animations - these have a separate naming scheme.
    char animation_iso_name[128];
    ISONameFromAnimationName(animation_iso_name, cmd->name);
    auto file_record = isofs->find_in(animation_iso_name);

    if (!file_record) {
      // didn't find the file
      printf("[OVERLORD STR] Failed to find animation %s\n", cmd->name);
      cmd->result = STR_RPC_RESULT_ERROR;
    } else {
      // found it! See if we've cached this animation's header.
      int cache_entry = 0;
      int oldest = INT32_MAX;
      int oldest_idx = -1;
      while (cache_entry < STR_INDEX_CACHE_SIZE && sCache[cache_entry].fr != file_record) {
        sCache[cache_entry].countdown--;
        if (sCache[cache_entry].countdown < oldest) {
          oldest_idx = cache_entry;
          oldest = sCache[cache_entry].countdown;
        }
        cache_entry++;
      }

      if (cache_entry == STR_INDEX_CACHE_SIZE) {
        // cache miss, we need to load the header to the header cache on the IOP
        cache_entry = oldest_idx;
        sCache[oldest_idx].fr = file_record;
        sCache[oldest_idx].countdown = INT32_MAX - 1;
        if (!LoadISOFileToIOP(file_record, &sCache[oldest_idx].header, sizeof(StrFileHeader))) {
          printf("[OVERLORD STR] Failed to load chunk file header for animation %s\n", cmd->name);
          cmd->result = 1;
          return cmd;
        }
      }

      // load data, using the cached header to find the location of the chunk.
      if (!LoadISOFileChunkToEE(file_record, cmd->ee_addr,
                                sCache[cache_entry].header.sizes[cmd->chunk_id],
                                sCache[cache_entry].header.sectors[cmd->chunk_id])) {
        printf("[OVERLORD STR] Failed to load chunk %d for animation %s\n", cmd->chunk_id,
               cmd->name);
        cmd->result = 1;
      } else {
        // successful load!
        cmd->length = sCache[cache_entry].header.sizes[cmd->chunk_id];
        cmd->result = 0;
      }
    }
  }
  printf("Command result %d\n", cmd->result);
  return cmd;
}