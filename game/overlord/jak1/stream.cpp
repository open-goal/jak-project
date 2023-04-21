/*!
 * @file stream.cpp
 * OVERLORD streaming driver.
 * Supports loading a file directly to the EE, or loading chunks of a chunked file.
 */

#include "stream.h"

#include <cstring>

#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/common/play_rpc_types.h"
#include "game/common/str_rpc_types.h"
#include "game/overlord/common/iso.h"
#include "game/overlord/common/isocommon.h"
#include "game/overlord/jak1/iso.h"
#include "game/overlord/jak1/iso_api.h"
#include "game/overlord/jak1/srpc.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak1 {
static RPC_Str_Cmd_Jak1 sSTRBuf;
static RPC_Play_Cmd_Jak1 sPLAYBuf[2];

void* RPC_STR(unsigned int fno, void* _cmd, int y);
void* RPC_PLAY(unsigned int fno, void* _cmd, int y);

static constexpr int PLAY_MSG_SIZE = 0x40;

static u32 global_vag_count = 0;

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
  StrFileHeaderJ1 header;
};

// the actual header cache.
constexpr int STR_INDEX_CACHE_SIZE = 4;
CacheEntry sCacheJ1[STR_INDEX_CACHE_SIZE];

void stream_init_globals() {
  memset(&sSTRBuf, 0, sizeof(RPC_Str_Cmd_Jak1));
  memset(&sPLAYBuf, 0, sizeof(RPC_Play_Cmd_Jak1) * 2);
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
  sceSifRegisterRpc(&serve, STR_RPC_ID[g_game_version], RPC_STR, &sSTRBuf, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

u32 PLAYThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, PLAY_RPC_ID[g_game_version], RPC_PLAY, sPLAYBuf, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

/*!
 * The STR RPC handler.
 */
void* RPC_STR(unsigned int fno, void* _cmd, int y) {
  (void)fno;
  (void)y;
  auto* cmd = (RPC_Str_Cmd_Jak1*)_cmd;
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
    file_util::ISONameFromAnimationName(animation_iso_name, cmd->name);
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
      while (cache_entry < STR_INDEX_CACHE_SIZE && sCacheJ1[cache_entry].fr != file_record) {
        sCacheJ1[cache_entry].countdown--;
        if (sCacheJ1[cache_entry].countdown < oldest) {
          oldest_idx = cache_entry;
          oldest = sCacheJ1[cache_entry].countdown;
        }
        cache_entry++;
      }

      if (cache_entry == STR_INDEX_CACHE_SIZE) {
        // cache miss, we need to load the header to the header cache on the IOP
        cache_entry = oldest_idx;
        sCacheJ1[oldest_idx].fr = file_record;
        sCacheJ1[oldest_idx].countdown = INT32_MAX - 1;
        if (!LoadISOFileToIOP(file_record, &sCacheJ1[oldest_idx].header, sizeof(StrFileHeaderJ1))) {
          printf("[OVERLORD STR] Failed to load chunk file header for animation %s\n", cmd->name);
          cmd->result = 1;
          return cmd;
        }
      }

      // load data, using the cached header to find the location of the chunk.
      if (!LoadISOFileChunkToEE(file_record, cmd->ee_addr,
                                sCacheJ1[cache_entry].header.sizes[cmd->chunk_id],
                                sCacheJ1[cache_entry].header.sectors[cmd->chunk_id])) {
        printf("[OVERLORD STR] Failed to load chunk %d for animation %s\n", cmd->chunk_id,
               cmd->name);
        cmd->result = 1;
      } else {
        // successful load!
        cmd->length = sCacheJ1[cache_entry].header.sizes[cmd->chunk_id];
        cmd->result = 0;
      }
    }
  }

  return cmd;
}

void* RPC_PLAY([[maybe_unused]] unsigned int fno, void* _cmd, int size) {
  s32 n_messages = size / PLAY_MSG_SIZE;
  char namebuf[16];

  auto* cmd = (RPC_Play_Cmd_Jak1*)(_cmd);
  while (n_messages > 0) {
    if (cmd->name[0] == '$') {
      char* name_part = &cmd->name[1];
      size_t name_len = strlen(name_part);

      if (name_len < 9) {
        memset(namebuf, ' ', 8);
        memcpy(namebuf, name_part, name_len);
      } else {
        memcpy(namebuf, name_part, 8);
      }

      // ASCII toupper
      for (int i = 0; i < 8; i++) {
        if (namebuf[i] >= 0x61 && namebuf[i] < 0x7b) {
          namebuf[i] -= 0x20;
        }
      }
    } else {
      file_util::ISONameFromAnimationName(namebuf, cmd->name);
    }

    auto vag = FindVAGFile(namebuf);
    memcpy(namebuf, "VAGWAD  ", 8);
    strcpy(&namebuf[8], gLanguage);

    FileRecord* file = nullptr;

    global_vag_count = (global_vag_count + 1) & 0x3f;
    if (!cmd->result && global_vag_count == 0) {
      namebuf[0] -= 3;
      file = isofs->find_in(namebuf);
      namebuf[0] += 3;
    }

    file = isofs->find_in(namebuf);

    if (cmd->result == 0) {
      PlayVAGStream(file, vag, cmd->address, 0x400, 1, nullptr);
    } else if (cmd->result == 1) {
      StopVAGStream(vag, 1);
    } else {
      QueueVAGStream(file, vag, 0, 1);
    }

    n_messages--;
    cmd++;
  }

  return _cmd;
}

}  // namespace jak1
