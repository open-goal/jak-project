#include "stream.h"

#include <cstring>

#include "common/util/FileUtil.h"

#include "game/common/play_rpc_types.h"
#include "game/common/str_rpc_types.h"
#include "game/overlord/common/iso.h"
#include "game/overlord/common/iso_api.h"
#include "game/overlord/common/isocommon.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak2 {
static RPC_Str_Cmd_Jak2 sSTRBuf;
static RPC_Play_Cmd_Jak2 sPLAYBuf[2];

void* RPC_STR(unsigned int fno, void* _cmd, int y);
void* RPC_PLAY(unsigned int fno, void* _cmd, int y);

struct CacheEntry {
  FileRecord* fr = nullptr;
  s32 countdown = 0;
  StrFileHeaderJ2 header;
};

constexpr int STR_INDEX_CACHE_SIZE = 4;
CacheEntry sCache[STR_INDEX_CACHE_SIZE];

void stream_init_globals() {
  memset(&sSTRBuf, 0, sizeof(RPC_Str_Cmd_Jak2));
  memset(&sPLAYBuf, 0, sizeof(RPC_Play_Cmd_Jak2) * 2);
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
  auto* cmd = (RPC_Str_Cmd_Jak2*)_cmd;
  if (cmd->section < 0) {
    // it's _not_ a stream file. So we just treat it like a normal load.

    // find the file with the given name
    auto file_record = isofs->find(cmd->basename);
    if (file_record == nullptr) {
      // file not found!
      printf("[OVERLORD STR] Failed to find file %s for loading.\n", cmd->basename);
      cmd->result = STR_RPC_RESULT_ERROR;
    } else {
      // load directly to the EE
      cmd->maxlen = LoadISOFileToEE(file_record, cmd->address, cmd->maxlen);
      if (cmd->maxlen) {
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
    file_util::ISONameFromAnimationName(animation_iso_name, cmd->basename);
    auto file_record = isofs->find_in(animation_iso_name);

    if (!file_record) {
      // didn't find the file
      printf("[OVERLORD STR] Failed to find animation %s (%s)\n", cmd->basename,
             animation_iso_name);
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
        if (!LoadISOFileToIOP(file_record, &sCache[oldest_idx].header, sizeof(StrFileHeaderJ2))) {
          printf("[OVERLORD STR] Failed to load chunk file header for animation %s\n",
                 cmd->basename);
          cmd->result = 1;
          return cmd;
        }
      }

      // load data, using the cached header to find the location of the chunk.
      if (!LoadISOFileChunkToEE(file_record, cmd->address,
                                sCache[cache_entry].header.sizes[cmd->section],
                                sCache[cache_entry].header.sectors[cmd->section])) {
        printf("[OVERLORD STR] Failed to load chunk %d for animation %s\n", cmd->section,
               cmd->basename);
        cmd->result = 1;
      } else {
        // successful load!
        cmd->maxlen = sCache[cache_entry].header.sizes[cmd->section];
        cmd->result = 0;
      }
    }
  }

  return cmd;
}

/*!
 * This is just copied from Jak 1, and is totally wrong for jak 2.
 * It does nothing.
 */
void* RPC_PLAY([[maybe_unused]] unsigned int fno, void* _cmd, int size) {
  // maybe wrong.
  static constexpr int PLAY_MSG_SIZE = 0x40;
  s32 n_messages = size / PLAY_MSG_SIZE;
  char namebuf[16];

  auto* cmd = (RPC_Play_Cmd_Jak2*)(_cmd);
  while (n_messages > 0) {
    if (cmd->names[0].chars[0] == '$') {
      char* name_part = &cmd->names[0].chars[1];
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
      file_util::ISONameFromAnimationName(namebuf, cmd->names[0].chars);
    }

    n_messages--;
    cmd++;
  }

  return _cmd;
}

}  // namespace jak2
