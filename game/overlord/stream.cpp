/*!
 * @file stream.cpp
 * OVERLORD streaming driver.
 * Supports loading a file directly to the EE, or loading chunks of a chunked file.
 */

#include "stream.h"

#include <cstring>

#include "game/common/play_rpc_types.h"
#include "game/common/str_rpc_types.h"
#include "game/overlord/iso.h"
#include "game/overlord/iso_api.h"
#include "game/overlord/isocommon.h"
#include "game/overlord/srpc.h"
#include "game/overlord/streamlist.h"
#include "game/overlord/vag.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

#include "third-party/fmt/core.h"

using namespace iop;

static constexpr int STR_MSG_SIZE_J1 = 0x40;
static constexpr int STR_MSG_SIZE_J2 = 0x50;
static u8 sSTRBuf[STR_MSG_SIZE_J2];

static constexpr int PLAY_MSG_SIZE_J1 = 0x40;
static constexpr int PLAY_MSG_SIZE_J2 = 0x100;
static u8 sPLAYBuf[PLAY_MSG_SIZE_J2 * 2];

static u32 global_vag_count = 0;

void* RPC_STR_jak1(unsigned int fno, void* _cmd, int y);
void* RPC_STR_jak2(unsigned int fno, void* _cmd, int y);
PerGameVersion<void* (*)(unsigned int, void*, int)> RPC_STR_Func = {RPC_STR_jak1, RPC_STR_jak2};
void* RPC_PLAY_jak1(unsigned int fno, void* _cmd, int y);
void* RPC_PLAY_jak2(unsigned int fno, void* _cmd, int y);
PerGameVersion<void* (*)(unsigned int, void*, int)> RPC_PLAY_Func = {RPC_PLAY_jak1, RPC_PLAY_jak2};

/*!
 * We cache the chunk file headers so we can avoid seeking to the chunk header each time we
 * need to load another chunk, even if we load chunks out of order.
 */
struct CacheEntryJ1 {
  // the record for the chunk file described.
  FileRecord* fr = nullptr;
  // counts down from INT32_MAX - 1 each time we have a cache miss.
  s32 countdown = 0;
  // the actual cached data.
  StrFileHeaderJ1 header;
};

struct CacheEntryJ2 {
  FileRecord* fr = nullptr;
  s32 countdown = 0;
  StrFileHeaderJ2 header;
};

// the actual header cache.
constexpr int STR_INDEX_CACHE_SIZE = 4;
CacheEntryJ1 sCacheJ1[STR_INDEX_CACHE_SIZE];
CacheEntryJ2 sCacheJ2[STR_INDEX_CACHE_SIZE];

void stream_init_globals() {
  memset(sSTRBuf, 0, sizeof(sSTRBuf));
  memset(sPLAYBuf, 0, sizeof(sPLAYBuf));
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

  sceSifRegisterRpc(&serve, STR_RPC_ID[g_game_version], RPC_STR_Func[g_game_version], sSTRBuf,
                    nullptr, nullptr, &dq);
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
  sceSifRegisterRpc(&serve, PLAY_RPC_ID[g_game_version], RPC_PLAY_Func[g_game_version], sPLAYBuf,
                    nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

/*!
 * The STR RPC handler.
 */
void* RPC_STR_jak1(unsigned int fno, void* _cmd, int y) {
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

/*!
 * The STR RPC handler.
 */
void* RPC_STR_jak2(unsigned int fno, void* _cmd, int y) {
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
    ISONameFromAnimationName(animation_iso_name, cmd->basename);
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
      while (cache_entry < STR_INDEX_CACHE_SIZE && sCacheJ2[cache_entry].fr != file_record) {
        sCacheJ2[cache_entry].countdown--;
        if (sCacheJ2[cache_entry].countdown < oldest) {
          oldest_idx = cache_entry;
          oldest = sCacheJ2[cache_entry].countdown;
        }
        cache_entry++;
      }

      if (cache_entry == STR_INDEX_CACHE_SIZE) {
        // cache miss, we need to load the header to the header cache on the IOP
        cache_entry = oldest_idx;
        sCacheJ2[oldest_idx].fr = file_record;
        sCacheJ2[oldest_idx].countdown = INT32_MAX - 1;
        if (!LoadISOFileToIOP(file_record, &sCacheJ2[oldest_idx].header, sizeof(StrFileHeaderJ2))) {
          printf("[OVERLORD STR] Failed to load chunk file header for animation %s\n",
                 cmd->basename);
          cmd->result = 1;
          return cmd;
        }
      }

      // load data, using the cached header to find the location of the chunk.
      if (!LoadISOFileChunkToEE(file_record, cmd->address,
                                sCacheJ2[cache_entry].header.sizes[cmd->section],
                                sCacheJ2[cache_entry].header.sectors[cmd->section])) {
        printf("[OVERLORD STR] Failed to load chunk %d for animation %s\n", cmd->section,
               cmd->basename);
        cmd->result = 1;
      } else {
        // successful load!
        cmd->maxlen = sCacheJ2[cache_entry].header.sizes[cmd->section];
        cmd->result = 0;
      }
    }
  }

  return cmd;
}

void* RPC_PLAY_jak1([[maybe_unused]] unsigned int fno, void* _cmd, int size) {
  s32 n_messages = size / PLAY_MSG_SIZE_J1;
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
      ISONameFromAnimationName(namebuf, cmd->name);
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

void* RPC_PLAY_jak2([[maybe_unused]] unsigned int fno, void* _cmd, int size) {
  s32 n_messages = size / PLAY_MSG_SIZE_J2;
  VagCommand2* vagcmd;
  VagStream stream;

  auto* cmd = (RPC_Play_Cmd_Jak2*)(_cmd);
  for (int i = 0; i < n_messages; i++, cmd++) {
    auto cmd_type = static_cast<RPCPlayCommand>(cmd->result);
    switch (cmd_type) {
      case RPCPlayCommand::PlayAsync: {
        s32 prio = 9;  // maybe
        for (int i = 0; i < 4; i++) {
          if (cmd->basename[i].chars[0] == 0 || cmd->id[i]) {
            continue;
          }
          strncpy(stream.name, cmd->basename[i].chars, sizeof(stream.name));
          stream.id = cmd->id[i];
          stream.unk0x44 = 0;
          stream.unk0x54 = prio;
          stream.unk0x48 = 0;
          vagcmd = FindThisVagStream(cmd->basename[i].chars, cmd->id[i]);
          if (!vagcmd || !vagcmd->unk0xd4) {
            WaitSema(EEPlayList.sema);
            if (!FindVagStreamInList(&stream, &EEPlayList)) {
              VagStream* inserted = InsertVagStreamInList(&stream, &EEPlayList);
              strncpy(inserted->name, stream.name, sizeof(inserted->name));
              inserted->id = stream.id;
              inserted->unk0x54 = stream.unk0x54;
              inserted->unk0x48 = stream.unk0x48;
              inserted->unk0x44 = stream.unk0x44;
              inserted->unk0x4c = 0;
              inserted->unk0x50 = 0;
              inserted->unk0x5c = 0;
            }
            SignalSema(EEPlayList.sema);
          }
          if (prio == 8) {
            prio = 2;
          } else {
            prio--;
          }
        }
      } break;
      case RPCPlayCommand::Stop: {
        for (int i = 0; i < 4; i++) {
          if (cmd->basename[i].chars[0] == 0) {
            continue;
          }
          strncpy(stream.name, cmd->basename[i].chars, sizeof(stream.name));
          stream.id = cmd->id[i];

          WaitSema(EEStreamsList.sema);
          RemoveVagSreamFromList(&stream, &EEStreamsList);
          SignalSema(EEStreamsList.sema);

          WaitSema(EEPlayList.sema);
          RemoveVagSreamFromList(&stream, &EEPlayList);
          SignalSema(EEPlayList.sema);
        }
      } break;
      case RPCPlayCommand::Queue: {
        WaitSema(EEStreamsList.sema);
        EmptyVagStreamList(&EEStreamsList);
        s32 prio = 9;  // maybe
        for (int i = 0; i < 4; i++) {
          if (cmd->basename[i].chars[0] == 0 || cmd->id[i]) {
            continue;
          }

          strncpy(stream.name, cmd->basename[i].chars, sizeof(stream.name));
          stream.id = cmd->id[i];
          stream.unk0x4c = (u8)cmd->address & (1 << i) & 0xf;
          stream.unk0x54 = prio;
          stream.unk0x48 = 0;
          stream.unk0x50 = (u8)cmd->address & (16 << i) & 0xf0;
          vagcmd = FindThisVagStream(stream.name, stream.id);
          if (vagcmd) {
            vagcmd->unk0x120 = stream.unk0x4c;
            vagcmd->unk0x124 = stream.unk0x50;
            if (vagcmd->unk0x120)
              vagcmd->unk0xda = 1;
            if (vagcmd->unk0x124)
              vagcmd->unk0xe8 = 1;
          }
          InsertVagStreamInList(&stream, &EEStreamsList);
          if (prio == 8) {
            prio = 2;
          } else {
            prio--;
          }
        }

        SignalSema(EEStreamsList.sema);
      } break;
    }
  }

  return _cmd;
}
