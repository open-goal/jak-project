#include "stream.h"

#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/iso_cd.h"
#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/rpc_interface.h"
#include "game/overlord/jak3/streamlist.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"

namespace jak3 {

using namespace iop;

constexpr int kStrBufSize = sizeof(RPC_Str_Cmd);
static RPC_Str_Cmd sSTRBuf;

constexpr int kNumPlayCmds = 4;
constexpr int kRpcBuf2Size = sizeof(RPC_Play_Cmd) * kNumPlayCmds;
static RPC_Play_Cmd sRPCBuf2[kNumPlayCmds];

constexpr int SECTOR_TABLE_SIZE = 512;

struct StrFileHeader {
  u32 sectors[SECTOR_TABLE_SIZE];  // start of chunk, in sectors. including this sector.
  u32 sizes[SECTOR_TABLE_SIZE];    // size of chunk, in bytes. always an integer number of sectors
};

static_assert(sizeof(StrFileHeader) == 0x1000, "Sector header size");

struct CacheEntry {
  ISOFileDef* filedef = nullptr;
  s32 countdown = 0;
  StrFileHeader header;
};

constexpr int STR_INDEX_CACHE_SIZE = 4;
CacheEntry sCache[STR_INDEX_CACHE_SIZE];

void jak3_overlord_init_globals_stream() {}

u32 STRThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, RpcId::STR, RPC_STR, &sSTRBuf, kStrBufSize, nullptr, nullptr, &dq);

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
  sceSifRegisterRpc(&serve, RpcId::PLAY, RPC_PLAY, &sRPCBuf2, kRpcBuf2Size, nullptr, nullptr, &dq);

  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

void* RPC_STR(unsigned int, void* msg_in, int size) {
  auto* msg = (RPC_Str_Cmd*)msg_in;
  ASSERT(size == sizeof(RPC_Str_Cmd));

  if (msg->section < 0) {
    ovrld_log(LogCategory::STR_RPC, "RPC_STR loading full file {}", msg->basename);
    // not a stream file - treat it like a normal load
    auto* filedef = get_file_system()->Find(msg->basename);
    if (filedef) {
      msg->maxlen = LoadISOFileToEE(filedef, msg->address, msg->maxlen);
      if (msg->maxlen) {
        msg->result = 0;
        return msg;
      } else {
        ovrld_log(LogCategory::WARN, "Failed to LoadISOFileToEE in RPC_STR for {}", msg->basename);
      }
    } else {
      ovrld_log(LogCategory::WARN, "Failed to open {} for RPC STR", msg->basename);
    }
  } else {
    // this is an animation load. Convert name:
    ISOName animation_iso_name;
    file_util::ISONameFromAnimationName(animation_iso_name.data, msg->basename);
    auto* filedef = get_file_system()->FindIN(&animation_iso_name);
    ovrld_log(LogCategory::STR_RPC, "STR_RPC for {} chunk {}", msg->basename, msg->section);

    if (filedef) {
      // found it! See if we've cached this animation's header.
      int cache_entry = 0;
      int oldest = INT32_MAX;
      int oldest_idx = -1;
      while (cache_entry < STR_INDEX_CACHE_SIZE && sCache[cache_entry].filedef != filedef) {
        sCache[cache_entry].countdown--;
        if (sCache[cache_entry].countdown < oldest) {
          oldest_idx = cache_entry;
          oldest = sCache[cache_entry].countdown;
        }
        cache_entry++;
      }

      if (cache_entry == STR_INDEX_CACHE_SIZE) {
        // cache miss, we need to load the header to the header cache on the IOP
        ovrld_log(LogCategory::STR_RPC,
                  "STR_RPC header cache miss - loading .str file header now.");
        cache_entry = oldest_idx;
        sCache[oldest_idx].filedef = filedef;
        sCache[oldest_idx].countdown = INT32_MAX - 1;
        if (!LoadISOFileToIOP(filedef, (u8*)&sCache[oldest_idx].header, sizeof(StrFileHeader))) {
          ovrld_log(LogCategory::WARN, "STR_RPC failed to load .str file header for {}",
                    msg->basename);
          msg->result = 1;
          return msg;
        }
      }

      // load data, using the cached header to find the location of the chunk.
      if (!LoadISOFileChunkToEE(filedef, msg->address,
                                sCache[cache_entry].header.sizes[msg->section],
                                sCache[cache_entry].header.sectors[msg->section])) {
        ovrld_log(LogCategory::WARN, "STR_RPC failed to load .str file chunk {} for {}",
                  msg->section, msg->basename);
        msg->result = 1;
      } else {
        // successful load!
        msg->maxlen = sCache[cache_entry].header.sizes[msg->section];
        msg->result = 0;
        return msg;
      }
    }
  }
  msg->result = 1;
  return msg;
}

void* RPC_PLAY(unsigned int, void* msg_in, int size) {
  static_assert(sizeof(RPC_Play_Cmd) == 256);

  if (size <= 0) {
    return msg_in;
  }

  auto* msg_array = (RPC_Play_Cmd*)msg_in;

  for (u32 msg_idx = 0; msg_idx < size / sizeof(RPC_Play_Cmd); msg_idx++) {
    auto* msg = &msg_array[msg_idx];

    // the operation is stashed in the "result" field of the message
    switch (msg->result) {
      case 1: {
        // remove vag streams by name
        for (int s = 0; s < 4; s++) {
          VagStreamData vsd;
          if (msg->names[s].chars[0] != 0) {
            // lg::warn("RPC PLAY remove {}", msg->names[s].chars);
            strncpy(vsd.name, msg->names[s].chars, 0x30);
            vsd.id = msg->id[s];
            WaitSema(g_EEStreamsList.sema);
            RemoveVagStreamFromList(&vsd, &g_EEStreamsList);
            SignalSema(g_EEStreamsList.sema);
            WaitSema(g_EEPlayList.sema);
            RemoveVagStreamFromList(&vsd, &g_EEPlayList);
            SignalSema(g_EEPlayList.sema);
          }
        }
      } break;
      case 2: {
        // completely redefine the set of vag streams to queue up.
        WaitSema(g_EEStreamsList.sema);        // lock stream list
        EmptyVagStreamList(&g_EEStreamsList);  // clear all existing streams

        // the first stream has the highest priority.
        int priority = 9;
        for (int s = 0; s < 4; s++) {
          if (msg->names[s].chars[0] && msg->id[s]) {
            // lg::warn("RPC PLAY queue {}", msg->names[s].chars);

            // set up list entry for this stream
            VagStreamData vsd;
            strncpy(vsd.name, msg->names[s].chars, 0x30);
            vsd.id = msg->id[s];
            vsd.art_load = msg->address & 1 << (s & 0x1f) & 0xf;
            vsd.movie_art_load = msg->address & 0x10 << (s & 0x1f) & 0xf0;
            vsd.sound_handler = 0;
            vsd.priority = priority;

            // if we have an existing one, make sure it has the appropriate flags
            auto* existing_vag = FindThisVagStream(vsd.name, vsd.id);
            if (existing_vag) {
              existing_vag->art_flag = (u32)(vsd.art_load != 0);
              existing_vag->music_flag = 0;
              existing_vag->movie_flag = (u32)(vsd.movie_art_load != 0);
              if (vsd.art_load != 0) {
                existing_vag->flags.art = 1;
              }
              if (existing_vag->movie_flag != 0) {
                existing_vag->flags.movie = 1;
              }
            }

            // add to list
            InsertVagStreamInList(&vsd, &g_EEStreamsList);
          }

          if (priority == 8) {
            priority = 2;
          } else {
            if (0 < priority) {
              priority = priority + -1;
            }
          }
          s = s + 1;
        }
        SignalSema(g_EEStreamsList.sema);
      } break;
      case 0: {
        int priority = 9;
        for (int s = 0; s < 4; s++) {
          if (msg->names[s].chars[0] && msg->id[s]) {
            // lg::warn("RPC PLAY play {}", msg->names[s].chars);

            VagStreamData vsd;
            strncpy(vsd.name, msg->names[s].chars, 0x30);
            vsd.id = msg->id[s];
            vsd.volume2 = msg->section;
            vsd.group = msg->maxlen;
            vsd.plugin_id = 0;
            vsd.sound_handler = 0;
            vsd.maybe_volume_3 = 0;
            vsd.priority = priority;
            auto* existing_vag = FindThisVagStream(msg->names[s].chars, vsd.id);
            if (existing_vag != (ISO_VAGCommand*)0x0) {
              existing_vag->play_volume = vsd.volume2;
              existing_vag->play_group = vsd.group;
              if (existing_vag->flags.running != 0)
                goto LAB_000092a4;
            }
            WaitSema(g_EEPlayList.sema);
            auto* already_playing = FindVagStreamInList(&vsd, &g_EEPlayList);
            if (!already_playing) {
              already_playing = InsertVagStreamInList(&vsd, &g_EEPlayList);
              strncpy(already_playing->name, vsd.name, 0x30);
              already_playing->id = vsd.id;
              already_playing->priority = vsd.priority;
              already_playing->sound_handler = vsd.sound_handler;
              already_playing->plugin_id = vsd.plugin_id;
              already_playing->unk1 = 0;
              already_playing->art_load = 0;
              already_playing->movie_art_load = 0;
            }
            SignalSema(g_EEPlayList.sema);
          } else {
            // lg::warn("RPC PLAY play (NONE)");
          }
        LAB_000092a4:
          if (priority == 8) {
            priority = 2;
          } else {
            if (0 < priority) {
              priority = priority + -1;
            }
          }
        }

      } break;
    }
  }

  return msg_in;
}
}  // namespace jak3