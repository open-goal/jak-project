#include "stream.h"

#include <cstring>

#include "common/util/FileUtil.h"

#include "game/common/play_rpc_types.h"
#include "game/common/str_rpc_types.h"
#include "game/overlord/common/iso.h"
#include "game/overlord/common/iso_api.h"
#include "game/overlord/common/isocommon.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/iso_api.h"
#include "game/overlord/jak2/streamlist.h"
#include "game/overlord/jak2/vag.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak2 {
constexpr int kStrBufSize = sizeof(RPC_Str_Cmd_Jak2);
static RPC_Str_Cmd_Jak2 sSTRBuf;
// the size has been increased to 4 to fit jak3. This is a total hack, since the structures
// are probably just completely different.
constexpr int kPlayBufSize = 4 * sizeof(RPC_Play_Cmd_Jak2);
static RPC_Play_Cmd_Jak2 sPLAYBuf[4];  // called sRPCBuf2

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
 * The STR RPC handler.
 */
void* RPC_STR(unsigned int /*fno*/, void* _cmd, int /*y*/) {
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
        if (!LoadISOFileToIOP(file_record, (u8*)&sCache[oldest_idx].header,
                              sizeof(StrFileHeaderJ2))) {
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

  // don't remember why we changed this...
  return cmd;
  // return nullptr;
}

void* RPC_PLAY([[maybe_unused]] unsigned int fno, void* _cmd, int size) {
  // uint16_t uVar1;
  VagCmd* pRVar2;
  VagStrListNode* iVar3;
  VagStrListNode* iVar4;
  // RPC_Play_Cmd_Jak2* pRVar3;
  // char* __src;
  int iVar5;
  // int iVar6;
  // uint uVar7;
  // RPC_Play_Cmd_Jak2* pRVar8;
  // int iVar10;
  VagStrListNode list_node;
  // int local_30;
  // int local_2c;

  //  if (size < 0) {
  //    size = size + 0xff;
  //  }
  //  local_30 = size >> 8;
  //  local_2c = 0;

  int n_messages = size / sizeof(RPC_Play_Cmd_Jak2);
  auto* cmd_iter = (RPC_Play_Cmd_Jak2*)_cmd;

  for (int i = 0; i < n_messages; i++) {
    // printf("RPC_PLAY message %d\n", i);
    auto cmd_result = cmd_iter->result;

    if (cmd_result == 1) {
      for (int s = 0; s < 4; s++) {
        if (cmd_iter->names[s].chars[0]) {
          strncpy(list_node.name, cmd_iter->names[s].chars, 0x30);
          list_node.id = cmd_iter->id[s];
          WaitSema(EEStreamsList.sema);
          RemoveVagStreamFromList(&list_node, &EEStreamsList);
          SignalSema(EEStreamsList.sema);
          WaitSema(EEPlayList.sema);
          RemoveVagStreamFromList(&list_node, &EEPlayList);
          SignalSema(EEPlayList.sema);
        }
      }

    } else {
      iVar5 = 9;
      if (cmd_result == 2) {
        // uVar7 = 0;
        // iVar6 = 0x20;
        WaitSema(EEStreamsList.sema);
        EmptyVagStreamList(&EEStreamsList);

        for (int s = 0; s < 4; s++) {
          if (cmd_iter->names[s].chars[0] && cmd_iter->id[s]) {
            // printf("got queue command %d: %s %d\n", s, cmd_iter->names[s].chars,
            // cmd_iter->id[s]);
            strncpy(list_node.name, cmd_iter->names[s].chars, 0x30);
            list_node.id = cmd_iter->id[s];
            list_node.unk_76 = cmd_iter->address & 1 << (s & 0x1f) & 0xf;
            list_node.sound_handler = 0;
            list_node.unk_80 = cmd_iter->address & 0x10 << (s & 0x1f) & 0xf0;
            list_node.prio = iVar5;
            pRVar2 = FindThisVagStream(list_node.name, list_node.id);
            if (pRVar2 != 0x0) {
              pRVar2->unk_288 = list_node.unk_76;
              pRVar2->unk_292 = list_node.unk_80;
              if (pRVar2->unk_288 != 0) {
                pRVar2->byte10 = '\x01';
              }
              if (pRVar2->unk_292 != 0) {
                pRVar2->unk_232 = '\x01';
              }
            }
            InsertVagStreamInList(&list_node, &EEStreamsList);
          }
          if (iVar5 == 8) {
            iVar5 = 2;
          } else if (0 < iVar5) {
            iVar5 = iVar5 + -1;
          }
        }

        /*
        pRVar3 = cmd_iter;
        pRVar8 = cmd_iter;
        do {
          if ((pRVar8->names[0].data[0] != '\0') && (pRVar3->id[0] != 0)) {
            strncpy(list_node.name, (char*)((int)cmd_iter->id + iVar6 + -0x10), 0x30);
            list_node.id = pRVar3->id[0];
            list_node.unk_76 = cmd_iter->address & 1 << (uVar7 & 0x1f) & 0xf;
            list_node.unk_72 = 0;
            list_node.unk_80 = cmd_iter->address & 0x10 << (uVar7 & 0x1f) & 0xf0;
            list_node.prio = iVar5;
            pRVar2 = FindThisVagStream(list_node.name, list_node.id);
            if (pRVar2 != (RealVagCmd*)0x0) {
              pRVar2->unk_288 = list_node.unk_76;
              pRVar2->unk_292 = list_node.unk_80;
              if (pRVar2->unk_288 != 0) {
                pRVar2->byte10 = '\x01';
              }
              if (pRVar2->unk_292 != 0) {
                pRVar2->unk_232 = '\x01';
              }
            }
            InsertVagStreamInList(&list_node, (List*)EEStreamsList);
          }
          if (iVar5 == 8) {
            iVar5 = 2;
          } else if (0 < iVar5) {
            iVar5 = iVar5 + -1;
          }
          iVar6 = iVar6 + 0x30;
          pRVar3 = (RPC_Play_Cmd*)&pRVar3->address;
          uVar7 = uVar7 + 1;
          pRVar8 = (RPC_Play_Cmd*)(pRVar8->names[0].data + 0x10);
        } while ((int)uVar7 < 4);
         */

        SignalSema(EEStreamsList.sema);
      } else if (cmd_result == 0) {
        iVar5 = 9;

        for (int s = 0; s < 4; s++) {
          if (cmd_iter->names[s].chars[0] && cmd_iter->id[s]) {
            // __src = (char*)((int)cmd_iter->id + iVar10 + -0x10);
            strncpy(list_node.name, cmd_iter->names[s].chars, 0x30);
            list_node.id = cmd_iter->id[s];
            list_node.unk_68 = 0;
            list_node.sound_handler = 0;
            list_node.prio = iVar5;
            pRVar2 = FindThisVagStream(cmd_iter->names[s].chars, cmd_iter->id[s]);
            if ((pRVar2 == 0x0) || (pRVar2->byte4 == '\0')) {
              // printf(" didn't exist, looks like it needs to be added!\n");
              WaitSema(EEPlayList.sema);
              iVar3 = (VagStrListNode*)FindVagStreamInList(&list_node, &EEPlayList);
              if (iVar3 == (VagStrListNode*)0x0) {
                // printf("node also doesn't exist, adding it!\n");
                iVar4 = (VagStrListNode*)InsertVagStreamInList(&list_node, &EEPlayList);

                iVar4->id = list_node.id;
                iVar4->prio = list_node.prio;
                iVar4->sound_handler = list_node.sound_handler;
                iVar4->unk_76 = 0;
                iVar4->unk_80 = 0;
                iVar4->unk_92 = 0;
                iVar4->unk_68 = list_node.unk_68;
                strncpy(iVar4->name, list_node.name, 0x30);
              }
              SignalSema(EEPlayList.sema);
            }
          }

          if (iVar5 == 8) {
            iVar5 = 2;
          } else if (0 < iVar5) {
            iVar5 = iVar5 + -1;
          }
        }

        /*
        iVar6 = 0;
        iVar10 = 0x20;
        pRVar3 = cmd_iter;
        pRVar8 = cmd_iter;
        do {
          if ((pRVar8->names[0].data[0] != '\0') && (pRVar3->id[0] != 0)) {
            __src = (char*)((int)cmd_iter->id + iVar10 + -0x10);
            strncpy(list_node.name, __src, 0x30);
            list_node.id = pRVar3->id[0];
            list_node.unk_68 = 0;
            list_node.unk_72 = 0;
            list_node.prio = iVar5;
            pRVar2 = FindThisVagStream(__src, pRVar3->id[0]);
            if ((pRVar2 == (RealVagCmd*)0x0) || (pRVar2->byte4 == '\0')) {
              WaitSema(EEPlayList._12_4_);
              iVar3 = (VagStrListNode*)FindVagStreamInList(&list_node, (List*)EEPlayList);
              if (iVar3 == (VagStrListNode*)0x0) {
                iVar4 = (VagStrListNode*)InsertVagStreamInList(&list_node, (List*)EEPlayList);
                strncpy(iVar4->name, list_node.name, 0x30);
                iVar4->id = list_node.id;
                iVar4->prio = list_node.prio;
                iVar4->unk_72 = list_node.unk_72;
                iVar4->unk_76 = 0;
                iVar4->unk_80 = 0;
                iVar4->unk_92 = 0;
                iVar4->unk_68 = list_node.unk_68;
              }
              SignalSema(EEPlayList._12_4_);
            }
          }
          if (iVar5 == 8) {
            iVar5 = 2;
          } else if (0 < iVar5) {
            iVar5 = iVar5 + -1;
          }
          iVar10 = iVar10 + 0x30;
          pRVar3 = (RPC_Play_Cmd*)&pRVar3->address;
          iVar6 = iVar6 + 1;
          pRVar8 = (RPC_Play_Cmd*)(pRVar8->names[0].data + 0x10);
        } while (iVar6 < 4);
         */
      }
    }
    cmd_iter = cmd_iter + 1;
  }
  return _cmd;
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
  sceSifRegisterRpc(&serve, STR_RPC_ID[g_game_version], RPC_STR, &sSTRBuf, kStrBufSize, nullptr,
                    nullptr, &dq);

  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}
sceSifServeData* gserve = nullptr;
u32 PLAYThread() {
  sceSifQueueData dq;
  sceSifServeData serve;
  gserve = &serve;
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, PLAY_RPC_ID[g_game_version], RPC_PLAY, sPLAYBuf, kPlayBufSize, nullptr,
                    nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}
}  // namespace jak2
