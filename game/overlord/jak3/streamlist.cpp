#include "streamlist.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;
List g_RequestedStreamsList;
List g_NewStreamsList;
List g_EEStreamsList;
List g_EEPlayList;
void jak3_overlord_init_globals_streamlist() {
  g_RequestedStreamsList = {};
  g_NewStreamsList = {};
  g_EEStreamsList = {};
  g_EEPlayList = {};
}

void InitVagStreamList(List* list, int size, const char* name) {
  strncpy(list->name, name, 8);

  InitList(list, size, sizeof(VagStreamData));

  auto* iter = list->next;
  if (0 < size) {
    do {
      iter->in_use = 0;
      strncpy(iter->name, "free", 0x30);
      iter->group = 2;
      iter->id = 0;
      iter->sound_handler = 0;
      iter->priority = 0;
      iter->art_load = 0;
      iter->movie_art_load = 0;
      iter->unk2 = 0;
      iter->unk1 = 0;
      iter->volume2 = 0;
      iter->maybe_volume_3 = 0;
      iter = iter + 1;
      size = size + -1;
    } while (size != 0);
  }

  ASSERT(list->buffer);
}

VagStreamData* FindVagStreamInList(VagStreamData* stream, List* list) {
  int iVar1;
  VagStreamData* iter;
  u32 max_idx;
  u32 idx;
  u32 uVar2;
  VagStreamData* ret;
  VagStreamData* pVVar3;

  max_idx = list->count;
  iter = list->next;
  ret = nullptr;
  idx = 0;
  if (max_idx != 0) {
    do {
      uVar2 = idx;
      pVVar3 = ret;
      if ((iter->id != stream->id) || (iVar1 = strncmp(iter->name, stream->name, 0x30),
                                       uVar2 = max_idx, pVVar3 = iter, iVar1 == 0)) {
        ret = pVVar3;
        idx = uVar2;
      }
      idx = idx + 1;
      iter = iter->next;
    } while (idx < max_idx);
  }
  return ret;
}

VagStreamData* GetVagStreamInList(u32 idx, List* list) {
  VagStreamData* iter = nullptr;
  if ((idx < (u32)list->count) && (iter = list->next, idx != 0)) {
    do {
      idx = idx - 1;
      iter = iter->next;
    } while (idx != 0);
  }
  return iter;
}

void EmptyVagStreamList(List* list) {
  VagStreamData* elt;
  u32 i;
  u32 cnt;

  cnt = list->count;
  elt = (VagStreamData*)list->buffer;
  i = 0;
  if (cnt != 0) {
    do {
      i = i + 1;
      strncpy(elt->name, "free", 0x30);
      elt->group = 2;
      elt->id = 0;
      elt->sound_handler = 0;
      elt->priority = 0;
      elt->art_load = 0;
      elt->movie_art_load = 0;
      elt->unk2 = 0;
      elt->unk1 = 0;
      elt->volume2 = 0;
      elt->maybe_volume_3 = 0;
      elt->in_use = 0;
      elt = elt + 1;
    } while (i < cnt);
  }
  list->unk_flag = 1;
}

void RemoveVagStreamFromList(VagStreamData* stream, List* list) {
  VagStreamData* elt = FindVagStreamInList(stream, list);
  if (elt) {
    elt->in_use = 0;
    strncpy(elt->name, "free", 0x30);
    elt->group = 2;
    elt->id = 0;
    elt->priority = 0;
    elt->art_load = 0;
    elt->movie_art_load = 0;
    elt->unk1 = 0;
    elt->volume2 = 0;
    elt->maybe_volume_3 = 0;
    elt->sound_handler = 0;
    list->unk_flag = 1;
    elt->unk2 = 0;
  }
}

VagStreamData* InsertVagStreamInList(VagStreamData* user_stream, List* list) {
  u32 uVar1;
  VagStreamData* pVVar10;
  VagStreamData* pVVar11;
  VagStreamData* pVVar12;

  u32 count = list->count;
  VagStreamData* free_elt = nullptr;
  u32 free_elt_idx = 0;
  VagStreamData* iter = list->next;
  if (count != 0) {
    do {
      if (iter->id == 0) {
        free_elt = iter;
        free_elt_idx = count;
      }
      free_elt_idx = free_elt_idx + 1;
      iter = iter->next;
    } while (free_elt_idx < count);
  }
  if ((free_elt != (VagStreamData*)0x0) &&
      (uVar1 = 0, pVVar11 = list->next, pVVar12 = nullptr, count != 0)) {
    do {
      pVVar10 = pVVar11;
      uVar1 = uVar1 + 1;
      if (pVVar10->priority < user_stream->priority) {
        list->unk_flag = 1;
        free_elt->in_use = 1;
        strncpy(free_elt->name, user_stream->name, 0x30);
        free_elt->id = user_stream->id;
        free_elt->plugin_id = user_stream->plugin_id;
        free_elt->art_load = user_stream->art_load;
        free_elt->movie_art_load = user_stream->movie_art_load;
        free_elt->priority = user_stream->priority;
        free_elt->sound_handler = user_stream->sound_handler;
        free_elt->volume2 = user_stream->volume2;
        free_elt->maybe_volume_3 = user_stream->maybe_volume_3;
        free_elt->group = user_stream->group;
        free_elt->unk1 = 0;
        if (pVVar12 == (VagStreamData*)0x0) {
          if (free_elt == pVVar10) {
            return free_elt;
          }
          auto* prev = free_elt->next;
          auto* next = free_elt->prev;
          list->next = free_elt;
          prev->prev = next;
          pVVar11 = free_elt->prev;
          pVVar10->prev = free_elt;
          pVVar11->next = prev;
          free_elt->prev = nullptr;
          free_elt->next = pVVar10;
          return free_elt;
        }
        if (free_elt == pVVar10) {
          return free_elt;
        }
        auto* pVVar13 = free_elt->prev;
        pVVar13->next = free_elt->next;
        pVVar11 = pVVar12->next;
        free_elt->next->prev = pVVar13;
        free_elt->next = pVVar11;
        pVVar10->prev = free_elt;
        pVVar12->next = free_elt;
        free_elt->prev = pVVar12;
        return free_elt;
      }
      pVVar11 = pVVar10->next;
      pVVar12 = pVVar10;
    } while (uVar1 < count);
  }
  return free_elt;
}

void MergeVagStreamLists(List* list_a, List* list_b) {
  VagStreamData* stream;
  VagStreamData* pVVar1;
  u32 uVar2;
  u32 idx;

  idx = 0;
  uVar2 = 0;
LAB_0000fde8:
  do {
    stream = GetVagStreamInList(idx, list_a);
    idx = idx + 1;
    if (stream != (VagStreamData*)0x0) {
      if (stream->id == 0)
        goto LAB_0000fde8;
      pVVar1 = FindVagStreamInList(stream, list_b);
      if (pVVar1 == (VagStreamData*)0x0) {
        InsertVagStreamInList(stream, list_b);
      }
    }
    uVar2 = uVar2 + 1;
    if (3 < uVar2) {
      return;
    }
  } while (true);
}

void QueueNewStreamsFromList(List* list) {
  VagStreamData* stream;
  ISO_VAGCommand* pIVar1;
  u32 uVar2;
  u32 idx;

  SetVagStreamsNotScanned();
  idx = 0;
  EmptyVagStreamList(&g_NewStreamsList);
  g_NewStreamsList.unk_flag = 0;
  uVar2 = 0;
LAB_0000fe94:
  do {
    stream = GetVagStreamInList(idx, list);
    idx = idx + 1;
    if (stream == (VagStreamData*)0x0) {
      uVar2 = 4;
    } else {
      if (stream->id == 0)
        goto LAB_0000fe94;
      pIVar1 = FindThisVagStream(stream->name, stream->id);
      if (pIVar1 == (ISO_VAGCommand*)0x0) {
        pIVar1 = FindThisVagStream(stream->name, stream->id);
        if (pIVar1 == (ISO_VAGCommand*)0x0) {
          InsertVagStreamInList(stream, &g_NewStreamsList);
        }
      } else {
        pIVar1->flags.scanned = 1;
        if (pIVar1->stereo_sibling != (ISO_VAGCommand*)0x0) {
          pIVar1->stereo_sibling->flags.scanned = 1;
        }
        if (stream->priority != pIVar1->priority_pq) {
          SetNewVagCmdPri(pIVar1, stream->priority);
        }
      }
    }
    uVar2 = uVar2 + 1;
    if (3 < uVar2) {
      return;
    }
  } while (true);
}

void CheckPlayList(List* list) {
  int count;
  ISO_VAGCommand* cmd;
  VagStreamData* iter;

  count = list->count;
  iter = list->next;
joined_r0x0000ff80:
  do {
    while (true) {
      if (count == 0) {
        return;
      }
      count = count + -1;
      if (iter->id != 0)
        break;
      iter = iter->next;
    }
    cmd = FindThisVagStream(iter->name, iter->id);
  } while (cmd == (ISO_VAGCommand*)0x0);
  if (cmd->flags.running == 0)
    goto code_r0x0000ffc4;
  goto LAB_00010004;
code_r0x0000ffc4:
  if (((cmd->flags.saw_chunks1 != 0) || (cmd->flags.file_disappeared != 0)) &&
      (cmd->flags.nostart == 0)) {
    IsoPlayVagStream(cmd);
  LAB_00010004:
    RemoveVagStreamFromList(iter, list);
  }
  goto joined_r0x0000ff80;
}

void StreamListThread() {
  if (g_RequestedStreamsList.pending_data == 0) {
    WaitSema(g_RequestedStreamsList.sema);
    EmptyVagStreamList(&g_RequestedStreamsList);
    g_RequestedStreamsList.unk_flag = 0;
    //    WaitSema(DAT_00015dd0);
    //    MergeVagStreamLists((List*)&g_PluginStreamsList, &g_RequestedStreamsList);
    //    SignalSema(DAT_00015dd0);
    WaitSema(g_EEStreamsList.sema);
    MergeVagStreamLists(&g_EEStreamsList, &g_RequestedStreamsList);
    SignalSema(g_EEStreamsList.sema);
    g_RequestedStreamsList.pending_data = 1;
    SignalSema(g_RequestedStreamsList.sema);
    WaitSema(g_EEPlayList.sema);
    CheckPlayList(&g_EEPlayList);
    SignalSema(g_EEPlayList.sema);
    //    WaitSema(DAT_0001e31c);
    //    CheckLfoList(&g_LfoStreamsList);
    //    SignalSema(DAT_0001e31c);
  }
}
}  // namespace jak3