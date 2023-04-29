#pragma once

#include "game/overlord/jak2/list.h"

namespace jak2 {

struct VagStrListNode {
  ListNode list;
  char name[48];
  int unk_60;
  int id;
  int unk_68;
  int unk_72;
  int unk_76;
  int unk_80;
  int prio;
  int unk_88;
  int unk_92;
  int vol_multiplier;
  int unk_100;
};

struct LfoListNode {
  ListNode list;
  int unk_12;
  int unk_16;
  int unk_20;
  int unk_24;
  int unk_28;
  int unk_32;
  int id;
  int plugin_id;
};

extern List PluginStreamsList;
extern List LfoList;
extern List EEPlayList;
extern List RequestedStreamsList;
extern List NewStreamsList;
extern List EEStreamsList;

void init_globals_streamlist();
void RemoveVagStreamFromList(VagStrListNode* param_1, List* param_2);
void EmptyVagStreamList(List* param_1);
VagStrListNode* InsertVagStreamInList(VagStrListNode* param_1, List* param_2);
VagStrListNode* FindVagStreamInList(VagStrListNode* param_1, List* param_2);
void QueueNewStreamsFromList(List* list);
bool InitVagStreamList(List* param_1, u32 param_2, const char* param_3);
u32 StreamListThread();
void RemoveLfoStreamFromList(void*, void*);
}  // namespace jak2
