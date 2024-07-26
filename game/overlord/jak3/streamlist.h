#pragma once

#include "game/overlord/jak3/list.h"

namespace jak3 {
void jak3_overlord_init_globals_streamlist();

struct ISO_VAGCommand;

extern List g_RequestedStreamsList;
extern List g_NewStreamsList;
extern List g_EEStreamsList;
extern List g_EEPlayList;

void QueueNewStreamsFromList(List* list);
void RemoveVagStreamFromList(VagStreamData* entry, List* list);
void EmptyVagStreamList(List* list);
VagStreamData* InsertVagStreamInList(VagStreamData* entry, List* list);
VagStreamData* FindVagStreamInList(VagStreamData* entry, List* list);
void InitVagStreamList(List* list, int size, const char* name);
void StreamListThread();
}  // namespace jak3