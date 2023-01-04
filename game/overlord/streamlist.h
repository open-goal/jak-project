#ifndef STREAMLIST_H_
#define STREAMLIST_H_

#include "list.h"
#include "vag.h"

extern List RequestedStreamsList;
extern List NewStreamsList;
extern List EEStreamsList;
extern List EEPlayList;
extern List PluginStreamsList;

VagStream* InsertVagStreamInList(VagStream* stream, List* list);
void RemoveVagSreamFromList(VagStream* stream, List* list);
VagStream* FindVagStreamInList(VagStream* stream, List* list);
void EmptyVagStreamList(List* list);

#endif  // STREAMLIST_H_
