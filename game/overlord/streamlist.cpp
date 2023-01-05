#include "streamlist.h"

#include <cstring>

List RequestedStreamsList;
List NewStreamsList;
List EEStreamsList;
List EEPlayList;
List PluginStreamsList;

VagStream* InsertVagStreamInList(VagStream* stream, List* list) {
  return nullptr;
}

void RemoveVagSreamFromList(VagStream* stream, List* list) {
  VagStream* elem = (VagStream*)list->head;
  VagStream* found = nullptr;

  if (list->elements) {
    for (int i = 0; i < list->elements; i++) {
      if (elem->id == stream->id && !strncmp(elem->name, stream->name, sizeof(elem->name))) {
        found = elem;
        break;
      }

      elem = (VagStream*)elem->l.next;
    }
  }

  if (found) {
    found->l.unk = 0;
    strncpy(found->name, "free", 48);
    found->id = 0;
    found->unk0x48 = 0;
    found->unk0x54 = 0;
    found->unk0x4c = 0;
    found->unk0x58 = 0;
    found->unk0x5c = 0;
    found->unk0x60 = 0;
    found->unk0x64 = 0;
    list->unk0x10 = 1;
  }
}

VagStream* FindVagStreamInList(VagStream* stream, List* list) {
  return nullptr;
}

void EmptyVagStreamList(List* list) {
  VagStream* elem = (VagStream*)list->head;
  for (int i = 0; i < list->elements; i++) {
    strncpy(elem->name, "free", sizeof(elem->name));
    elem->id = 0;
    elem->unk0x48 = 0;
    elem->unk0x54 = 0;
    elem->unk0x4c = 0;
    elem->unk0x58 = 0;
    elem->unk0x5c = 0;
    elem->unk0x60 = 0;
    elem->unk0x64 = 0;
    elem->l.unk = 0;
    elem++;
  }

  list->unk0x10 = 1;
}
