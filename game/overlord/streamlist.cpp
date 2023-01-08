#include "streamlist.h"

#include <cstring>

List RequestedStreamsList;
List NewStreamsList;
List EEStreamsList;
List EEPlayList;
List PluginStreamsList;

static void resetVagStream(VagStream* stream) {
  strncpy(stream->name, "free", sizeof(stream->name));
  stream->id = 0;
  stream->unk0x48 = 0;
  stream->unk0x4c = 0;
  stream->unk0x54 = 0;
  stream->unk0x58 = 0;
  stream->unk0x5c = 0;
  stream->unk0x60 = 0;
  stream->unk0x64 = 0;
  stream->l.unk = 0;
}

VagStream* FindVagStreamInList(VagStream* stream, List* list) {
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

  return found;
}

static VagStream* getFreeVagStreamEntry(List* list) {
  VagStream* elem = (VagStream*)list->head;
  VagStream* found = nullptr;

  if (list->elements) {
    for (int i = 0; i < list->elements; i++) {
      if (!elem->id) {
        found = elem;
        break;
      }

      elem = (VagStream*)elem->l.next;
    }
  }

  return found;
}

VagStream* InsertVagStreamInList(VagStream* stream, List* list) {
  VagStream* free = getFreeVagStreamEntry(list);

  if (free == nullptr) {
    printf("IOP: ======================================================================\n");
    printf("IOP: streamlist InsertVagStreamInList: no free spot in list %s\n", list->name);
    printf("IOP: ======================================================================\n");
    return free;
  }

  for (int i = 0; i < list->elements; i++) {
  }

  return free;
}

void RemoveVagSreamFromList(VagStream* stream, List* list) {
  VagStream* found = FindVagStreamInList(stream, list);

  if (found) {
    resetVagStream(found);
    list->unk0x10 = 1;
  }
}

void EmptyVagStreamList(List* list) {
  VagStream* elem = (VagStream*)list->head;
  for (int i = 0; i < list->elements; i++) {
    resetVagStream(elem);
    elem++;
  }

  list->unk0x10 = 1;
}
