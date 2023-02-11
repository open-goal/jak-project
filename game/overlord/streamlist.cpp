#include "streamlist.h"

#include <cstring>

#include "streamlfo.h"

#include "game/overlord/iso.h"

using namespace iop;

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
  stream->l.unk0x8 = 0;
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

void InitVagStreamList(List* list, u32 elements, const char* listName) {
  strncpy(list->name, listName, sizeof(list->name));
  InitList(list, elements, sizeof(VagStream));

  // Probably not actually inlined?
  // this function doesn't normally set list->unk0x10
  // TODO does it matter?
  EmptyVagStreamList(list);
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

VagStream* InsertVagStreamInList(VagStream* stream, List* list) {
  VagStream* free = getFreeVagStreamEntry(list);
  VagStream* walk = (VagStream*)list->head;
  VagStream* slot = nullptr;

  if (free == nullptr) {
    printf("IOP: ======================================================================\n");
    printf("IOP: streamlist InsertVagStreamInList: no free spot in list %s\n", list->name);
    printf("IOP: ======================================================================\n");
    return free;
  }

  // Find where to slot it in based on priority
  int idx = 0;
  while (true) {
    idx++;
    if (walk->unk0x54 < stream->unk0x54) {
      break;
    }

    slot = walk;
    walk = (VagStream*)walk->l.next;

    if (idx >= list->elements) {
      return free;
    }
  }

  list->unk0x10 = 1;
  free->l.unk0x8 = 1;
  strncpy(free->name, stream->name, sizeof(free->name));
  free->id = stream->id;
  free->unk0x44 = stream->unk0x44;
  free->unk0x48 = stream->unk0x48;
  free->unk0x4c = stream->unk0x4c;
  free->unk0x50 = stream->unk0x50;
  free->unk0x54 = stream->unk0x54;
  free->unk0x5c = 0;
  free->unk0x60 = stream->unk0x60;
  free->unk0x64 = stream->unk0x64;

  if (free == walk) {
    return free;
  }

  if (slot) {
    free->l.prev->next = free->l.next;
    free->l.next->prev = free->l.prev;
    free->l.next = slot->l.next;
    slot->l.next = &free->l;
    walk->l.prev = &free->l;
    free->l.prev = &slot->l;
  } else {
    free->l.next->prev = free->l.prev;
    free->l.prev->next = free->l.next;
    walk->l.prev = &free->l;
    free->l.next = &walk->l;
    free->l.prev = nullptr;
    list->head = &free->l;
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

void MergeVagStreamLists(List* src, List* dest) {
  u32 index = 0;
  for (int i = 0; i < 4; ++i) {
    while (true) {
      VagStream* walk = nullptr;

      if (index < src->elements) {
        walk = (VagStream*)src->head;
        for (int j = index; j; --j) {
          walk = (VagStream*)walk->l.next;
        }
      }

      index++;

      if (!walk)
        break;

      if (walk->id) {
        if (!FindVagStreamInList(walk, dest)) {
          InsertVagStreamInList(walk, dest);
        }
        break;
      }
    }
  }
}

void CheckPlayList(List* list) {
  VagStream* stream = (VagStream*)list->head;
  for (int i = list->elements - 1; i != 0; i--) {
    if (stream->id != 0) {
      VagCommand2* vag = FindThisVagStream(stream->name, stream->id);
      VagStream* free = nullptr;

      if (!vag) {
        continue;
      }

      if (vag->unk0xd4) {
        free = FindVagStreamInList(stream, list);
      } else {
        if (vag->unk0xd0 == 0 && vag->unk0xd6 == 0) {
          continue;
        }

        if (vag->unk0xe7 != 0) {
          continue;
        }

        IsoPlayVagStream(vag, 1);
        free = FindVagStreamInList(stream, list);
      }

      if (free) {
        resetVagStream(free);
        list->unk0x10 = 1;
      }
    } else {
      stream = (VagStream*)stream->l.next;
    }
  }
}

void StreamListThread() {
  while (true) {
    while (RequestedStreamsList.unk0x18) {
      SleepThread();
    }

    WaitSema(RequestedStreamsList.sema);
    EmptyVagStreamList(&RequestedStreamsList);
    RequestedStreamsList.unk0x10 = 0;

    WaitSema(PluginStreamsList.sema);
    MergeVagStreamLists(&PluginStreamsList, &RequestedStreamsList);
    SignalSema(PluginStreamsList.sema);

    WaitSema(EEStreamsList.sema);
    MergeVagStreamLists(&EEStreamsList, &RequestedStreamsList);
    SignalSema(EEStreamsList.sema);

    RequestedStreamsList.unk0x18 = 1;
    SignalSema(RequestedStreamsList.sema);

    WaitSema(EEPlayList.sema);
    CheckPlayList(&EEPlayList);
    SignalSema(EEPlayList.sema);

    WaitSema(LfoList.sema);
    CheckLfoList(&LfoList);
    SignalSema(LfoList.sema);
  }
}
