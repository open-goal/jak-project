#pragma once

#include "game/overlord/jak3/list.h"

namespace jak3 {
void jak3_overlord_init_globals_streamlist();

extern List g_RequestedStreamsList;
extern List g_NewStreamsList;


void QueueNewStreamsFromList(List* list);
}