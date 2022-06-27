#pragma once

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"

extern Ptr<u32> print_column;
extern u32 ListenerStatus;

void klisten_init_globals();
void ClearPending();
void SendAck();