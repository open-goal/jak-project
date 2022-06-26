#pragma once

/*!
 * @file klisten.h
 * Implementation of the Listener protocol
 * Done
 */

#include "game/kernel/jak1/kscheme.h"

extern Ptr<jak1::Symbol> ListenerFunction;
extern Ptr<jak1::Symbol> kernel_dispatcher;
extern Ptr<u32> print_column;
extern Ptr<jak1::Symbol> kernel_packages;

void klisten_init_globals();
void InitListener();
void ClearPending();
void SendAck();
void ProcessListenerMessage(Ptr<char> msg);
