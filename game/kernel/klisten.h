#pragma once

/*!
 * @file klisten.h
 * Implementation of the Listener protocol
 * Done
 */


#include "kscheme.h"

extern Ptr<Symbol> ListenerFunction;
extern Ptr<Symbol> kernel_dispatcher;
extern Ptr<u32> print_column;
extern Ptr<Symbol> kernel_packages;

void klisten_init_globals();
void InitListener();
void ClearPending();
void SendAck();
void ProcessListenerMessage(Ptr<char> msg);

