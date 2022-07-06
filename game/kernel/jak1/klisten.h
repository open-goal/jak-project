#pragma once

#include "game/kernel/jak1/kscheme.h"

namespace jak1 {
extern Ptr<jak1::Symbol> ListenerFunction;
extern Ptr<jak1::Symbol> kernel_dispatcher;
extern Ptr<jak1::Symbol> kernel_packages;

void klisten_init_globals();

void InitListener();
void ProcessListenerMessage(Ptr<char> msg);
}  // namespace jak1