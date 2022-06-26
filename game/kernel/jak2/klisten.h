#pragma once
#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/Symbol4.h"
namespace jak2 {
extern Ptr<Symbol4<u32>> ListenerLinkBlock;
extern Ptr<Symbol4<u32>> ListenerFunction;
extern Ptr<Symbol4<u32>> KernelFunction;  // new in jak2
extern Ptr<Symbol4<u32>> kernel_dispatcher;
extern Ptr<Symbol4<u32>> sync_dispatcher;  // new in jak2
extern Ptr<Symbol4<u32>> kernel_packages;

void klisten_init_globals();
}  // namespace jak2