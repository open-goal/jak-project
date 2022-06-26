#pragma once

#include "game/kernel/common/kmalloc.h"

void kmemopen_from_c(Ptr<kheapinfo> heap, const char* name);
void kmemopen(u32 heap, u32 name);
void kmemclose();
