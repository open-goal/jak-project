#pragma once

#include "kernel/common/kernel_types.h"

void http_register(u64 mpInfo, u64 selfPlayerInfo);
void http_update();
void http_mark_found(int idx);
void http_get();
