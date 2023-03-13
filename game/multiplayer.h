#pragma once

#include "kernel/common/kernel_types.h"

extern MultiplayerInfo& gMultiplayerInfo;

void pc_http_register(MultiplayerInfo mpInfo);
void pc_http_update_position();
void pc_http_get_positions();
