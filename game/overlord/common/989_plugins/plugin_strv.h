#pragma once

#include "common/common_types.h"

namespace plugin_strv {

enum class PluginOp { Queue, Stop, SetVol, StopAll };

struct PluginStreamRequest {
  PluginOp op{};
  u32 owner{};
  char name[0x30]{};
  s32 prio{};
  s32 vol{};
  s32 pan{};
};

struct PluginStreamOps {
  void (*queue)(const PluginStreamRequest&) = nullptr;
  void (*stop)(const PluginStreamRequest&) = nullptr;
  void (*set_vol)(const PluginStreamRequest&) = nullptr;
  void (*stop_all)() = nullptr;
  s32 wakeup_thread = 0;
};

void Init(const PluginStreamOps& ops);
void HandleRequests();

}  // namespace plugin_strv
