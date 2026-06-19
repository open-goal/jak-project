#include <cstring>

#include "plugin.h"

#include "common/log/log.h"

namespace snd {

namespace {
PluginHandler g_plugin_handler = nullptr;

s32 LogPluginMessage(u32 id,
                     u32 index,
                     u32 owner,
                     const u8* data,
                     const s8* /*regs*/,
                     s32 /*cur_vol*/) {
  char id_str[4] = {static_cast<char>(id >> 24), static_cast<char>(id >> 16),
                    static_cast<char>(id >> 8), static_cast<char>(id)};
  switch (id) {
    case VAG_STREAM_PLUGIN_ID: {
      // skip SetVagStreamVolume so it doesn't spam logs
      if (index == 5) {
        return 0;
      }

      VagStreamPluginData msg{};
      memcpy(&msg, data, sizeof(msg));
      char name[9] = {};
      memcpy(name, msg.name, 8);

      lg::info("989snd plugin: {} index={} owner={} name='{}' prio={} vol={} pan={} use_regs={}",
               std::string_view(id_str, 4), index, owner, name, msg.priority, msg.vol, msg.pan,
               msg.use_regs);
      return 0;
    }
    default:
      lg::info("989snd plugin: unhandled id={} ({:#x}) index={} owner={}",
               std::string_view(id_str, 4), id, index, owner);
      return 0;
  }
}
}  // namespace

void RegisterPluginHandler(PluginHandler handler) {
  g_plugin_handler = handler;
}

s32 HandlePluginMessage(u32 id, u32 index, u32 owner, const u8* data, const s8* regs, s32 cur_vol) {
  if (g_plugin_handler) {
    // LogPluginMessage(id, index, owner, data, regs, cur_vol);
    return g_plugin_handler(id, index, owner, data, regs, cur_vol);
  }
  // default handler
  return LogPluginMessage(id, index, owner, data, regs, cur_vol);
}

}  // namespace snd
