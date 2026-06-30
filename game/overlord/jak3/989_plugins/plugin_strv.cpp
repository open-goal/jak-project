#include "plugin_strv.h"

#include <cstring>

#include "../../common/989_plugins/plugin_strv.h"
#include "game/overlord/jak3/streamlist.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"

using namespace iop;
using plugin_strv::PluginStreamRequest;

namespace jak3 {

namespace {

int g_PluginId = 0;

void DoQueue(const PluginStreamRequest& req) {
  VagStreamData key{};
  strncpy(key.name, req.name, 0x30);
  key.id = req.owner;
  if (!++g_PluginId) {
    g_PluginId = 1;
  }
  key.plugin_id = g_PluginId;
  key.sound_handler = req.owner;
  key.priority = req.prio;
  key.volume2 = req.vol;
  key.maybe_volume_3 = 0;
  key.group = 2;
  key.pan = req.pan;
  key.art_load = 0;
  key.movie_art_load = 0;

  WaitSema(g_PluginStreamsList.sema);
  if (!FindVagStreamInList(&key, &g_PluginStreamsList) &&
      InsertVagStreamInList(&key, &g_PluginStreamsList)) {
    WaitSema(g_EEPlayList.sema);
    if (!FindVagStreamInList(&key, &g_EEPlayList)) {
      InsertVagStreamInList(&key, &g_EEPlayList);
    }
    SignalSema(g_EEPlayList.sema);
  }
  SignalSema(g_PluginStreamsList.sema);
}

void DoStop(const PluginStreamRequest& req) {
  VagStreamData key{};
  strncpy(key.name, req.name, 0x30);
  key.id = req.owner;
  WaitSema(g_PluginStreamsList.sema);
  RemoveVagStreamFromList(&key, &g_PluginStreamsList);
  SignalSema(g_PluginStreamsList.sema);
}

void DoSetVol(const PluginStreamRequest& req) {
  ISO_VAGCommand* cmd = FindThisVagStream(req.name, req.owner);
  if (cmd) {
    cmd->play_volume = req.vol;
    SetVAGVol(cmd);
  }
}

void DoStopAll() {
  for (auto& cmd : g_aVagCmds) {
    if (cmd.id != 0) {
      VagStreamData key{};
      strncpy(key.name, cmd.name, 0x30);
      key.id = cmd.id;
      WaitSema(g_PluginStreamsList.sema);
      RemoveVagStreamFromList(&key, &g_PluginStreamsList);
      SignalSema(g_PluginStreamsList.sema);
    }
  }
}

}  // namespace

void Init989Plugins() {
  g_PluginId = 0;
  plugin_strv::PluginStreamOps ops;
  ops.queue = DoQueue;
  ops.stop = DoStop;
  ops.set_vol = DoSetVol;
  ops.stop_all = DoStopAll;
  ops.wakeup_thread = 0;
  plugin_strv::Init(ops);
}

void HandlePluginRequests() {
  plugin_strv::HandleRequests();
}

}  // namespace jak3
