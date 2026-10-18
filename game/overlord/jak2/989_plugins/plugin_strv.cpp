#include "plugin_strv.h"

#include <cstring>

#include "../../common/989_plugins/plugin_strv.h"
#include "game/overlord/jak2/ssound.h"
#include "game/overlord/jak2/streamlist.h"
#include "game/overlord/jak2/vag.h"
#include "game/sce/iop.h"

using namespace iop;
using plugin_strv::PluginStreamRequest;

namespace jak2 {

namespace {

int g_PluginId = 0;

void DoQueue(const PluginStreamRequest& req) {
  VagStrListNode key{};
  strncpy(key.name, req.name, 0x30);
  key.id = req.owner;
  if (!++g_PluginId) {
    g_PluginId = 1;
  }
  key.unk_68 = g_PluginId;
  key.sound_handler = req.owner;
  key.prio = req.prio;
  key.vol_multiplier = req.vol;
  key.unk_100 = req.pan;

  WaitSema(PluginStreamsList.sema);
  if (!FindVagStreamInList(&key, &PluginStreamsList) &&
      InsertVagStreamInList(&key, &PluginStreamsList)) {
    WaitSema(EEPlayList.sema);
    if (!FindVagStreamInList(&key, &EEPlayList)) {
      auto* node = InsertVagStreamInList(&key, &EEPlayList);
      if (node) {
        strncpy(node->name, req.name, 0x30);
        node->id = req.owner;
        node->unk_68 = g_PluginId;
        node->sound_handler = req.owner;
        node->unk_76 = 0;
        node->unk_80 = 0;
        node->prio = req.prio;
        node->unk_88 = 0;
        node->unk_92 = 0;
        node->vol_multiplier = req.vol;
        node->unk_100 = req.pan;
      }
    }
    SignalSema(EEPlayList.sema);
  }
  SignalSema(PluginStreamsList.sema);
}

void DoStop(const PluginStreamRequest& req) {
  VagStrListNode key{};
  strncpy(key.name, req.name, 0x30);
  key.id = req.owner;
  WaitSema(PluginStreamsList.sema);
  RemoveVagStreamFromList(&key, &PluginStreamsList);
  SignalSema(PluginStreamsList.sema);
}

void DoSetVol(const PluginStreamRequest& req) {
  VagCmd* cmd = FindThisVagStream(req.name, req.owner);
  if (cmd) {
    cmd->vol_multiplier = req.vol;
    SetVAGVol(cmd, 1);
  }
}

void DoStopAll() {
  for (auto& cmd : VagCmds) {
    if (cmd.id != 0) {
      VagStrListNode key{};
      strncpy(key.name, cmd.name, 0x30);
      key.id = cmd.id;
      WaitSema(PluginStreamsList.sema);
      RemoveVagStreamFromList(&key, &PluginStreamsList);
      SignalSema(PluginStreamsList.sema);
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
  ops.wakeup_thread = StreamThread;
  plugin_strv::Init(ops);
}

void HandlePluginRequests() {
  plugin_strv::HandleRequests();
}

}  // namespace jak2
