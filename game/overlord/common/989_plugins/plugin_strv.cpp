#include "plugin_strv.h"

#include <cstring>
#include <mutex>
#include <queue>

#include "common/log/log.h"

#include "game/sce/iop.h"
#include "game/sound/989snd/plugin.h"
#include "game/sound/sndshim.h"

namespace plugin_strv {

namespace {

PluginStreamOps g_ops;

std::mutex g_req_mtx;
std::queue<PluginStreamRequest> g_reqs;

void PushRequest(const PluginStreamRequest& req) {
  {
    std::scoped_lock lock(g_req_mtx);
    g_reqs.push(req);
  }
  if (g_ops.wakeup_thread != 0) {
    iop::iWakeupThread(g_ops.wakeup_thread);
  }
}

s32 ResolveVol(const snd::VagStreamPluginData& msg, const s8* regs, s32 cur_vol) {
  s32 vol;
  if (msg.use_regs) {
    vol = (regs[0] << 10) / 127;
  } else {
    vol = ((s32)msg.vol << 10) / 127;
  }
  return (vol * cur_vol) >> 10;
}

s32 ResolvePan(const snd::VagStreamPluginData& msg, const s8* regs) {
  if (msg.use_regs) {
    return 360 * regs[1] / 127;
  }
  return 360 * (s32)msg.pan / 127;
}

s32 VagStreamPluginHandler(u32 id,
                           u32 index,
                           u32 owner,
                           const u8* data,
                           const s8* regs,
                           s32 cur_vol) {
  if (id != snd::VAG_STREAM_PLUGIN_ID) {
    return 0;
  }

  snd::VagStreamPluginData msg{};
  memcpy(&msg, data, sizeof(msg));

  PluginStreamRequest req{};
  req.owner = owner;
  strncpy(req.name, msg.name, 8);

  switch (index) {
    case 0:  // QueueVagStream989
      req.op = PluginOp::Queue;
      req.prio = (msg.priority != 0 && msg.priority < 10) ? (s32)msg.priority : 4;
      req.vol = ResolveVol(msg, regs, cur_vol);
      req.pan = ResolvePan(msg, regs);
      PushRequest(req);
      break;
    case 3:  // StopVagStream989
      req.op = PluginOp::Stop;
      PushRequest(req);
      break;
    case 5:  // SetVagStreamVolume989
      req.op = PluginOp::SetVol;
      req.vol = ResolveVol(msg, regs, cur_vol);
      PushRequest(req);
      break;
    case 7:  // StopEmAll989
      req.op = PluginOp::StopAll;
      PushRequest(req);
      break;
    default:
      lg::info("VagStreamPluginHandler: unhandled idx {}", index);
      break;
  }

  return 0;
}

}  // namespace

void Init(const PluginStreamOps& ops) {
  g_ops = ops;
  snd_RegisterPluginHandler(VagStreamPluginHandler);
}

void HandleRequests() {
  while (true) {
    PluginStreamRequest req;
    {
      std::scoped_lock lock(g_req_mtx);
      if (g_reqs.empty()) {
        break;
      }
      req = g_reqs.front();
      g_reqs.pop();
    }

    switch (req.op) {
      case PluginOp::Queue:
        if (g_ops.queue) {
          g_ops.queue(req);
        }
        break;
      case PluginOp::Stop:
        if (g_ops.stop) {
          g_ops.stop(req);
        }
        break;
      case PluginOp::SetVol:
        if (g_ops.set_vol) {
          g_ops.set_vol(req);
        }
        break;
      case PluginOp::StopAll:
        if (g_ops.stop_all) {
          g_ops.stop_all();
        }
        break;
    }
  }
}

}  // namespace plugin_strv
