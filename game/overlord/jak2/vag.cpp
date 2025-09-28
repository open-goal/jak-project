#include "vag.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak2/iso_queue.h"
#include "game/overlord/jak2/spustreams.h"
#include "game/overlord/jak2/srpc.h"
#include "game/overlord/jak2/ssound.h"
#include "game/overlord/jak2/streamlist.h"
#include "game/sound/sdshim.h"

namespace jak2 {
VagCmd VagCmds[N_VAG_CMDS];
int StreamSRAM[N_VAG_CMDS];
int TrapSRAM[N_VAG_CMDS];
int StreamVoice[N_VAG_CMDS];

// sizes seem sketchy
// maybe pri 0 is a special 'free' priority.
VagCmdPriListEntry VagCmdsPriList[11];
int VagCmdsPriCounter[11];
int ActiveVagStreams;

void CalculateVAGVolumes(VagCmd* cmd, u32* l_out, u32* r_out);
void StopVAG(VagCmd* cmd, int /*param_2*/);

enum VolumeCategory {
  DIALOGUE = 2,  // VAG streams. Copied "dialogue" name from jak 1.
};
int MasterVolume[32];

void vag_init_globals() {
  memset(VagCmds, 0, sizeof(VagCmds));
  memset(StreamSRAM, 0, sizeof(StreamSRAM));
  memset(TrapSRAM, 0, sizeof(TrapSRAM));
  memset(StreamVoice, 0, sizeof(StreamVoice));
  memset(VagCmdsPriList, 0, sizeof(VagCmdsPriList));
  memset(VagCmdsPriCounter, 0, sizeof(VagCmdsPriCounter));

  for (auto& x : MasterVolume) {
    x = 0x400;  // check!!!
  }
  ActiveVagStreams = 0;
}

void InitVagCmds() {
  int cmd_idx = 0;
  for (auto& cmd : VagCmds) {
    for (auto& x : cmd.status_bytes) {
      x = 0;
    }
    // puVar5 is offset 292
    cmd.unk_236 = 0;                          // puVar5[-0xe] = 0;
    cmd.unk_140 = 0;                          // puVar5[-0x26] = 0;
    cmd.sb_paused = 1;                        // *(undefined*)((int)puVar5 + -0x52) = 1;
    cmd.unk_196 = 0;                          // puVar5[-0x18] = 0;
    cmd.unk_200 = 0;                          // puVar5[-0x17] = 0;
    cmd.unk_204 = 0;                          // puVar5[-0x16] = 0;
    cmd.unk_180 = 0;                          // puVar5[-0x1c] = 0;
    cmd.unk_184 = 0;                          // puVar5[-0x1b] = 0;
    cmd.unk_188 = 0;                          // puVar5[-0x1a] = 0;
    cmd.unk_192 = 0;                          // puVar5[-0x19] = 0;
    cmd.status_bytes[VagCmdByte::BYTE5] = 0;  // *(undefined*)((int)puVar5 + -0x4f) = 0;
    cmd.status_bytes[VagCmdByte::BYTE6] = 0;  // *(undefined*)((int)puVar5 + -0x4e) = 0;
    cmd.num_processed_chunks = 0;             // puVar5[-0xd] = 0;
    cmd.safe_to_change_dma_fields = 1;        // puVar5[-0x3a] = 1;
    cmd.xfer_size = 0;                        // puVar5[-0xc] = 0;
    cmd.sample_rate = 0;                      // puVar5[-0xb] = 0;
    cmd.unk_260 = 0;                          // puVar5[-8] = 0;
    cmd.unk_264 = 0x4000;                     // puVar5[-7] = 0x4000;
    cmd.unk_268 = 0;                          // puVar5[-6] = 0;
    cmd.header.callback_buffer = nullptr;     // puVar5[-0x42] = 0;
    cmd.header.ready_for_data = 1;            // puVar5[-0x43] = 1;
    cmd.header.callback = NullCallback;       // puVar5[-0x41] = NullCallback;
    cmd.header.lse = nullptr;                 // puVar5[-0x40] = 0;
    cmd.stereo_sibling = nullptr;             // puVar5[-0x3d] = 0;
    cmd.dma_iop_mem_ptr = nullptr;            // puVar5[-0x3c] = 0;
    cmd.dma_chan = -1;                        // puVar5[-0x3b] = 0xffffffff;
    cmd.unk_256_pitch2 = 0;                   // puVar5[-9] = 0;
    cmd.unk_296 = 0;                          // puVar5[1] = 0;
    cmd.vec3.x = 0;                           // puVar5[2] = 0;
    cmd.vec3.y = 0;                           // puVar5[3] = 0;
    cmd.vec3.z = 0;                           // puVar5[4] = 0;
    cmd.fo_min = 5;                           // puVar5[5] = 5;
    cmd.fo_max = 0x1e;                        // puVar5[6] = 0x1e;
    cmd.fo_curve = 1;                         // puVar5[7] = 1;
    // puVar5[-0x2b] = *(undefined4*)(StreamSRAM + cmd_idx * 4);
    cmd.spu_stream_dma_mem_addr = StreamSRAM[cmd_idx];
    // puVar5[-0x2a] = *(undefined4*)(TrapSRAM + cmd_idx * 4);
    cmd.spu_trap_mem_addr = TrapSRAM[cmd_idx];
    // pRVar7 = pRVar7 + 1;
    // puVar5[-0x28] = iVar6;
    cmd.idx_in_cmd_arr = cmd_idx;
    // iVar6 = iVar6 + 1;
    cmd.file_record = nullptr;    // puVar5[-0x3f] = 0;
    cmd.vag_dir_entry = nullptr;  // puVar5[-0x3e] = 0;
    cmd.sb_playing = 0;           // *(undefined*)((int)puVar5 + -0x53) = 0;
    cmd.vol_multiplier = 0;       // puVar5[-5] = 0;
    cmd.unk_256_pitch2 = 0;       // puVar5[-9] = 0;
    cmd.id = 0;                   // puVar5[-4] = 0;
    cmd.plugin_id = 0;            // puVar5[-3] = 0;
    cmd.sound_handler = 0;        // puVar5[-0x27] = 0;
    cmd.unk_176 = 0;              // puVar5[-0x1d] = 0;
    cmd.priority = 0;             // puVar5[-2] = 0;
    cmd.unk_288 = 0;              // puVar5[-1] = 0;
    cmd.unk_292 = 0;              // *puVar5 = 0;
    cmd.voice = StreamVoice[cmd_idx];
    // puVar5[-0x29] = uVar2;
    // puVar5 = puVar5 + 0x51;
    cmd_idx++;
  }

  for (auto& entry : VagCmdsPriList) {
    for (auto& c : entry.cmds) {
      c = nullptr;
    }
  }

  for (auto& c : VagCmdsPriCounter) {
    c = 0;
  }
  VagCmdsPriCounter[0] = 4;
}

/*!
 * Get a VagCmd from VagCmds for the given VagCmd.
 */
VagCmd* SmartAllocVagCmd(VagCmd* cmd) {
  VagCmd* selected = nullptr;

  // first, just try looking for any free commands in the list.
  for (auto& c : VagCmds) {
    if (c.id == 0) {
      // free!
      selected = &c;
      break;
    }
  }

  // next, try FindNotQueuedVagCmd
  if (!selected) {
    selected = FindNotQueuedVagCmd();
  }

  // next, try some existing ones.
  if (!selected) {
    int our_priority = cmd->priority;

    // if we have a nonzero priority, try taking over a lower priority command
    if (our_priority) {
      int check_priority = 0;
      do {
        // loop over other commands at this priority
        int cmd_at_pri_idx = 0;
        do {
          auto* potential_cmd = VagCmdsPriList[check_priority].cmds[cmd_at_pri_idx];
          if (potential_cmd) {
            // this part is a bit strange... as we iterate through lower priority commands, we
            // immediately take the first one with byte11 set to 0. But if this isn't 0, we keep
            // looking. This means that we won't take the lowest priority free command if they have
            // nonzero BYTE11's. (this would make sense if BYTE11 was some exclusive "please don't
            // interrupt me" bit)
            selected = potential_cmd;
            if (selected->status_bytes[VagCmdByte::BYTE11] == 0) {
              // exit immediately
              cmd_at_pri_idx = 4;
              check_priority = cmd->priority;
            }
          }
          cmd_at_pri_idx++;
        } while (cmd_at_pri_idx < 4);
        check_priority++;
      } while (check_priority < our_priority);
    }
  }

  if (!selected) {
    // failed.
    return nullptr;
  }

  ActiveVagStreams = ActiveVagStreams + 1;
  if (ActiveVagStreams < 2) {
    WakeSpuStreamsUp();
  }
  return selected;
}

void TerminateVAG(VagCmd* cmd, int /*param_2*/) {
  VagStrListNode vag_node;
  LfoListNode lfo_node;
  // undefined4 auStack32 [2];

  // if (param_2 == 1) {
  //  CpuSuspendIntr(auStack32);
  //}

  auto* sibling = cmd->stereo_sibling;
  strncpy(vag_node.name, cmd->name, 0x30);
  vag_node.id = cmd->id;
  cmd->sb_scanned = '\0';

  if (cmd->status_bytes[BYTE5] != '\0') {
    StopVAG(cmd, 0);
  }

  ReleaseMessage(&cmd->header, 0);
  RemoveVagCmd(cmd, 0);
  FreeVagCmd(cmd, 0);

  if (sibling != nullptr) {
    sibling->sb_scanned = '\0';
    RemoveVagCmd(sibling, 0);
    FreeVagCmd(sibling, 0);
  }

  if (cmd->sound_handler) {
    RemoveVagStreamFromList(&vag_node, &PluginStreamsList);
    lfo_node.id = cmd->id;
    lfo_node.plugin_id = cmd->plugin_id;
    RemoveLfoStreamFromList(&lfo_node, &LfoList);
  }
  // printf("termina removing %s (2)\n", vag_node.name);

  RemoveVagStreamFromList(&vag_node, &EEPlayList);
  // if (param_2 == 1) {
  //  CpuResumeIntr(auStack32[0]);
  //}
}

void PauseVAG(VagCmd* cmd, int /*param_2*/) {
  if (!cmd->sb_paused) {
    // if (param_2 == 1) {
    // CpuSuspendIntr(local_20);
    // }
    if (cmd->status_bytes[BYTE11] == '\0') {
      auto* stereo_cmd = cmd->stereo_sibling;
      if (!stereo_cmd) {
        if (cmd->sb_playing != '\0') {
          sceSdSetParam(SD_VP_VOLL | cmd->voice, 0);
          sceSdSetParam(SD_VP_VOLR | cmd->voice, 0);
        }
        sceSdSetParam(SD_VP_PITCH | cmd->voice, 0);
        sceSdSetSwitch(SD_S_KOFF | CORE_BIT(cmd->voice), VOICE_BIT(cmd->voice));
        if (cmd->status_bytes[BYTE5] == '\0') {
          cmd->spu_addr_to_start_playing = 0;
        } else {
          cmd->spu_addr_to_start_playing = GetSpuRamAddress(cmd);
        }
        cmd->sb_paused = 1;
      } else {
        if (cmd->sb_playing != '\0') {
          sceSdSetParam(SD_VP_VOLL | cmd->voice, 0);
          sceSdSetParam(SD_VP_VOLR | cmd->voice, 0);
          sceSdSetParam(SD_VP_VOLL | stereo_cmd->voice, 0);
          sceSdSetParam(SD_VP_VOLR | stereo_cmd->voice, 0);
        }
        sceSdSetParam(SD_VP_PITCH | stereo_cmd->voice, 0);
        sceSdSetParam(SD_VP_PITCH | cmd->voice, 0);
        sceSdSetSwitch(SD_S_KOFF | CORE_BIT(cmd->voice),
                       VOICE_BIT(cmd->voice) | VOICE_BIT(stereo_cmd->voice));

        if (cmd->status_bytes[BYTE5] == '\0') {
          cmd->spu_addr_to_start_playing = 0;
          stereo_cmd->spu_addr_to_start_playing = 0;
        } else {
          u32 ram_addr = GetSpuRamAddress(cmd);
          cmd->spu_addr_to_start_playing = ram_addr & 0xfffffff8;
          stereo_cmd->spu_addr_to_start_playing =
              ((ram_addr & 0xfffffff8) - cmd->spu_stream_dma_mem_addr) +
              stereo_cmd->spu_stream_dma_mem_addr;
        }
        cmd->sb_paused = 1;
        stereo_cmd->sb_paused = 1;
      }
    }
    // if (param_2 == 1) {
    // CpuResumeIntr(local_20[0]);
    //}
  }
}

void UnPauseVAG(VagCmd* param_1, int /*param_2*/) {
  if (param_1->sb_paused) {
    // if (param_2 == 1) {
    // CpuSuspendIntr(&local_30);
    //}
    if (param_1->status_bytes[BYTE11] == '\0') {
      auto* stereo_cmd = param_1->stereo_sibling;
      int pitch = CalculateVAGPitch(param_1->pitch1, param_1->unk_256_pitch2);
      u32 vol_l, vol_r;
      CalculateVAGVolumes(param_1, &vol_l, &vol_r);
      if (!stereo_cmd) {
        if (param_1->sb_playing != '\0') {
          sceSdSetParam(SD_VP_PITCH | param_1->voice, pitch);
          if (param_1->spu_addr_to_start_playing != 0) {
            sceSdSetAddr(SD_VA_SSA | param_1->voice, param_1->spu_addr_to_start_playing);
          }
          sceSdSetSwitch(SD_S_KON | CORE_BIT(param_1->voice), VOICE_BIT(param_1->voice));
          sceSdSetParam(SD_VP_VOLL | param_1->voice, vol_l);
          sceSdSetParam(SD_VP_VOLR | param_1->voice, vol_r);
        }
        param_1->sb_paused = 0;
      } else {
        if (param_1->sb_playing != '\0') {
          sceSdSetParam(SD_VP_PITCH | param_1->voice, pitch);
          sceSdSetParam(SD_VP_PITCH | stereo_cmd->voice, pitch);
          if (param_1->spu_addr_to_start_playing != 0) {
            sceSdSetAddr(SD_VA_SSA | param_1->voice, param_1->spu_addr_to_start_playing);
            sceSdSetAddr(SD_VA_SSA | stereo_cmd->voice, stereo_cmd->spu_addr_to_start_playing);
          }

          sceSdSetSwitch(SD_S_KON | CORE_BIT(param_1->voice),
                         VOICE_BIT(param_1->voice) | VOICE_BIT(stereo_cmd->voice));
          sceSdSetParam(SD_VP_VOLL | param_1->voice, vol_l);
          sceSdSetParam(SD_VP_VOLL | stereo_cmd->voice, 0);
          sceSdSetParam(SD_VP_VOLR | param_1->voice, 0);
          sceSdSetParam(SD_VP_VOLR | stereo_cmd->voice, vol_r);
        }
        param_1->sb_paused = 0;
        stereo_cmd->sb_paused = 0;
      }
    }
    // if (param_2 == 1) {
    // CpuResumeIntr(local_30);
    //}
  }
}

void RestartVag(VagCmd* param_1, int param_2, int /*param_3*/) {
  //  u16 uVar1;
  //  int iVar2;
  //  RealVagCmd *stereo_sibling;
  //  u32 uVar4;
  //  int iVar5;
  //  undefined4 local_30;
  //  undefined2 local_2c [2];
  //  undefined2 local_28 [4];

  // if (param_3 == 1) {
  // CpuSuspendIntr(&local_30);
  //}
  u32 vol_l, vol_r;
  CalculateVAGVolumes(param_1, &vol_l, &vol_r);
  if (param_1->status_bytes[BYTE11] == '\0') {
    auto* stereo_sibling = param_1->stereo_sibling;
    int sram_offset = param_2 ? 0x2000 : 0;

    int voices = VOICE_BIT(param_1->voice);
    if (stereo_sibling) {
      voices |= VOICE_BIT(stereo_sibling->voice);
    }

    sceSdSetSwitch(SD_S_KOFF | CORE_BIT(param_1->voice), voices);

    sceSdSetParam(SD_VP_VOLL | param_1->voice, 0);
    sceSdSetParam(SD_VP_VOLR | param_1->voice, 0);
    if (stereo_sibling) {
      sceSdSetParam(SD_VP_VOLL | stereo_sibling->voice, 0);
      sceSdSetParam(SD_VP_VOLR | stereo_sibling->voice, 0);
    }

    sceSdSetAddr(SD_VA_SSA | param_1->voice, param_1->spu_stream_dma_mem_addr + sram_offset);
    if (stereo_sibling) {
      sceSdSetAddr(SD_VA_SSA | stereo_sibling->voice,
                   stereo_sibling->spu_stream_dma_mem_addr + sram_offset);
    }

    sceSdSetSwitch(SD_S_KON | CORE_BIT(param_1->voice), voices);

    if (!stereo_sibling) {
      sceSdSetParam(SD_VP_VOLL | param_1->voice, vol_l);
      sceSdSetParam(SD_VP_VOLR | param_1->voice, vol_r);
    } else {
      sceSdSetParam(SD_VP_VOLL | param_1->voice, vol_l);
      sceSdSetParam(SD_VP_VOLL | stereo_sibling->voice, 0);
      sceSdSetParam(SD_VP_VOLR | param_1->voice, 0);
      sceSdSetParam(SD_VP_VOLR | stereo_sibling->voice, vol_r);
    }
  }
  // if (param_3 == 1) {
  // CpuResumeIntr(local_30);
  //}
}

void SetVAGVol(VagCmd* cmd, int /*param_2*/) {
  VagCmd* stereo_cmd;
  u32 lvol, rvol;

  if (cmd == nullptr) {
    return;
  }
  if (cmd->byte4 == '\0') {
    return;
  }
  if (cmd->sb_paused != '\0') {
    return;
  }
  if (cmd->byte11 != '\0') {
    return;
  }

  if (!cmd->sound_handler) {
    CalculateVAGVolumes(cmd, &lvol, &rvol);
  } else {
    ASSERT_NOT_REACHED();
    // TODO vag 989snd plugin
    // SoundHandler* hnd = cmd->sound_handler;
    // u32 vol =
    //    0x3fff *
    //    ((((cmd->vol_multiplier * MasterVolume[hnd->VolGroup]) >> 10) * hnd->Current_Vol) >> 10)
    //    >> 10;
    // lvol = (vol * gPanTable[(cmd->unk_176 + 90) % 360].left) >> 10;
    // lvol = (vol * gPanTable[(cmd->unk_176 + 90) % 360].right) >> 10;
    // if (lvol >= 0x4000) {
    //  lvol = 0x3fff;
    //}
    // if (rvol >= 0x4000) {
    //  rvol = 0x3fff;
    //}
  }

  // Originally used ProcBatch, buts this is easier to read.
  stereo_cmd = cmd->stereo_sibling;
  if (stereo_cmd) {
    sceSdSetParam(SD_VP_VOLL | cmd->voice, lvol);
    sceSdSetParam(SD_VP_VOLR | cmd->voice, 0);

    sceSdSetParam(SD_VP_VOLL | stereo_cmd->voice, 0);
    sceSdSetParam(SD_VP_VOLR | stereo_cmd->voice, rvol);

    sceSdSetParam(SD_VP_PITCH | cmd->voice, CalculateVAGPitch(cmd->pitch1, cmd->unk_256_pitch2));
    sceSdSetParam(SD_VP_PITCH | stereo_cmd->voice,
                  CalculateVAGPitch(cmd->pitch1, cmd->unk_256_pitch2));
  } else {
    sceSdSetParam(SD_VP_VOLL | cmd->voice, lvol);
    sceSdSetParam(SD_VP_VOLR | cmd->voice, rvol);
    sceSdSetParam(SD_VP_PITCH | cmd->voice, CalculateVAGPitch(cmd->pitch1, cmd->unk_256_pitch2));
  }
}

void SetVagStreamsNoStart(int param_1, int /*param_2*/) {
  // if (param_2 == 1) {
  // CpuSuspendIntr(local_18);
  //}
  // pRVar2 = VagCmds;
  for (auto& cmd : VagCmds) {
    cmd.status_bytes[VagCmdByte::BYTE23_NOSTART] = param_1;
  }
  // if (param_2 == 1) {
  // CpuResumeIntr(local_18[0]);
  //}
}

void InitVAGCmd(VagCmd* param_1, int param_2) {
  for (auto& x : param_1->status_bytes) {
    x = 0;
  }
  param_1->unk_236 = 0;
  param_1->unk_140 = 0;
  param_1->sb_paused = param_2 ? 1 : 0;
  param_1->unk_264 = 0x4000;
  (param_1->header).callback = NullCallback;
  param_1->dma_chan = -1;
  param_1->fo_min = 5;
  param_1->unk_196 = 0;
  param_1->unk_200 = 0;
  param_1->unk_204 = 0;
  param_1->unk_180 = 0;
  param_1->unk_184 = 0;
  param_1->unk_188 = 0;
  param_1->unk_192 = 0;
  param_1->status_bytes[VagCmdByte::BYTE5] = '\0';
  param_1->status_bytes[VagCmdByte::BYTE5] = '\0';
  param_1->num_processed_chunks = 0;
  param_1->safe_to_change_dma_fields = 1;
  param_1->xfer_size = 0;
  param_1->sample_rate = 0;
  param_1->unk_260 = 0;
  param_1->unk_268 = 0;
  (param_1->header).callback_buffer = nullptr;
  (param_1->header).ready_for_data = 1;
  (param_1->header).lse = nullptr;
  param_1->stereo_sibling = nullptr;
  param_1->dma_iop_mem_ptr = nullptr;
  param_1->unk_256_pitch2 = 0;
  param_1->unk_296 = 0;
  param_1->vec3.x = 0;
  param_1->vec3.y = 0;
  param_1->vec3.z = 0;
  param_1->fo_max = 0x1e;
  param_1->fo_curve = 1;
}

void SetVagStreamsNotScanned() {
  for (auto& cmd : VagCmds) {
    cmd.sb_scanned = 0;
  }
}

void RemoveVagCmd(VagCmd* cmd, int /*param_2*/) {
  // if (param_2 == 1) {
  // CpuSuspendIntr(local_18);
  //}
  VagCmdsPriList[cmd->priority].cmds[cmd->idx_in_cmd_arr] = nullptr;
  if (VagCmdsPriCounter[cmd->priority] < 1) {
    printf("IOP: ======================================================================\n");
    printf("IOP: vag RemoveVagCmd: VagCmdsPriCounter[%d] is zero\n", cmd->priority);
    printf("IOP: ======================================================================\n");
  } else {
    VagCmdsPriCounter[cmd->priority]--;
  }
  VagCmdsPriCounter[0]++;
  // if (param_2 == 1) {
  // CpuResumeIntr(local_18[0]);
  //}
}

VagCmd* FindFreeVagCmd() {
  for (auto& cmd : VagCmds) {
    if (cmd.id == 0) {
      return &cmd;
    }
  }
  return nullptr;
}

VagCmd* FindNotQueuedVagCmd() {
  for (auto& cmd : VagCmds) {
    if (!cmd.sb_scanned && !cmd.status_bytes[BYTE11] && !cmd.status_bytes[BYTE4]) {
      return &cmd;
    }
  }
  return nullptr;
}

VagCmd* FindWhosPlaying() {
  for (auto& cmd : VagCmds) {
    if (!cmd.sb_paused && cmd.sb_playing) {
      return &cmd;
    }
  }
  return nullptr;
}

VagCmd* FindVagStreamId(int id) {
  if (id) {
    for (auto& cmd : VagCmds) {
      if (cmd.id == id) {
        return &cmd;
      }
    }
  }
  return nullptr;
}

VagCmd* FindVagStreamPluginId(int plugin_id) {
  if (plugin_id) {
    for (auto& cmd : VagCmds) {
      if (cmd.plugin_id == plugin_id) {
        return &cmd;
      }
    }
  }
  return nullptr;
}

VagCmd* FindVagStreamName(const char* name) {
  for (auto& cmd : VagCmds) {
    if (strcmp(cmd.name, name) == 0) {
      return &cmd;
    }
  }
  return nullptr;
}

/*!
 * Check the global VagCmds array for an existing command with this name or ID.
 */
VagCmd* FindThisVagStream(const char* name, int id) {
  for (auto& cmd : VagCmds) {
    if (strcmp(cmd.name, name) == 0 && cmd.id == id) {
      return &cmd;
    }
  }
  return nullptr;
}

int AnyVagRunning() {
  int cnt = 0;
  for (auto& cmd : VagCmds) {
    if (cmd.status_bytes[BYTE4]) {
      cnt++;
    }
  }
  return cnt;
}

void FreeVagCmd(VagCmd* cmd, int /*param_2*/) {
  // if (param_2 == 1) {
  // CpuSuspendIntr(local_18);
  //}
  for (auto& x : cmd->status_bytes) {
    x = 0;
  }
  cmd->sb_playing = '\0';
  cmd->sb_paused = 1;
  cmd->sb_scanned = '\0';
  cmd->unk_180 = 0;
  cmd->unk_184 = 0;
  cmd->unk_188 = 0;
  cmd->unk_192 = 0;
  SetVagStreamName(cmd, 0, 0);

  cmd->name[0] = '\0';
  cmd->unk_264 = 0x4000;
  (cmd->header).callback = NullCallback;
  cmd->unk_140 = 0;
  cmd->pitch1 = 0;
  cmd->file_record = nullptr;
  cmd->vag_dir_entry = nullptr;
  cmd->unk_196 = 0;
  cmd->unk_200 = 0;
  cmd->unk_204 = 0;
  cmd->num_processed_chunks = 0;
  cmd->safe_to_change_dma_fields = 1;
  cmd->xfer_size = 0;
  cmd->sample_rate = 0;
  cmd->unk_260 = 0;
  cmd->unk_268 = 0;
  cmd->vol_multiplier = 0;
  cmd->unk_256_pitch2 = 0;
  cmd->id = 0;
  cmd->plugin_id = 0;
  cmd->sound_handler = 0;
  cmd->priority = 0;
  cmd->unk_288 = 0;
  cmd->unk_292 = 0;
  cmd->unk_296 = 0;
  (cmd->header).callback_buffer = nullptr;
  (cmd->header).ready_for_data = 0;
  (cmd->header).lse = nullptr;
  cmd->dma_iop_mem_ptr = (uint8_t*)0x0;
  cmd->dma_chan = -1;
  cmd->unk_236 = 0;
  if (0 < ActiveVagStreams) {
    ActiveVagStreams--;
  }
  // if (param_2 == 1) {
  // CpuResumeIntr(local_18[0]);
  //}
}

void SetNewVagCmdPri(VagCmd* cmd, int new_pri, int /*param_3*/) {
  // if (param_3 == 1) {
  // CpuSuspendIntr(local_20);
  //}
  if (cmd) {
    VagCmdsPriList[cmd->priority].cmds[cmd->idx_in_cmd_arr] = nullptr;
    if (VagCmdsPriCounter[cmd->priority] < 1) {
      printf("IOP: ======================================================================\n");
      printf("IOP: vag SetNewVagCmdPri: VagCmdsPriCounter[%d] is zero\n", cmd->priority);
      printf("IOP: ======================================================================\n");
    } else {
      VagCmdsPriCounter[cmd->priority]--;
    }
    VagCmdsPriList[new_pri].cmds[cmd->idx_in_cmd_arr] = cmd;
    VagCmdsPriCounter[new_pri]++;
    cmd->priority = new_pri;
  }
  // if (param_3 == 1) {
  // CpuResumeIntr(local_20[0]);
  //}
}

int HowManyBelowThisPriority(int pri, int /*disable_intr*/) {
  int cnt = 0;
  for (int p = 0; p < pri; p++) {
    cnt += VagCmdsPriCounter[p];
  }
  return cnt;
}

void StopVAG(VagCmd* cmd, int /*param_2*/) {
  //  int *piVar1;
  //  int iVar2;
  //  u32 uVar3;
  //  RealVagCmd *sibling;
  //  undefined4 local_20 [2];

  // if (param_2 == 1) {
  // CpuSuspendIntr(local_20);
  //}
  auto& sibling = cmd->stereo_sibling;
  PauseVAG(cmd, 0);
  if (cmd->status_bytes[BYTE5] != '\0') {
    int val = VOICE_BIT(cmd->voice);
    if (sibling) {
      val = val | VOICE_BIT(sibling->voice);
    }
    sceSdSetSwitch(SD_S_KOFF | CORE_BIT(cmd->voice), val);
  }
  for (auto& x : cmd->status_bytes) {
    x = 0;
  }
  (cmd->header).callback = NullCallback;
  cmd->vol_multiplier = 0;
  cmd->unk_256_pitch2 = 0;
  cmd->id = 0;
  cmd->plugin_id = 0;
  (cmd->header).ready_for_data = 0;
  cmd->sound_handler = 0;
  cmd->unk_140 = 0;
  cmd->pitch1 = 0;
  cmd->unk_180 = 0;
  cmd->unk_184 = 0;
  cmd->unk_188 = 0;
  cmd->unk_192 = 0;
  // if (param_2 == 1) {
  // CpuResumeIntr(local_20[0]);
  //}
}

void VAG_MarkLoopEnd(int8_t* data, int offset) {
  data[offset + -0xf] = '\x03';
}

void VAG_MarkLoopStart(int8_t* param_1) {
  param_1[1] = 6;
  param_1[0x11] = 2;
}

int CalculateVAGPitch(int param_1, int param_2) {
  if (param_2) {
    if (param_2 <= 0) {
      return 0x5f4 * param_1 / (0x5f4 - param_2);
    } else {
      return param_1 * (param_2 + 0x5f4) / 0x5f4;
    }
  }

  return param_1;
}

void PauseVagStreams() {
  for (auto& cmd : VagCmds) {
    if (cmd.status_bytes[BYTE4] && !cmd.sb_paused) {
      PauseVAG(&cmd, 1);
    }
  }
}

void UnPauseVagStreams() {
  for (auto& cmd : VagCmds) {
    if (cmd.status_bytes[BYTE4] && cmd.sb_paused) {
      UnPauseVAG(&cmd, 1);
    }
  }
}

void SetAllVagsVol(int param_1) {
  if (param_1 >= 0) {
    for (auto& VagCmd : VagCmds) {
      if (VagCmd.sound_handler /* && VagCmd.sound_handler->VolGroup == param_1 */) {
        ASSERT_NOT_REACHED();
        SetVAGVol(&VagCmd, 1);
      }
    }
  } else {
    for (auto& VagCmd : VagCmds) {
      SetVAGVol(&VagCmd, 1);
    }
  }
}

void CalculateVAGVolumes(VagCmd* cmd, u32* l_out, u32* r_out) {
  if (cmd->unk_296 == 0) {
    u32 vol = (u32)(cmd->vol_multiplier * MasterVolume[VolumeCategory::DIALOGUE]) >> 6;
    if (vol >= 0x4000) {
      vol = 0x3fff;
    }
    *l_out = vol;
    *r_out = vol;
  } else {
    int fo_vol =
        CalculateFalloffVolume(&cmd->vec3, (u32)(cmd->vol_multiplier * MasterVolume[2]) >> 10,
                               cmd->fo_curve, cmd->fo_min, cmd->fo_max);

    auto* pan = &gPanTable[(630 - CalculateAngle(&cmd->vec3)) % 360];
    *l_out = (pan->left * fo_vol) >> 10;
    *r_out = (pan->right * fo_vol) >> 10;

    if (*l_out >= 0x4000) {
      *l_out = 0x3fff;
    }
    if (*r_out >= 0x4000) {
      *r_out = 0x3fff;
    }
  }
}

}  // namespace jak2
