#include "vag.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/basefile.h"
#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/iso_queue.h"
#include "game/overlord/jak3/spustreams.h"
#include "game/overlord/jak3/srpc.h"
#include "game/overlord/jak3/ssound.h"
#include "game/overlord/jak3/streamlist.h"
#include "game/sce/iop.h"
#include "game/sound/sdshim.h"

#define VOICE_BIT(voice) (1 << ((voice) >> 1))

namespace jak3 {
using namespace iop;

bool g_bExtPause = false;
bool g_bExtResume = false;
ISO_VAGCommand g_aVagCmds[6];
int g_anMasterVolume[32];
bool voice_key_flags[0x30];
u32 voice_key_times[0x30];
u32 g_nTimeOfLastVoiceKey = 0;
bool g_bRecentlyKeyedVoice = false;
u32 g_nActiveVagStreams = 0;
u32 g_nPlaybackMode = 2;

struct VagCmdPriListEntry {
  ISO_VAGCommand* cmds[6];
};

VagCmdPriListEntry g_aapVagCmdsPriList[10];
u32 g_anVagCmdPriCounter[10];

void jak3_overlord_init_globals_vag() {
  g_bExtPause = false;
  g_bExtResume = false;
  for (auto& c : g_aVagCmds) {
    c = {};
  }
  for (auto& x : g_anMasterVolume) {
    x = 0x400;
  }
  g_nActiveVagStreams = 0;
  g_nPlaybackMode = 2;
}

void ISO_VAGCommand::set_all_flags_zero() {
  flags.bit0 = 0;
  flags.saw_chunks1 = 0;
  flags.paused = 0;
  flags.bit3 = 0;

  flags.running = 0;
  flags.clocks_set = 0;
  flags.file_disappeared = 0;
  flags.scanned = 0;

  flags.bit8 = 0;
  flags.stop = 0;
  flags.art = 0;
  flags.stereo_secondary = 0;
  flags.bit12 = 0;
  flags.bit13 = 0;
  flags.bit14 = 0;
  flags.bit15 = 0;
  flags.bit16 = 0;
  flags.bit17 = 0;
  flags.dma_complete_even_chunk_count = 0;
  flags.dma_complete_odd_chunk_count = 0;
  flags.bit20 = 0;
  flags.bit21 = 0;
  flags.bit22 = 0;
  flags.nostart = 0;
  flags.movie = 0;
  flags.bit25 = 0;
}

u32 ISO_VAGCommand::pack_flags() {
  u32 ret = 0;
  u8* ptr = &flags.bit0;
  u32 mask = 1;
  for (; ptr <= &flags.bit25; ptr++) {
    if (*ptr) {
      ret |= mask;
    }
    mask <<= 1;
  }
  return ret;
}

void InitVAGCmd(ISO_VAGCommand* cmd, int paused) {
  set_active_a(cmd, 0);
  set_active_b(cmd, 0);
  set_active_c(cmd, 0);
  cmd->set_all_flags_zero();
  cmd->unk_gvsp_state2 = 0;
  cmd->lfo_callback = nullptr;
  if (paused == 0) {
    cmd->flags.paused = 0;
  } else {
    cmd->flags.paused = 1;
  }
  cmd->safe_to_modify_dma = 1;
  cmd->unk_spu_mem_offset = 0x4000;
  cmd->callback = NullCallback;
  cmd->dma_chan = -1;
  cmd->fo_min = 5;
  cmd->fo_max = 0x1e;
  cmd->unk_gvsp_len = 0;
  cmd->position_for_ee = 0;
  cmd->unk_gvsp_cntr = 0;
  cmd->clocka = 0;
  cmd->clockb = 0;
  cmd->clockc = 0;
  cmd->clockd = 0;
  cmd->flags.clocks_set = 0;
  cmd->flags.file_disappeared = 0;
  cmd->num_isobuffered_chunks = 0;
  cmd->xfer_size = 0;
  cmd->vag_file_rate = 0;
  cmd->error = 0;
  cmd->unk_gvsp_flag = 0;
  cmd->m_pBaseFile = nullptr;
  cmd->stereo_sibling = nullptr;
  cmd->dma_iop_mem_ptr = nullptr;
  cmd->pitch_cmd = 0;
  cmd->updated_trans = 0;
  cmd->trans[0] = 0;
  cmd->trans[1] = 0;
  cmd->trans[2] = 0;
  cmd->fo_curve = GetFalloffCurve(1);
  cmd->play_group = 2;
  set_active_a(cmd, 1);
  set_active_b(cmd, 1);
}

void InitVagCmds() {
  auto now = GetSystemTimeLow();
  for (auto& x : voice_key_flags) {
    x = false;
  }
  for (auto& x : voice_key_times) {
    x = now;
  }
  g_nTimeOfLastVoiceKey = 0;
  g_bRecentlyKeyedVoice = false;

  for (int i = 0; i < 6; i++) {
    auto* cmd = &g_aVagCmds[i];
    InitVAGCmd(cmd, 1);
    cmd->info_idx = i;
    cmd->stream_sram = g_auStrmSRAM[i];
    cmd->trap_sram = g_auTrapSRAM[i];
    cmd->voice = g_anStreamVoice[i];
    cmd->music_flag = i >= 4;
    cmd->file_def = nullptr;
    cmd->vag_file_def = nullptr;
    cmd->vag_dir_entry = nullptr;
    cmd->flags.saw_chunks1 = 0;
    cmd->play_volume = 0;
    cmd->pitch_cmd = 0;
    cmd->id = 0;
    cmd->plugin_id = 0;
    cmd->maybe_sound_handler = 0;
    cmd->oog = 0;
    cmd->dolby_pan_angle = 0;
    cmd->priority_pq = 0;
    cmd->art_flag = 0;
    cmd->movie_flag = 0;
  }

  for (auto& level : g_aapVagCmdsPriList) {
    for (auto& cmd : level.cmds) {
      cmd = nullptr;
    }
  }
  for (auto& x : g_anVagCmdPriCounter) {
    x = 0;
  }
  // oops - wrong value from jak 2 maybe?
  g_anVagCmdPriCounter[0] = 4;
}

void RemoveVagCmd(ISO_VAGCommand* cmd) {
  ASSERT(cmd);
  ASSERT(!cmd->music_flag);
  ASSERT(cmd->info_idx < 4);

  g_aapVagCmdsPriList[cmd->priority_pq].cmds[cmd->info_idx] = nullptr;
  if (g_anVagCmdPriCounter[cmd->priority_pq]) {
    g_anVagCmdPriCounter[cmd->priority_pq]--;
  } else {
    ASSERT_NOT_REACHED();
  }
  g_anVagCmdPriCounter[0]++;
}

ISO_VAGCommand* FindFreeVagCmd() {
  for (int i = 0; i < 4; i++) {
    if (g_aVagCmds[i].id == 0) {
      return &g_aVagCmds[i];
    }
  }
  return nullptr;
}

void SetNewVagCmdPri(ISO_VAGCommand* cmd, int pri) {
  ASSERT(cmd);
  ASSERT(!cmd->music_flag);
  ASSERT(cmd->info_idx < 4);
  auto old_pri = cmd->priority_pq;
  g_aapVagCmdsPriList[old_pri].cmds[cmd->info_idx] = nullptr;
  if (0 < g_anVagCmdPriCounter[old_pri]) {
    g_anVagCmdPriCounter[old_pri]--;
  } else {
    ASSERT_NOT_REACHED();
  }
  g_anVagCmdPriCounter[pri]++;
  cmd->priority_pq = pri;
  g_aapVagCmdsPriList[pri].cmds[cmd->info_idx] = cmd;
}

int HowManyBelowThisPriority(int max_pri) {
  int count = 0;
  for (int i = 0; i < max_pri; i++) {
    count += g_anVagCmdPriCounter[i];
  }
  return count;
}

/*!
 * The "smart"ness of this function is less than advertised.
 */
ISO_VAGCommand* SmartAllocMusicVagCommand(const ISO_VAGCommand* user_command, int flag) {
  ASSERT(user_command);
  int idx = flag ? 5 : 4;
  auto* cmd = &g_aVagCmds[idx];
  if (cmd->id == 0) {
    g_nActiveMusicStreams++;
  }
  return cmd;
}

ISO_VAGCommand* SmartAllocVagCmd(ISO_VAGCommand* user_cmd) {
  ASSERT(user_cmd);
  ISO_VAGCommand* cmd = FindFreeVagCmd();
  if (!cmd && (cmd = FindNotQueuedVagCmd(), !cmd)) {
    u32 pri = 0;
    if ((user_cmd->priority_pq != 0) && (pri = 0, user_cmd->priority_pq != 0)) {
      do {
        u32 cmd_idx = 0;
        do {
          auto* pIVar1 = g_aapVagCmdsPriList[pri].cmds[cmd_idx];
          if (pIVar1 && (cmd = pIVar1, pIVar1->flags.stereo_secondary == 0)) {
            pri = user_cmd->priority_pq;
            cmd_idx = 4;
            cmd = pIVar1;
          }
          cmd_idx = cmd_idx + 1;
        } while (cmd_idx < 4);
        pri = pri + 1;
      } while (pri < (u32)user_cmd->priority_pq);
    }
    if (!cmd) {
      return nullptr;
    }
  }
  g_nActiveVagStreams = g_nActiveVagStreams + 1;
  return cmd;
}

void FreeVagCmd(ISO_VAGCommand* cmd) {
  set_active_a(cmd, 0);
  set_active_b(cmd, 0);
  set_active_c(cmd, 0);
  cmd->set_all_flags_zero();

  cmd->flags.paused = 1;
  cmd->flags.saw_chunks1 = 0;
  cmd->flags.scanned = 0;
  cmd->clocka = 0;
  cmd->clockb = 0;
  cmd->clockc = 0;
  cmd->clockd = 0;
  SetVagStreamName(cmd, 0);
  cmd->safe_to_modify_dma = 1;
  cmd->unk_spu_mem_offset = 0x4000;
  cmd->callback = NullCallback;
  cmd->dma_chan = -1;
  cmd->lfo_callback = 0;
  cmd->pitch1 = 0;
  cmd->pitch1_file = 0;
  cmd->file_def = nullptr;
  cmd->vag_file_def = nullptr;
  cmd->vag_dir_entry = nullptr;
  cmd->unk_gvsp_len = 0;
  cmd->position_for_ee = 0;
  cmd->unk_gvsp_cntr = 0;
  cmd->name[0] = 0;
  cmd->num_isobuffered_chunks = 0;
  cmd->xfer_size = 0;
  cmd->vag_file_rate = 0;
  cmd->error = 0;
  cmd->unk_gvsp_flag = 0;
  cmd->play_volume = 0;
  cmd->pitch_cmd = 0;
  cmd->id = 0;
  cmd->plugin_id = 0;
  cmd->maybe_sound_handler = 0;
  cmd->priority_pq = 0;
  cmd->art_flag = 0;
  cmd->movie_flag = 0;
  cmd->updated_trans = 0;
  cmd->m_pBaseFile = nullptr;
  cmd->dma_iop_mem_ptr = nullptr;
  cmd->unk_gvsp_state2 = 0;
  if (!cmd->music_flag) {
    if (0 < g_nActiveVagStreams) {
      g_nActiveVagStreams = g_nActiveVagStreams + -1;
    } else {
      ASSERT_NOT_REACHED();
    }
  } else {
    if (0 < g_nActiveMusicStreams) {
      g_nActiveMusicStreams = g_nActiveMusicStreams + -1;
    } else {
      ASSERT_NOT_REACHED();
    }
  }
  // CpuResumeIntr(local_10[0]);
}

void SetVagStreamsNotScanned() {
  for (int i = 0; i < 4; i++) {
    g_aVagCmds[i].flags.scanned = false;
  }
}

void SetVagStreamsNoStart(int value) {
  for (int i = 0; i < 4; i++) {
    g_aVagCmds[i].flags.nostart = value;
  }
}

ISO_VAGCommand* FindNotQueuedVagCmd() {
  for (int i = 0; i < 4; i++) {
    auto* cmd = &g_aVagCmds[i];
    if (!cmd->flags.scanned && !cmd->flags.stereo_secondary && !cmd->flags.running) {
      return cmd;
    }
  }
  return nullptr;
}

int AnyVagRunning() {
  int count = 0;
  for (int i = 0; i < 4; i++) {
    if (g_aVagCmds[i].flags.running) {
      count++;
    }
  }
  return count;
}

ISO_VAGCommand* FindVagStreamPluginId(int id) {
  if (id == 0) {
    return nullptr;
  }
  for (int i = 0; i < 4; i++) {
    auto* cmd = &g_aVagCmds[i];
    if (cmd->plugin_id == id) {
      return cmd;
    }
  }
  return nullptr;
}

ISO_VAGCommand* FindVagStreamId(int id) {
  if (id == 0) {
    return nullptr;
  }
  for (int i = 0; i < 4; i++) {
    auto* cmd = &g_aVagCmds[i];
    if (cmd->id == id) {
      return cmd;
    }
  }
  return nullptr;
}

ISO_VAGCommand* FindVagStreamName(const char* name) {
  for (int i = 0; i < 4; i++) {
    auto* cmd = &g_aVagCmds[i];
    if (strcmp(cmd->name, name) == 0) {
      return cmd;
    }
  }
  return nullptr;
}

ISO_VAGCommand* FindThisVagStream(const char* name, int id) {
  for (int i = 0; i < 4; i++) {
    auto* cmd = &g_aVagCmds[i];
    if (strcmp(cmd->name, name) == 0 && cmd->id == id) {
      return cmd;
    }
  }
  return nullptr;
}

ISO_VAGCommand* FindMusicStreamName(const char* name) {
  for (int i = 4; i < 6; i++) {
    auto* cmd = &g_aVagCmds[i];
    if (strcmp(cmd->name, name) == 0) {
      return cmd;
    }
  }
  return nullptr;
}

ISO_VAGCommand* FindThisMusicStream(const char* name, int id) {
  for (int i = 4; i < 6; i++) {
    auto* cmd = &g_aVagCmds[i];
    if (strcmp(cmd->name, name) == 0 && cmd->id == id) {
      return cmd;
    }
  }
  return nullptr;
}

/*!
 * Immediately stop the audio playback of a command, without the ability to resume.
 * This will key off voices.
 */
void StopVAG(ISO_VAGCommand* cmd) {
  // mark buffer inactive, so we don't stream more data
  set_active_a(cmd, 0);
  set_active_b(cmd, 0);

  // safely pause playback
  PauseVAG(cmd);

  // deal with SPU voices:
  if (cmd->flags.clocks_set) {
    // do individual voice blocks
    u32 voice_mask = VOICE_BIT(cmd->voice);
    BlockUntilVoiceSafe(cmd->voice, 0x900);
    auto* sibling = cmd->stereo_sibling;
    if (sibling) {
      voice_mask |= VOICE_BIT(sibling->voice);
      BlockUntilVoiceSafe(sibling->voice, 0x900);
    }
    BlockUntilAllVoicesSafe();

    sceSdSetSwitch((cmd->voice & 1) | SD_S_KOFF, voice_mask);

    auto now = GetSystemTimeLow();
    MarkVoiceKeyedOnOff(cmd->voice, now);
    if (sibling) {
      MarkVoiceKeyedOnOff(sibling->voice, now);
    }
  }

  // clear out command
  cmd->set_all_flags_zero();
  cmd->callback = NullCallback;
  cmd->clockd = 0;
  cmd->play_volume = 0;
  cmd->pitch_cmd = 0;
  cmd->id = 0;
  cmd->plugin_id = 0;
  cmd->maybe_sound_handler = 0;
  cmd->lfo_callback = 0;
  cmd->pitch1 = 0;
  cmd->pitch1_file = 0;
  cmd->clocka = 0;
  cmd->clockb = 0;
  cmd->clockc = 0;
}

/*!
 * Stop a VAG, also remove it from lists of active commands.
 */
void TerminateVAG(ISO_VAGCommand* in_cmd) {
  //  ISO_VAGCommand *sibling;
  //  VagStreamData vsd;
  //  undefined auStack64 [36];
  //  int local_1c;
  //  int local_18;
  //  bool not_music;
  ASSERT(in_cmd);
  bool not_music = !in_cmd->music_flag;
  VagStreamData vsd;
  strncpy(vsd.name, in_cmd->name, 0x30);
  vsd.id = in_cmd->id;
  StopVAG(in_cmd);
  ReleaseMessage(in_cmd);
  if (not_music) {
    RemoveVagCmd(in_cmd);
  }
  FreeVagCmd(in_cmd);
  auto* sibling = in_cmd->stereo_sibling;
  if (sibling) {
    sibling->flags.scanned = 0;
    if (not_music) {
      RemoveVagCmd(sibling);
    }
    FreeVagCmd(sibling);
  }
  if (not_music) {
    if (in_cmd->maybe_sound_handler != 0) {
      // TODO LFO support
      // RemoveVagStreamFromList(&vsd, &g_PluginStreamsList);
      // RemoveLfoStreamFromList(auStack64, &g_LfoStreamsList);
      ASSERT_NOT_REACHED();
    }
    RemoveVagStreamFromList(&vsd, &g_EEPlayList);
  }
}

/*!
 * Pause a vag stream by setting the volume to 0, the pitch to 0, then keying off the voice.
 */
void PauseVAG(ISO_VAGCommand* cmd) {
  if (cmd->flags.paused == 0) {
    // CpuSuspendIntr(local_18);
    if (cmd->flags.stereo_secondary == 0) {
      auto* sibling = cmd->stereo_sibling;
      if (!sibling) {
        if (cmd->flags.saw_chunks1) {
          // this means we enabled volume before, so disable it.
          static_assert(SD_VP_VOLL == 0);
          static_assert(SD_VP_VOLR == 0x100);
          static_assert(SD_VP_PITCH == 0x200);
          sceSdSetParam(cmd->voice | SD_VP_VOLL, 0);
          sceSdSetParam(cmd->voice | SD_VP_VOLR, 0);
        }

        // in either case, set pitch to 0 to stop playback.
        sceSdSetParam(cmd->voice | SD_VP_PITCH, 0);

        auto voice = cmd->voice;
        BlockUntilVoiceSafe(voice, 0x900);
        BlockUntilAllVoicesSafe();
        // key off
        sceSdSetSwitch((cmd->voice & 1) | SD_S_KOFF, VOICE_BIT(voice));
        MarkVoiceKeyedOnOff(cmd->voice, GetSystemTimeLow());

        // remember where to resume from
        if (cmd->flags.clocks_set == 0) {
          cmd->current_spu_address = 0;
        } else {
          cmd->current_spu_address = GetSpuRamAddress(cmd);
        }
        cmd->flags.paused = 1;
      } else {
        if (cmd->flags.saw_chunks1 != 0) {
          sceSdSetParam(cmd->voice | SD_VP_VOLL, 0);
          sceSdSetParam(cmd->voice | SD_VP_VOLR, 0);
          sceSdSetParam(sibling->voice | SD_VP_VOLL, 0);
          sceSdSetParam(sibling->voice | SD_VP_VOLR, 0);
        }
        sceSdSetParam(sibling->voice | SD_VP_PITCH, 0);
        sceSdSetParam(cmd->voice | SD_VP_PITCH, 0);
        auto sibling_voice = sibling->voice;
        auto voice = cmd->voice;
        BlockUntilVoiceSafe(voice, 0x900);
        BlockUntilVoiceSafe(sibling->voice, 0x900);
        BlockUntilAllVoicesSafe();
        sceSdSetSwitch((cmd->voice & 1) | SD_S_KOFF,
                       (VOICE_BIT(voice)) | (VOICE_BIT(sibling_voice)));
        auto now = GetSystemTimeLow();
        MarkVoiceKeyedOnOff(cmd->voice, now);
        MarkVoiceKeyedOnOff(sibling->voice, now);
        if (cmd->flags.clocks_set == 0) {
          cmd->current_spu_address = 0;
          sibling->current_spu_address = 0;
        } else {
          u32 spu_ram_addr = GetSpuRamAddress(cmd);
          cmd->current_spu_address = spu_ram_addr & 0xfffffff8;
          sibling->current_spu_address =
              ((spu_ram_addr & 0xfffffff8) - cmd->stream_sram) + sibling->stream_sram;
        }
        cmd->flags.paused = 1;
        sibling->flags.paused = 1;
      }
    }
    // CpuResumeIntr(local_18[0]);
  }
}

namespace {
u32 read_rate_calc(u32 pitch) {
  u64 pitch1 = (pitch >> 3);
  u64 mult_result = pitch1 * 0x2492'4925ull;
  return mult_result >> 32;
}
}  // namespace
/*!
 * Start up a VAG stream after it was paused. Also, used to start a vag stream for the first time.
 */
void UnPauseVAG(ISO_VAGCommand* cmd) {
  if (cmd->flags.paused) {
    // CpuSuspendIntr(&local_28);
    if (cmd->flags.stereo_secondary == 0) {
      auto* sibling = cmd->stereo_sibling;
      auto pitch = CalculateVAGPitch(cmd->pitch1, cmd->pitch_cmd);

      // calculate read rate
      if (cmd->m_pBaseFile) {
        auto p2 = CalculateVAGPitch(cmd->pitch1_file, cmd->pitch_cmd);
        u32 rate;
        if (sibling == (ISO_VAGCommand*)0x0) {
          rate = p2 * 0x177;
        } else {
          rate = p2 * 0x2ee;
        }
        cmd->m_pBaseFile->m_ReadRate = read_rate_calc(rate);
      }

      int lvol, rvol;
      CalculateVAGVolumes(cmd, &lvol, &rvol);
      if (!sibling) {
        // only update volume here if we've actually started the stream.
        // otherwise, the stream is still getting set up, and the unpause will occur in
        // the SPU interrupt handler.
        if (cmd->flags.saw_chunks1) {
          BlockUntilVoiceSafe(cmd->voice, 0x900);
          if (cmd->current_spu_address != 0) {
            sceSdSetAddr(cmd->voice | SD_VA_SSA, cmd->current_spu_address);
          }
          sceSdSetParam(cmd->voice | SD_VP_PITCH, pitch & 0xffff);
          BlockUntilAllVoicesSafe();
          // lg::error("keying on 1!");
          sceSdSetSwitch((cmd->voice & 1) | SD_S_KON, VOICE_BIT(cmd->voice));
          MarkVoiceKeyedOnOff(cmd->voice, GetSystemTimeLow());
          sceSdSetParam(cmd->voice | SD_VP_VOLL, lvol);
          sceSdSetParam(cmd->voice | SD_VP_VOLR, rvol);
        }
        cmd->flags.paused = 0;
      } else {
        if (cmd->flags.saw_chunks1) {
          BlockUntilVoiceSafe(cmd->voice, 0x900);
          BlockUntilVoiceSafe(sibling->voice, 0x900);
          sceSdSetParam(cmd->voice | SD_VP_PITCH, pitch & 0xffff);
          sceSdSetParam(sibling->voice | SD_VP_PITCH, pitch & 0xffff);
          if (cmd->current_spu_address != 0) {
            sceSdSetAddr(cmd->voice | SD_VA_SSA, cmd->current_spu_address);
            sceSdSetAddr(sibling->voice | SD_VA_SSA, sibling->current_spu_address);
          }
          BlockUntilAllVoicesSafe();
          sceSdSetSwitch((cmd->voice & 1) | SD_S_KON,
                         (VOICE_BIT(cmd->voice)) | (VOICE_BIT(sibling->voice)));
          auto now = GetSystemTimeLow();
          MarkVoiceKeyedOnOff(cmd->voice, now);
          MarkVoiceKeyedOnOff(sibling->voice, now);
          sceSdSetParam(cmd->voice | SD_VP_VOLL, lvol);
          sceSdSetParam(sibling->voice | SD_VP_VOLL, 0);
          sceSdSetParam(cmd->voice | SD_VP_VOLR, 0);
          sceSdSetParam(sibling->voice | SD_VP_VOLR, rvol);
        }
        cmd->flags.paused = 0;
        sibling->flags.paused = 0;
      }
    }
    // CpuResumeIntr(local_28);
  }
}

// TODO: needs some cleanup
void RestartVag(ISO_VAGCommand* cmd, int p) {
  (void)p;
  // CpuSuspendIntr(&local_28);
  int lvol, rvol;
  CalculateVAGVolumes(cmd, &lvol, &rvol);
  if (cmd->flags.stereo_secondary == 0) {
    auto* sibling = cmd->stereo_sibling;
    u32 voice_mask = VOICE_BIT(cmd->voice);
    BlockUntilVoiceSafe(cmd->voice, 0x900);
    if (sibling) {
      voice_mask |= VOICE_BIT(sibling->voice);
      BlockUntilVoiceSafe(sibling->voice, 0x900);
    }
    BlockUntilAllVoicesSafe();
    sceSdSetSwitch((cmd->voice & 1) | SD_S_KOFF, voice_mask);
    sceSdSetParam(cmd->voice | SD_VP_VOLL, 0);
    sceSdSetParam(cmd->voice | SD_VP_VOLR, 0);
    // wtf is this crap
    // CpuResumeIntr(local_28);
    DelayThread(100);
    // CpuSuspendIntr(&local_28);
    u32 bVar1;
    u32 uVar4;
    if (sibling == (ISO_VAGCommand*)0x0) {
      bVar1 = cmd->voice;
      uVar4 = cmd->stream_sram;
    } else {
      sceSdSetParam(sibling->voice, 0);
      sceSdSetParam(sibling->voice | 0x100, 0);
      sceSdSetAddr(cmd->voice | 0x2040, cmd->stream_sram);
      bVar1 = sibling->voice;
      uVar4 = sibling->stream_sram;
    }
    sceSdSetAddr(bVar1 | 0x2040, uVar4);
    BlockUntilAllVoicesSafe();
    sceSdSetSwitch((cmd->voice & 1) | 0x1500, voice_mask);
    auto uVar3 = GetSystemTimeLow();
    MarkVoiceKeyedOnOff(cmd->voice, uVar3);
    u32 uVar2;
    if (!sibling) {
      sceSdSetParam(cmd->voice, lvol);
      uVar2 = cmd->voice;
    } else {
      MarkVoiceKeyedOnOff(sibling->voice, uVar3);
      sceSdSetParam(cmd->voice, lvol);
      sceSdSetParam(sibling->voice, 0);
      sceSdSetParam(cmd->voice | 0x100, 0);
      uVar2 = sibling->voice;
    }
    sceSdSetParam(uVar2 | 0x100, rvol);
  }
  // CpuResumeIntr(local_28);
}

void PauseVagStreams(bool music) {
  if (music != 0) {
    WaitSema(g_nMusicSemaphore);
    g_bAnotherMusicPauseFlag = true;
    g_bMusicIsPaused = true;
  }

  for (int i = 0; i < (music ? 6 : 4); i++) {
    auto* cmd = &g_aVagCmds[i];
    if (cmd->flags.running && !cmd->flags.paused) {
      PauseVAG(cmd);
    }
  }

  if (music != 0) {
    SignalSema(g_nMusicSemaphore);
  }
}

void UnPauseVagStreams(bool music) {
  int iVar1;
  ISO_VAGCommand* cmd;

  if (music != 0) {
    WaitSema(g_nMusicSemaphore);
  }
  cmd = g_aVagCmds;
  iVar1 = 4;
  if ((music != 0) && (iVar1 = 6, g_bMusicPause != 0)) {
    iVar1 = 4;
  }
  while (iVar1 != 0) {
    iVar1 = iVar1 + -1;
    if ((cmd->flags.running != 0) && (cmd->flags.paused != 0)) {
      UnPauseVAG(cmd);
    }
    cmd = cmd + 1;
  }
  if (music != 0) {
    g_bAnotherMusicPauseFlag = false;
    if (g_bMusicPause == 0) {
      g_bMusicIsPaused = false;
    }
    SignalSema(g_nMusicSemaphore);
  }
}

int CalculateDolbyPanAngle(ISO_VAGCommand* cmd, u32 angle) {
  int iVar1;
  int iVar2;

  iVar1 = 0;
  if (cmd != (ISO_VAGCommand*)0x0) {
    if (0x167 < angle) {
      // TODO: sus 64-bit multiply
      angle = angle + (u32)((u64)(angle >> 3) * 0x16c16c17 >> 0x22) * -0x168;
    }
    iVar1 = cmd->dolby_pan_angle;
    iVar2 = iVar1;
    if (0x167 < iVar1) {
      iVar2 = iVar1 + -0x168;
    }
    iVar2 = angle - iVar2;
    if (iVar2 < -0xb4) {
      iVar2 = iVar2 + 0x168;
    } else {
      if (0xb4 < iVar2) {
        iVar2 = iVar2 + -0x168;
      }
    }
    iVar1 = iVar1 + iVar2;
    if (iVar1 < 0) {
      iVar1 = iVar1 + 0x2d0;
    }
    if (0x2cf < iVar1) {
      iVar1 = iVar1 + -0x2d0;
    }
    cmd->dolby_pan_angle = iVar1;
  }
  return iVar1;
}

void CalculateVAGVolumes(ISO_VAGCommand* cmd, int* lvol, int* rvol) {
  u32 angle;
  int iVar3;
  int iVar4;
  int iVar5;
  u32 uVar7;
  u32 uVar8;
  int iVar9;

  ASSERT(cmd && lvol && rvol);

  iVar9 = g_nPlaybackMode;
  iVar3 = 0x3fff;
  iVar4 = 0x3fff;
  uVar8 = 0;
  if (cmd->music_flag == 0) {
    iVar5 = cmd->maybe_sound_handler;
    if (iVar5 == 0) {
      if (cmd->updated_trans == 0) {
        uVar7 = (u32)(cmd->play_volume * g_anMasterVolume[cmd->play_group]) >> 6;
        goto LAB_0000bb58;
      }
      uVar7 = CalculateFalloffVolume(cmd->trans,
                                     (cmd->play_volume * g_anMasterVolume[cmd->play_group]) >> 10,
                                     cmd->fo_curve, cmd->fo_min, cmd->fo_max, 0, 0);
      iVar3 = CalculateAngle(cmd->trans, cmd->fo_curve, 0);
      angle = iVar3 + 0x5aU + (u32)((u64)((iVar3 + 0x5aU) >> 3) * 0x16c16c17 >> 0x22) * -0x168;
      if (0x400 < uVar7) {
        uVar7 = 0x400;
      }
      uVar7 = uVar7 << 4 | uVar7 >> 6;
    } else {
      ASSERT_NOT_REACHED();
    }

    if (iVar9 == 1) {
      iVar3 = 0x2d41;
      iVar4 = 0x2d41;
    } else {
      if (iVar9 < 2) {
        iVar3 = 0x2d41;
        if (iVar9 == 0) {
          // ASSERT_NOT_REACHED();  // dolby crap
          // TODO:
          iVar4 = 0x2d41;
          //  uVar8 = CalculateDolbyPanAngle(cmd, angle);
          //  iVar9 = (uint)((u64)(uVar8 >> 1) * 0xb60b60b7 >> 0x25) * 4;
          //  if (0x167 < uVar8) {
          //    uVar8 = uVar8 - 0x168;
          //  }
          //  if (uVar8 < 0xb4) {
          //    psVar6 = (short*)(g_aPanTable + uVar8 * 4);
          //    sVar1 = psVar6[1];
          //    sVar2 = *psVar6;
          //  } else {
          //    iVar3 = g_aPanTable + uVar8 * 4;
          //    sVar1 = *(short*)(iVar3 + -0x2d0);
          //    sVar2 = *(short*)(iVar3 + -0x2ce);
          //  }
          //  iVar4 = (int)sVar1;
          //  iVar3 = (int)sVar2;
          //  if ((int)((uint) * (ushort*)(iVar9 + 0x15800) << 0x10) < 0) {
          //    iVar3 = -iVar3;
          //  }
          //  if (*(short*)(iVar9 + 0x15802) < 0) {
          //    iVar4 = -iVar4;
          //  }
          //  uVar8 = 0xffffc001;
        } else {
          iVar4 = 0x2d41;
        }
      } else {
        iVar3 = 0x2d41;
        if (iVar9 == 2) {
          cmd->dolby_pan_angle = angle;
          if (angle < 0xb4) {
            auto pte = g_aPanTable[angle];
            // psVar6 = (short*)(g_aPanTable + angle * 4);
            iVar4 = pte.right;
            iVar3 = pte.left;
          } else {
            // iVar3 = g_aPanTable + angle * 4;
            u32 angle2 = (angle - 180);
            ASSERT(angle2 < 360);
            auto pte = g_aPanTable[angle2];
            iVar4 = pte.left;
            iVar3 = pte.right;
          }
        } else {
          iVar4 = 0x2d41;
        }
      }
    }
  } else {
    uVar7 = ((u32)(g_nMusicFade * g_anMasterVolume[1]) >> 0xc) * g_nMusicTweak >> 7;
  }

LAB_0000bb58:
  iVar9 = iVar4;
  if (g_CameraInvert != 0) {
    iVar9 = iVar3;
    iVar3 = iVar4;
  }
  iVar3 = iVar3 * uVar7;
  iVar9 = iVar9 * uVar7;
  if (iVar3 < 0) {
    iVar3 = iVar3 + 0x3fff;
  }
  uVar7 = iVar3 >> 0xe;
  if (iVar9 < 0) {
    iVar9 = iVar9 + 0x3fff;
  }
  angle = iVar9 >> 0xe;
  if (0x3fff < (int)uVar7) {
    uVar7 = 0x3fff;
  }
  if ((int)uVar7 < (int)uVar8) {
    uVar7 = uVar8;
  }
  *lvol = uVar7 & 0x7fff;
  if (0x3fff < (int)angle) {
    angle = 0x3fff;
  }
  if ((int)angle < (int)uVar8) {
    angle = uVar8;
  }
  *rvol = angle & 0x7fff;
}

s32 CalculateVAGPitch(int param_1, int param_2) {
  if (param_2) {
    if (param_2 <= 0) {
      return 0x5f4 * param_1 / (0x5f4 - param_2);
    } else {
      return param_1 * (param_2 + 0x5f4) / 0x5f4;
    }
  }

  return param_1;
  //  if (b != 0) {
  //    if (0 < b) {
  //      return (a * (b + 0x5f4)) / 0x5f4;
  //    }
  //    a = (a * 0x5f4) / (0x5f4U - b);
  //  }
  //  return a;
}

void SetVAGVol(ISO_VAGCommand* cmd) {
  if (cmd && cmd->flags.running && !cmd->flags.paused && !cmd->flags.stereo_secondary) {
    auto* sibling = cmd->stereo_sibling;
    int lvol, rvol;
    CalculateVAGVolumes(cmd, &lvol, &rvol);
    auto* file = cmd->m_pBaseFile;
    if (file) {
      u32 rate = CalculateVAGPitch(cmd->pitch1_file, cmd->pitch_cmd);
      if (!sibling) {
        rate = rate * 0x177;
      } else {
        rate = rate * 0x2ee;
      }
      file->m_ReadRate = read_rate_calc(rate);
    }

    // like jak2, rewritten to not use sceSdProcBatch
    if (!sibling) {
      sceSdSetParam(SD_VP_VOLL | cmd->voice, lvol);
      sceSdSetParam(SD_VP_VOLR | cmd->voice, rvol);
      sceSdSetParam(SD_VP_PITCH | cmd->voice, CalculateVAGPitch(cmd->pitch1, cmd->pitch_cmd));
    } else {
      // left channel, left vol
      if (g_CameraInvert) {
        sceSdSetParam(SD_VP_VOLL | cmd->voice, 0);
      } else {
        sceSdSetParam(SD_VP_VOLL | cmd->voice, lvol);
      }

      // right channel, left vol
      if (g_CameraInvert) {
        sceSdSetParam(SD_VP_VOLL | sibling->voice, rvol);
      } else {
        sceSdSetParam(SD_VP_VOLL | sibling->voice, 0);
      }

      if (g_CameraInvert) {
        sceSdSetParam(SD_VP_VOLR | cmd->voice, lvol);
      } else {
        sceSdSetParam(SD_VP_VOLR | cmd->voice, 0);
      }

      if (g_CameraInvert) {
        sceSdSetParam(SD_VP_VOLR | sibling->voice, 0);
      } else {
        sceSdSetParam(SD_VP_VOLR | sibling->voice, rvol);
      }

      sceSdSetParam(SD_VP_PITCH | cmd->voice, CalculateVAGPitch(cmd->pitch1, cmd->pitch_cmd));
    }
    // CpuSuspendIntr(local_20);
    // sceSdProcBatch(batch, 0, count);
    // CpuResumeIntr(local_20[0]);
  }
}

void SetAllVagsVol(int x) {
  if (x < 0) {
    for (int i = 0; i < 4; i++) {
      SetVAGVol(&g_aVagCmds[i]);
    }
  } else {
    for (int i = 0; i < 4; i++) {
      if (g_aVagCmds[i].maybe_sound_handler) {
        ASSERT_NOT_REACHED();
        // there's more check before this...
        SetVAGVol(&g_aVagCmds[i]);
      }
    }
  }
}

void VAG_MarkLoopStart(uint8_t* data) {
  data[0x11] = 2;
  data[1] = 6;
}

void VAG_MarkLoopEnd(uint8_t* data, int offset) {
  data[offset + -0xf] = 3;
}
}  // namespace jak3