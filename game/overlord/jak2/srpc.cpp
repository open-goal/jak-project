#include "srpc.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/common/loader_rpc_types.h"
#include "game/common/player_rpc_types.h"
#include "game/overlord/common/soundcommon.h"
#include "game/overlord/common/srpc.h"
#include "game/overlord/common/ssound.h"
#include "game/overlord/jak2/iso_api.h"
#include "game/overlord/jak2/spustreams.h"
#include "game/overlord/jak2/ssound.h"
#include "game/overlord/jak2/vag.h"
#include "game/runtime.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

using namespace iop;
namespace jak2 {

// korean inserted
static const char* languages[] = {"ENG", "FRE", "GER", "SPA", "ITA", "JAP", "KOR", "UKE"};
static u32 gInfoEE = 0;
static u32 IopTicks = 0;
static SoundIopInfo info;
constexpr int kPlayerBufSize = 0x50 * 128;
static uint8_t gPlayerBuf[kPlayerBufSize];
constexpr int kLoaderBufSize = 0x50;
static uint8_t gLoaderBuf[kLoaderBufSize];

void srpc_init_globals() {}

void* RPC_Player(unsigned int /*fno*/, void* data, int size) {
  if (!gSoundEnable) {
    return nullptr;
  }

  // gFreeMem = QueryTotalFreeMemSize();
  if (!PollSema(gSema)) {
    if (gMusic) {
      if (!gMusicPause && !LookupSound(666)) {
        Sound* music = AllocateSound(true);
        if (music != nullptr) {
          gMusicFade = 0;
          gMusicFadeDir = 1;
          SetMusicVol();
          music->sound_handle = snd_PlaySoundVolPanPMPB(gMusic, 0, 0x400, -1, 0, 0);
          music->id = 666;
          music->is_music = 1;
        }
      }
    }
    SignalSema(gSema);
  }

  SetMusicVol();
  Sound* music = LookupSound(666);
  if (music != nullptr) {
    snd_SetSoundVolPan(music->sound_handle, 0x7FFFFFFF, 0);
  }

  constexpr int kMsgSize = 0x50;
  static_assert(sizeof(SoundRpcCommand) == kMsgSize);
  int n_messages = size / kMsgSize;
  SoundRpcCommand* cmd = (SoundRpcCommand*)(data);
  if (!gSoundEnable) {
    return nullptr;
  }

  while (n_messages > 0) {
    switch (cmd->j2command) {
      case Jak2SoundCommand::play: {
        if (!cmd->play.sound_id) {
          break;
        }

        auto sound = LookupSound(cmd->play.sound_id);
        if (sound != nullptr) {
          // update
          sound->params = cmd->play.parms;
          sound->is_music = false;
          SFXUserData data{};
          s32 found = snd_GetSoundUserData(0, nullptr, -1, sound->name, &data);
          if ((sound->params.mask & 0x40) == 0) {
            s16 fo_min = 5;
            if (found && data.data[0])
              fo_min = data.data[0];
            sound->params.fo_min = fo_min;
          }
          if ((sound->params.mask & 0x80) == 0) {
            s16 fo_max = 30;
            if (found && data.data[1])
              fo_max = data.data[1];
            sound->params.fo_max = fo_max;
          }
          if ((sound->params.mask & 0x100) == 0) {
            s16 fo_curve = 2;
            if (found && data.data[2])
              fo_curve = data.data[2];
            sound->params.fo_curve = fo_curve;
          }
          UpdateVolume(sound);
          snd_SetSoundPitchModifier(sound->sound_handle, sound->params.pitch_mod);
          if (sound->params.mask & 0x4) {
            snd_SetSoundPitchBend(sound->sound_handle, sound->params.bend);
          }
          if (sound->params.mask & 0x800) {
            snd_SetSoundReg(sound->sound_handle, 0, sound->params.reg[0]);
          }
          if (sound->params.mask & 0x1000) {
            snd_SetSoundReg(sound->sound_handle, 1, sound->params.reg[1]);
          }
          if (sound->params.mask & 0x2000) {
            snd_SetSoundReg(sound->sound_handle, 2, sound->params.reg[2]);
          }

        } else {
          // new sound
          sound = AllocateSound(true);
          if (sound == nullptr) {
            // no free sounds
            break;
          }
          strcpy_toupper(sound->name, cmd->play.name);
          // TODO update params struct
          sound->params = cmd->play.parms;
          sound->is_music = false;
          sound->bank_entry = nullptr;

          SFXUserData data{};
          s32 found = snd_GetSoundUserData(0, nullptr, -1, sound->name, &data);
          if ((sound->params.mask & 0x40) == 0) {
            s16 fo_min = 5;
            if (found && data.data[0])
              fo_min = data.data[0];
            sound->params.fo_min = fo_min;
          }
          if ((sound->params.mask & 0x80) == 0) {
            s16 fo_max = 30;
            if (found && data.data[1])
              fo_max = data.data[1];
            sound->params.fo_max = fo_max;
          }
          if ((sound->params.mask & 0x100) == 0) {
            s16 fo_curve = 2;
            if (found && data.data[2])
              fo_curve = data.data[2];
            sound->params.fo_curve = fo_curve;
          }
          // lg::warn("RPC: PLAY {} v:{}, p:{}", sound->name, GetVolume(sound), GetPan(sound));

          s32 handle = snd_PlaySoundByNameVolPanPMPB(0, nullptr, sound->name, GetVolume(sound),
                                                     GetPan(sound), sound->params.pitch_mod,
                                                     sound->params.bend);
          sound->sound_handle = handle;
          if (handle != 0) {
            sound->id = cmd->play.sound_id;
            if (sound->params.mask & 0x800) {
              snd_SetSoundReg(sound->sound_handle, 0, sound->params.reg[0]);
            }
            if (sound->params.mask & 0x1000) {
              snd_SetSoundReg(sound->sound_handle, 1, sound->params.reg[1]);
            }
            if (sound->params.mask & 0x2000) {
              snd_SetSoundReg(sound->sound_handle, 2, sound->params.reg[2]);
            }
          }
        }
      } break;
      case Jak2SoundCommand::pause_sound: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        if (sound != nullptr) {
          snd_PauseSound(sound->sound_handle);
        } else {
          auto* vs = FindVagStreamId(cmd->sound_id.sound_id);
          if (vs) {
            PauseVAG(vs, 1);
          }
        }
      } break;
      case Jak2SoundCommand::stop_sound: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        if (sound != nullptr) {
          snd_StopSound(sound->sound_handle);
        } else {
          auto* vs = FindVagStreamId(cmd->sound_id.sound_id);
          if (vs) {
            StopVagStream(vs, 1);
          }
        }
      } break;
      case Jak2SoundCommand::continue_sound: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        if (sound != nullptr) {
          snd_ContinueSound(sound->sound_handle);
        } else {
          auto* vs = FindVagStreamId(cmd->sound_id.sound_id);
          if (vs) {
            UnPauseVAG(vs, 1);
          }
        }
      } break;
      case Jak2SoundCommand::set_param: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        u32 mask = cmd->param.parms.mask;
        if (sound != nullptr) {
          if (mask & 1) {
            if (mask & 0x10) {
              sound->auto_time = cmd->param.auto_time;
              sound->new_volume = cmd->param.parms.volume;
            } else {
              sound->params.volume = cmd->param.parms.volume;
            }
          }
          if (mask & 0x20) {
            sound->params.trans = cmd->param.parms.trans;
          }
          if (mask & 0x21) {
            UpdateVolume(sound);
          }
          if (mask & 2) {
            sound->params.pitch_mod = cmd->param.parms.pitch_mod;
            if (mask & 0x10) {
              snd_AutoPitch(sound->sound_handle, sound->params.pitch_mod, cmd->param.auto_time,
                            cmd->param.auto_from);
            } else {
              snd_SetSoundPitchModifier(sound->sound_handle, cmd->param.parms.pitch_mod);
            }
          }
          if (mask & 4) {
            sound->params.bend = cmd->param.parms.bend;
            if (mask & 0x10) {
              snd_AutoPitchBend(sound->sound_handle, sound->params.bend, cmd->param.auto_time,
                                cmd->param.auto_from);
            } else {
              snd_SetSoundPitchBend(sound->sound_handle, cmd->param.parms.bend);
            }
          }
          if (mask & 0x400) {
            sound->params.priority = cmd->param.parms.priority;
          }
          if (mask & 0x8) {
            sound->params.group = cmd->param.parms.group;
          }
          if (mask & 0x40) {
            sound->params.fo_min = cmd->param.parms.fo_min;
          }
          if (mask & 0x80) {
            sound->params.fo_max = cmd->param.parms.fo_max;
          }
          if (mask & 0x100) {
            sound->params.fo_curve = cmd->param.parms.fo_curve;
          }
          if (mask & 0x800) {
            sound->params.reg[0] = cmd->param.parms.reg[0];
            snd_SetSoundReg(sound->sound_handle, 0, cmd->param.parms.reg[0]);
          }
          if (mask & 0x1000) {
            sound->params.reg[1] = cmd->param.parms.reg[1];
            snd_SetSoundReg(sound->sound_handle, 1, cmd->param.parms.reg[1]);
          }
          if (mask & 0x2000) {
            sound->params.reg[2] = cmd->param.parms.reg[2];
            snd_SetSoundReg(sound->sound_handle, 2, cmd->param.parms.reg[2]);
          }
        } else {
          auto* vs = FindVagStreamId(cmd->param.sound_id);
          if (vs) {
            if (mask & 0x2) {
              SetVAGStreamPitch(cmd->param.sound_id, cmd->param.parms.pitch_mod);
            }
            if (mask & 0x20) {
              vs->vec3 = cmd->param.parms.trans;
              vs->unk_296 = 1;
            }
            if (mask & 0x40) {
              vs->fo_min = cmd->param.parms.fo_min;
            }
            if (mask & 0x80) {
              vs->fo_max = cmd->param.parms.fo_max;
            }
            if (mask & 0x100) {
              vs->fo_curve = cmd->param.parms.fo_curve;
            }
            if (mask & 0x1) {
              vs->vol_multiplier = cmd->param.parms.volume;
            }
          }
        }
      } break;
      case Jak2SoundCommand::set_master_volume: {
        u32 group = cmd->master_volume.group.group;
        // FIXME array of set volumes
        for (int i = 0; i < 32; i++) {
          if (((group >> i) & 1) != 0) {
            if (i == 1) {
              // gMusicVol = cmd->master_volume.volume;
              MasterVolume[1] = cmd->master_volume.volume;
            } else if (i == 2) {
              MasterVolume[2] = cmd->master_volume.volume;
              SetDialogVolume(cmd->master_volume.volume);
            } else {
              MasterVolume[i] = cmd->master_volume.volume;
              snd_SetMasterVolume(i, cmd->master_volume.volume);
              SetAllVagsVol(i);
            }
          }
        }
      } break;
      case Jak2SoundCommand::pause_group: {
        snd_PauseAllSoundsInGroup(cmd->group.group);
        if (cmd->group.group & 2) {
          gMusicPause = 1;
        }
        if (cmd->group.group & 4) {
          PauseVagStreams();
        }
      } break;
      case Jak2SoundCommand::stop_group: {
        KillSoundsInGroup(cmd->group.group);
        if (cmd->group.group & 4) {
          VagCmd local_178;
          local_178.header.cmd_kind = 0x402;
          local_178.header.mbx_to_reply = 0;
          local_178.header.thread_id = 0;
          local_178.vag_dir_entry = nullptr;
          local_178.name[0] = '\0';
          local_178.sound_handler = 0;
          local_178.id = 0;
          local_178.priority = 0;
          StopVagStream(&local_178, 1);
        }
      } break;
      case Jak2SoundCommand::continue_group: {
        snd_ContinueAllSoundsInGroup(cmd->group.group);
        if (cmd->group.group & 2) {
          gMusicPause = 0;
        }
        if (cmd->group.group & 4) {
          UnPauseVagStreams();
        }
      } break;
      case Jak2SoundCommand::set_midi_reg: {
        if (cmd->midi_reg.reg == 16) {
          snd_SetGlobalExcite(cmd->midi_reg.value);
        } else {
          Sound* sound = LookupSound(666);
          if (sound != nullptr) {
            snd_SetMIDIRegister(sound->sound_handle, cmd->midi_reg.reg, cmd->midi_reg.value);
          }
        }
      } break;
      case Jak2SoundCommand::set_reverb: {
        lg::warn("RPC_Player: unimplemented set_reverb");
        // TODO reverb
      } break;
      case Jak2SoundCommand::set_ear_trans: {
        SetEarTrans(&cmd->ear_trans_j2.ear_trans0, &cmd->ear_trans_j2.ear_trans1,
                    &cmd->ear_trans_j2.cam_trans, cmd->ear_trans_j2.cam_angle);
      } break;
      case Jak2SoundCommand::shutdown: {
        gSoundEnable = 0;
      } break;
      case Jak2SoundCommand::set_fps: {
        gFPS = cmd->fps.fps;
      } break;
      case Jak2SoundCommand::cancel_dgo: {
        // temporary. here just so we don't assert.
        lg::error("RPC Player dgo cancel command received");
      } break;
      default:
        // ASSERT_MSG(false, fmt::format("Unhandled RPC Player command {}", int(cmd->j2command)));
        lg::error("Unhandled Jak2 RPC Player command {}\n", int(cmd->j2command));
    }

    n_messages--;
    cmd++;
  }

  return nullptr;
}

void* RPC_Loader(unsigned int /*fno*/, void* data, int size) {
  constexpr int kMsgSize = 0x50;
  static_assert(sizeof(SoundRpcCommand) == kMsgSize);
  int n_messages = size / kMsgSize;
  SoundRpcCommand* cmd = (SoundRpcCommand*)(data);
  if (!gSoundEnable) {
    return nullptr;
  }

  while (n_messages > 0) {
    switch (cmd->j2command) {
      case Jak2SoundCommand::load_bank: {
        if (LookupBank(cmd->load_bank.bank_name)) {
          break;
        }

        auto bank = AllocateBankName(cmd->load_bank.bank_name);
        if (bank == nullptr) {
          break;
        }

        strncpy(bank->name.data(), cmd->load_bank.bank_name, 16);
        bank->in_use = true;
        bank->unk4 = 0;
        LoadSoundBank(cmd->load_bank.bank_name, bank);
      } break;
      case Jak2SoundCommand::load_music: {
        while (WaitSema(gSema))
          ;
        if (gMusic) {
          UnLoadMusic(&gMusic);
        }
        LoadMusic(cmd->load_bank.bank_name, &gMusic);
        SignalSema(gSema);
      } break;
      case Jak2SoundCommand::unload_bank: {
        auto bank = LookupBank(cmd->load_bank.bank_name);
        if (!bank) {
          break;
        }
        auto handle = bank->bank_handle;
        if (!bank->unk4) {
          bank->in_use = false;
        }
        bank->in_use = false;
        snd_UnloadBank(handle);
        snd_ResolveBankXREFS();
      } break;
      case Jak2SoundCommand::get_irx_version: {
        cmd->irx_version.major = 4;
        cmd->irx_version.minor = 0;
        gInfoEE = cmd->irx_version.ee_addr;
        return data;
      } break;
      case Jak2SoundCommand::set_language: {
        gLanguage = languages[cmd->set_language.langauge_id];
      } break;
      case Jak2SoundCommand::list_sounds: {
        // Not present in real jak2 overlord
        PrintActiveSounds();
      } break;
      case Jak2SoundCommand::unload_music: {
        while (WaitSema(gSema))
          ;
        if (gMusic) {
          UnLoadMusic(&gMusic);
        }
        SignalSema(gSema);
      } break;
      case Jak2SoundCommand::set_stereo_mode: {
        s32 mode = cmd->stereo_mode.stereo_mode;
        if (mode == 0) {
          snd_SetPlayBackMode(1);
        } else if (mode == 1) {
          snd_SetPlayBackMode(2);
        } else if (mode == 2) {
          snd_SetPlayBackMode(0);
        }
      } break;
      case Jak2SoundCommand::mirror_mode: {
        gMirrorMode = cmd->mirror.value;
      } break;
      default:
        ASSERT_MSG(false, fmt::format("Unhandled RPC Loader command {}", int(cmd->j2command)));
    }

    n_messages--;
    cmd++;
  }

  return nullptr;
}

int VBlank_Handler(void*) {
  IopTicks = IopTicks + 1;
  if (gSoundEnable == 0) {
    return 1;
  }
  iWakeupThread(StreamThread);
  if (gMusicFadeDir < 0) {
    gMusicFade = gMusicFade + -0x200;
    if (-1 < gMusicFade)
      goto LAB_00008d9c;
    gMusicFade = 0;
  } else {
    if ((gMusicFadeDir < 1) || (gMusicFade = gMusicFade + 0x400, gMusicFade < 0x10001))
      goto LAB_00008d9c;
    gMusicFade = 0x10000;
  }
  gMusicFadeDir = 0;
LAB_00008d9c:
  if (gInfoEE != 0) {
    gFrameNum = gFrameNum + 1;
    // instant dma
    //    if (dmaid != 0) {
    //      iVar2 = sceSifDmaStat();
    //      if (-1 < iVar2) {
    //        return 1;
    //      }
    //      dmaid = 0;
    //    }

    for (int i = 0; i < 4; i++) {
      u32 status_bits = 0;
      u32 pos;
      for (int j = 0; j < 24; j++) {
        if (VagCmds[i].status_bytes[j]) {
          status_bits |= (1 << j);
        }
      }
      if (VagCmds[i].unk_232) {
        status_bits |= (1 << 24);
      }

      if (VagCmds[i].byte6 && VagCmds[i].sb_paused == 0) {
        VagCmds[i].unk_192 += CalculateVAGPitch(0x400, VagCmds[i].unk_256_pitch2) / gFPS;
      }

      if (VagCmds[i].sb_playing == 0 && VagCmds[i].byte5) {
        pos = 0;
      } else {
        pos = VagCmds[i].unk_200;
      }

      info.stream_status[i] = status_bits;
      info.stream_position[i] = pos;
      // printf("positions: %d\n", pos);
      info.stream_id[i] = VagCmds[i].id;
    }

    info.iop_ticks = IsoThreadCounter;
    info.frame = gFrameNum;
    info.freemem = 100 /*gFreeMem*/;
    info.freemem2 = QueryTotalFreeMemSize();
    info.nocd = 0 /*gNoCD*/;
    info.dirtycd = 0 /*gDirtyCD*/;
    info.diskspeed[0] = 0 /*gDiskSpeed*/;
    info.diskspeed[1] = 0 /*DAT_00013488*/;
    info.lastspeed = 0 /*gLastSpeed*/;
    info.dupseg = 0 /*gDupSeg*/;
    for (int i = 0; i < 48; i++) {
      if (snd_GetVoiceStatus(i) == 1) {
        info.chinfo[i] = -1;
      } else {
        info.chinfo[i] = 0;
      }
    }
    LookupSound(666);  // music

    /*
    local_38 = &info;
    local_30 = 0x250;
    local_2c = 0;
    local_34 = gInfoEE;
    dmaid = sceSifSetDma(&local_38,1);
     */

    sceSifDmaData dma;
    dma.data = &info;
    dma.addr = (void*)(uintptr_t)gInfoEE;
    dma.size = 0x250;
    dma.mode = 0;
    /*dmaid =*/sceSifSetDma(&dma, 1);
  }
  return 1;
}

u32 Thread_Player() {
  sceSifQueueData dq;
  sceSifServeData serve;

  // set up RPC
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, PLAYER_RPC_ID[g_game_version], RPC_Player, gPlayerBuf, kPlayerBufSize,
                    nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

u32 Thread_Loader() {
  sceSifQueueData dq;
  sceSifServeData serve;

  // set up RPC
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, LOADER_RPC_ID[g_game_version], RPC_Loader, gLoaderBuf, kLoaderBufSize,
                    nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

void SetVagStreamName(VagCmd* param_1, int param_2, int param_3) {
  // undefined4 local_18 [2];

  if (param_3 == 1) {
    // CpuSuspendIntr(local_18);
  }
  if (param_2 == 0) {
    info.stream_name[param_1->idx_in_cmd_arr].dat[0] = '\0';
  } else {
    strncpy(info.stream_name[param_1->idx_in_cmd_arr].dat, param_1->name, 0x30);
  }
  if (param_3 == 1) {
    // CpuResumeIntr(local_18[0]);
  }
}

void SetVagName(int param_1, char* param_2, int param_3) {
  //  CpuSuspendIntr(local_18);
  if (param_3 == 0) {
    info.stream_name[param_1].dat[0] = '\0';
  } else {
    strncpy(info.stream_name[param_1].dat, param_2, 0x30);
  }
  // CpuResumeIntr(local_18[0]);
}

}  // namespace jak2
