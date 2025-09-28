#include "srpc.h"

#include "common/util/Assert.h"

#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/rpc_interface.h"
#include "game/overlord/jak3/sbank.h"
#include "game/overlord/jak3/soundcommon.h"
#include "game/overlord/jak3/spustreams.h"
#include "game/overlord/jak3/ssound.h"
#include "game/overlord/jak3/vag.h"
#include "game/overlord/jak3/vblank_handler.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

namespace jak3 {

using namespace iop;

// This file has two RPCs: PLAYER and LOADER
// Generally, PLAYER receives commands to play/pause sound effects or streams, which complete
// quickly.

// LOADER will load soundbanks, and can take some time to complete - likely why it is moved
// into its own RPC, to avoid having soundbank loads block playback of other sounds.

constexpr int kPlayerBufSize = 0x50 * 128;
static uint8_t s_anRPC_PlayerBuf[kPlayerBufSize];

constexpr int kLoaderBufSize = 0x50;
static uint8_t s_anRPC_LoaderBuf[kLoaderBufSize];

constexpr u32 kNumLanguages = 12;
static const char* languages[kNumLanguages] = {"ENG", "FRE", "GER", "SPA", "ITA", "COM",
                                               "JAP", "KOR", "RUS", "POR", "DUT", "UKE"};

const char* g_pszLanguage = languages[0];
u8 g_nFPS = 60;
SoundBankInfo* g_LoadingSoundBank = nullptr;

void jak3_overlord_init_globals_srpc() {
  g_nFPS = 60;
  g_LoadingSoundBank = nullptr;
  g_pszLanguage = languages[0];
}

u32 Thread_Player() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, RpcId::Player, RPC_Player, &s_anRPC_PlayerBuf, kPlayerBufSize, nullptr,
                    nullptr, &dq);

  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

u32 Thread_Loader() {
  sceSifQueueData dq;
  sceSifServeData serve;

  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, RpcId::Loader, RPC_Loader, &s_anRPC_LoaderBuf, kLoaderBufSize, nullptr,
                    nullptr, &dq);

  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

void* RPC_Player(unsigned int, void* msg, int size) {
  if (!g_bSoundEnable) {
    return nullptr;
  }

  // const auto* cmd = (RPC_Player_Cmd*)msg;
  ovrld_log(LogCategory::PLAYER_RPC, "Got Player RPC with {} cmds", size / kPlayerCommandStride);
  const u8* m_ptr = (const u8*)msg;
  const u8* end = m_ptr + size;

  for (; m_ptr < end; m_ptr += kPlayerCommandStride) {
    switch (((const Rpc_Player_Base_Cmd*)m_ptr)->command) {
      case SoundCommand::PLAY: {
        const auto* cmd = (const Rpc_Player_Play_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[Player RPC] command PLAY {} id {}", cmd->name.data,
                  cmd->sound_id);
        s32 id = cmd->sound_id;
        if (id) {
          auto* sound = LookupSound(id);
          if (!sound) {
            ovrld_log(LogCategory::PLAYER_RPC, "[Player RPC] allocating a new one");
            sound = AllocateSound();
            if (sound) {
              SFXUserData user_val;
              strcpy_toupper(sound->name.data, cmd->name.data);
              sound->params = cmd->params;
              sound->auto_time = 0;
              s32 get_status =
                  snd_GetSoundUserData(nullptr, nullptr, -1, sound->name.data, &user_val);
              s32 mask = sound->params.mask;
              if ((mask & 8) == 0) {
                sound->params.group = 0;
              }
              if ((mask & 0x40) == 0) {
                if (get_status == 0 || user_val.data[0] == 0) {
                  sound->params.fo_min = 5;
                } else {
                  sound->params.fo_min = (int16_t)user_val.data[0];
                }
              }
              if ((mask & 0x80) == 0) {
                if (get_status == 0 || user_val.data[1] == 0) {
                  sound->params.fo_max = 0x1e;
                } else {
                  sound->params.fo_max = (int16_t)user_val.data[1];
                }
              }
              if ((mask & 0x100) == 0) {
                u32 fo_curve = 0;
                if (get_status != 0) {
                  fo_curve = user_val.data[2];
                }
                (sound->params).fo_curve = fo_curve;
              }
              (sound->params).fo_curve = GetFalloffCurve(sound->params.fo_curve);
              auto handle = snd_PlaySoundByNameVolPanPMPB(
                  0, 0, sound->name.data, GetVolume(sound), GetPan(sound),
                  (int)(sound->params).pitch_mod, (int)(sound->params).bend);
              sound->id = id;
              sound->sound_handle = handle;
              if (handle != 0) {
                if ((sound->params.mask & 0x800) != 0) {
                  snd_SetSoundReg(sound->sound_handle, 0, sound->params.reg[0]);
                }
                if ((sound->params.mask & 0x1000) != 0) {
                  snd_SetSoundReg(sound->sound_handle, 1, sound->params.reg[1]);
                }
                if ((sound->params.mask & 0x2000) != 0) {
                  snd_SetSoundReg(sound->sound_handle, 2, sound->params.reg[2]);
                }
              }
            }
          } else {
            SFXUserData user_val;
            sound->params = cmd->params;
            s32 get_status =
                snd_GetSoundUserData(nullptr, nullptr, -1, sound->name.data, &user_val);
            s32 mask = (sound->params).mask;
            if ((mask & 8) == 0) {
              (sound->params).group = 0;
            }
            if ((mask & 0x40) == 0) {
              if (get_status == 0 || user_val.data[0] == 0) {
                (sound->params).fo_min = 5;
              } else {
                (sound->params).fo_min = user_val.data[0];
              }
            }
            if ((mask & 0x80) == 0) {
              if (get_status == 0 || user_val.data[1] == 0) {
                (sound->params).fo_max = 0x1e;
              } else {
                (sound->params).fo_max = user_val.data[1];
              }
            }
            if ((mask & 0x100) == 0) {
              s8 fo_curve = 0;
              if (get_status != 0) {
                fo_curve = user_val.data[2];
              }
              (sound->params).fo_curve = fo_curve;
            }
            (sound->params).fo_curve = GetFalloffCurve(sound->params.fo_curve);
            UpdateVolume(sound);
            snd_SetSoundPitchModifier(sound->sound_handle, (int)(sound->params).pitch_mod);
            if (((sound->params).mask & 4) != 0) {
              snd_SetSoundPitchBend(sound->sound_handle, (int)(sound->params).bend);
            }
            if (((sound->params).mask & 0x800) != 0) {
              snd_SetSoundReg(sound->sound_handle, 0, sound->params.reg[0]);
            }
            if (((sound->params).mask & 0x1000) != 0) {
              snd_SetSoundReg(sound->sound_handle, 1, (int)(char)(sound->params).reg[1]);
            }
            if (((sound->params).mask & 0x2000) != 0) {
              snd_SetSoundReg(sound->sound_handle, 2, sound->params.reg[2]);
            }
          }
        }
      } break;
      case SoundCommand::PAUSE_SOUND: {
        const auto* cmd = (const Rpc_Player_Sound_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Pause Sound ID {}", cmd->sound_id);
        if (cmd->sound_id) {
          auto* sound = LookupSound(cmd->sound_id);
          if (sound) {
            ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching Sound {} to pause",
                      sound->name.data);
            snd_PauseSound(sound->sound_handle);
          } else {
            auto* vag = FindVagStreamId(cmd->sound_id);
            if (vag) {
              ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching VAG {} to pause",
                        vag->name);
              PauseVAG(vag);
            }
          }
        }
      } break;
      case SoundCommand::STOP_SOUND: {
        const auto* cmd = (const Rpc_Player_Sound_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] stop Sound ID {}", cmd->sound_id);
        if (cmd->sound_id) {
          auto* sound = LookupSound(cmd->sound_id);
          if (sound) {
            ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching Sound {} to stop",
                      sound->name.data);
            snd_StopSound(sound->sound_handle);
          } else {
            auto* vag = FindVagStreamId(cmd->sound_id);
            if (vag) {
              ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching VAG {} to stop",
                        vag->name);
              StopVagStream(vag);
            }
          }
        }
      } break;
      case SoundCommand::CONTINUE_SOUND: {
        const auto* cmd = (const Rpc_Player_Sound_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] continue Sound ID {}", cmd->sound_id);
        if (cmd->sound_id) {
          auto* sound = LookupSound(cmd->sound_id);
          if (sound) {
            ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching Sound {} to continue",
                      sound->name.data);
            snd_ContinueSound(sound->sound_handle);
          } else {
            auto* vag = FindVagStreamId(cmd->sound_id);
            if (vag) {
              ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching VAG {} to continue",
                        vag->name);
              UnPauseVAG(vag);
            }
          }
        }
      } break;
      case SoundCommand::SET_PARAM: {
        const auto* cmd = (const Rpc_Player_Set_Param_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] SET_PARAM Sound ID {}", cmd->sound_id);
        if (cmd->sound_id) {
          auto* sound = LookupSound(cmd->sound_id);
          if (sound) {
            ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching Sound {} to SET_PARAM",
                      sound->name.data);
            auto& params = cmd->params;
            u16 mask = cmd->params.mask;
            s32 atime = cmd->auto_time;
            s32 afrom = cmd->auto_from;
            if ((mask & 1) != 0) {
              if ((mask & 0x10) == 0) {
                sound->params.volume = params.volume;
              } else {
                sound->auto_time = atime;
                sound->new_volume = params.volume;
              }
            }
            if ((mask & 0x20) != 0) {
              sound->params.trans[0] = params.trans[0];
              sound->params.trans[1] = params.trans[1];
              sound->params.trans[2] = params.trans[2];
            }
            if ((mask & 0x21) != 0) {
              UpdateVolume(sound);
            }
            if ((mask & 2) != 0) {
              auto pitch_mod = params.pitch_mod;
              sound->params.pitch_mod = pitch_mod;
              if ((mask & 0x10) == 0) {
                snd_SetSoundPitchModifier(sound->sound_handle, params.pitch_mod);
              } else {
                snd_AutoPitch(sound->sound_handle, pitch_mod, atime, afrom);
              }
            }
            if ((mask & 4) != 0) {
              auto bend = params.bend;
              sound->params.bend = bend;
              if ((mask & 0x10) == 0) {
                snd_SetSoundPitchBend(sound->sound_handle, params.bend);
              } else {
                snd_AutoPitchBend(sound->sound_handle, (int)bend, atime, afrom);
              }
            }
            if ((mask & 0x400) != 0) {
              sound->params.priority = params.priority;
            }
            if ((mask & 8) != 0) {
              sound->params.group = params.group;
            }
            if ((mask & 0x40) != 0) {
              sound->params.fo_min = params.fo_min;
            }
            if ((mask & 0x80) != 0) {
              sound->params.fo_max = params.fo_max;
            }
            if ((mask & 0x100) != 0) {
              sound->params.fo_curve = GetFalloffCurve(params.fo_curve);
            }
            if ((mask & 0x800) != 0) {
              sound->params.reg[0] = params.reg[0];
              snd_SetSoundReg(sound->sound_handle, 0, params.reg[0]);
            }
            if ((mask & 0x1000) != 0) {
              sound->params.reg[1] = params.reg[1];
              snd_SetSoundReg(sound->sound_handle, 1, params.reg[1]);
            }
            if ((mask & 0x2000) != 0) {
              sound->params.reg[2] = params.reg[2];
              snd_SetSoundReg(sound->sound_handle, 2, params.reg[2]);
            }
          } else {
            auto* vag = FindVagStreamId(cmd->sound_id);
            if (vag) {
              ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Found matching VAG {} to SET_PARAM",
                        vag->name);
              auto& params = cmd->params;
              auto mask = params.mask;
              if ((mask & 2) != 0) {
                SetVAGStreamPitch(cmd->sound_id, params.pitch_mod);
              }
              if ((mask & 0x20) != 0) {
                vag->trans[0] = params.trans[0];
                vag->trans[1] = params.trans[1];
                vag->trans[2] = params.trans[2];
                vag->updated_trans = 1;
              }
              if ((mask & 8) != 0) {
                vag->play_group = params.group;
              }
              if ((mask & 0x40) != 0) {
                vag->fo_min = (int)params.fo_min;
              }
              if ((mask & 0x80) != 0) {
                vag->fo_max = (int)params.fo_max;
              }
              if ((mask & 0x100) != 0) {
                vag->fo_curve = GetFalloffCurve(params.fo_curve);
              }
              if ((mask & 1) != 0) {
                vag->play_volume = params.volume;
              }
            }
          }
        }
      } break;
      case SoundCommand::SET_MASTER_VOLUME: {
        const auto* cmd = (const Rpc_Player_Set_Master_Volume_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Set Master Volume to {}", cmd->volume);
        for (int i = 0; i < 17; i++) {
          if (cmd->group & (1 << i)) {
            g_anMasterVolume[i] = cmd->volume;
            snd_SetMasterVolume(i, cmd->volume);
            SetAllVagsVol(i);
          }
        }
      } break;
      case SoundCommand::PAUSE_GROUP: {
        const auto* cmd = (const Rpc_Player_Group_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Pause groups 0b{:b}", cmd->group);
        snd_PauseAllSoundsInGroup(cmd->group);
        if (cmd->group & 4) {
          PauseVAGStreams();
        }
        if (cmd->group & 2) {
          g_bMusicPause = true;
        }
      } break;
      case SoundCommand::STOP_GROUP: {
        const auto* cmd = (const Rpc_Player_Group_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Stop groups 0b{:b}", cmd->group);
        KillSoundsInGroup(cmd->group);
        if (cmd->group & 4) {
          ISO_VAGCommand vag_cmd;
          vag_cmd.msg_type = ISO_Hdr::MsgType::VAG_STOP;  // seems unsupported by iso thread.
          vag_cmd.mbox_reply = 0;
          vag_cmd.thread_to_wake = 0;
          vag_cmd.vag_dir_entry = nullptr;
          vag_cmd.name[0] = 0;
          vag_cmd.maybe_sound_handler = 0;
          vag_cmd.id = 0;
          vag_cmd.priority_pq = 0;
          StopVagStream(&vag_cmd);
        }
      } break;
      case SoundCommand::CONTINUE_GROUP: {
        const auto* cmd = (const Rpc_Player_Group_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Continue groups 0b{:b}", cmd->group);
        snd_ContinueAllSoundsInGroup(cmd->group);
        if (cmd->group & 4) {
          UnpauseVAGStreams();
        }
        if (cmd->group & 2) {
          g_bMusicPause = false;
        }
      } break;
      case SoundCommand::SET_REVERB: {
        ovrld_log(LogCategory::WARN, "[RPC Player] Unimplemented set reverb.");
      } break;
      case SoundCommand::SET_EAR_TRANS: {
        const auto* cmd = (const Rpc_Player_Set_Ear_Trans_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] set ear trans");
        SetEarTrans(cmd->ear_trans0, cmd->ear_trans1, cmd->ear_trans, cmd->cam_forward,
                    cmd->cam_left, cmd->cam_scale, (cmd->cam_inverted != 0));
      } break;
      case SoundCommand::SHUTDOWN: {
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] Shutdown!");
        WaitSema(g_n989Semaphore);
        if (g_bSoundEnable) {
          g_bSoundEnable = false;
          snd_StopSoundSystem();
        }
        SignalSema(g_n989Semaphore);
      } break;
      case SoundCommand::SET_FPS: {
        const auto* cmd = (const Rpc_Player_Set_Fps_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] set fps {}", (int)cmd->fps);
        g_nFPS = cmd->fps;
      } break;
      case SoundCommand::CANCEL_DGO: {
        const auto* cmd = (const Rpc_Player_Cancel_Dgo_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Player] cancel dgo {}", cmd->id);
        CancelDGONoSync(cmd->id);
      } break;
      case SoundCommand::SET_MIDI_REG:
        // this is what the real overlord does - just ignore it!
        break;
      default:
        ovrld_log(LogCategory::WARN, "[RPC Player] Unsupported Player {}",
                  (int)((const Rpc_Player_Base_Cmd*)m_ptr)->command);
        ASSERT_NOT_REACHED();
    }
  }

  return nullptr;
}

void* RPC_Loader(unsigned int, void* msg, int size) {
  if (!g_bSoundEnable) {
    return nullptr;
  }

  // const auto* cmd = (RPC_Player_Cmd*)msg;
  ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got Loader RPC with {} cmds",
            size / kLoaderCommandStride);
  u8* m_ptr = (u8*)msg;
  const u8* end = m_ptr + size;

  for (; m_ptr < end; m_ptr += kLoaderCommandStride) {
    switch (((const Rpc_Player_Base_Cmd*)m_ptr)->command) {
      case SoundCommand::LOAD_BANK: {
        auto* cmd = (const Rpc_Loader_Load_Bank_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got sound bank load command: {}",
                  cmd->bank_name.data);
        // src = &cmd->bank_name;
        if (!LookupBank(cmd->bank_name.data)) {
          auto* info = AllocateBankName(cmd->bank_name.data, cmd->mode);
          if (info) {
            strncpyz(info->m_name1, cmd->bank_name.data, 0x10);
            info->in_use = 1;
            info->unk0 = 0;
            g_LoadingSoundBank = info;
            if (LoadSoundBankToIOP(cmd->bank_name.data, info, cmd->priority) == 0) {
              info->loaded = 1;
            } else {
              info->loaded = 0;
              info->in_use = 0;
            }
            g_LoadingSoundBank = nullptr;
          }
        }
      } break;
      case SoundCommand::LOAD_MUSIC: {
        auto* cmd = (const Rpc_Loader_Bank_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got music load command: {}",
                  cmd->bank_name.data);

        // lock
        u32 wait_status = 1;
        while (wait_status) {
          wait_status = WaitSema(g_nMusicSemaphore);
        }

        // set music name
        if ((cmd->bank_name).data[0] == 0) {
          g_szTargetMusicName[0] = 0;
        } else {
          strcpy(g_szTargetMusicName, cmd->bank_name.data);
        }

        // release
        SignalSema(g_nMusicSemaphore);
      } break;

      case SoundCommand::UNLOAD_BANK: {
        auto* cmd = (const Rpc_Loader_Bank_Cmd*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got bank load unload command: {}",
                  cmd->bank_name.data);
        SoundBankInfo* ifno = LookupBank(cmd->bank_name.data);
        if (ifno) {
          auto snd_handle = ifno->snd_handle;
          ifno->snd_handle = nullptr;
          if (ifno->unk0 == 0) {
            ifno->in_use = 0;
          }
          ifno->mode = 0;
          ifno->loaded = 0;
          snd_UnloadBank(snd_handle);
          snd_ResolveBankXREFS();
        }
      } break;

      case SoundCommand::GET_IRX_VERSION: {
        auto* cmd = (Rpc_Loader_Get_Irx_Version*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got IRX version command");
        g_nInfoEE = cmd->ee_addr;
        cmd->major = 4;
        cmd->minor = 0;
        return cmd;
      } break;

      case SoundCommand::SET_LANGUAGE: {
        auto* cmd = (Rpc_Loader_Set_Language*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got set language command {}", cmd->lang);
        ASSERT(cmd->lang < kNumLanguages);
        g_pszLanguage = languages[cmd->lang];
      } break;

      case SoundCommand::UNLOAD_MUSIC: {
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got unload music command");

        // lock
        u32 wait_status = 1;
        while (wait_status) {
          wait_status = WaitSema(g_nMusicSemaphore);
        }

        // set music name
        g_szTargetMusicName[0] = 0;

        // release
        SignalSema(g_nMusicSemaphore);
      } break;

      case SoundCommand::SET_STEREO_MODE: {
        auto* cmd = (Rpc_Loader_Set_Stereo_Mode*)m_ptr;
        ovrld_log(LogCategory::PLAYER_RPC, "[RPC Loader] Got set stereo command {}", cmd->mode);

        switch (cmd->mode) {
          case 0:
            SetPlaybackMode(1);
            break;
          case 1:
            SetPlaybackMode(2);
            break;
          case 2:
            SetPlaybackMode(0);
            break;
          default:
            ASSERT_NOT_REACHED();
        }
      } break;

      // added
      case SoundCommand::LIST_SOUNDS: {
        PrintBanks();
        PrintSounds();
      } break;

      default:
        ovrld_log(LogCategory::WARN, "[RPC Loader] Unsupported Loader {}",
                  (int)((const Rpc_Player_Base_Cmd*)m_ptr)->command);
        ASSERT_NOT_REACHED();
        break;
    }
  }
  return nullptr;
}

void SetVagStreamName(ISO_VAGCommand* cmd, int len) {
  ASSERT(cmd);
  if (!cmd->music_flag && cmd->info_idx < 4) {
    if (!len) {
      g_SRPCSoundIOPInfo.stream_name[cmd->info_idx].chars[0] = 0;
    } else {
      strncpy(g_SRPCSoundIOPInfo.stream_name[cmd->info_idx].chars, cmd->name, 0x30);
    }
  } else {
    //    ASSERT_NOT_REACHED();
  }
}

}  // namespace jak3