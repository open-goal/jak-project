#include "srpc.h"

#include <cstdio>
#include <cstring>

#include "iso_api.h"
#include "ramdisk.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/versions/versions.h"

#include "game/common/game_common_types.h"
#include "game/common/loader_rpc_types.h"
#include "game/common/player_rpc_types.h"
#include "game/graphics/gfx.h"
#include "game/overlord/common/iso.h"
#include "game/overlord/common/sbank.h"
#include "game/overlord/common/soundcommon.h"
#include "game/overlord/common/srpc.h"
#include "game/overlord/common/ssound.h"
#include "game/overlord/jak1/iso.h"
#include "game/runtime.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

#include "fmt/core.h"
#include "third-party/magic_enum.hpp"

using namespace iop;

namespace jak1 {
constexpr int SRPC_MESSAGE_SIZE = 0x50;
constexpr int SRPC_MESSAGE_COUNT = 128;
static uint8_t gLoaderBuf[SRPC_MESSAGE_SIZE];
static uint8_t gPlayerBuf[SRPC_MESSAGE_SIZE * SRPC_MESSAGE_COUNT];
static u32 gInfoEE = 0;  // EE address where we should send info on each frame.
s16 gFlava;
u32 gFreeMem = 0;

static SoundIopInfo info;

s32 gVAG_Id = 0;  // TODO probably doesn't belong here.

// english, french, germain, spanish, italian, japanese, uk.
const char* languages[] = {"ENG", "FRE", "GER", "SPA", "ITA", "JAP", "UKE"};

void srpc_init_globals() {
  memset((void*)gLoaderBuf, 0, sizeof(gLoaderBuf));
  memset((void*)gPlayerBuf, 0, sizeof(gPlayerBuf));
  gSoundEnable = 1;
  gInfoEE = 0;
  gLanguage = languages[(int)Language::English];
}

void* RPC_Player(unsigned int fno, void* data, int size);

u32 Thread_Player() {
  sceSifQueueData dq;
  sceSifServeData serve;

  // set up RPC
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, PLAYER_RPC_ID[g_game_version], RPC_Player, gPlayerBuf,
                    SRPC_MESSAGE_SIZE * SRPC_MESSAGE_COUNT, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

void* RPC_Loader(unsigned int fno, void* data, int size);

u32 Thread_Loader() {
  sceSifQueueData dq;
  sceSifServeData serve;

  // set up RPC
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, LOADER_RPC_ID[g_game_version], RPC_Loader, gLoaderBuf,
                    SRPC_MESSAGE_SIZE, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

void* RPC_Player(unsigned int /*fno*/, void* data, int size) {
  if (gSoundEnable) {
    gFreeMem = QueryTotalFreeMemSize();
    if (!PollSema(gSema)) {
      if (gMusic) {
        if (!gMusicPause && !LookupSound(666)) {
          Sound* music = AllocateSound(false);
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
      snd_SetMIDIRegister(music->sound_handle, 0, gFlava);
    }

    int n_messages = size / SRPC_MESSAGE_SIZE;
    SoundRpcCommand* cmd = (SoundRpcCommand*)(data);
    while (n_messages > 0) {
      switch (cmd->j1command) {
        case Jak1SoundCommand::PLAY: {
          if (cmd->play.sound_id == 0) {
            break;
          }
          if (!memcmp(cmd->play.name, "spool-", 6)) {
            char namebuf[16];
            const char* name = &cmd->play.name[6];
            size_t len = strlen(name);
            if (len < 9) {
              memset(namebuf, ' ', 8);
              memcpy(namebuf, name, len);
            } else {
              memcpy(namebuf, name, 8);
            }

            // ASCII toupper
            for (int i = 0; i < 8; i++) {
              if (namebuf[i] >= 0x61 && namebuf[i] < 0x7b) {
                namebuf[i] -= 0x20;
              }
            }

            auto vagfile = FindVAGFile(namebuf);

            memcpy(namebuf, "VAGWAD  ", 8);
            strcpy(&namebuf[8], gLanguage);

            FileRecord* rec = isofs->find_in(namebuf);
            if (vagfile != nullptr) {
              if (cmd->play.parms.pitch_mod) {  // ??? TODO Verify what is being checked here
                PlayVAGStream(rec, vagfile, cmd->play.sound_id, cmd->play.parms.volume, 0,
                              &cmd->play.parms.trans);
              } else {
                PlayVAGStream(rec, vagfile, cmd->play.sound_id, cmd->play.parms.volume, 0, nullptr);
              }
            }
            break;
          }

          SoundBank* bank = nullptr;
          s32 index = LookupSoundIndex(cmd->play.name, &bank);
          if (index < 0) {
            break;
          }

          Sound* sound = LookupSound(cmd->play.sound_id);
          if (sound) {
            memcpy(&sound->params, &cmd->play.parms, sizeof(sound->params));
            sound->bank_entry = &bank->sound[index];
            sound->is_music = 0;
            if ((sound->params.mask & 0x40) == 0) {
              sound->params.fo_min = sound->bank_entry->fallof_params & 0x3fff;
            }
            if ((sound->params.mask & 0x80) == 0) {
              sound->params.fo_max = (sound->bank_entry->fallof_params >> 14) & 0x3fff;
            }
            if ((sound->params.mask & 0x100) == 0) {
              sound->params.fo_curve = sound->bank_entry->fallof_params >> 28;
            }
            UpdateVolume(sound);
            snd_SetSoundPitchModifier(sound->sound_handle, cmd->play.parms.pitch_mod);
            snd_SetSoundPitchBend(sound->sound_handle, cmd->play.parms.bend);
            break;
          }

          sound = AllocateSound(false);
          if (!sound) {
            break;
          }
          memcpy(&sound->params, &cmd->play.parms, sizeof(sound->params));
          sound->bank_entry = &bank->sound[index];
          sound->is_music = 0;
          sound->auto_time = 0;

          if ((sound->params.mask & 0x40) == 0) {
            sound->params.fo_min = sound->bank_entry->fallof_params & 0x3fff;
          }
          if ((sound->params.mask & 0x80) == 0) {
            sound->params.fo_max = (sound->bank_entry->fallof_params >> 14) & 0x3fff;
          }
          if ((sound->params.mask & 0x100) == 0) {
            sound->params.fo_curve = sound->bank_entry->fallof_params >> 28;
          }
          s32 vol = GetVolume(sound);
          s32 pan = GetPan(sound);
          s32 handle = snd_PlaySoundVolPanPMPB(bank->bank_handle, index, vol, pan,
                                               sound->params.pitch_mod, sound->params.bend);
          sound->sound_handle = handle;
          if (sound->sound_handle) {
            sound->id = cmd->play.sound_id;
          }
        } break;
        case Jak1SoundCommand::PAUSE_SOUND: {
          Sound* sound = LookupSound(cmd->sound_id.sound_id);
          if (sound != nullptr) {
            snd_PauseSound(sound->sound_handle);
          } else if (cmd->sound_id.sound_id == (u32)gVAG_Id) {
            PauseVAGStream();
          }
        } break;
        case Jak1SoundCommand::STOP_SOUND: {
          Sound* sound = LookupSound(cmd->sound_id.sound_id);
          if (sound != nullptr) {
            snd_StopSound(sound->sound_handle);
          } else if (cmd->sound_id.sound_id == (u32)gVAG_Id) {
            StopVAGStream(nullptr, 0);
          }
        } break;
        case Jak1SoundCommand::CONTINUE_SOUND: {
          Sound* sound = LookupSound(cmd->sound_id.sound_id);
          if (sound != nullptr) {
            snd_ContinueSound(sound->sound_handle);
          } else if (cmd->sound_id.sound_id == (u32)gVAG_Id) {
            UnpauseVAGStream();
          }
        } break;
        case Jak1SoundCommand::SET_PARAM: {
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

          } else if (cmd->sound_id.sound_id == (u32)gVAG_Id) {
            SetVAGStreamVolume(cmd->param.parms.volume);
          }
        } break;
        case Jak1SoundCommand::SET_MASTER_VOLUME: {
          u32 group = cmd->master_volume.group.group;
          for (int i = 0; i < 32; i++) {
            if (((group >> i) & 1) != 0) {
              if (i == 1) {
                gMusicVol = cmd->master_volume.volume;
              } else if (i == 2) {
                SetDialogVolume(cmd->master_volume.volume);
              } else {
                snd_SetMasterVolume(i, cmd->master_volume.volume);
              }
            }
          }
        } break;
        case Jak1SoundCommand::PAUSE_GROUP: {
          snd_PauseAllSoundsInGroup(cmd->group.group);
          if ((cmd->group.group & 4) != 0) {
            PauseVAGStream();
          }
          if (cmd->group.group & 2) {
            gMusicPause = 1;
          }
        } break;
        case Jak1SoundCommand::STOP_GROUP: {
          u8 group = cmd->group.group;
          KillSoundsInGroup(group);
          if ((group & 4) != 0) {
            StopVAGStream(nullptr, 0);
          }
        } break;
        case Jak1SoundCommand::CONTINUE_GROUP: {
          snd_ContinueAllSoundsInGroup(cmd->group.group);
          if (cmd->group.group & 4) {
            UnpauseVAGStream();
          }

          if (cmd->group.group & 2) {
            gMusicPause = 0;
          }
        } break;
        case Jak1SoundCommand::SET_FALLOFF_CURVE: {
          SetCurve(cmd->fallof_curve.curve, cmd->fallof_curve.falloff, cmd->fallof_curve.ease);
        } break;
        case Jak1SoundCommand::SET_SOUND_FALLOFF: {
          SoundBank* bank;
          s32 idx = LookupSoundIndex(cmd->fallof.name, &bank);
          if (idx >= 0) {
            bank->sound[idx].fallof_params =
                (cmd->fallof.curve << 28) | (cmd->fallof.max << 14) | cmd->fallof.min;
          }
        } break;
        case Jak1SoundCommand::SET_FLAVA: {
          gFlava = cmd->flava.flava;
        } break;
        case Jak1SoundCommand::SET_EAR_TRANS: {
          SetEarTrans(&cmd->ear_trans.ear_trans, &cmd->ear_trans.ear_trans,
                      &cmd->ear_trans.cam_trans, cmd->ear_trans.cam_angle);
        } break;
        case Jak1SoundCommand::SHUTDOWN: {
          gSoundEnable = 0;
          snd_StopSoundSystem();
          // TODO ShutdownFilingSystem();
        } break;
        default: {
          ASSERT_MSG(false, fmt::format("Unhandled RPC Player command {}",
                                        magic_enum::enum_name(cmd->j1command)));
        } break;
      }
      n_messages--;
      cmd++;
    }
  }
  return nullptr;
}

void* RPC_Loader(unsigned int /*fno*/, void* data, int size) {
  int n_messages = size / SRPC_MESSAGE_SIZE;
  SoundRpcCommand* cmd = (SoundRpcCommand*)(data);
  if (gSoundEnable) {
    // I don't think it should be possible to have > 1 message here - the buffer isn't big enough.
    if (n_messages > 1) {
      ASSERT(false);
    }
    while (n_messages > 0) {
      switch (cmd->j1command) {
        case Jak1SoundCommand::LOAD_BANK: {
          // see if it's already loaded
          auto bank = LookupBank(cmd->load_bank.bank_name);
          if (!bank) {
            // see if we have room to load another
            auto new_bank = AllocateBank();
            if (new_bank) {
              // we do!
              LoadSoundBank(cmd->load_bank.bank_name, new_bank);
            }
          }
        } break;
        case Jak1SoundCommand::UNLOAD_BANK: {
          SoundBank* bank = LookupBank(cmd->load_bank.bank_name);
          if (bank != nullptr) {
            snd::BankHandle id = bank->bank_handle;
            bank->bank_handle = 0;
            snd_UnloadBank(id);
            snd_ResolveBankXREFS();
          }
        } break;
        case Jak1SoundCommand::GET_IRX_VERSION: {
          cmd->irx_version.major = IRX_VERSION_MAJOR;
          cmd->irx_version.minor = IRX_VERSION_MINOR;
          gInfoEE = cmd->irx_version.ee_addr;
          return cmd;
        } break;
        case Jak1SoundCommand::RELOAD_INFO: {
          ReloadBankInfo();
        } break;
        case Jak1SoundCommand::SET_LANGUAGE: {
          gLanguage = languages[cmd->set_language.langauge_id];
          printf("IOP language: %s\n", gLanguage);  // added.
        } break;
        case Jak1SoundCommand::LOAD_MUSIC: {
          while (WaitSema(gSema))
            ;
          if (gMusic) {
            gMusicFadeDir = -1;
            while (gMusicFade) {
              DelayThread(1000);
            }
            snd_UnloadBank(gMusic);
            snd_ResolveBankXREFS();
            gMusic = nullptr;
          }
          LoadMusic(cmd->load_bank.bank_name, &gMusic);
          SignalSema(gSema);
        } break;
        case Jak1SoundCommand::LIST_SOUNDS: {
          PrintActiveSounds();
        } break;
        case Jak1SoundCommand::UNLOAD_MUSIC: {
          while (WaitSema(gSema))
            ;
          if (gMusic) {
            gMusicFadeDir = -1;
            while (gMusicFade) {
              DelayThread(1000);
            }
            snd_UnloadBank(gMusic);
            snd_ResolveBankXREFS();
            gMusic = 0;
          }
          SignalSema(gSema);
        } break;
        case Jak1SoundCommand::MIRROR_MODE: {
          gMirrorMode = cmd->mirror.value;
        } break;
        default:
          ASSERT_MSG(false,
                     fmt::format("Unhandled RPC Loader command {} ({})",
                                 magic_enum::enum_name(cmd->j1command), (int)cmd->j1command));
      }
      n_messages--;
      cmd++;
    }
  }
  return nullptr;
}

static s32 dmaid = 0;

s32 VBlank_Handler(void*) {
  if (!gSoundEnable)
    return 1;

  if (gMusicFadeDir > 0) {
    gMusicFade += (0x10000 / 64);
    if (gMusicFade > 0x10000 || (gMusicFadeHack & 1)) {
      gMusicFade = 0x10000;
      gMusicFadeDir = 0;
    }
  } else if (gMusicFadeDir < 0) {
    gMusicFade -= (0x10000 / 128);
    if (gMusicFade < 0 || (gMusicFadeHack & 2)) {
      gMusicFade = 0;
      gMusicFadeDir = 0;
    }
  }

  if (!gInfoEE)
    return 1;

  gFrameNum++;

  if (gFakeVAGClockRunning && !gFakeVAGClockPaused) {
    gFakeVAGClock += (s32)(1024 / Gfx::g_global_settings.target_fps);
  }

  // We don't need this, our DMA's are instant
  // if (dmaid) {
  //  if (sceSifDmaStat(dmaid) >= 0) {
  //    return 1;
  //  }
  //  dmaid = 0;
  //}

  info.frame = gFrameNum;
  info.strpos = GetVAGStreamPos();
  info.std_id = gVAG_Id;
  info.freemem = gFreeMem;
  info.freemem2 = gMemFreeAtStart;
  // info.nocd = gNoCD;
  // info.dirtycd = gDirtyCD;
  info.nocd = 0;
  info.dirtycd = 0;
  // info.diskspeed[0] = gDiskSpeed[0];
  // info.diskspeed[1] = gDiskSpeed[1];
  // info.lastspeed = gLastSpeed;
  // info.dupseg = gDupSeg;

  for (int i = 0; i < 48; i++) {
    if (snd_GetVoiceStatus(i) == 1) {
      info.chinfo[i] = -1;
    } else {
      info.chinfo[i] = 0;
    }
  }

  sceSifDmaData dma;
  dma.data = &info;
  dma.addr = (void*)(uintptr_t)gInfoEE;
  dma.size = 0x110;
  dma.mode = 0;
  dmaid = sceSifSetDma(&dma, 1);

  return 1;
}
}  // namespace jak1
