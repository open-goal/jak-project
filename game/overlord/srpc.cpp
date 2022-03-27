#include <cstring>
#include <cstdio>
#include "game/overlord/sndshim.h"
#include "srpc.h"
#include "game/sce/iop.h"
#include "game/common/loader_rpc_types.h"
#include "game/common/player_rpc_types.h"
#include "game/common/game_common_types.h"
#include "common/versions.h"
#include "sbank.h"
#include "iso_api.h"
#include "common/util/Assert.h"

using namespace iop;

MusicTweaks gMusicTweakInfo;
constexpr int SRPC_MESSAGE_SIZE = 0x50;
static uint8_t gLoaderBuf[SRPC_MESSAGE_SIZE];
static uint8_t gPlayerBuf[SRPC_MESSAGE_SIZE * 127];
int32_t gSoundEnable = 1;
static u32 gInfoEE = 0;  // EE address where we should send info on each frame.
s16 gFlava;
static s32 gMusic;
s32 gMusicTweak = 0x80;
s32 gMusicPause = 0;
u32 gFreeMem = 0;

s32 gVAG_Id = 0;  // TODO probably doesn't belong here.

// english, french, germain, spanish, italian, japanese, uk.
static const char* languages[] = {"ENG", "FRE", "GER", "SPA", "ITA", "JAP", "UKE"};
const char* gLanguage = nullptr;

void srpc_init_globals() {
  memset((void*)&gMusicTweakInfo, 0, sizeof(gMusicTweakInfo));
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
  sceSifRegisterRpc(&serve, PLAYER_RPC_ID, RPC_Player, gPlayerBuf, nullptr, nullptr, &dq);
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
  sceSifRegisterRpc(&serve, LOADER_RPC_ID, RPC_Loader, gLoaderBuf, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

void* RPC_Player(unsigned int /*fno*/, void* data, int size) {
  if (gSoundEnable) {
    gFreeMem = QueryTotalFreeMemSize();
    // if (!PollSema(gSema)) {
    if (gMusic) {
      if (!gMusicPause && !LookupSound(666)) {
        Sound* music = AllocateSound();
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
    //}

    SetMusicVol();
    Sound* music = LookupSound(666);
    if (music != nullptr) {
      snd_SetSoundVolPan(music->sound_handle, 0x7FFFFFFF, 0);
      snd_SetMIDIRegister(music->sound_handle, 0, gFlava);
    }

    int n_messages = size / SRPC_MESSAGE_SIZE;
    SoundRpcCommand* cmd = (SoundRpcCommand*)(data);
    while (n_messages > 0) {
      switch (cmd->command) {
        case SoundCommand::PLAY: {
          // spool- soundsn are vag sounds?
          if (!memcmp(cmd->play.name, "spool-", 6)) {
            char namebuf[8];
            char langbuf[8];
            auto name = cmd->play.name;
            size_t len = strlen(name);
            if (len < 9) {
              memset(namebuf, 32, sizeof(namebuf));
              memcpy(namebuf, name, len);
            } else {
              memcpy(namebuf, name, sizeof(namebuf));
            }

            for (int i = 0; i < 8; i++) {
              if (namebuf[i] >= 0x61 && namebuf[i] < 0x7b) {
                namebuf[i] -= 0x20;
              }
            }

            // TODO vagfile = FindVAGFile(namebuf);
            void* vagfile = nullptr;

            memcpy(namebuf, "VAGWAD ", sizeof(namebuf));
            strcpy(langbuf, gLanguage);

            FileRecord* rec = isofs->find_in(namebuf);
            if (vagfile != nullptr) {
              if (cmd->play.parms.pitch_mod) {  // ??? TODO Verify what is being checked here
                // PlayVagStream(rec, vagfile, cmd->play.sound_id, cmd->play.parms.volume, 0,
                // cmd->play.parms.trans);
              } else {
                // PlayVagStream(rec, vagfile, cmd->play.sound_id, cmd->play.parms.volume, 0, 0);
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

          sound = AllocateSound();
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
            sound->id = index;
          }
        } break;
        case SoundCommand::PAUSE_SOUND: {
          Sound* sound = LookupSound(cmd->sound_id.sound_id);
          if (sound != nullptr) {
            snd_PauseSound(sound->sound_handle);
          } else if (cmd->sound_id.sound_id == gVAG_Id) {
            // TODO PauseVAGStream();
          }
        } break;
        case SoundCommand::STOP_SOUND: {
          Sound* sound = LookupSound(cmd->sound_id.sound_id);
          if (sound != nullptr) {
            snd_StopSound(sound->sound_handle);
          } else if (cmd->sound_id.sound_id == gVAG_Id) {
            // TODO StopVAGStream();
          }
        } break;
        case SoundCommand::CONTINUE_SOUND: {
          Sound* sound = LookupSound(cmd->sound_id.sound_id);
          if (sound != nullptr) {
            snd_ContinueSound(sound->sound_handle);
          } else if (cmd->sound_id.sound_id == gVAG_Id) {
            // TODO UNpauseVAGStream();
          }
        } break;
        case SoundCommand::SET_PARAM: {
          Sound* sound = LookupSound(cmd->sound_id.sound_id);
          u32 mask = cmd->param.parms.mask;
          if (sound != nullptr) {
            if (mask & 1) {
              if (mask & 0x20) {
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
                              cmd->param.auto_time);
              } else {
                snd_SetSoundPitchModifier(sound->sound_handle, cmd->param.parms.pitch_mod);
              }
            }
            if (mask & 4) {
              sound->params.bend = cmd->param.parms.bend;
              if (mask & 0x10) {
                snd_AutoPitchBend(sound->sound_handle, sound->params.bend, cmd->param.auto_time,
                                  cmd->param.auto_time);
              } else {
                snd_SetSoundPitchBend(sound->sound_handle, cmd->param.parms.bend);
              }
            }

          } else if (cmd->sound_id.sound_id == gVAG_Id) {
            // TODO SetVAGStreamVolume();
          }
        } break;
        case SoundCommand::SET_MASTER_VOLUME: {
          u32 group = cmd->master_volume.group.group;
          for (int i = 0; i < 32; i++) {
            if (((group >> i) & 1) != 0) {
              if (i == 1) {
                gMusicVol = cmd->master_volume.volume;
              } else if (i == 2) {
                // TODO SetDialogVolume(cmd->master_volume.volume);
              } else {
                snd_SetMasterVolume(i, cmd->master_volume.volume);
              }
            }
          }
        } break;
        case SoundCommand::PAUSE_GROUP: {
          snd_PauseAllSoundsInGroup(cmd->group.group);
          if ((cmd->group.group & 4) != 0) {
            // TODO PauseVAGStream(0,0);
          }
          if (cmd->group.group & 2) {
            gMusicPause = 1;
          }
        } break;
        case SoundCommand::STOP_GROUP: {
          u8 group = cmd->group.group;
          KillSoundsInGroup(group);
          if ((group & 4) != 0) {
            // TODO StopVAGStream(0,0);
          }
        } break;
        case SoundCommand::CONTINUE_GROUP: {
          snd_ContinueAllSoundsInGroup(cmd->group.group);
          if (cmd->group.group & 4) {
            //   UnpauseVAGStream();
          }

          if (cmd->group.group & 2) {
            gMusicPause = 0;
          }
        } break;
        case SoundCommand::SET_FALLOFF_CURVE: {
          SetCurve(cmd->fallof_curve.curve, cmd->fallof_curve.falloff, cmd->fallof_curve.ease);
        } break;
        case SoundCommand::SET_SOUND_FALLOFF: {
          SoundBank* bank;
          s32 idx = LookupSoundIndex(cmd->fallof.name, &bank);
          if (idx >= 0) {
            bank->sound[idx].fallof_params =
                (cmd->fallof.curve << 28) | (cmd->fallof.max << 14) | cmd->fallof.min;
          }
        } break;
        case SoundCommand::SET_FLAVA: {
          gFlava = cmd->flava.flava;
        } break;
        case SoundCommand::SET_EAR_TRANS: {
          SetEarTrans(&cmd->ear_trans.ear_trans, &cmd->ear_trans.cam_trans,
                      cmd->ear_trans.cam_angle);
        } break;
        case SoundCommand::SHUTDOWN: {
          gSoundEnable = 0;
          snd_StopSoundSystem();
          // TODO ShutdownFilingSystem();
        } break;
        default: {
          printf("Unhandled RPC Player command %d\n", (int)cmd->command);
          ASSERT(false);
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
      switch (cmd->command) {
        case SoundCommand::LOAD_BANK: {
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
        case SoundCommand::UNLOAD_BANK: {
          SoundBank* bank = LookupBank(cmd->load_bank.bank_name);
          if (bank != nullptr) {
            s32 id = bank->bank_handle;
            bank->bank_handle = 0;
            snd_UnloadBank(id);
            snd_ResolveBankXREFS();
          }
        } break;
        case SoundCommand::GET_IRX_VERSION: {
          cmd->irx_version.major = IRX_VERSION_MAJOR;
          cmd->irx_version.minor = IRX_VERSION_MINOR;
          gInfoEE = cmd->irx_version.ee_addr;
          return cmd;
        } break;
        case SoundCommand::RELOAD_INFO: {
          ReloadBankInfo();
        } break;
        case SoundCommand::SET_LANGUAGE: {
          gLanguage = languages[cmd->set_language.langauge_id];
          printf("IOP language: %s\n", gLanguage);  // added.
        } break;
        case SoundCommand::LOAD_MUSIC: {
          // while (WaitSema(gSema))
          //   ;
          if (gMusic) {
            gMusicFadeDir = -1;
            while (gMusicFade) {
              DelayThread(1000);
            }
            snd_UnloadBank(gMusic);
            snd_ResolveBankXREFS();
            gMusic = 0;
          }
          LoadMusic(cmd->load_bank.bank_name, &gMusic);
          //  SignalSema(gSema);
        } break;
        case SoundCommand::LIST_SOUNDS: {
          PrintActiveSounds();
        } break;
        case SoundCommand::UNLOAD_MUSIC: {
          // while (WaitSema(gSema))
          //   ;
          if (gMusic) {
            gMusicFadeDir = -1;
            while (gMusicFade) {
              DelayThread(1000);
            }
            snd_UnloadBank(gMusic);
            snd_ResolveBankXREFS();
            gMusic = 0;
          }
          // SignalSema(gSema);
        } break;
        default:
          printf("Unhandled RPC Loader command %d\n", (int)cmd->command);
          ASSERT(false);
      }
      n_messages--;
      cmd++;
    }
  }
  return nullptr;
}

s32 VBlank_Handler() {
  return 1;
}
