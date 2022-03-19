#include <cstring>
#include <cstdio>
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
  int n_messages = size / SRPC_MESSAGE_SIZE;
  SoundRpcCommand* cmd = (SoundRpcCommand*)(data);
  if (gSoundEnable) {
    while (n_messages > 0) {
      switch (cmd->command) {
        case SoundCommand::PLAY: {
          printf("Ignoring Play Sound command for sound %.16s\n", cmd->play.name);
        } break;
        case SoundCommand::PAUSE_SOUND: {
          printf("Ignoring Pause Sound command\n");
        } break;
        case SoundCommand::STOP_SOUND: {
          printf("Ignoring Stop Sound command\n");
        } break;
        case SoundCommand::CONTINUE_SOUND: {
          printf("Ignoring Continue Sound command\n");
        } break;
        case SoundCommand::SET_PARAM: {
          printf("Ignoring Set param command\n");
        } break;
        case SoundCommand::SET_MASTER_VOLUME: {
          printf("Ignoring Set master volume command\n");
        } break;
        case SoundCommand::PAUSE_GROUP: {
          printf("Ignoring Pause Group comman\n");
        } break;
        case SoundCommand::STOP_GROUP: {
          u8 group = cmd->group.group;
          KillSoundsInGroup(group);
          if ((group & 4) != 0) {
            // TODO StopVAGStream(0,0);
          }
        } break;
        case SoundCommand::CONTINUE_GROUP: {
          printf("Ignoring Continue Group command\n");
        } break;
        case SoundCommand::SET_FALLOFF_CURVE: {
          SetCurve(cmd->fallof_curve.curve, cmd->fallof_curve.falloff, cmd->fallof_curve.ease);
        } break;
        case SoundCommand::SET_SOUND_FALLOFF: {
          printf("Ignoring Set Sound Falloff command\n");
        } break;
        case SoundCommand::SET_FLAVA: {
          gFlava = cmd->flava.flava;
        } break;
        case SoundCommand::SET_EAR_TRANS: {
          SetEarTrans(&cmd->ear_trans.ear_trans, &cmd->ear_trans.cam_trans,
                      cmd->ear_trans.cam_angle);
        } break;
        case SoundCommand::SHUTDOWN: {
          printf("Ignoring Shutdown command\n");
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
        case SoundCommand::UNLOAD_BANK:
          printf("ignoring unload bank command!\n");
          break;
        case SoundCommand::GET_IRX_VERSION: {
          cmd->irx_version.major = IRX_VERSION_MAJOR;
          cmd->irx_version.minor = IRX_VERSION_MINOR;
          gInfoEE = cmd->irx_version.ee_addr;
          return cmd;
        } break;
        case SoundCommand::SET_LANGUAGE: {
          gLanguage = languages[cmd->set_language.langauge_id];
          printf("IOP language: %s\n", gLanguage);  // added.
          break;
        }
        case SoundCommand::LOAD_MUSIC:
          // s32 res;
          // do {
          //   res = WaitSema(gSema);
          // } while (res != 0);
          // if (gMusic != 0) {
          // }
          // SignalSema(gSema);
          break;
        case SoundCommand::UNLOAD_MUSIC:
          printf("ignoring unload music\n");
          break;
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
