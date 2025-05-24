#include "sdshim.h"

#include <cstring>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/sound/common/voice.h"

#include "fmt/core.h"

std::shared_ptr<snd::Voice> voices[kNVoices];
u8 spu_memory[0x15160 * 10];

static sceSdTransIntrHandler trans_handler[2] = {nullptr, nullptr};
static void* userdata[2] = {nullptr, nullptr};

u32 sceSdGetSwitch(u32 entry) {
  // we can ignore this, only used for getting vmix
  return 0;
}

snd::Voice* voice_from_entry(u32 entry) {
  u32 it = entry % kNVoices;
  return voices[it].get();
}

u32 sceSdGetAddr(u32 entry) {
  [[maybe_unused]] u32 core = entry & 1;
  [[maybe_unused]] u32 voice_id = (entry >> 1) & 0x1f;

  auto* voice = voice_from_entry(voice_id);
  if (!voice) {
    return 0;
  }
  // u32 core = entry & 1;
  // u32 voice->id = (entry >> 1) & 0x1f;
  // u32 reg = entry & ~0x3f;

  // Only ever used for getting NAX
  return voice->GetNax() << 1;
}

void sceSdSetSwitch(u32 entry, u32 value) {
  u32 reg = entry & ~0x3f;
  u8 voice = 0;
  while (value) {
    u8 bit = value & 1;

    if (bit) {
      switch (reg) {
        case SD_S_KON:
          voice_from_entry(voice)->KeyOn();
          break;
        case SD_S_KOFF:
          voice_from_entry(voice)->KeyOff();
          break;
      }
    }

    voice++;
    value >>= 1;
  }
}

void sceSdSetAddr(u32 entry, u32 value) {
  [[maybe_unused]] u32 core = entry & 1;
  [[maybe_unused]] u32 voice_id = (entry >> 1) & 0x1f;
  auto* voice = voice_from_entry(voice_id);
  if (!voice) {
    return;
  }
  u32 reg = entry & ~0x3f;

  switch (reg) {
    case SD_VA_SSA: {
      voice->SetSsa(value >> 1);
    } break;
    case SD_VA_LSAX: {
      voice->SetLsa(value >> 1);
    } break;
    default:
      printf("unknown 0x%x\n", reg);
      ASSERT_NOT_REACHED();
      break;
  }
}

void sceSdSetParam(u32 entry, u32 value) {
  [[maybe_unused]] u32 core = entry & 1;
  [[maybe_unused]] u32 voice_id = (entry >> 1) & 0x1f;

  auto* voice = voice_from_entry(voice_id);
  if (!voice) {
    return;
  }
  u32 reg = entry & ~0x3f;

  switch (reg) {
    case SD_VP_VOLL: {
      voice->SetVolumeL(value);
    } break;
    case SD_VP_VOLR: {
      voice->SetVolumeR(value);
    } break;
    case SD_VP_PITCH: {
      voice->SetPitch(value);
    } break;
    case SD_VP_ADSR1: {
      voice->SetAsdr1(value);
    } break;
    case SD_VP_ADSR2: {
      voice->SetAsdr2(value);
    } break;
    default: {
    } break;
  }
}

void sceSdSetTransIntrHandler(s32 channel, sceSdTransIntrHandler handler, void* data) {
  trans_handler[channel] = handler;
  userdata[channel] = data;
}

u32 sceSdVoiceTrans(s32 channel, s32 mode, const void* iop_addr, u32 spu_addr, u32 size) {
  memcpy(&spu_memory[spu_addr], iop_addr, size);
  if (trans_handler[channel] != nullptr) {
    trans_handler[channel](channel, userdata[channel]);
  }
  return size;
}
