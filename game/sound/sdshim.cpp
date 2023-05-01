#include "sdshim.h"

#include <cstring>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/sound/common/voice.h"

#include "third-party/fmt/core.h"

std::shared_ptr<snd::voice> voices[4];
u8 spu_memory[0x15160 * 10];

static sceSdTransIntrHandler trans_handler[2] = {nullptr, nullptr};
static void* userdata[2] = {nullptr, nullptr};

u32 sceSdGetSwitch(u32 entry) {
  // we can ignore this, only used for getting vmix
  return 0;
}

snd::voice* voice_from_entry(u32 entry) {
  u32 it = entry & 3;
  return voices[it].get();
}

u32 sceSdGetAddr(u32 entry) {
  auto* voice = voice_from_entry(entry);
  if (!voice) {
    return 0;
  }
  // u32 core = entry & 1;
  // u32 voice->id = (entry >> 1) & 0x1f;
  // u32 reg = entry & ~0x3f;

  // Only ever used for getting NAX
  return voice->get_nax() << 1;
}

void sceSdSetSwitch(u32 entry, u32 /*value*/) {
  // we can ignore this, only used for vmix
  u32 reg = entry & ~0x3f;
  switch (reg) {
    case 0x1500:
      voice_from_entry(entry)->key_on();
      voice_from_entry(entry + 1)->key_on();
      break;
    case 0x1600:
      voice_from_entry(entry)->key_off();
      break;
  }
}

void sceSdkey_on_jak2_voice(int id) {
  voice_from_entry(id)->key_on();
}

void sceSdkey_off_jak2_voice(int id) {
  voice_from_entry(id)->key_off();
}

void sceSdSetAddr(u32 entry, u32 value) {
  auto* voice = voice_from_entry(entry);
  if (!voice) {
    return;
  }
  [[maybe_unused]] u32 core = entry & 1;
  [[maybe_unused]] u32 voice_id = (entry >> 1) & 0x1f;
  u32 reg = entry & ~0x3f;

  switch (reg) {
    case SD_VA_SSA: {
      voice->set_ssa(value >> 1);
    } break;
    case SD_VA_LSAX: {
      voice->set_lsa(value >> 1);
    } break;
    default:
      printf("unknown 0x%x\n", reg);
      ASSERT_NOT_REACHED();
      break;
  }
}

void sceSdSetParam(u32 entry, u32 value) {
  auto* voice = voice_from_entry(entry);
  if (!voice) {
    return;
  }
  [[maybe_unused]] u32 core = entry & 1;
  [[maybe_unused]] u32 voice_id = (entry >> 1) & 0x1f;
  u32 reg = entry & ~0x3f;

  switch (reg) {
    case SD_VP_VOLL: {
      voice->set_volume_l(value);
    } break;
    case SD_VP_VOLR: {
      voice->set_volume_r(value);
    } break;
    case SD_VP_PITCH: {
      voice->set_pitch(value);
    } break;
    case SD_VP_ADSR1: {
      voice->set_asdr1(value);
    } break;
    case SD_VP_ADSR2: {
      voice->set_asdr2(value);
    } break;
    default: {
    } break;
  }
}

void sceSdSetTransIntrHandler(s32 channel, sceSdTransIntrHandler handler, void* data) {
  trans_handler[channel] = handler;
  userdata[channel] = data;
}

u32 sceSdVoiceTrans(s32 channel, s32 mode, void* iop_addr, u32 spu_addr, u32 size) {
  memcpy(&spu_memory[spu_addr], iop_addr, size);
  if (trans_handler[channel] != nullptr) {
    trans_handler[channel](channel, userdata);
  }
  return size;
}
