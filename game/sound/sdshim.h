#pragma once
#include <memory>

#include "common/common_types.h"

#include "game/sound/common/voice.h"

#define SD_VA_SSA ((0x20 << 8) + (0x01 << 6))
#define SD_VA_LSAX ((0x21 << 8) + (0x01 << 6))
#define SD_S_VMIXL (0x18 << 8)
#define SD_S_VMIXR (0x1a << 8)
#define SD_VP_VOLL (0x00 << 8)
#define SD_VP_VOLR (0x01 << 8)
#define SD_VP_PITCH (0x02 << 8)
#define SD_VP_ADSR1 (0x03 << 8)
#define SD_VP_ADSR2 (0x04 << 8)
#define SD_VA_NAX ((0x22 << 8) + (0x01 << 6))

extern std::shared_ptr<snd::voice> voices[4];
extern u8 spu_memory[0x15160 * 10];

using sceSdTransIntrHandler = int (*)(int, void*);

u32 sceSdGetSwitch(u32 entry);
u32 sceSdGetAddr(u32 entry);
void sceSdSetSwitch(u32 entry, u32 value);
void sceSdSetAddr(u32 entry, u32 value);
void sceSdSetParam(u32 entry, u32 value);
void sceSdSetTransIntrHandler(s32 channel, sceSdTransIntrHandler, void* data);
u32 sceSdVoiceTrans(s32 channel, s32 mode, void* iop_addr, u32 spu_addr, u32 size);
void sceSdkey_on_jak2_voice(int id);
void sceSdkey_off_jak2_voice(int id);
