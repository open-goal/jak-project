#pragma once

#include "common/common_types.h"

//! Mirror of cpad-info
#pragma pack(push, 1)
struct CPadInfo {
  u8 valid;
  u8 status;
  u16 button0;
  u8 rightx;
  u8 righty;
  u8 leftx;
  u8 lefty;
  u8 abutton[12];
  u8 dummy[12];
  s32 number;
  s32 cpad_file;
  u32 button0_abs[3];
  u32 button0_shadow_abs[1];
  u32 button0_rel[3];
  float stick0_dir;
  float stick0_speed;
  s32 new_pad;
  s32 state;
  u8 align[6];
  u8 direct[6];
  u8 buzz_val[2];
  u8 buzz_pause_val[1];
  u8 buzz_pause_time;
  u64 buzz_time[2];
  u32 buzz;
  s32 buzz_act;
  s64 change_time;  // actually u64 in goal!
};
// static_assert(offsetof(CPadInfo, number) == 32, "CPadInfo number field is wrong");
static_assert(sizeof(CPadInfo) == 132, "CPadInfo size is wrong");
#pragma pack(pop)

struct FileStream {
  u32 flags;
  u32 mode;  // basic
  u32 name;  // basic
  s32 file;  // int32
};
