#pragma once

/*!
 * @file kmachine.h
 * GOAL Machine.  Contains low-level hardware interfaces for GOAL.
 */

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"





// Discord RPC
struct DiscordRichPresence;
extern int gDiscordRpcEnabled;
extern int64_t gStartTime;

/*!
 * Initialize globals for kmachine.
 * This should be called before running main.
 */
void kmachine_init_globals();




/*!
 * Initialize the I/O Processor
 */
void InitIOP();

/*!
 * Initialize the GS and display the splash screen.
 */
void InitVideo();

/*!
 * Initialze GOAL Runtime
 */
int InitMachine();

/*!
 * Shutdown GOAL runtime.
 */
int ShutdownMachine();

/*!
 * Flush caches.  Does all the memory, regardless of what you specify
 */
void CacheFlush(void* mem, int size);

void InitMachineScheme();

//! Mirror of cpad-info
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
  u8 __pad[2];
  u64 buzz_time[2];
  u32 buzz;
  s32 buzz_act;
  s32 change_time;  // actually u64 in goal!
};

struct FileStream {
  u32 flags;
  u32 mode;  // basic
  u32 name;  // basic
  s32 file;  // int32
};

struct DiscordInfo {
  u32 fuel;
  u32 money_total;
  u32 buzzer_total;
  u32 deaths;
  u32 status;
  u32 level;
  u32 cutscene;   // check if cutscene is playing
  u32 ogreboss;   // are we fighting ogreboss?
  u32 plantboss;  // are we fighting plant-boss?
  u32 racer;      // are we driving the zoomer?
  u32 flutflut;   // are we riding on flut flut?
  u32 time_of_day;
};

// static_assert(offsetof(CpadInfo, new_pad) == 76, "cpad type offset");

void vif_interrupt_callback();
u32 offset_of_s7();
