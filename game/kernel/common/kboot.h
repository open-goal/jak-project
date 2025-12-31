#pragma once
#include "common/common_types.h"

#define GAME_TERRITORY_SCEA 0  // sony america
#define GAME_TERRITORY_SCEE 1  // sony europe
#define GAME_TERRITORY_SCEI 2  // sony inc. (japan)
#define GAME_TERRITORY_SCEK 3  // sony korea

enum class RuntimeExitStatus {
  RUNNING = 0,
  RESTART_RUNTIME = 1,
  EXIT = 2,
  RESTART_IN_DEBUG = 3,
};

enum class VideoMode {
  NTSC = 0,
  PAL = 1,
  FPS100 = 2,
  FPS150 = 3,
};

// Set to nonzero to kill GOAL kernel
extern RuntimeExitStatus MasterExit;

// Set to 1 to load game engine after boot automatically
extern u32 DiskBoot;

// Set to 1 to enable debug heap
extern u32 MasterDebug;

// Set to 1 to load debug code
extern u32 DebugSegment;

// Level to load on boot
extern char DebugBootLevel[64];

// Pass to GOAL kernel on boot
extern char DebugBootMessage[64];

// Added in PC port, option to run listener functions without the kernel for debugging
extern u32 MasterUseKernel;

struct MasterConfig {
  u16 language;          //! GOAL language 0
  u16 aspect;            //! SCE_ASPECT    2
  u16 disable_game;      //                4
  u16 inactive_timeout;  // todo           6
  u16 timeout;           // todo           8
  u16 jak2_only_unused;  // seems unused   12
  u16 volume;            // todo           14 in jak 2, 12 in jak 1

  u16 disable_sound = 0;  // added. disables all sound code.
};

extern MasterConfig masterConfig;

/*!
 * Initialize global variables for kboot
 */
void kboot_init_globals_common();
