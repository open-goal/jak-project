#pragma once

/*!
 * @file kboot.h
 * GOAL Boot.  Contains the "main" function to launch GOAL runtime.
 */

#ifndef RUNTIME_KBOOT_H
#define RUNTIME_KBOOT_H

#include "common/common_types.h"

//! Supported languages.
enum class Language {
  English = 0,
  French = 1,
  German = 2,
  Spanish = 3,
  Italian = 4,
  Japanese = 5,
  UK_English = 6,
  // uk english?
};

struct MasterConfig {
  u16 language;          //! GOAL language 0
  u16 aspect;            //! SCE_ASPECT    2
  u16 disable_game;      //                4
  u16 inactive_timeout;  // todo           6
  u16 timeout;           // todo           8
  u16 volume;            // todo           12
};

// Level to load on boot
extern char DebugBootLevel[64];

// Pass to GOAL kernel on boot
extern char DebugBootMessage[64];

// Set to 1 to kill GOAL kernel
extern u32 MasterExit;

// Set to 1 to enable debug heap
extern u32 MasterDebug;

// Set to 1 to load debug code
extern u32 DebugSegment;

// Set to 1 to load game engine after boot automatically
extern u32 DiskBoot;

extern MasterConfig masterConfig;

/*!
 * Initialize global variables for kboot
 */
void kboot_init_globals();

/*!
 * Launch the GOAL Kernel (EE).
 * See InitParms for launch argument details.
 * @param argc : argument count
 * @param argv : argument list
 * @return 0 on success, otherwise failure.
 */
s32 goal_main(int argc, const char* const* argv);

/*!
 * Run the GOAL Kernel.
 */
void KernelCheckAndDispatch();

/*!
 * Stop running the GOAL Kernel.
 */
void KernelShutdown();

extern u32 MasterUseKernel;

#endif  // RUNTIME_KBOOT_H
