#pragma once

/*!
 * @file kmachine.h
 * GOAL Machine.  Contains low-level hardware interfaces for GOAL.
 */

#ifndef RUNTIME_KMACHINE_H
#define RUNTIME_KMACHINE_H

#include "common/common_types.h"
#include "Ptr.h"

//! How much space to leave for the stack when creating the debug heap
constexpr u32 DEBUG_HEAP_SPACE_FOR_STACK = 0x4000;

//! First free address for the GOAL heap
constexpr u32 HEAP_START = 0x13fd20;

//! Where to end the global heap so it doesn't overlap with the stack.
constexpr u32 GLOBAL_HEAP_END = 0x1ffc000;

//! Location of kglobalheap, kdebugheap kheapinfo structures.
constexpr u32 GLOBAL_HEAP_INFO_ADDR = 0x13AD00;
constexpr u32 DEBUG_HEAP_INFO_ADDR = 0x13AD10;

//! Where to place the debug heap
constexpr u32 DEBUG_HEAP_START = 0x5000000;

/*!
 * Where does OVERLORD load its data from?
 */
enum OverlordDataSource : u32 {
  fakeiso = 0,  //! some sort of development way of getting data
  deviso = 1,   //! some sort of development way of getting data
  iso_cd = 2,   //! use the actual DVD drive
};

extern OverlordDataSource isodrv;

// Get IOP modules from DVD or from dsefilesv
extern u32 modsrc;

// Reboot IOP on start?
extern u32 reboot;

/*!
 * Initialize globals for kmachine.
 * This should be called before running main.
 */
void kmachine_init_globals();

/*!
 * Initialize global variables based on command line parameters
 */
void InitParms(int argc, const char* const* argv);

/*!
 * Initialize the CD Drive
 */
void InitCD();

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
struct CpadInfo {
  u8 valid;
  u8 status;
  s16 button0;
  u8 rx;
  u8 ry;
  u8 lx;
  u8 ly;
  u8 abutton[12];
  u8 dummy[12];
  s32 number;
  s32 cpad_file;
  u8 _pad0[36];
  s32 new_pad;
  s32 state;
};

struct FileStream {
  u32 flags;
  u32 mode;  // basic
  u32 name;  // basic
  s32 file;  // int32
};

// static_assert(offsetof(CpadInfo, new_pad) == 76, "cpad type offset");

#endif  // RUNTIME_KMACHINE_H
