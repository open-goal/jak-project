#pragma once

#include "common/common_types.h"

#include "game/graphics/gfx.h"
#include "game/kernel/common/kscheme.h"

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
extern u32 reboot_iop;  // renamed to reboot_iop to avoid conflict

extern const char* init_types[];
extern u32 vblank_interrupt_handler;

void kmachine_init_globals_common();

/*!
 * Initialize the CD Drive
 */
void InitCD();

/*!
 * Initialize the GS and display the splash screen.
 */
void InitVideo();

/*!
 * Flush caches.  Does all the memory, regardless of what you specify
 */
void CacheFlush(void* mem, int size);
u64 CPadOpen(u64 cpad_info, s32 pad_number);
u64 CPadGetData(u64 cpad_info);
void InstallHandler(u32 handler_idx, u32 handler_func);
void InstallDebugHandler();
s32 klength(u64 fs);
s32 kseek(u64 fs, s32 offset, s32 where);
s32 kread(u64 fs, u64 buffer, s32 size);
s32 kwrite(u64 fs, u64 buffer, s32 size);
u64 kclose(u64 fs);
void dma_to_iop();
u64 DecodeLanguage();
u64 DecodeAspect();
u64 DecodeVolume();
u64 DecodeTerritory();
u64 DecodeTimeout();
u64 DecodeInactiveTimeout();
void DecodeTime(u32 ptr);

u32 offset_of_s7();
void vif_interrupt_callback(int bucket_id);

struct BindAssignmentInfo {
  u32 port;
  u32 device_type;
  u32 buttons;
  u32 input_idx;
  u32 analog_min_range;
};

struct InternFromCInfo {
  u32 offset;
  u32 value;
};

// Holds function references to game specific functions for setting up common PC Port functions
// this is needed because the handlers for the functions are stateless
// and using the functions via the handler's capture lists requires templating nonsense
struct CommonPCPortFunctionWrappers {
  std::function<InternFromCInfo(const char*)> intern_from_c;
  std::function<u64(const char*)> make_string_from_c;
};

extern CommonPCPortFunctionWrappers g_pc_port_funcs;

/// Initializes all common PC Port functions for all Jak games
void init_common_pc_port_functions(
    std::function<Ptr<Function>(const char*, void*)> make_func_symbol_func,
    std::function<InternFromCInfo(const char*)> intern_from_c_func,
    std::function<u64(const char*)> make_string_from_c_func);
