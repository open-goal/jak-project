#pragma once

#include "common/common_types.h"

#include "game/graphics/gfx.h"

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

extern const char* init_types[];

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
u64 read_ee_timer();
void c_memmove(u32 dst, u32 src, u32 size);
void get_window_size(u32 w_ptr, u32 h_ptr);
void get_window_scale(u32 x_ptr, u32 y_ptr);
void get_screen_size(s64 vmode_idx, u32 w_ptr, u32 h_ptr);
s64 get_screen_rate(s64 vmode_idx);
s64 get_screen_vmode_count();
int get_monitor_count();
void mkdir_path(u32 filepath);
u64 filepath_exists(u32 filepath);
void prof_event(u32 name, u32 kind);
void set_frame_rate(s64 rate);
void set_vsync(u32 symptr);
void set_window_lock(u32 symptr);
void set_collision(u32 symptr);
void set_collision_wireframe(u32 symptr);
void set_collision_mask(GfxGlobalSettings::CollisionRendererMode mode, int mask, u32 symptr);
u32 get_collision_mask(GfxGlobalSettings::CollisionRendererMode mode, int mask);
void set_gfx_hack(u64 which, u32 symptr);
u32 offset_of_s7();
void vif_interrupt_callback();
