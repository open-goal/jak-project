#include "kmachine.h"

#include <random>

#include "common/global_profiler/GlobalProfiler.h"
#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/string_util.h"

#include "game/graphics/gfx.h"
#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kernel_types.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/mips2c/mips2c_table.h"
#include "game/sce/libcdvd_ee.h"
#include "game/sce/libpad.h"
#include "game/sce/libscf.h"
#include "game/sce/sif_ee.h"

/*!
 * Where does OVERLORD load its data from?
 */
OverlordDataSource isodrv;

// Get IOP modules from DVD or from dsefilesv
u32 modsrc;

// Reboot IOP with IOP kernel from DVD/CD on boot
u32 reboot_iop;

const char* init_types[] = {"fakeiso", "deviso", "iso_cd"};
u8 pad_dma_buf[2 * SCE_PAD_DMA_BUFFER_SIZE];

// added
u32 vif1_interrupt_handler = 0;
u32 vblank_interrupt_handler = 0;

Timer ee_clock_timer;

void kmachine_init_globals_common() {
  memset(pad_dma_buf, 0, sizeof(pad_dma_buf));
  isodrv = fakeiso;  // changed. fakeiso is the only one that works in opengoal.
  modsrc = 1;
  reboot_iop = 1;
  vif1_interrupt_handler = 0;
  vblank_interrupt_handler = 0;
  ee_clock_timer = Timer();
}

/*!
 * Initialize the CD Drive
 * DONE, EXACT
 */
void InitCD() {
  lg::info("Initializing CD drive. This may take a while...");
  ee::sceCdInit(SCECdINIT);
  ee::sceCdMmode(SCECdDVD);
  while (ee::sceCdDiskReady(0) == SCECdNotReady) {
    lg::debug("Drive not ready... insert a disk!");
  }
  lg::debug("Disk type {}\n", ee::sceCdGetDiskType());
}

/*!
 * Initialize the GS and display the splash screen.
 * Not yet implemented. TODO
 */
void InitVideo() {}

/*!
 * Flush caches.  Does all the memory, regardless of what you specify
 */
void CacheFlush(void* mem, int size) {
  (void)mem;
  (void)size;
  // FlushCache(0);
  // FlushCache(2);
}

/*!
 * Open a new controller pad.
 * Set the new_pad flag to 1 and state to 0.
 * Prints an error if it fails to open.
 */
u64 CPadOpen(u64 cpad_info, s32 pad_number) {
  auto cpad = Ptr<CPadInfo>(cpad_info).c();
  if (cpad->cpad_file == 0) {
    // not open, so we will open it
    cpad->cpad_file =
        ee::scePadPortOpen(pad_number, 0, pad_dma_buf + pad_number * SCE_PAD_DMA_BUFFER_SIZE);
    if (cpad->cpad_file < 1) {
      MsgErr("dkernel: !open cpad #%d (%d)\n", pad_number, cpad->cpad_file);
    }
    cpad->new_pad = 1;
    cpad->state = 0;
  }
  return cpad_info;
}

/*!
 * Not checked super carefully for jak 2, but looks the same
 */
u64 CPadGetData(u64 cpad_info) {
  using namespace ee;
  auto cpad = Ptr<CPadInfo>(cpad_info).c();
  auto pad_state = scePadGetState(cpad->number, 0);
  if (pad_state == scePadStateDiscon) {
    cpad->state = 0;
  }
  cpad->valid = pad_state | 0x80;
  switch (cpad->state) {
    // case 99: // functional
    default:  // controller is functioning as normal
      if (pad_state == scePadStateStable || pad_state == scePadStateFindCTP1) {
        scePadRead(cpad->number, 0, (u8*)cpad);
        // ps2 controllers would send an enabled bit if the button was NOT pressed, but we don't do
        // that here. removed code that flipped the bits.

        if (cpad->change_time) {
          scePadSetActDirect(cpad->number, 0, cpad->direct);
        }
        cpad->valid = pad_state;
      }
      break;
    case 0:  // unavailable
      if (pad_state == scePadStateStable || pad_state == scePadStateFindCTP1) {
        auto pad_mode = scePadInfoMode(cpad->number, 0, InfoModeCurID, 0);
        if (pad_mode != 0) {
          auto vibration_mode = scePadInfoMode(cpad->number, 0, InfoModeCurExID, 0);
          if (vibration_mode > 0) {
            // vibration supported
            pad_mode = vibration_mode;
          }
          if (pad_mode == 4) {
            // controller mode
            cpad->state = 40;
          } else if (pad_mode == 7) {
            // dualshock mode
            cpad->state = 70;
          } else {
            // who knows mode
            cpad->state = 90;
          }
        }
      }
      break;
    case 40:  // controller mode - check for extra modes
      // cpad->change_time = 0;
      cpad->change_time = 0;
      if (scePadInfoMode(cpad->number, 0, InfoModeIdTable, -1) == 0) {
        // no controller modes
        cpad->state = 90;
        return cpad_info;
      }
      cpad->state = 41;
    case 41:  // controller mode - change to dualshock mode!
      // try to enter the 2nd controller mode (dualshock for ds2's)
      if (scePadSetMainMode(cpad->number, 0, 1, 3) == 1) {
        cpad->state = 42;
      }
      break;
    case 42:  // controller mode change check
      if (scePadGetReqState(cpad->number, 0) == scePadReqStateFailed) {
        // failed to change to DS2
        cpad->state = 41;
      }
      if (scePadGetReqState(cpad->number, 0) == scePadReqStateComplete) {
        // change successful. go back to the beginning.
        cpad->state = 0;
      }
      break;
    case 70:  // dualshock mode - check vibration
      // get number of actuators (2 for DS2)
      if (scePadInfoAct(cpad->number, 0, -1, 0) < 1) {
        // no actuators means no vibration. skip to end!
        // cpad->change_time = 0;
        cpad->change_time = 0;
        cpad->state = 99;
      } else {
        // we have actuators to use.
        // cpad->change_time = 1;  // remember to update pad times.
        cpad->change_time = 1;
        cpad->state = 75;
      }
      break;
    case 75:  // set actuator vib param info
      if (scePadSetActAlign(cpad->number, 0, cpad->align) != 0) {
        if (scePadInfoPressMode(cpad->number, 0) == 1) {
          // pressure buttons supported
          cpad->state = 76;
        } else {
          // no pressure buttons, done with controller setup
          cpad->state = 99;
        }
      }
      break;
    case 76:  // enter pressure mode
      if (scePadEnterPressMode(cpad->number, 0) == 1) {
        cpad->state = 78;
      }
      break;
    case 78:  // pressure mode request check
      if (scePadGetReqState(cpad->number, 0) == scePadReqStateFailed) {
        cpad->state = 76;
      }
      if (scePadGetReqState(cpad->number, 0) == scePadReqStateComplete) {
        cpad->state = 99;
      }
      break;
    case 90:
      break;  // unsupported controller. too bad!
  }
  return cpad_info;
}

// should make sure this works the same way in jak 2
void InstallHandler(u32 handler_idx, u32 handler_func) {
  switch (handler_idx) {
    case 3:
      vblank_interrupt_handler = handler_func;
      break;
    case 5:
      vif1_interrupt_handler = handler_func;
      break;
    default:
      printf("unknown handler: %d\n", handler_idx);
      ASSERT(false);
  }
}

// nothing used this in jak1, hopefully same for 2
void InstallDebugHandler() {
  ASSERT(false);
}

/*!
 * Get length of a file.
 */
s32 klength(u64 fs) {
  auto file_stream = Ptr<FileStream>(fs).c();
  if ((file_stream->flags ^ 1) & 1) {
    // first flag bit not set. This means no errors
    auto end_seek = ee::sceLseek(file_stream->file, 0, SCE_SEEK_END);
    auto reset_seek = ee::sceLseek(file_stream->file, 0, SCE_SEEK_SET);
    if (reset_seek < 0 || end_seek < 0) {
      // seeking failed, flag it
      file_stream->flags |= 1;
    }
    return end_seek;
  } else {
    return 0;
  }
}

/*!
 * Seek a file stream.
 */
s32 kseek(u64 fs, s32 offset, s32 where) {
  s32 result = -1;
  auto file_stream = Ptr<FileStream>(fs).c();
  if ((file_stream->flags ^ 1) & 1) {
    result = ee::sceLseek(file_stream->file, offset, where);
    if (result < 0) {
      file_stream->flags |= 1;
    }
  }
  return result;
}

/*!
 * Read from a file stream.
 */
s32 kread(u64 fs, u64 buffer, s32 size) {
  s32 result = -1;
  auto file_stream = Ptr<FileStream>(fs).c();
  if ((file_stream->flags ^ 1) & 1) {
    result = ee::sceRead(file_stream->file, Ptr<u8>(buffer).c(), size);
    if (result < 0) {
      file_stream->flags |= 1;
    }
  }
  return result;
}

/*!
 * Write to a file stream.
 */
s32 kwrite(u64 fs, u64 buffer, s32 size) {
  s32 result = -1;
  auto file_stream = Ptr<FileStream>(fs).c();
  if ((file_stream->flags ^ 1) & 1) {
    result = ee::sceWrite(file_stream->file, Ptr<u8>(buffer).c(), size);
    if (result < 0) {
      file_stream->flags |= 1;
    }
  }
  return result;
}

/*!
 * Close a file stream.
 */
u64 kclose(u64 fs) {
  auto file_stream = Ptr<FileStream>(fs).c();
  if ((file_stream->flags ^ 1) & 1) {
    ee::sceClose(file_stream->file);
    file_stream->file = -1;
  }
  file_stream->flags = 0;
  return fs;
}

// TODO dma_to_iop
void dma_to_iop() {
  ASSERT(false);
}

u64 DecodeLanguage() {
  return masterConfig.language;
}

u64 DecodeAspect() {
  return masterConfig.aspect;
}

u64 DecodeVolume() {
  return masterConfig.volume;
}

// NOTE: this is originally hardcoded, and returns different values depending on the disc region.
// it returns 0 for NTSC-U, 1 for PAL and 2 for NTSC-J
u64 DecodeTerritory() {
  return GAME_TERRITORY_SCEA;
}

u64 DecodeTimeout() {
  return masterConfig.timeout;
}

u64 DecodeInactiveTimeout() {
  return masterConfig.inactive_timeout;
}

void DecodeTime(u32 ptr) {
  Ptr<ee::sceCdCLOCK> clock(ptr);
  // in jak2, if this fails, they do a sceScfGetLocalTimefromRTC
  sceCdReadClock(clock.c());
}

/*!
 * PC PORT FUNCTIONS BEGIN
 */
/*!
 * Get a 300MHz timer value.
 * Called from EE thread
 */
u64 read_ee_timer() {
  u64 ns = ee_clock_timer.getNs();
  return (ns * 3) / 10;
}

/*!
 * Do a fast memory copy.
 */
void c_memmove(u32 dst, u32 src, u32 size) {
  memmove(Ptr<u8>(dst).c(), Ptr<u8>(src).c(), size);
}

void set_game_resolution(s64 w, s64 h) {
  Gfx::set_game_resolution(w, h);
}

void set_msaa(s64 samples) {
  Gfx::set_msaa(samples);
}

/*!
 * Returns size of window. Called from game thread
 */
void get_window_size(u32 w_ptr, u32 h_ptr) {
  if (w_ptr) {
    auto w = Ptr<u32>(w_ptr).c();
    *w = Gfx::get_window_width();
  }
  if (h_ptr) {
    auto h = Ptr<u32>(h_ptr).c();
    *h = Gfx::get_window_height();
  }
}

/*!
 * Returns scale of window. This is for DPI stuff.
 */
void get_window_scale(u32 x_ptr, u32 y_ptr) {
  float* x = x_ptr ? Ptr<float>(x_ptr).c() : NULL;
  float* y = y_ptr ? Ptr<float>(y_ptr).c() : NULL;
  Gfx::get_window_scale(x, y);
}

/*!
 * Returns resolution of the monitor.
 */
void get_screen_size(s64 vmode_idx, u32 w_ptr, u32 h_ptr) {
  s32 *w_out = 0, *h_out = 0;
  if (w_ptr) {
    w_out = Ptr<s32>(w_ptr).c();
  }
  if (h_ptr) {
    h_out = Ptr<s32>(h_ptr).c();
  }
  Gfx::get_screen_size(vmode_idx, w_out, h_out);
}

/*!
 * Returns refresh rate of the monitor.
 */
s64 get_screen_rate(s64 vmode_idx) {
  return Gfx::get_screen_rate(vmode_idx);
}

/*!
 * Returns amount of video modes of the monitor.
 */
s64 get_screen_vmode_count() {
  return Gfx::get_screen_vmode_count();
}

/*!
 * Returns the number of available monitors.
 */
int get_monitor_count() {
  return Gfx::get_monitor_count();
}

int get_unix_timestamp() {
  return std::time(nullptr);
}

void mkdir_path(u32 filepath) {
  auto filepath_str = std::string(Ptr<String>(filepath).c()->data());
  file_util::create_dir_if_needed_for_file(filepath_str);
}

u64 filepath_exists(u32 filepath) {
  auto filepath_str = std::string(Ptr<String>(filepath).c()->data());
  if (fs::exists(filepath_str)) {
    return s7.offset + true_symbol_offset(g_game_version);
  }
  return s7.offset;
}

void prof_event(u32 name, u32 kind) {
  prof().event(Ptr<String>(name).c()->data(), (ProfNode::Kind)kind);
}

void set_frame_rate(s64 rate) {
  Gfx::set_frame_rate(rate);
}

void set_vsync(u32 symptr) {
  Gfx::set_vsync(symptr != s7.offset);
}

void set_window_lock(u32 symptr) {
  Gfx::set_window_lock(symptr == s7.offset);
}

void set_collision(u32 symptr) {
  Gfx::g_global_settings.collision_enable = symptr != s7.offset;
}

void set_collision_wireframe(u32 symptr) {
  Gfx::g_global_settings.collision_wireframe = symptr != s7.offset;
}

void set_collision_mask(GfxGlobalSettings::CollisionRendererMode mode, int mask, u32 symptr) {
  if (symptr != s7.offset) {
    Gfx::CollisionRendererSetMask(mode, mask);
  } else {
    Gfx::CollisionRendererClearMask(mode, mask);
  }
}

u32 get_collision_mask(GfxGlobalSettings::CollisionRendererMode mode, int mask) {
  return Gfx::CollisionRendererGetMask(mode, mask) ? s7.offset + true_symbol_offset(g_game_version)
                                                   : s7.offset;
}

void set_gfx_hack(u64 which, u32 symptr) {
  switch (which) {
    case 0:  // no tex
      Gfx::g_global_settings.hack_no_tex = symptr != s7.offset;
      break;
  }
}

/*!
 * PC PORT FUNCTIONS END
 */

void vif_interrupt_callback(int bucket_id) {
  // added for the PC port for faking VIF interrupts from the graphics system.
  if (vif1_interrupt_handler && MasterExit == RuntimeExitStatus::RUNNING) {
    call_goal(Ptr<Function>(vif1_interrupt_handler), bucket_id, 0, 0, s7.offset, g_ee_main_mem);
  }
}

/*!
 * Added in PC port.
 */
u32 offset_of_s7() {
  return s7.offset;
}

/*!
 * Called from the game thread at initialization.
 * The game thread is the only one to touch the mips2c function table (through the linker and
 * through this function), so no locking is needed.
 */
u64 pc_get_mips2c(u32 name) {
  const char* n = Ptr<String>(name).c()->data();
  return Mips2C::gLinkedFunctionTable.get(n);
}

/*!
 * Called from game thread to submit rendering DMA chain.
 */
void send_gfx_dma_chain(u32 /*bank*/, u32 chain) {
  Gfx::send_chain(g_ee_main_mem, chain);
}

/*!
 * Called from game thread to upload a texture outside of the main DMA chain.
 */
void pc_texture_upload_now(u32 page, u32 mode) {
  Gfx::texture_upload_now(Ptr<u8>(page).c(), mode, s7.offset);
}

void pc_texture_relocate(u32 dst, u32 src, u32 format) {
  Gfx::texture_relocate(dst, src, format);
}

u64 pc_filter_debug_string(u32 str_ptr, u32 dist_ptr) {
  auto str = std::string(Ptr<String>(str_ptr).c()->data());
  float dist;
  memcpy(&dist, &dist_ptr, 4);

  // Check distance first
  if (Gfx::g_debug_settings.debug_text_check_range) {
    if (dist / 4096.F > Gfx::g_debug_settings.debug_text_max_range) {
      return s7.offset + true_symbol_offset(g_game_version);
    }
  }

  // Get the current filters
  const auto& filters = Gfx::g_debug_settings.debug_text_filters;
  if (filters.empty()) {
    // there are no filters, exit early
    return s7.offset;
  }

  // Currently very dumb contains check
  for (const auto& filter : filters) {
    if (filter.type == DebugTextFilter::Type::CONTAINS) {
      if (!str.empty() && !filter.content.empty() && !str_util::contains(str, filter.content)) {
        return s7.offset + true_symbol_offset(g_game_version);
      }
    } else if (filter.type == DebugTextFilter::Type::NOT_CONTAINS) {
      if (!str.empty() && !filter.content.empty() && str_util::contains(str, filter.content)) {
        return s7.offset + true_symbol_offset(g_game_version);
      }
    } else if (filter.type == DebugTextFilter::Type::REGEX) {
      if (str_util::valid_regex(filter.content) &&
          std::regex_match(str, std::regex(filter.content))) {
        return s7.offset + true_symbol_offset(g_game_version);
      }
    }
  }
  return s7.offset;
}

std::mt19937 extra_random_generator;
u32 pc_rand() {
  return (u32)extra_random_generator();
}
