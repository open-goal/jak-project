/*!
 * @file kmachine.cpp
 * GOAL Machine.  Contains low-level hardware interfaces for GOAL.
 * Not yet done - some controller stuff isn't implemented, and also many of the SCE functions
 * are just stubs or commented out for now.  Legal splash screen stuff is also missing.
 */

#include <string>
#include <cstring>
#include "kmachine.h"
#include "kboot.h"
#include "kprint.h"
#include "fileio.h"
#include "kmalloc.h"
#include "kdsnetm.h"
#include "ksocket.h"
#include "kscheme.h"
#include "ksound.h"
#include "kdgo.h"
#include "ksound.h"
#include "klink.h"
#include "klisten.h"
#include "game/sce/sif_ee.h"
#include "game/sce/libcdvd_ee.h"
#include "game/sce/stubs.h"
#include "game/sce/libdma.h"
#include "game/sce/libgraph.h"
#include "game/sce/libpad.h"
#include "common/symbols.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "game/graphics/sceGraphicsInterface.h"
#include "game/graphics/gfx.h"
#include "common/dma/dma_chain_read.h"
#include "common/dma/dma_copy.h"
#include "game/mips2c/mips2c_table.h"
#include "game/system/vm/vm.h"
#include "game/system/newpad.h"
#include "game/sce/libscf.h"
#include "common/util/Assert.h"
#include "game/discord.h"

#include "svnrev.h"

using namespace ee;

/*!
 * Where does OVERLORD load its data from?
 */
OverlordDataSource isodrv;

// Get IOP modules from DVD or from dsefilesv
u32 modsrc;

// Reboot IOP with IOP kernel from DVD/CD on boot
u32 reboot;

u8 pad_dma_buf[2 * SCE_PAD_DMA_BUFFER_SIZE];

const char* init_types[] = {"fakeiso", "deviso", "iso_cd"};

Timer ee_clock_timer;

// added
u32 vif1_interrupt_handler = 0;

void kmachine_init_globals() {
  isodrv = iso_cd;
  modsrc = 1;
  reboot = 1;
  memset(pad_dma_buf, 0, sizeof(pad_dma_buf));
  ee_clock_timer = Timer();
  vif1_interrupt_handler = 0;
}

/*!
 * Initialize global variables based on command line parameters. Not called in retail versions,
 * but it is present in the ELF.
 * DONE
 * Modified to use std::string, and removed call to fflush.
 */
void InitParms(int argc, const char* const* argv) {
  // Modified default settings:
  if (argc == 1) {
    DiskBoot = 1;
    isodrv = fakeiso;
    modsrc = 0;
    reboot = 0;
  }

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    // DVD Settings
    // ----------------------------

    // the "cd" mode uses the DVD drive for everything. This is how the game runs in retail
    if (arg == "-cd") {
      Msg(6, "dkernel: cd mode\n");
      isodrv = iso_cd;  // use the actual DVD drive for data files
      modsrc = 1;       // use the DVD drive data for IOP modules
      reboot = 1;       // Reboot the IOP (load new IOP runtime)
    }

    // the "cddata" uses the DVD drive for everything but IOP modules.
    if (arg == "-cddata") {
      Msg(6, "dkernel: cddata mode\n");
      isodrv = iso_cd;  // tell IOP to use actual DVD drive for data files
      modsrc = 0;       // don't use DVD drive for IOP modules
      reboot = 0;       // no need to reboot the IOP
    }

    // the "deviso" mode is one of two modes for testing without the need for DVDs
    if (arg == "-deviso") {
      Msg(6, "dkernel: deviso mode\n");
      isodrv = deviso;  // IOP deviso mode
      modsrc = 0;       // no IOP module loading (there's no DVD to load from!)
      reboot = 0;
    }

    // the "fakeiso" mode is the other of two modes for testing without the need for DVDs
    if (arg == "-fakeiso") {
      Msg(6, "dkernel: fakeiso mode\n");
      isodrv = fakeiso;  // IOP fakeeiso mode
      modsrc = 0;        // no IOP module loading (there's no DVD to load from!)
      reboot = 0;
    }

    // an added mode to allow booting without a KERNEL.CGO for testing
    if (arg == "-nokernel") {
      Msg(6, "dkernel: no kernel mode\n");
      MasterUseKernel = false;
    }

    // GOAL Settings
    // ----------------------------

    // the "demo" mode is used to pass the message "demo" to the gkernel in the DebugBootMessage
    // (instead of play)
    if (arg == "-demo") {
      Msg(6, "dkernel: demo mode\n");
      kstrcpy(DebugBootMessage, "demo");
    }

    // the "boot" mode is used to set GOAL up for running the game in retail mode
    if (arg == "-boot") {
      Msg(6, "dkernel: boot mode\n");
      MasterDebug = 0;
      DiskBoot = 1;
      DebugSegment = 0;
    }

    // the "debug" mode is used to set GOAL up for debugging/development
    if (arg == "-debug") {
      Msg(6, "dkernel: debug mode\n");
      MasterDebug = 1;
      DebugSegment = 1;
    }

    // the "debug-mem" mode is used to set up GOAL in debug mode, but not to load debug-segments
    if (arg == "-debug-mem") {
      Msg(6, "dkernel: debug-mem mode\n");
      MasterDebug = 1;
      DebugSegment = 0;
    }

    // the "-level [level-name]" mode is used to inform the game to boot a specific level
    // the default level is "#f".
    if (arg == "-level") {
      i++;
      std::string levelName = argv[i];
      Msg(6, "dkernel: level %s\n", levelName.c_str());
      kstrcpy(DebugBootLevel, levelName.c_str());
    }
  }
}

/*!
 * Initialize the CD Drive
 * DONE, EXACT
 */
void InitCD() {
  lg::info("Initializing CD drive. This may take a while...");
  sceCdInit(SCECdINIT);
  sceCdMmode(SCECdDVD);
  while (sceCdDiskReady(0) == SCECdNotReady) {
    lg::debug("Drive not ready... insert a disk!");
  }
  lg::debug("Disk type {}\n", sceCdGetDiskType());
}

/*!
 * Initialize the I/O Processor
 * Removed calls to exit(0) if loading modules fails.
 */
void InitIOP() {
  // before doing anything with the I/O Processor, we need to set up SIF RPC
  sceSifInitRpc(0);

  if ((isodrv == iso_cd) || modsrc || reboot) {
    // we will need the DVD drive to bring up the IOP
    InitCD();
  }

  if (!reboot) {
    // reboot with development IOP kernel
    lg::debug("Rebooting IOP...");
    while (!sceSifRebootIop("host0:/usr/local/sce/iop/modules/ioprp221.img")) {
      lg::debug("Failed, retrying");
    }
    while (!sceSifSyncIop()) {
      lg::debug("Syncing...");
    }
  } else {
    // reboot with IOP kernel off of the disk
    // reboot with development IOP kernel
    lg::debug("Rebooting IOP...");
    while (!sceSifRebootIop("cdrom0:\\DRIVERS\\IOPRP221.IMG;1")) {
      lg::debug("Failed, retrying");
    }
    while (!sceSifSyncIop()) {
      lg::debug("Syncing...");
    }
  }

  // now that the IOP is booted with the correct kernel, we need to connect SIF RPC again
  sceSifInitRpc(0);

  // if we plan to get files off of the DVD drive, we get ready to load files again.
  // resetting the file system may not be needed here, but it does not hurt.
  if ((isodrv == iso_cd) || modsrc) {
    InitCD();
    sceFsReset();
  }

  // we begin putting together a boot command for OVERLORD, the IOP driver, which must know the data
  // source and the name of the boot splash screen of the game.
  char overlord_boot_command[256];
  kstrcpy(overlord_boot_command, init_types[(int)isodrv]);
  char* cmd = overlord_boot_command + strlen(overlord_boot_command) + 1;
  kstrcpy(cmd, "SCREEN1.USA");
  auto len = strlen(cmd);

  if (modsrc == fakeiso) {
    // load from network

    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/sio2man.irx", 0, nullptr) < 0) {
      MsgErr("loading sio2man.irx failed\n");
    }

    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/padman.irx", 0, nullptr) < 0) {
      MsgErr("loading padman.irx failed\n");
    }

    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/libsd.irx", 0, nullptr) < 0) {
      MsgErr("loading libsd.irx failed\n");
    }

    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/mcman.irx", 0, nullptr) < 0) {
      MsgErr("loading mcman.irx failed\n");
    }

    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/mcserv.irx", 0, nullptr) < 0) {
      MsgErr("loading mcserv.irx failed\n");
    }

    if (sceSifLoadModule("host0:/usr/home/src/989snd10/iop/989snd.irx", 0, nullptr) < 0) {
      MsgErr("loading 989snd.irx failed\n");
    }

    sceSifLoadModule("host0:/usr/home/src/989snd10/iop/989ERR.IRX", 0, nullptr);

    lg::debug("Initializing CD library...");
    auto rv = sceSifLoadModule("host0:binee/overlord.irx", cmd + len + 1 - overlord_boot_command,
                               overlord_boot_command);
    if (rv < 0) {
      MsgErr("loading overlord.irx failed\n");
    }
  } else {
    // load from DVD drive
    if (sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\SIO2MAN.IRX;1", 0, nullptr) < 0) {
      MsgErr("loading sio2man.irx failed\n");
    }

    if (sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\PADMAN.IRX;1", 0, nullptr) < 0) {
      MsgErr("loading padman.irx failed\n");
    }

    if (sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\LIBSD.IRX;1", 0, nullptr) < 0) {
      MsgErr("loading libsd.irx failed\n");
    }

    if (sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\MCMAN.IRX;1", 0, nullptr) < 0) {
      MsgErr("loading mcman.irx failed\n");
    }

    if (sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\MCSERV.IRX;1", 0, nullptr) < 0) {
      MsgErr("loading mcserv.irx failed\n");
    }

    if (sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\989SND.IRX;1", 0, nullptr) < 0) {
      MsgErr("loading 989snd.irx failed\n");
    }

    lg::debug("Initializing CD library in ISO_CD mode...");
    auto rv = sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\OVERLORD.IRX;1",
                               cmd + len + 1 - overlord_boot_command, overlord_boot_command);
    if (rv < 0) {
      MsgErr("loading overlord.irx failed\n");
    }
  }
  auto rv = sceMcInit();
  if (rv < 0) {
    MsgErr("MC driver init failed %d\n", rv);
  } else {
    lg::info("InitIOP OK");
  }
}

/*!
 * Initialize the GS and display the splash screen.
 * Not yet implemented. TODO
 */
void InitVideo() {}

/*!
 * Initialize GOAL Runtime. This is the main initialization which is called before entering
 * the GOAL kernel dispatch loop (KernelCheckAndDispatch).
 * TODO finish up things which are commented.
 */
int InitMachine() {
  u32 debug_heap_end = (0xffffffff - DEBUG_HEAP_SPACE_FOR_STACK + 1) & 0x7ffffff;

  // initialize the global heap
  u32 global_heap_size = GLOBAL_HEAP_END - HEAP_START;
  float size_mb = ((float)global_heap_size) / (float)(1 << 20);
  lg::info("gkernel: global heap 0x{:08x} to 0x{:08x} (size {:.3f} MB)", HEAP_START,
           GLOBAL_HEAP_END, size_mb);
  kinitheap(kglobalheap, Ptr<u8>(HEAP_START), global_heap_size);

  // initialize the debug heap, if appropriate
  if (MasterDebug) {
    u32 debug_heap_size = debug_heap_end - DEBUG_HEAP_START;
    kinitheap(kdebugheap, Ptr<u8>(DEBUG_HEAP_START), debug_heap_size);
    float debug_size_mb = ((float)debug_heap_size) / (float)(1 << 20);
    float gap_size_mb = ((float)DEBUG_HEAP_START - GLOBAL_HEAP_END) / (float)(1 << 20);
    lg::info("gkernel: debug heap 0x{:08x} to 0x{:08x} (size {:.3f} MB, gap {:.3f} MB)",
             DEBUG_HEAP_START, debug_heap_end, debug_size_mb, gap_size_mb);
  } else {
    // if no debug, we make the kheapinfo structure NULL so GOAL knows not to use it.
    kdebugheap.offset = 0;
  }

  init_output();  // GOAL input/output buffer setup
  InitIOP();      // start IOP/OVERLORD, loading our legal splash screen

  //  sceGsResetPath(); // reset VIF1, VU1, GIF

  InitVideo();  // display legal splash screen

  //  FlushCache(WRITEBACK_DCACHE);
  //  FlushCache(INVALIDATE_ICACHE);
  //  sceGsSyncV(0);   // wait for it to show up on the screen
  //
  //  if(scePadInit(0) != 1) { // init controllers
  //    MsgErr("dkernel: !init pad\n");
  //  }

  if (MasterDebug) {  // connect to GOAL compiler
    InitGoalProto();
  }

  lg::info("InitSound");
  InitSound();  // do nothing!
  lg::info("InitRPC");
  InitRPC();       // connect to IOP
  reset_output();  // reset output buffers
  clear_print();

  s32 goal_status = InitHeapAndSymbol();  // init GOAL runtime, load kernel and engine
  if (goal_status < 0) {
    return goal_status;
  }

  lg::info("InitListenerConnect");
  InitListenerConnect();
  lg::info("InitCheckListener");
  InitCheckListener();
  Msg(6, "kernel: machine started\n");
  return 0;
}

/*!
 * Shutdown the runtime.
 */
int ShutdownMachine() {
  StopIOP();
  CloseListener();
  ShutdownSound();
  ShutdownGoalProto();

  // OpenGOAL only - kill ps2 VM
  if (VM::use) {
    VM::vm_kill();
  }

  Msg(6, "kernel: machine shutdown\n");
  return 0;
}

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

u64 CPadGetData(u64 cpad_info) {
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

        if (cpad->change_time != 0) {
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
        cpad->change_time = 0;
        cpad->state = 99;
      } else {
        // we have actuators to use.
        cpad->change_time = 1;  // remember to update pad times.
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

// TODO InstallHandler
void InstallHandler(u32 handler_idx, u32 handler_func) {
  ASSERT(handler_idx == 5);  // vif1
  vif1_interrupt_handler = handler_func;
}
// TODO InstallDebugHandler
void InstallDebugHandler() {
  ASSERT(false);
}

void send_gfx_dma_chain(u32 /*bank*/, u32 chain) {
  Gfx::send_chain(g_ee_main_mem, chain);
}

void pc_texture_upload_now(u32 page, u32 mode) {
  Gfx::texture_upload_now(Ptr<u8>(page).c(), mode, s7.offset);
}

void pc_texture_relocate(u32 dst, u32 src, u32 format) {
  Gfx::texture_relocate(dst, src, format);
}

u64 pc_get_mips2c(u32 name) {
  const char* n = Ptr<String>(name).c()->data();
  return Mips2C::gLinkedFunctionTable.get(n);
}

void pc_set_levels(u32 l0, u32 l1) {
  std::string l0s = Ptr<String>(l0).c()->data();
  std::string l1s = Ptr<String>(l1).c()->data();

  std::vector<std::string> levels;
  if (l0s != "none" && l0s != "#f") {
    levels.push_back(l0s);
  }

  if (l1s != "none" && l1s != "#f") {
    levels.push_back(l1s);
  }

  Gfx::set_levels(levels);
}

/*!
 * Open a file-stream.  Name is a GOAL string. Mode is a GOAL symbol.  Use 'read for readonly
 * and anything else for write only.
 */
u64 kopen(u64 fs, u64 name, u64 mode) {
  auto file_stream = Ptr<FileStream>(fs).c();
  file_stream->mode = mode;
  file_stream->name = name;
  file_stream->flags = 0;
  printf("****** CALL TO kopen() ******\n");
  char buffer[128];
  // sprintf(buffer, "host:%s", Ptr<String>(name)->data());
  sprintf(buffer, "%s", Ptr<String>(name)->data());
  if (!strcmp(info(Ptr<Symbol>(mode))->str->data(), "read")) {
    file_stream->file = sceOpen(buffer, SCE_RDONLY);
  } else {
    // 0x602
    file_stream->file = sceOpen(buffer, SCE_TRUNC | SCE_CREAT | SCE_WRONLY);
  }

  return fs;
}

/*!
 * Get length of a file.
 */
s32 klength(u64 fs) {
  auto file_stream = Ptr<FileStream>(fs).c();
  if ((file_stream->flags ^ 1) & 1) {
    // first flag bit not set. This means no errors
    auto end_seek = sceLseek(file_stream->file, 0, SCE_SEEK_END);
    auto reset_seek = sceLseek(file_stream->file, 0, SEEK_SET);
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
    result = sceLseek(file_stream->file, offset, where);
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
    result = sceRead(file_stream->file, Ptr<u8>(buffer).c(), size);
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
    result = sceWrite(file_stream->file, Ptr<u8>(buffer).c(), size);
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
    sceClose(file_stream->file);
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
  return masterConfig.territory;
}

u64 DecodeTimeout() {
  return masterConfig.timeout;
}

u64 DecodeInactiveTimeout() {
  return masterConfig.inactive_timeout;
}

void DecodeTime(u32 ptr) {
  Ptr<sceCdCLOCK> clock(ptr);
  sceCdReadClock(clock.c());
}

// TODO PutDisplayEnv
void PutDisplayEnv(u32 ptr) {
  u8 alp = Ptr<u8>(ptr).c()[1];
  auto* renderer = Gfx::GetCurrentRenderer();
  if (renderer) {
    renderer->set_pmode_alp(alp / 255.f);
  }
  // ASSERT(false);
}

/*!
 * PC PORT FUNCTIONS BEGIN
 */
/*!
 * Get a 300MHz timer value.
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

/*!
 * Return the current OS as a symbol. Actually returns what it was compiled for!
 */
u64 get_os() {
#ifdef _WIN32
  return intern_from_c("windows").offset;
#elif __linux__
  return intern_from_c("linux").offset;
#else
  return s7.offset;
#endif
}

/*!
 * Returns size of window.
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
void get_screen_size(s64 vmode_idx, u32 w_ptr, u32 h_ptr, u32 c_ptr) {
  s32 *w_out = 0, *h_out = 0, *c_out = 0;
  if (w_ptr) {
    w_out = Ptr<s32>(w_ptr).c();
  }
  if (h_ptr) {
    h_out = Ptr<s32>(h_ptr).c();
  }
  if (c_ptr) {
    c_out = Ptr<s32>(c_ptr).c();
  }
  Gfx::get_screen_size(vmode_idx, w_out, h_out, c_out);
}

void update_discord_rpc(u32 discord_info) {
  if (gDiscordRpcEnabled) {
    DiscordRichPresence rpc;
    char state[128];
    auto info = discord_info ? Ptr<DiscordInfo>(discord_info).c() : NULL;
    if (info) {
      int cells = (int)*Ptr<float>(info->fuel).c();
      int orbs = (int)*Ptr<float>(info->money_total).c();
      int scout_flies = (int)*Ptr<float>(info->buzzer_total).c();
      auto cutscene = Ptr<Symbol>(info->cutscene)->value;
      char* status = Ptr<String>(info->status).c()->data();
      char* level = Ptr<String>(info->level).c()->data();
      const char* full_level_name = jak1_get_full_level_name(Ptr<String>(info->level).c()->data());
      memset(&rpc, 0, sizeof(rpc));
      if (!strcmp(level, "finalboss")) {
        strcpy(state, "Fighting Final Boss");
      } else if (!strcmp(level, "title")) {
        strcpy(state, "On title screen");
      } else if (!strcmp(level, "intro")) {
        strcpy(state, "Intro");
      } else if (cutscene != offset_of_s7()) {
        strcpy(state, "Watching a cutscene");
      } else {
        strcpy(state, "Cells: ");
        strcat(state, std::to_string(cells).c_str());
        strcat(state, " | Orbs: ");
        strcat(state, std::to_string(orbs).c_str());
        strcat(state, " | Flies: ");
        strcat(state, std::to_string(scout_flies).c_str());
      }
      rpc.state = state;
      rpc.startTimestamp = gStartTime;
      rpc.details = status;
      rpc.largeImageKey = level;
      rpc.largeImageText = full_level_name;
      rpc.smallImageKey = 0;
      rpc.smallImageText = 0;
      rpc.partySize = 0;
      rpc.partyMax = 0;
      Discord_UpdatePresence(&rpc);
    }
  } else {
    Discord_ClearPresence();
  }
}

u64 filepath_exists(u32 filepath) {
  auto filepath_str = std::string(Ptr<String>(filepath).c()->data());
  if (std::filesystem::exists(filepath_str)) {
    return intern_from_c("#t").offset;
  }
  return s7.offset;
}

void mkdir_path(u32 filepath) {
  auto filepath_str = std::string(Ptr<String>(filepath).c()->data());
  file_util::create_dir_if_needed_for_file(filepath_str);
}

u32 get_fullscreen() {
  switch (Gfx::get_fullscreen()) {
    default:
    case Gfx::DisplayMode::Windowed:
      return intern_from_c("windowed").offset;
    case Gfx::DisplayMode::Borderless:
      return intern_from_c("borderless").offset;
    case Gfx::DisplayMode::Fullscreen:
      return intern_from_c("fullscreen").offset;
  }
}

void set_fullscreen(u32 symptr, s64 screen) {
  if (symptr == intern_from_c("windowed").offset || symptr == s7.offset) {
    Gfx::set_fullscreen(Gfx::DisplayMode::Windowed, screen);
  } else if (symptr == intern_from_c("borderless").offset) {
    Gfx::set_fullscreen(Gfx::DisplayMode::Borderless, screen);
  } else if (symptr == intern_from_c("fullscreen").offset) {
    Gfx::set_fullscreen(Gfx::DisplayMode::Fullscreen, screen);
  }
}

void InitMachine_PCPort() {
  // PC Port added functions
  make_function_symbol_from_c("__read-ee-timer", (void*)read_ee_timer);
  make_function_symbol_from_c("__mem-move", (void*)c_memmove);
  make_function_symbol_from_c("__send-gfx-dma-chain", (void*)send_gfx_dma_chain);
  make_function_symbol_from_c("__pc-texture-upload-now", (void*)pc_texture_upload_now);
  make_function_symbol_from_c("__pc-texture-relocate", (void*)pc_texture_relocate);
  make_function_symbol_from_c("__pc-get-mips2c", (void*)pc_get_mips2c);
  make_function_symbol_from_c("__pc-set-levels", (void*)pc_set_levels);

  // pad stuff
  make_function_symbol_from_c("pc-pad-get-mapped-button", (void*)Gfx::get_mapped_button);
  make_function_symbol_from_c("pc-pad-input-map-save!", (void*)Gfx::input_mode_save);
  make_function_symbol_from_c("pc-pad-input-mode-set", (void*)Gfx::input_mode_set);
  make_function_symbol_from_c("pc-pad-input-pad-set", (void*)Pad::input_mode_pad_set);
  make_function_symbol_from_c("pc-pad-input-mode-get", (void*)Pad::input_mode_get);
  make_function_symbol_from_c("pc-pad-input-key-get", (void*)Pad::input_mode_get_key);
  make_function_symbol_from_c("pc-pad-input-index-get", (void*)Pad::input_mode_get_index);

  // os stuff
  make_function_symbol_from_c("pc-get-os", (void*)get_os);
  make_function_symbol_from_c("pc-get-window-size", (void*)get_window_size);
  make_function_symbol_from_c("pc-get-window-scale", (void*)get_window_scale);
  make_function_symbol_from_c("pc-get-fullscreen", (void*)get_fullscreen);
  make_function_symbol_from_c("pc-get-screen-size", (void*)get_screen_size);
  make_function_symbol_from_c("pc-set-window-size", (void*)Gfx::set_window_size);
  make_function_symbol_from_c("pc-set-letterbox", (void*)Gfx::set_letterbox);
  make_function_symbol_from_c("pc-set-fullscreen", (void*)set_fullscreen);
  make_function_symbol_from_c("pc-renderer-tree-set-lod", (void*)Gfx::SetLod);

  // file related functions
  make_function_symbol_from_c("pc-filepath-exists?", (void*)filepath_exists);
  make_function_symbol_from_c("pc-mkdir-file-path", (void*)mkdir_path);

  // discord rich presence
  make_function_symbol_from_c("pc-discord-rpc-set", (void*)set_discord_rpc);
  make_function_symbol_from_c("pc-discord-rpc-update", (void*)update_discord_rpc);

  // init ps2 VM
  if (VM::use) {
    make_function_symbol_from_c("vm-ptr", (void*)VM::get_vm_ptr);
    VM::vm_init();
  }

  // setup string constants
  auto user_dir_path = file_util::get_user_game_dir();
  intern_from_c("*pc-user-dir-base-path*")->value =
      make_string_from_c(user_dir_path.string().c_str());
  // TODO - we will eventually need a better way to know what game we are playing
  auto settings_path = file_util::get_user_settings_dir();
  intern_from_c("*pc-settings-folder*")->value = make_string_from_c(settings_path.string().c_str());
  intern_from_c("*pc-settings-built-sha*")->value = make_string_from_c(GIT_SHORT_SHA);
}
/*!
 * PC PORT FUNCTIONS END
 */

void vif_interrupt_callback() {
  // added for the PC port for faking VIF interrupts from the graphics system.
  if (vif1_interrupt_handler && MasterExit == 0) {
    call_goal(Ptr<Function>(vif1_interrupt_handler), 0, 0, 0, s7.offset, g_ee_main_mem);
  }
}

/*!
 * Added in PC port.
 */
u32 offset_of_s7() {
  return s7.offset;
}

/*!
 * Final initialization of the system after the kernel is loaded.
 * This is called from InitHeapAndSymbol at the very end.
 * Exports the last of the functions written in C to the GOAL symbol table
 * If DiskBooting, will load the GAME CGO, containing the engine, and calls "play", the function
 * which should prepare the game engine.
 */
void InitMachineScheme() {
  make_function_symbol_from_c("put-display-env", (void*)PutDisplayEnv);       // used in drawable
  make_function_symbol_from_c("syncv", (void*)sceGsSyncV);                    // used in drawable
  make_function_symbol_from_c("sync-path", (void*)sceGsSyncPath);             // used
  make_function_symbol_from_c("reset-path", (void*)sceGsResetPath);           // used in dma
  make_function_symbol_from_c("reset-graph", (void*)sceGsResetGraph);         // used
  make_function_symbol_from_c("dma-sync", (void*)sceDmaSync);                 // used
  make_function_symbol_from_c("gs-put-imr", (void*)sceGsPutIMR);              // unused
  make_function_symbol_from_c("gs-get-imr", (void*)sceGsGetIMR);              // unused
  make_function_symbol_from_c("gs-store-image", (void*)sceGsExecStoreImage);  // used
  make_function_symbol_from_c("flush-cache", (void*)FlushCache);              // used
  make_function_symbol_from_c("cpad-open", (void*)CPadOpen);                  // used
  make_function_symbol_from_c("cpad-get-data", (void*)CPadGetData);           // used
  make_function_symbol_from_c("install-handler", (void*)InstallHandler);      // used
  make_function_symbol_from_c("install-debug-handler", (void*)InstallDebugHandler);       // used
  make_function_symbol_from_c("file-stream-open", (void*)kopen);                          // used
  make_function_symbol_from_c("file-stream-close", (void*)kclose);                        // used
  make_function_symbol_from_c("file-stream-length", (void*)klength);                      // used
  make_function_symbol_from_c("file-stream-seek", (void*)kseek);                          // unused
  make_function_symbol_from_c("file-stream-read", (void*)kread);                          // used
  make_function_symbol_from_c("file-stream-write", (void*)kwrite);                        // used
  make_function_symbol_from_c("scf-get-language", (void*)DecodeLanguage);                 // used
  make_function_symbol_from_c("scf-get-time", (void*)DecodeTime);                         // used
  make_function_symbol_from_c("scf-get-aspect", (void*)DecodeAspect);                     // used
  make_function_symbol_from_c("scf-get-volume", (void*)DecodeVolume);                     // used
  make_function_symbol_from_c("scf-get-territory", (void*)DecodeTerritory);               // used
  make_function_symbol_from_c("scf-get-timeout", (void*)DecodeTimeout);                   // used
  make_function_symbol_from_c("scf-get-inactive-timeout", (void*)DecodeInactiveTimeout);  // used
  make_function_symbol_from_c("dma-to-iop", (void*)dma_to_iop);                           // unused
  make_function_symbol_from_c("kernel-shutdown", (void*)KernelShutdown);                  // used
  make_function_symbol_from_c("aybabtu", (void*)sceCdMmode);                              // used

  InitMachine_PCPort();
  InitSoundScheme();
  intern_from_c("*stack-top*")->value = 0x07ffc000;
  intern_from_c("*stack-base*")->value = 0x07ffffff;
  intern_from_c("*stack-size*")->value = 0x4000;

  if (DiskBoot) {
    intern_from_c("*kernel-boot-message*")->value = intern_from_c(DebugBootMessage).offset;
    intern_from_c("*kernel-boot-mode*")->value = intern_from_c("boot").offset;  // or debug-boot
    intern_from_c("*kernel-boot-level*")->value = intern_from_c(DebugBootLevel).offset;
  }

  // todo remove MasterUseKernel
  if (DiskBoot && MasterUseKernel) {
    *EnableMethodSet = (*EnableMethodSet) + 1;
    load_and_link_dgo_from_c("game", kglobalheap,
                             LINK_FLAG_OUTPUT_LOAD | LINK_FLAG_EXECUTE | LINK_FLAG_PRINT_LOGIN,
                             0x400000, true);
    *EnableMethodSet = (*EnableMethodSet) - 1;

    kernel_packages->value =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE).cast<u32>()),
                 make_string_from_c("engine"), kernel_packages->value);
    kernel_packages->value =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE).cast<u32>()),
                 make_string_from_c("art"), kernel_packages->value);
    kernel_packages->value =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE).cast<u32>()),
                 make_string_from_c("common"), kernel_packages->value);

    lg::info("calling fake play");
    call_goal_function_by_name("play");
  }
}
