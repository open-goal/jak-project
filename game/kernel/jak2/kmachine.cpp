#include "kmachine.h"

#include <cstring>
#include <string>

#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/FileUtil.h"

#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kboot.h"
#include "game/kernel/common/kdgo.h"
#include "game/kernel/common/kdsnetm.h"
#include "game/kernel/common/kernel_types.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/common/ksocket.h"
#include "game/kernel/common/ksound.h"
#include "game/kernel/common/memory_layout.h"
#include "game/kernel/jak2/kboot.h"
#include "game/kernel/jak2/kdgo.h"
#include "game/kernel/jak2/klink.h"
#include "game/kernel/jak2/klisten.h"
#include "game/kernel/jak2/kmalloc.h"
#include "game/kernel/jak2/kscheme.h"
#include "game/kernel/jak2/ksound.h"
#include "game/kernel/svnrev.h"
#include "game/sce/libdma.h"
#include "game/sce/libgraph.h"
#include "game/sce/sif_ee.h"
#include "game/sce/stubs.h"
#include "game/system/vm/vm.h"

using namespace ee;

namespace jak2 {
/*!
 * Initialize global variables based on command line parameters. Not called in retail versions,
 * but it is present in the ELF.
 * DONE
 * Modified to use std::string, and removed call to fflush.
 */
void InitParms(int argc, const char* const* argv) {
  // Modified default settings to boot up the game like normal if no arguments are present.
  if (argc == 1) {
    DiskBoot = 1;
    isodrv = fakeiso;
    modsrc = 0;
    reboot = 0;
    DebugSegment = 0;
    MasterDebug = 0;
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

    if (arg == "-demo") {
      Msg(6, "dkernel: demo mode\n");
      kstrcpy(DebugBootMessage, "demo");
    }

    // new for jak 2
    if (arg == "-kiosk") {
      Msg(6, "dkernel: kiosk mode\n");
      kstrcpy(DebugBootMessage, "kiosk");
    }

    // new for jak 2
    if (arg == "-preview") {
      Msg(6, "dkernel: preview mode\n");
      kstrcpy(DebugBootMessage, "preview");
    }

    // the "deviso" mode is one of two modes for testing without the need for DVDs
    if (arg == "-deviso") {
      Msg(6, "dkernel: deviso mode\n");
      isodrv = deviso;  // IOP deviso mode
      modsrc = 2;       // now 2 for Jak 2
      reboot = 0;
    }
    // the "fakeiso" mode is the other of two modes for testing without the need for DVDs
    if (arg == "-fakeiso") {
      Msg(6, "dkernel: fakeiso mode\n");
      isodrv = fakeiso;  // IOP fakeeiso mode
      modsrc = 0;        // no IOP module loading (there's no DVD to load from!)
      reboot = 0;
    }

    // the "boot" mode is used to set GOAL up for running the game in retail mode
    if (arg == "-boot") {
      Msg(6, "dkernel: boot mode\n");
      MasterDebug = 0;
      DiskBoot = 1;
      DebugSegment = 0;
    }

    // new for jak 2
    if (arg == "-debug-boot") {
      Msg(6, "dkernel: debug-boot mode\n");
      MasterDebug = 0;
      DebugSegment = 1;
      DiskBoot = 1;
    }

    // traditional debug mode
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

    // new for jak 2
    if (arg == "-user") {
      i++;
      std::string levelName = argv[i];
      Msg(6, "dkernel: user %s\n", levelName.c_str());
      kstrcpy(DebugBootUser, levelName.c_str());
    }

    // new for jak 2
    if (arg == "-art") {
      i++;
      std::string levelName = argv[i];
      Msg(6, "dkernel: art-group %s\n", levelName.c_str());
      kstrcpy(DebugBootArtGroup, levelName.c_str());
      kstrcpy(DebugBootMessage, "art-group");
    }

    // an added mode to allow booting without a KERNEL.CGO for testing
    if (arg == "-nokernel") {
      Msg(6, "dkernel: no kernel mode\n");
      MasterUseKernel = false;
    }

    // an added mode to allow booting without sound for testing
    if (arg == "-nosound") {
      Msg(6, "dkernel: no sound mode\n");
      masterConfig.disable_sound = true;
    }
  }
}

void InitIOP() {
  Msg(6, "dkernel: boot:%d debug:%d mem:%d dev:%d mod:%d\n", DiskBoot, MasterDebug, DebugSegment,
      isodrv, modsrc);
  sceSifInitRpc(0);

  // init cd if we need it
  if (((isodrv == iso_cd) || (modsrc == 1)) || (reboot == 1)) {
    InitCD();
  }

  if (reboot == 0) {
    // iop with dev kernel
    printf("Rebooting IOP...\n");
    while (!sceSifRebootIop("host0:/usr/local/sce/iop/modules/ioprp271.img")) {
      printf("Failed, retrying...\n");
    }
    while (!sceSifSyncIop()) {
    }
  } else {
    printf("Rebooting IOP...\n");
    while (!sceSifRebootIop("cdrom0:\\DRIVERS\\IOPRP271.IMG")) {
      printf("Failed, retrying...\n");
    }
    while (!sceSifSyncIop()) {
    }
  }
  sceSifInitRpc(0);
  if ((isodrv == iso_cd) || (modsrc == 1)) {
    InitCD();
    sceFsReset();
  }

  char overlord_boot_command[256];
  char* cmd = overlord_boot_command;
  kstrcpy(cmd, init_types[(int)isodrv]);
  cmd = cmd + strlen(cmd) + 1;
  if (!strncmp(DebugBootMessage, "demo", 4)) {
    kstrcpy(cmd, "SCREEN1.DEM");
  } else {
    kstrcpy(cmd, "SCREEN1.USA");
  }
  cmd = cmd + strlen(cmd) + 1;
  if (masterConfig.disable_sound) {
    kstrcpy(cmd, "-nosound");
    cmd = cmd + strlen(cmd) + 1;
  }

  int total_len = cmd - overlord_boot_command;

  if (modsrc == 0) {
    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/sio2man.irx", 0, nullptr) < 0) {
      MsgErr("loading sio2man.irx failed\n");
      exit(0);
    }

    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/padman.irx", 0, nullptr) < 0) {
      MsgErr("loading padman.irx failed\n");
      exit(0);
    }
    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/libsd.irx", 0, nullptr) < 0) {
      MsgErr("loading libsd.irx failed\n");
      exit(0);
    }
    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/mcman.irx", 0, nullptr) < 0) {
      MsgErr("loading mcman.irx failed\n");
      exit(0);
    }
    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/mcserv.irx", 0, nullptr) < 0) {
      MsgErr("loading mcserv.irx failed\n");
      exit(0);
    }
    if (sceSifLoadModule("host0:/usr/home/src/989snd23/iop/sce27/989nostr.irx", 0, nullptr) < 0) {
      MsgErr("loading %s failed\n", "host0:/usr/home/src/989snd23/iop/sce27/989nostr.irx");
      exit(0);
    }
    if (DebugSegment) {
      sceSifLoadModule("host0:/usr/home/src/989snd23/iop/sce27/989err.irx", 0, nullptr);
    }
    if (sceSifLoadModule("host0:/usr/local/sce/iop/modules/scrtchpd.irx", 0, nullptr) < 0) {
      MsgErr("loading scrtchpd.irx failed\n");
      exit(0);
    }
    printf("Initializing CD library in FAKEISO mode\n");
    if (sceSifLoadModule("host0:bin/overlord.irx", total_len, overlord_boot_command) < 0) {
      MsgErr("loading overlord.irx <3> failed\n");
      exit(0);
    }
  } else {
    if (modsrc == 1) {
      if (sceSifLoadModule("cdrom0:\\DRIVERS\\SIO2MAN.IRX;1", 0, nullptr) < 0) {
        MsgErr("loading SIO2MAN.IRX failed\n");
        exit(0);
      }
      if (sceSifLoadModule("cdrom0:\\DRIVERS\\PADMAN.IRX;1", 0, nullptr) < 0) {
        MsgErr("loading PADMAN.IRX failed\n");
        exit(0);
      }
      if (sceSifLoadModule("cdrom0:\\DRIVERS\\LIBSD.IRX;1", 0, nullptr) < 0) {
        MsgErr("loading LIBSD.IRX failed\n");
        exit(0);
      }
      if (sceSifLoadModule("cdrom0:\\DRIVERS\\MCMAN.IRX;1", 0, nullptr) < 0) {
        MsgErr("loading MCMAN.IRX failed\n");
        exit(0);
      }

      if (sceSifLoadModule("cdrom0:\\DRIVERS\\MCSERV.IRX;1", 0, nullptr) < 0) {
        MsgErr("loading MCSERV.IRX failed\n");
        exit(0);
      }
      if (sceSifLoadModule("cdrom0:\\DRIVERS\\989NOSTR.IRX;1", 0, nullptr) < 0) {
        MsgErr("loading 989SND.IRX failed\n");
        exit(0);
      }
      if (sceSifLoadModule("cdrom0:\\DRIVERS\\SCRTCHPD.IRX;1", 0, nullptr) < 0) {
        MsgErr("loading SCRTCHPD.IRX failed\n");
        exit(0);
      }
      printf("Initializing CD library in ISO_CD mode\n");
      auto rv =
          sceSifLoadModule("cdrom0:\\DRIVERS\\OVERLORD.IRX;1", total_len, overlord_boot_command);
      if (rv < 0) {
        MsgErr("loading OVERLORD.IRX failed %d\n", rv);
        exit(0);
      }
    } else {
      if (modsrc == 2) {
        if (sceSifLoadModule("host0:isoimage/DRIVERS/SIO2MAN.IRX", 0, nullptr) < 0) {
          MsgErr("loading sio2man.irx failed\n");
          exit(0);
        }
        if (sceSifLoadModule("host0:isoimage/DRIVERS/PADMAN.IRX", 0, nullptr) < 0) {
          MsgErr("loading padman.irx failed\n");
          exit(0);
        }
        if (sceSifLoadModule("host0:isoimage/DRIVERS/LIBSD.IRX", 0, nullptr) < 0) {
          MsgErr("loading libsd.irx failed\n");
          exit(0);
        }
        if (sceSifLoadModule("host0:isoimage/DRIVERS/MCMAN.IRX", 0, nullptr) < 0) {
          MsgErr("loading mcman.irx failed\n");
          exit(0);
        }
        if (sceSifLoadModule("host0:isoimage/DRIVERS/MCSERV.IRX", 0, nullptr) < 0) {
          MsgErr("loading mcserv.irx failed\n");
          exit(0);
        }
        if (sceSifLoadModule("host:isoimage/DRIVERS/989NOSTR.IRX", 1, "do_rpc=0") < 0) {
          MsgErr("loading 989snd.irx failed\n");
          exit(0);
        }
        sceSifLoadModule("host0:/usr/home/src/989snd23/iop/sce27/989err.irx", 0, nullptr);

        if (sceSifLoadModule("host0:isoimage/DRIVERS/SCRTCHPD.IRX;1", 0, nullptr) < 0) {
          MsgErr("loading scrtchpd.irx failed\n");
          exit(0);
        }
        printf("Initializing CD library in DEVISO mode\n");

        auto rv = sceSifLoadModule("host0:isoimage/DRIVERS/OVERLORD.IRX", total_len,
                                   overlord_boot_command);
        if (rv < 0) {
          MsgErr("loading overlord.irx <2> failed\n");
          exit(0);
        }
      }
    }
  }
  int rv = sceMcInit();
  if (rv < 0) {
    MsgErr("MC driver init failed %d\n", rv);
    exit(0);
  }
  printf("InitIOP OK\n");
}

int InitMachine() {
  // heap_start = malloc(0x10);
  // set up global heap (modified, the default size in the release game is 32 MB in all cases)
  u32 global_heap_size = GLOBAL_HEAP_END - HEAP_START;
  float size_mb = ((float)global_heap_size) / (float)(1 << 20);
  lg::info("gkernel: global heap 0x{:08x} to 0x{:08x} (size {:.3f} MB)", HEAP_START,
           GLOBAL_HEAP_END, size_mb);
  kinitheap(kglobalheap, Ptr<u8>(HEAP_START), global_heap_size);

  kmemopen_from_c(kglobalheap, "global");
  kmemopen_from_c(kglobalheap, "scheme-globals");
  if (!MasterDebug && !DebugSegment) {
    // if no debug, we make the kheapinfo structure NULL so GOAL knows not to use it.
    // note: either MasterDebug or DebugSegment is enough to give use the debug heap.
    kdebugheap.offset = 0;
  } else {
    kinitheap(kdebugheap, Ptr<u8>(DEBUG_HEAP_START), jak2::DEBUG_HEAP_SIZE);
  }
  init_output();
  InitIOP();
  // sceGsResetPath();
  InitVideo();
  // FlushCache(0);
  // FlushCache(2);
  // sceGsSyncV(0);
  // if (scePadInit(0) != 1) {
  //   MsgErr("dkernel: !init pad\n");
  // }
  // if (MasterDebug != 0) {
  InitGoalProto();
  // }

  printf("InitSound\n");
  InitSound();
  printf("InitRPC\n");
  InitRPC();
  reset_output();
  clear_print();
  auto status = InitHeapAndSymbol();
  if (status >= 0) {
    printf("InitListenerConnect\n");
    InitListenerConnect();
    printf("InitCheckListener\n");
    InitCheckListener();
    Msg(6, "kernel: machine started\n");
    return 0;
  } else {
    return status;
  }
}

/*!
 * Shutdown the runtime.
 */
int ShutdownMachine() {
  Msg(6, "kernel: machine shutdown (reason %d)\n", MasterExit);
  ASSERT(false);
  /*
  StopIOP();
  ShutdownSound();
  CloseListener();
   */
  ShutdownGoalProto();

  // OpenGOAL only - kill ps2 VM
  if (VM::use) {
    VM::vm_kill();
  }
  return 0;
}

Ptr<MouseInfo> MouseGetData(Ptr<MouseInfo> mouse) {
  // stubbed out in the actual game
  return mouse;
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
  if (!strcmp(Ptr<Symbol4<u8>>(mode)->name_cstr(), "read")) {
    // 0x1
    file_stream->file = sceOpen(buffer, SCE_RDONLY);
  } else if (!strcmp(Ptr<Symbol4<u8>>(mode)->name_cstr(), "append")) {
    // new in jak 2!
    // 0x202
    file_stream->file = sceOpen(buffer, SCE_CREAT | SCE_WRONLY);
  } else {
    // 0x602
    file_stream->file = sceOpen(buffer, SCE_TRUNC | SCE_CREAT | SCE_WRONLY);
  }

  return fs;
}

void InitMachine_PCPort() {
  // PC Port added functions

  make_function_symbol_from_c("__read-ee-timer", (void*)read_ee_timer);
  make_function_symbol_from_c("__mem-move", (void*)c_memmove);
  // make_function_symbol_from_c("__send-gfx-dma-chain", (void*)send_gfx_dma_chain);
  // make_function_symbol_from_c("__pc-texture-upload-now", (void*)pc_texture_upload_now);
  // make_function_symbol_from_c("__pc-texture-relocate", (void*)pc_texture_relocate);
  // make_function_symbol_from_c("__pc-get-mips2c", (void*)pc_get_mips2c);
  // make_function_symbol_from_c("__pc-set-levels", (void*)pc_set_levels);

  // pad stuff
  make_function_symbol_from_c("pc-pad-get-mapped-button", (void*)Gfx::get_mapped_button);
  make_function_symbol_from_c("pc-pad-input-map-save!", (void*)Gfx::input_mode_save);
  make_function_symbol_from_c("pc-pad-input-mode-set", (void*)Gfx::input_mode_set);
  make_function_symbol_from_c("pc-pad-input-pad-set", (void*)Pad::input_mode_pad_set);
  make_function_symbol_from_c("pc-pad-input-mode-get", (void*)Pad::input_mode_get);
  make_function_symbol_from_c("pc-pad-input-key-get", (void*)Pad::input_mode_get_key);
  make_function_symbol_from_c("pc-pad-input-index-get", (void*)Pad::input_mode_get_index);

  // os stuff
  // make_function_symbol_from_c("pc-get-os", (void*)get_os);
  make_function_symbol_from_c("pc-get-window-size", (void*)get_window_size);
  make_function_symbol_from_c("pc-get-window-scale", (void*)get_window_scale);
  // make_function_symbol_from_c("pc-get-fullscreen", (void*)get_fullscreen);
  make_function_symbol_from_c("pc-get-screen-size", (void*)get_screen_size);
  make_function_symbol_from_c("pc-get-screen-rate", (void*)get_screen_rate);
  make_function_symbol_from_c("pc-get-screen-vmode-count", (void*)get_screen_vmode_count);
  make_function_symbol_from_c("pc-set-window-size", (void*)Gfx::set_window_size);
  // make_function_symbol_from_c("pc-set-fullscreen", (void*)set_fullscreen);
  make_function_symbol_from_c("pc-set-frame-rate", (void*)set_frame_rate);
  make_function_symbol_from_c("pc-set-vsync", (void*)set_vsync);
  make_function_symbol_from_c("pc-set-window-lock", (void*)set_window_lock);

  // graphics things
  make_function_symbol_from_c("pc-set-letterbox", (void*)Gfx::set_letterbox);
  make_function_symbol_from_c("pc-renderer-tree-set-lod", (void*)Gfx::SetLod);
  make_function_symbol_from_c("pc-set-collision-mode", (void*)Gfx::CollisionRendererSetMode);
  make_function_symbol_from_c("pc-set-collision-mask", (void*)set_collision_mask);
  make_function_symbol_from_c("pc-get-collision-mask", (void*)get_collision_mask);
  make_function_symbol_from_c("pc-set-collision-wireframe", (void*)set_collision_wireframe);
  make_function_symbol_from_c("pc-set-collision", (void*)set_collision);

  // file related functions
  make_function_symbol_from_c("pc-filepath-exists?", (void*)filepath_exists);
  make_function_symbol_from_c("pc-mkdir-file-path", (void*)mkdir_path);

  // discord rich presence
  // make_function_symbol_from_c("pc-discord-rpc-set", (void*)set_discord_rpc);
  // make_function_symbol_from_c("pc-discord-rpc-update", (void*)update_discord_rpc);

  // profiler
  make_function_symbol_from_c("pc-prof", (void*)prof_event);

  // init ps2 VM
  if (VM::use) {
    make_function_symbol_from_c("vm-ptr", (void*)VM::get_vm_ptr);
    VM::vm_init();
  }

  // setup string constants
  auto user_dir_path = file_util::get_user_game_dir();
  intern_from_c("*pc-user-dir-base-path*")->value() =
      make_string_from_c(user_dir_path.string().c_str());
  // TODO - we will eventually need a better way to know what game we are playing
  auto settings_path = file_util::get_user_settings_dir();
  intern_from_c("*pc-settings-folder*")->value() =
      make_string_from_c(settings_path.string().c_str());
  intern_from_c("*pc-settings-built-sha*")->value() = make_string_from_c(GIT_VERSION);
}

void PutDisplayEnv(u32 /*ptr*/) {
  ASSERT(false);
}

u32 sceGsSyncV(u32 mode) {
  // stub, jak2 probably works differently here
  ASSERT(false);
  return 0;
  /*
  ASSERT(mode == 0);
  VBlank_Handler();
  return Gfx::vsync();
   */
}

u32 sceGsSyncPath(u32 mode, u32 timeout) {
  // stub, jak2 probably works differently here
  ASSERT(mode == 0 && timeout == 0);
  ASSERT(false);
  return 0;
  // return Gfx::sync_path();
}

void aybabtu() {}

void InitMachineScheme() {
  make_function_symbol_from_c("put-display-env", (void*)PutDisplayEnv);
  make_function_symbol_from_c("syncv", (void*)sceGsSyncV);
  make_function_symbol_from_c("sync-path", (void*)sceGsSyncPath);
  make_function_symbol_from_c("reset-path", (void*)ee::sceGsResetPath);
  make_function_symbol_from_c("reset-graph", (void*)sceGsResetGraph);
  make_function_symbol_from_c("dma-sync", (void*)sceDmaSync);
  make_function_symbol_from_c("gs-put-imr", (void*)sceGsPutIMR);
  make_function_symbol_from_c("gs-get-imr", (void*)sceGsGetIMR);
  make_function_symbol_from_c("gs-store-image", (void*)sceGsExecStoreImage);
  make_function_symbol_from_c("flush-cache", (void*)FlushCache);
  make_function_symbol_from_c("cpad-open", (void*)CPadOpen);
  make_function_symbol_from_c("cpad-get-data", (void*)CPadGetData);
  make_function_symbol_from_c("mouse-get-data", (void*)MouseGetData);
  make_function_symbol_from_c("install-handler", (void*)InstallHandler);
  make_function_symbol_from_c("install-debug-handler", (void*)InstallDebugHandler);
  make_function_symbol_from_c("file-stream-open", (void*)kopen);
  make_function_symbol_from_c("file-stream-close", (void*)kclose);
  make_function_symbol_from_c("file-stream-length", (void*)klength);
  make_function_symbol_from_c("file-stream-seek", (void*)kseek);
  make_function_symbol_from_c("file-stream-read", (void*)kread);
  make_function_symbol_from_c("file-stream-write", (void*)kwrite);
  make_function_symbol_from_c("scf-get-language", (void*)DecodeLanguage);
  make_function_symbol_from_c("scf-get-time", (void*)DecodeTime);
  make_function_symbol_from_c("scf-get-aspect", (void*)DecodeAspect);
  make_function_symbol_from_c("scf-get-volume", (void*)DecodeVolume);
  make_function_symbol_from_c("scf-get-territory", (void*)DecodeTerritory);
  make_function_symbol_from_c("scf-get-timeout", (void*)DecodeTimeout);
  make_function_symbol_from_c("scf-get-inactive-timeout", (void*)DecodeInactiveTimeout);
  make_function_symbol_from_c("dma-to-iop", (void*)dma_to_iop);
  make_function_symbol_from_c("kernel-shutdown", (void*)KernelShutdown);
  make_function_symbol_from_c("aybabtu", (void*)aybabtu);  // was nothing function

  InitMachine_PCPort();

  InitSoundScheme();
  intern_from_c("*stack-top*")->value() = 0x7f00000;
  intern_from_c("*stack-base*")->value() = 0x7ffffff;
  intern_from_c("*stack-size*")->value() = 0x100000;
  intern_from_c("*kernel-boot-message*")->value() = intern_from_c(DebugBootMessage).offset;
  intern_from_c("*user*")->value() = make_string_from_c(DebugBootUser);
  if (DiskBoot) {
    intern_from_c("*kernel-boot-mode*")->value() = intern_from_c("boot").offset;
  }
  intern_from_c("*kernel-boot-level*")->value() = intern_from_c(DebugBootLevel).offset;
  intern_from_c("*kernel-boot-art-group*")->value() = make_string_from_c(DebugBootArtGroup);
  if (DiskBoot) {
    *EnableMethodSet = *EnableMethodSet + 1;
    load_and_link_dgo_from_c("game", kglobalheap,
                             LINK_FLAG_OUTPUT_LOAD | LINK_FLAG_EXECUTE | LINK_FLAG_PRINT_LOGIN,
                             0x400000, true);
    *EnableMethodSet = *EnableMethodSet + -1;
    using namespace jak2_symbols;
    kernel_packages->value() =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE - 1).cast<u32>()),
                 make_string_from_c("engine"), kernel_packages->value());
    kernel_packages->value() =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE - 1).cast<u32>()),
                 make_string_from_c("art"), kernel_packages->value());
    kernel_packages->value() =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE).cast<u32>()),
                 make_string_from_c("common"), kernel_packages->value());
    printf("calling play-boot!\n");
    call_goal_function_by_name("play-boot");  // new function for jak2!
  }
}

}  // namespace jak2