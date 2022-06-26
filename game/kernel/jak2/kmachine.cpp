#include "kmachine.h"

#include <cstring>
#include <string>

#include "common/log/log.h"

#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kboot.h"
#include "game/kernel/common/kdsnetm.h"
#include "game/kernel/common/kernel_types.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/common/memory_layout.h"
#include "game/kernel/jak2/kboot.h"
#include "game/kernel/jak2/kmalloc.h"
#include "game/sce/sif_ee.h"
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

  ASSERT_MSG(false, "nyi");
  return -1;

  /*
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
   */
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

void InitMachineScheme() {
  // todo also the pc port functions
  ASSERT(false);
}

}  // namespace jak2