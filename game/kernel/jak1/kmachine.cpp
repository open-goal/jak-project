#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "kmachine.h"

#include <string>

#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "game/external/discord_jak1.h"
#include "game/graphics/display.h"
#include "game/graphics/gfx.h"
#include "game/graphics/sceGraphicsInterface.h"
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
#include "game/kernel/jak1/kboot.h"
#include "game/kernel/jak1/kdgo.h"
#include "game/kernel/jak1/klisten.h"
#include "game/kernel/jak1/kscheme.h"
#include "game/kernel/jak1/ksound.h"
#include "game/sce/deci2.h"
#include "game/sce/libcdvd_ee.h"
#include "game/sce/libdma.h"
#include "game/sce/libgraph.h"
#include "game/sce/sif_ee.h"
#include "game/sce/stubs.h"
#include "game/system/vm/vm.h"

using namespace ee;

namespace jak1 {

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
    reboot_iop = 0;
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
      reboot_iop = 1;   // Reboot the IOP (load new IOP runtime)
    }

    // the "cddata" uses the DVD drive for everything but IOP modules.
    if (arg == "-cddata") {
      Msg(6, "dkernel: cddata mode\n");
      isodrv = iso_cd;  // tell IOP to use actual DVD drive for data files
      modsrc = 0;       // don't use DVD drive for IOP modules
      reboot_iop = 0;   // no need to reboot the IOP
    }

    // the "deviso" mode is one of two modes for testing without the need for DVDs
    if (arg == "-deviso") {
      Msg(6, "dkernel: deviso mode\n");
      isodrv = deviso;  // IOP deviso mode
      modsrc = 0;       // no IOP module loading (there's no DVD to load from!)
      reboot_iop = 0;
    }

    // the "fakeiso" mode is the other of two modes for testing without the need for DVDs
    if (arg == "-fakeiso") {
      Msg(6, "dkernel: fakeiso mode\n");
      isodrv = fakeiso;  // IOP fakeeiso mode
      modsrc = 0;        // no IOP module loading (there's no DVD to load from!)
      reboot_iop = 0;
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
 * Initialize the I/O Processor
 * Removed calls to exit(0) if loading modules fails.
 */
void InitIOP() {
  // before doing anything with the I/O Processor, we need to set up SIF RPC
  sceSifInitRpc(0);

  if ((isodrv == iso_cd) || modsrc || reboot_iop) {
    // we will need the DVD drive to bring up the IOP
    InitCD();
  }

  if (!reboot_iop) {
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
  char* cmd = overlord_boot_command;
  kstrcpy(cmd, init_types[(int)isodrv]);
  cmd = cmd + strlen(cmd) + 1;
  kstrcpy(cmd, "SCREEN1.USA");
  cmd = cmd + strlen(cmd) + 1;
  if (masterConfig.disable_sound) {
    kstrcpy(cmd, "-nosound");
    cmd = cmd + strlen(cmd) + 1;
  }

  int total_len = cmd - overlord_boot_command;

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
    auto rv = sceSifLoadModule("host0:binee/overlord.irx", total_len, overlord_boot_command);
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
    auto rv =
        sceSifLoadModule("cdrom0:\\\\DRIVERS\\\\OVERLORD.IRX;1", total_len, overlord_boot_command);
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

AutoSplitterBlock gAutoSplitterBlock;

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

  init_output();    // GOAL input/output buffer setup
  jak1::InitIOP();  // start IOP/OVERLORD, loading our legal splash screen

  // sceGsResetPath(); // reset VIF1, VU1, GIF

  InitVideo();  // display legal splash screen

  // FlushCache(WRITEBACK_DCACHE);
  // FlushCache(INVALIDATE_ICACHE);
  // sceGsSyncV(0);   // wait for it to show up on the screen
  //
  // if(scePadInit(0) != 1) { // init controllers
  //   MsgErr("dkernel: !init pad\n");
  // }

  if (MasterDebug) {  // connect to GOAL compiler
    InitGoalProto();
  } else {
    // shut down the deci2 stuff, we don't need it.
    ee::sceDeci2Disable();
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

  // TODO - better place to put this?
  gAutoSplitterBlock.pointer_to_symbol =
      (u64)g_ee_main_mem + intern_from_c("*autosplit-info-jak1*")->value;

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

void PutDisplayEnv(u32 ptr) {
  // we can mostly ignore this, except for one value that sets the 'blackout' amount.
  u8 alp = Ptr<u8>(ptr).c()[1];
  auto* renderer = Gfx::GetCurrentRenderer();
  if (renderer) {
    renderer->set_pmode_alp(alp / 255.f);
  }
}

void update_discord_rpc(u32 discord_info) {
  if (gDiscordRpcEnabled) {
    DiscordRichPresence rpc;
    char state[128];
    char large_image_key[128];
    char large_image_text[128];
    char small_image_key[128];
    char small_image_text[128];
    auto info = discord_info ? Ptr<DiscordInfo>(discord_info).c() : NULL;
    if (info) {
      int cells = (int)*Ptr<float>(info->fuel).c();
      int orbs = (int)*Ptr<float>(info->money_total).c();
      int scout_flies = (int)*Ptr<float>(info->buzzer_total).c();
      int deaths = *Ptr<int>(info->deaths).c();
      float time = *Ptr<float>(info->time_of_day).c();
      auto cutscene = Ptr<Symbol>(info->cutscene)->value;
      auto ogreboss = Ptr<Symbol>(info->ogreboss)->value;
      auto plantboss = Ptr<Symbol>(info->plantboss)->value;
      auto racer = Ptr<Symbol>(info->racer)->value;
      auto flutflut = Ptr<Symbol>(info->flutflut)->value;
      char* status = Ptr<String>(info->status).c()->data();
      char* level = Ptr<String>(info->level).c()->data();
      const char* full_level_name =
          get_full_level_name(level_names, level_name_remap, Ptr<String>(info->level).c()->data());
      memset(&rpc, 0, sizeof(rpc));
      if (!indoors(indoor_levels, level)) {
        char level_with_tod[128];
        strcpy(level_with_tod, level);
        strcat(level_with_tod, "-");
        strcat(level_with_tod, time_of_day_str(time));
        strcpy(large_image_key, level_with_tod);
      } else {
        strcpy(large_image_key, level);
      }
      strcpy(large_image_text, full_level_name);
      if (!strcmp(full_level_name, "unknown")) {
        strcpy(large_image_key, full_level_name);
        strcpy(large_image_text, level);
      }
      rpc.largeImageKey = large_image_key;
      if (!strcmp(level, "finalboss")) {
        strcpy(state, "Fighting Final Boss");
      } else if (plantboss != offset_of_s7()) {
        strcpy(state, "Fighting Dark Eco Plant");
        rpc.largeImageKey = "plant-boss";
        strcpy(large_image_text, "Dark Eco Plant");
      } else if (ogreboss != offset_of_s7()) {
        strcpy(state, "Fighting Klaww");
        rpc.largeImageKey = "ogreboss";
        strcpy(large_image_text, "Klaww");
      } else if (!strcmp(level, "title")) {
        strcpy(state, "On title screen");
        rpc.largeImageKey = "title";
        strcpy(large_image_text, "Title screen");
      } else if (!strcmp(level, "intro")) {
        strcpy(state, "Intro");
      } else if (cutscene != offset_of_s7()) {
        strcpy(state, "Watching a cutscene");
        strcpy(large_image_text, fmt::format("Cells: {} | Orbs: {} | Flies: {} | Deaths: {}",
                                             std::to_string(cells), std::to_string(orbs),
                                             std::to_string(scout_flies), std::to_string(deaths))
                                     .c_str());
      } else {
        strcpy(state, fmt::format("Cells: {} | Orbs: {} | Flies: {}", std::to_string(cells),
                                  std::to_string(orbs), std::to_string(scout_flies))
                          .c_str());

        strcat(large_image_text, fmt::format(" | Deaths: {}", std::to_string(deaths)).c_str());
      }
      rpc.largeImageText = large_image_text;
      rpc.state = state;
      if (racer != offset_of_s7()) {
        strcpy(small_image_key, "target-racer");
        strcpy(small_image_text, "Driving A-Grav Zoomer");
      } else if (flutflut != offset_of_s7()) {
        strcpy(small_image_key, "flutflut");
        strcpy(small_image_text, "Riding on Flut Flut");
      } else {
        if (!indoors(indoor_levels, level)) {
          strcpy(small_image_key, time_of_day_str(time));
          strcpy(small_image_text, "Time of day: ");
          strcat(small_image_text, get_time_of_day(time).c_str());
        } else {
          strcpy(small_image_key, "");
          strcpy(small_image_text, "");
        }
      }
      rpc.smallImageKey = small_image_key;
      rpc.smallImageText = small_image_text;
      rpc.startTimestamp = gStartTime;
      rpc.details = status;
      rpc.partySize = 0;
      rpc.partyMax = 0;
      Discord_UpdatePresence(&rpc);
    }
  } else {
    Discord_ClearPresence();
  }
}

void pc_set_levels(u32 l0, u32 l1) {
  if (!Gfx::GetCurrentRenderer()) {
    return;
  }
  std::string l0s = Ptr<String>(l0).c()->data();
  std::string l1s = Ptr<String>(l1).c()->data();

  std::vector<std::string> levels;
  if (l0s != "none" && l0s != "#f") {
    levels.push_back(l0s);
  }

  if (l1s != "none" && l1s != "#f") {
    levels.push_back(l1s);
  }

  Gfx::GetCurrentRenderer()->set_levels(levels);
}

void InitMachine_PCPort() {
  // PC Port added functions
  init_common_pc_port_functions(
      make_function_symbol_from_c,
      [](const char* name) {
        const auto result = intern_from_c(name);
        InternFromCInfo info{};
        info.offset = result.offset;
        return info;
      },
      make_string_from_c);

  // Game specific functions
  // Called from the game thread at each frame to tell the PC rendering code which levels to start
  // loading. The loader internally handles locking.
  make_function_symbol_from_c("__pc-set-levels", (void*)pc_set_levels);

  make_function_symbol_from_c("pc-discord-rpc-update", (void*)update_discord_rpc);

  // setup string constants
  // TODO - these may be able to be moved into `init_common_pc_port_functions` but it's trickier
  // since they are accessing the Ptr's value
  auto user_dir_path = file_util::get_user_config_dir();
  intern_from_c("*pc-user-dir-base-path*")->value =
      make_string_from_c(user_dir_path.string().c_str());
  auto settings_path = file_util::get_user_settings_dir(g_game_version);
  intern_from_c("*pc-settings-folder*")->value = make_string_from_c(settings_path.string().c_str());
  intern_from_c("*pc-settings-built-sha*")->value = make_string_from_c(build_revision().c_str());
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
  make_function_symbol_from_c("kernel-shutdown", (void*)jak1::KernelShutdown);  // used TODO jak1
  make_function_symbol_from_c("aybabtu", (void*)sceCdMmode);                    // used

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
    using namespace jak1_symbols;
    kernel_packages->value =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE).cast<u32>()),
                 make_string_from_c("engine"), kernel_packages->value);
    kernel_packages->value =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE).cast<u32>()),
                 make_string_from_c("art"), kernel_packages->value);
    kernel_packages->value =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE).cast<u32>()),
                 make_string_from_c("common"), kernel_packages->value);

    lg::info("calling play");
    call_goal_function_by_name("play");
  }
}

}  // namespace jak1

#if defined(__GNUC__)
#pragma GCC diagnostic pop
#elif defined(__clang__)
#pragma clang diagnostic pop
#endif
