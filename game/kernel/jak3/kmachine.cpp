#include "kmachine.h"

#include <cstring>

#include "common/symbols.h"

#include "game/graphics/gfx.h"
#include "game/graphics/jak3_texture_remap.h"
#include "game/graphics/sceGraphicsInterface.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kdgo.h"
#include "game/kernel/common/kdsnetm.h"
#include "game/kernel/common/kernel_types.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/ksocket.h"
#include "game/kernel/common/ksound.h"
#include "game/kernel/common/memory_layout.h"
#include "game/kernel/jak3/kboot.h"
#include "game/kernel/jak3/kdgo.h"
#include "game/kernel/jak3/klisten.h"
#include "game/kernel/jak3/kmachine_extras.h"
#include "game/kernel/jak3/kmalloc.h"
#include "game/kernel/jak3/kscheme.h"
#include "game/kernel/jak3/ksound.h"
#include "game/sce/deci2.h"
#include "game/sce/libdma.h"
#include "game/sce/libgraph.h"
#include "game/sce/sif_ee.h"
#include "game/sce/stubs.h"

namespace jak3 {

using namespace ee;

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
    reboot_iop = 0;
    DebugSegment = 0;
    MasterDebug = 0;
    DebugSymbols = true;
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
      reboot_iop = 0;
    }
    // the "fakeiso" mode is the other of two modes for testing without the need for DVDs
    if (arg == "-fakeiso") {
      Msg(6, "dkernel: fakeiso mode\n");
      isodrv = fakeiso;  // IOP fakeeiso mode
      modsrc = 0;        // no IOP module loading (there's no DVD to load from!)
      reboot_iop = 0;
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

    // TODO overlord 1 vs. 2 switch

    if (arg == "-debug-symbols") {
      Msg(6, "dkernel: debug-symbols on\n");
      DebugSymbols = true;
    }

    if (arg == "-no-debug-symbols") {
      Msg(6, "dkernel: debug-symbols off\n");
      DebugSymbols = true;
    }

    // the "-level [level-name]" mode is used to inform the game to boot a specific level
    // the default level is "#f".
    if (arg == "-level") {
      i++;
      std::string levelName = argv[i];
      Msg(6, "dkernel: level %s\n", levelName.c_str());
      kstrcpy(DebugBootLevel, levelName.c_str());
      ASSERT_NOT_REACHED();  // symbol ID junk
    }

    // new for jak 2
    if (arg == "-user") {
      i++;
      std::string userName = argv[i];
      Msg(6, "dkernel: user %s\n", userName.c_str());
      kstrcpy(DebugBootUser, userName.c_str());
    }

    // new for jak 2
    if (arg == "-art") {
      i++;
      std::string artGroupName = argv[i];
      Msg(6, "dkernel: art-group %s\n", artGroupName.c_str());
      kstrcpy(DebugBootArtGroup, artGroupName.c_str());
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

/*!
 * This is mostly copy-pasted from jak2 and very simplified until we have overlord 2.
 */
void InitIOP() {
  Msg(6, "dkernel: boot:%d debug:%d mem:%d syms:%d dev:%d mod:%d\n", DiskBoot, MasterDebug,
      DebugSegment, DebugSymbols, isodrv, modsrc);
  sceSifInitRpc(0);

  // init cd if we need it
  if (((isodrv == iso_cd) || (modsrc == 1)) || (reboot_iop == 1)) {
    InitCD();
  }

  if ((isodrv == iso_cd) || (modsrc == 1)) {
    InitCD();
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
    printf("Initializing CD library in FAKEISO mode\n");
    if (sceSifLoadModule("host0:bin/overlord.irx", total_len, overlord_boot_command) < 0) {
      MsgErr("loading overlord.irx <3> failed\n");
      exit(0);
    }
  } else {
    ASSERT_NOT_REACHED();
  }
  int rv = sceMcInit();
  if (rv < 0) {
    MsgErr("MC driver init failed %d\n", rv);
    exit(0);
  }
  printf("InitIOP OK\n");
}

int InitMachine() {
  // uVar2 = FUN_00116ec8(0x10);
  // heap_start = malloc(0x10);

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
    kinitheap(kdebugheap, Ptr<u8>(DEBUG_HEAP_START), jak3::DEBUG_HEAP_SIZE);
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
  if (MasterDebug != 0) {
    InitGoalProto();
  } else {
    ee::sceDeci2Disable();  // added
  }
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
  }
  return status;
}

int ShutdownMachine() {
  Msg(6, "kernel: machine shutdown (reason %d)\n", (int)MasterExit);

  StopIOP();
  ShutdownSound();
  CloseListener();

  ShutdownGoalProto();
  return 0;
}

u32 KeybdGetData(u32 /*_mouse*/) {
  return 0;
  // ASSERT_NOT_REACHED();
}

u32 MouseGetData(u32 /*_mouse*/) {
  // ASSERT_NOT_REACHED();
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
  lg::print("****** CALL TO kopen() ******\n");
  // sprintf(buffer, "host:%s", Ptr<String>(name)->data());
  if (!strcmp(sym_to_cstring(Ptr<Symbol4<u8>>(mode)), "read")) {
    // 0x1
    file_stream->file = ee::sceOpen(Ptr<String>(name)->data(), SCE_RDONLY);
  } else if (!strcmp(sym_to_cstring(Ptr<Symbol4<u8>>(mode)), "append")) {
    // new in jak 2!
    // 0x202
    file_stream->file = ee::sceOpen(Ptr<String>(name)->data(), SCE_CREAT | SCE_WRONLY);
  } else {
    // 0x602
    file_stream->file = ee::sceOpen(Ptr<String>(name)->data(), SCE_TRUNC | SCE_CREAT | SCE_WRONLY);
  }

  return fs;
}

void PutDisplayEnv(u32 alp) {
  // we can mostly ignore this, except for one value that sets the 'blackout' amount.
  auto* renderer = Gfx::GetCurrentRenderer();
  if (renderer) {
    renderer->set_pmode_alp(alp / 255.f);
  }
}

void aybabtu() {}

//// PC Stuff
void InitMachine_PCPort() {
  // PC Port added functions
  init_common_pc_port_functions(
      make_function_symbol_from_c,
      [](const char* name) {
        const auto result = intern_from_c(-1, 0, name);
        InternFromCInfo info{};
        info.offset = result.offset;
        return info;
      },
      make_string_from_c);

  make_function_symbol_from_c("__pc-set-levels", (void*)kmachine_extras::pc_set_levels);
  make_function_symbol_from_c("__pc-set-active-levels",
                              (void*)kmachine_extras::pc_set_active_levels);
  make_function_symbol_from_c("__pc-get-tex-remap", (void*)lookup_jak3_texture_dest_offset);
  make_function_symbol_from_c("pc-init-autosplitter-struct",
                              (void*)kmachine_extras::init_autosplit_struct);

  // discord rich presence
  make_function_symbol_from_c("pc-discord-rpc-update", (void*)kmachine_extras::update_discord_rpc);

  // debugging tools
  make_function_symbol_from_c("alloc-vagdir-names", (void*)kmachine_extras::alloc_vagdir_names);

  // external RPCs
  /*
  make_function_symbol_from_c("pc-fetch-external-speedrun-times",
                              (void*)pc_fetch_external_speedrun_times);
  make_function_symbol_from_c("pc-fetch-external-race-times", (void*)pc_fetch_external_race_times);
  make_function_symbol_from_c("pc-fetch-external-highscores", (void*)pc_fetch_external_highscores);
  make_function_symbol_from_c("pc-get-external-speedrun-time",
                              (void*)pc_get_external_speedrun_time);
  make_function_symbol_from_c("pc-get-external-race-time", (void*)pc_get_external_race_time);
  make_function_symbol_from_c("pc-get-external-highscore", (void*)pc_get_external_highscore);
  make_function_symbol_from_c("pc-get-num-external-speedrun-times",
                              (void*)pc_get_num_external_speedrun_times);
  make_function_symbol_from_c("pc-get-num-external-race-times",
                              (void*)pc_get_num_external_race_times);
  make_function_symbol_from_c("pc-get-num-external-highscores",
                              (void*)pc_get_num_external_highscores);
 */

  // speedrunning stuff
  make_function_symbol_from_c("pc-sr-mode-get-practice-entries-amount",
                              (void*)kmachine_extras::pc_sr_mode_get_practice_entries_amount);
  make_function_symbol_from_c("pc-sr-mode-get-practice-entry-name",
                              (void*)kmachine_extras::pc_sr_mode_get_practice_entry_name);
  make_function_symbol_from_c("pc-sr-mode-get-practice-entry-continue-point",
                              (void*)kmachine_extras::pc_sr_mode_get_practice_entry_continue_point);
  make_function_symbol_from_c(
      "pc-sr-mode-get-practice-entry-history-success",
      (void*)kmachine_extras::pc_sr_mode_get_practice_entry_history_success);
  make_function_symbol_from_c(
      "pc-sr-mode-get-practice-entry-history-attempts",
      (void*)kmachine_extras::pc_sr_mode_get_practice_entry_history_attempts);
  make_function_symbol_from_c(
      "pc-sr-mode-get-practice-entry-session-success",
      (void*)kmachine_extras::pc_sr_mode_get_practice_entry_session_success);
  make_function_symbol_from_c(
      "pc-sr-mode-get-practice-entry-session-attempts",
      (void*)kmachine_extras::pc_sr_mode_get_practice_entry_session_attempts);
  make_function_symbol_from_c("pc-sr-mode-get-practice-entry-avg-time",
                              (void*)kmachine_extras::pc_sr_mode_get_practice_entry_avg_time);
  make_function_symbol_from_c("pc-sr-mode-get-practice-entry-fastest-time",
                              (void*)kmachine_extras::pc_sr_mode_get_practice_entry_fastest_time);
  make_function_symbol_from_c("pc-sr-mode-record-practice-entry-attempt!",
                              (void*)kmachine_extras::pc_sr_mode_record_practice_entry_attempt);
  make_function_symbol_from_c("pc-sr-mode-init-practice-info!",
                              (void*)kmachine_extras::pc_sr_mode_init_practice_info);
  make_function_symbol_from_c("pc-sr-mode-get-custom-category-amount",
                              (void*)kmachine_extras::pc_sr_mode_get_custom_category_amount);
  make_function_symbol_from_c("pc-sr-mode-get-custom-category-name",
                              (void*)kmachine_extras::pc_sr_mode_get_custom_category_name);
  make_function_symbol_from_c(
      "pc-sr-mode-get-custom-category-continue-point",
      (void*)kmachine_extras::pc_sr_mode_get_custom_category_continue_point);
  make_function_symbol_from_c("pc-sr-mode-init-custom-category-info!",
                              (void*)kmachine_extras::pc_sr_mode_init_custom_category_info);
  make_function_symbol_from_c("pc-sr-mode-dump-new-custom-category",
                              (void*)kmachine_extras::pc_sr_mode_dump_new_custom_category);

  // setup string constants
  auto user_dir_path = file_util::get_user_config_dir();
  intern_from_c(-1, 0, "*pc-user-dir-base-path*")->value() =
      make_string_from_c(user_dir_path.string().c_str());
  auto settings_path = file_util::get_user_settings_dir(g_game_version);
  intern_from_c(-1, 0, "*pc-settings-folder*")->value() =
      make_string_from_c(settings_path.string().c_str());
  intern_from_c(-1, 0, "*pc-settings-built-sha*")->value() =
      make_string_from_c(build_revision().c_str());
}
// End PC Stuff

void InitMachineScheme() {
  make_function_symbol_from_c("put-display-env", (void*)PutDisplayEnv);
  make_function_symbol_from_c("syncv", (void*)sceGsSyncV);
  make_function_symbol_from_c("sync-path", (void*)sceGsSyncPath);
  make_function_symbol_from_c("reset-path", (void*)sceGsResetPath);
  make_function_symbol_from_c("reset-graph", (void*)sceGsResetGraph);
  make_function_symbol_from_c("dma-sync", (void*)sceDmaSync);
  make_function_symbol_from_c("gs-put-imr", (void*)sceGsPutIMR);
  make_function_symbol_from_c("gs-get-imr", (void*)sceGsGetIMR);
  make_function_symbol_from_c("gs-store-image", (void*)sceGsExecStoreImage);
  make_function_symbol_from_c("flush-cache", (void*)FlushCache);
  make_function_symbol_from_c("cpad-open", (void*)CPadOpen);
  make_function_symbol_from_c("cpad-get-data", (void*)CPadGetData);
  make_function_symbol_from_c("mouse-get-data", (void*)MouseGetData);
  make_function_symbol_from_c("keybd-get-data", (void*)KeybdGetData);
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
  make_function_symbol_from_c("aybabtu", (void*)aybabtu);  // was nothing

  InitMachine_PCPort();

  InitSoundScheme();
  intern_from_c(-1, 0, "*stack-top*")->value() = 0x7f00000;
  intern_from_c(-1, 0, "*stack-base*")->value() = 0x7ffffff;
  intern_from_c(-1, 0, "*stack-size*")->value() = 0x100000;
  intern_from_c(-1, 0, "*kernel-boot-message*")->value() =
      intern_from_c(-1, 0, DebugBootMessage).offset;
  intern_from_c(-1, 0, "*user*")->value() = make_string_from_c(DebugBootUser);
  if (DiskBoot) {
    intern_from_c(-1, 0, "*kernel-boot-mode*")->value() = intern_from_c(-1, 0, "boot").offset;
  }
  if (strcmp(DebugBootLevel, "#f") == 0) {
    intern_from_c(-1, 0, "*kernel-boot-level*")->value() = s7.offset;
  } else {
    ASSERT_NOT_REACHED();
  }
  intern_from_c(-1, 0, "*kernel-boot-art-group*")->value() = make_string_from_c(DebugBootArtGroup);

  if (DiskBoot != 0) {
    *EnableMethodSet = *EnableMethodSet + 1;
    load_and_link_dgo_from_c("game", kglobalheap,
                             LINK_FLAG_OUTPUT_LOAD | LINK_FLAG_EXECUTE | LINK_FLAG_PRINT_LOGIN,
                             0x400000, true);
    *EnableMethodSet = *EnableMethodSet + -1;
    using namespace jak3_symbols;

    kernel_packages->value() =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE - 1).cast<u32>()),
                 make_string_from_c("engine"), kernel_packages->value());
    kernel_packages->value() =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE - 1).cast<u32>()),
                 make_string_from_c("art"), kernel_packages->value());
    kernel_packages->value() =
        new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE - 1).cast<u32>()),
                 make_string_from_c("common"), kernel_packages->value());
    printf("calling play-boot!\n");
    call_goal_function_by_name("play-boot");  // new function for jak2!
  }
}

}  // namespace jak3
