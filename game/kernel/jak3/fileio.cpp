#include "fileio.h"

#include <cstring>

#include "game/kernel/common/fileio.h"

namespace jak3 {

// This file naming system was used only in development, as it loads files from the development PC
// connected to the PS2 dev-kit.
// My theory is that the developers would use this to debug their level/art tools. They could use
// these file names to quickly load in new files and see if they worked correctly with the renderer,
// without needing to create/load entire new DGO files.
// They've been adding to this file over all 3 games, so I believe it is more than just a leftover
// from early jak 1.

/*!
 * Convert a file-name like $CODE/thing to the appropriate file path on the development computer.
 */
char* DecodeFileName(const char* name) {
  char* result;

  if (name[0] == '$') {
    if (!strncmp(name, "$TEXTURE/", 9)) {
      result = MakeFileName(TX_PAGE_FILE_TYPE, name + 9, 0);
    } else if (!strncmp(name, "$ART_GROUP/", 0xb)) {
      result = MakeFileName(ART_GROUP_FILE_TYPE, name + 0xb, 0);
    } else if (!strncmp(name, "$LEVEL/", 7)) {
      int len = (int)strlen(name);
      if (name[len - 4] == '.') {
        result = MakeFileName(LEVEL_WITH_EXTENSION_FILE_TYPE, name + 7, 0);
      } else {
        // level files can omit a file type if desired
        result = MakeFileName(LEVEL_FILE_TYPE, name + 7, 0);
      }
    } else if (!strncmp(name, "$FINAL/", 6)) {  // in jak2, this is FINAL instead of DATA
      result = MakeFileName(DATA_FILE_TYPE, name + 6, 0);
    } else if (!strncmp(name, "$CODE/", 6)) {
      result = MakeFileName(CODE_FILE_TYPE, name + 6, 0);
    } else if (!strncmp(name, "$RES/", 5)) {
      result = MakeFileName(RES_FILE_TYPE, name + 5, 0);
    } else if (!strncmp(name, "$MISC/", 6)) {
      result = MakeFileName(MISC_FILE_TYPE, name + 6, 0);
    } else if (!strncmp(name, "$MAP/", 5)) {
      result = MakeFileName(MAP_FILE_TYPE, name + 5, 0);
    } else if (!strncmp(name, "$ISO/", 5)) {
      result = MakeFileName(ISO_FILE_TYPE, name + 5, 0);
    } else {
      printf("[ERROR] DecodeFileName: UNKNOWN FILE NAME %s\n", name);
      result = nullptr;
    }
  } else {
    // no prefix. Treat this as a code file
    return MakeFileName(CODE_FILE_TYPE, name, 0);
  }
  return result;
}

/*!
 * Create a file name that looks in the appropriate folder in ND's development environment.
 * This is a bit of dumping ground for all possible files they'd load.
 */
char* MakeFileName(int type, const char* name, int new_string) {
  using namespace versions::jak3;
  // start with network filesystem
  // kstrcpy(buffer_633, "host:");
  kstrcpy(buffer_633, "");
  char* buf = strend(buffer_633);

  // prefix to build directory
  char prefix[64];
  kstrcpy(prefix, FOLDER_PREFIX);

  switch (type) {
      // Unused files that could be used to exchange data between the dev PS2 and the GOAL compiler.
    case LISTENER_TO_KERNEL_FILE_TYPE:
      kstrcpy(buf, "kernel/LISTENERTOKERNEL");
      break;
    case KERNEL_TO_LISTENER_FILE_TYPE:
      kstrcpy(buf, "kernel/KERNELTOLISTENER");
      break;

      // A GOAL object file containing code built from the GOAL compiler.
    case CODE_FILE_TYPE:
      sprintf(buf, "game/obj/%s.o", name);
      break;

      // Unused, opening the gamepad as a file.
    case GAMEPAD_FILE_TYPE:
      sprintf(buffer_633, "pad:0");
      break;

      // Locks for the unused kernel/listener interface. (funny that they added this after...)
    case LISTENER_TO_KERNEL_LOCK_FILE_TYPE:
      kstrcpy(buf, "kernel/LISTENERTOKERNEL_LOCK");
      break;
    case KERNEL_TO_LISTENER_LOCK_FILE_TYPE:
      kstrcpy(buf, "kernel/KERNELTOLISTENER_LOCK");
      break;

      // Host0 IOP modules (stored on the linux SBC inside the dev ps2 itself!)
    case IOP_MODULE_FILE_TYPE:  // 8
      sprintf(buffer_633, "host0:/usr/local/sce/iop/modules/%s.irx", name);
      break;

      // plain GOAL data object file
    case DATA_FILE_TYPE:  // 0x20
      // sprintf(buf, "%sfinal/%s.go", prefix, name);
      sprintf(buf, "%sout/jak3/obj/%s.go", prefix, name);
      break;

      // texture page
    case TX_PAGE_FILE_TYPE:  // 0x21
      // sprintf(buf, "%sdata/texture-page%d/%s.go", prefix, TX_PAGE_VERSION, name);
      sprintf(buf, "%sout/jak3/obj/%s.go", prefix, name);
      break;

      // joint animation
    case JA_FILE_TYPE:  // 0x22
      sprintf(buf, "%sdb/artdata%d/%s-ja.go", prefix, ART_FILE_VERSION, name);
      break;

      // joint geo (skeleton)
    case JG_FILE_TYPE:  // 0x23
      sprintf(buf, "%sdb/artdata%d/%s-jg.go", prefix, ART_FILE_VERSION, name);
      break;

      // mesh animation (unused)
    case MA_FILE_TYPE:  // 0x24
      sprintf(buf, "%sdb/artdata%d/%s-ma.go", prefix, ART_FILE_VERSION, name);
      break;

      // likely art-mesh-geo, and unused. Maybe was used before MERC?
    case MG_FILE_TYPE:  // 0x25
      sprintf(buf, "%sdb/artdata%d/%s-mg.go", prefix, ART_FILE_VERSION, name);
      break;

      // text group perhaps?
    case TG_FILE_TYPE:
      sprintf(buf, "%sdb/%s-tg.go", prefix, name);
      break;

      // level file
    case LEVEL_FILE_TYPE:  // 0x27
      sprintf(buf, "%sdb/level%d/%s-bt.go", prefix, LEVEL_FILE_VERSION, name);
      break;

      // Everybody's favorite "art group" file. Container of different art.
    case ART_GROUP_FILE_TYPE:  // 0x30
      // sprintf(buf, "%sfinal/art-group%d/%s-ag.go", prefix, ART_FILE_VERSION, name);
      sprintf(buf, "%sout/jak3/obj/%s.go", prefix, name);
      break;

      // GOAL data object file containing visibility data. This likely contained the visibility data
      // that's included in the BSP file.
    case VS_FILE_TYPE:  // 0x31
      sprintf(buf, "%sfinal/level%d/%s-vs.go", prefix, LEVEL_FILE_VERSION, name);
      break;

      // GOAL data object file containing text. Likely the same format as the .TXT in final ISOs.
    case TX_FILE_TYPE:  // 0x32
      sprintf(buf, "%sfinal/res%d/%s-tx.go", prefix, 1, name);
      break;

      // Binary format visibility. Likely the format of Jak 1's .VIS files.
    case VS_BIN_FILE_TYPE:  // 0x33
      sprintf(buf, "%sfinal/level%d/%s-vs.bin", prefix, LEVEL_FILE_VERSION, name);
      break;

      // DGO description files. These contain a list of files inside each DGO.
    case DGO_TXT_FILE_TYPE:  // 0x34
      sprintf(buf, "%sfinal/dgo%d/%s.txt", prefix, DGO_FILE_VERSION, name);
      break;

      // Level file! but you have to provide the extension.
    case LEVEL_WITH_EXTENSION_FILE_TYPE:  // 0x35
      sprintf(buf, "%sfinal/level%d/%s", prefix, LEVEL_FILE_VERSION, name);
      break;

      // DGO and CGO files. These can exist in either final/ or game/
    case DATA_DGO_FILE_TYPE:  // 0x36
      sprintf(buf, "%sfinal/dgo%d/%s.dgo", prefix, DGO_FILE_VERSION, name);
      break;
    case GAME_DGO_FILE_TYPE:  // 0x37
      sprintf(buf, "game/dgo%d/%s.dgo", DGO_FILE_VERSION, name);
      break;
    case DATA_CGO_FILE_TYPE:  // 0x38
      sprintf(buf, "%sfinal/dgo%d/%s.cgo", prefix, DGO_FILE_VERSION, name);
      break;
    case GAME_CGO_FILE_TYPE:  // 0x39
      sprintf(buf, "game/dgo%d/%s.cgo", DGO_FILE_VERSION, name);
      break;

      // Jak 1 had a weird game-cnt.gco file containing the total number of orbs/cells.
    case CNT_FILE_TYPE:  // 0x3a
      sprintf(buf, "%sfinal/res%d/game-cnt.go", prefix, 1);
      break;

      // Any res file with .go extension.
    case RES_FILE_TYPE:  // 0x3b
      sprintf(buf, "%sfinal/res%d/%s.go", prefix, 1, name);
      break;

      // sound bank (sound effects)
    case SND_BNK_FILE_TYPE:                                     // 0x3c
      sprintf(buf, "%sfinal/sound%d/%s.bnk", prefix, 1, name);  // v1
      break;

      // music file
    case MUSIC_BNK_FILE_TYPE:                                   // 0x3d
      sprintf(buf, "%sfinal/music%d/%s.bnk", prefix, 1, name);  // v1
      break;

      // vag file, but it probably doesn't work due to the file extension.
    case VAG_FILE_TYPE:  // 0x3e
      // interestingly, jak 2 used vagwad2, but jak 3 doesn't. But the memory bug is still there.
      sprintf(buf, "%sfinal/vagwad/%s.%s", prefix, name, "<INVALID>");  // v1, memory bug here
      break;

      // whatever you want.
    case MISC_FILE_TYPE:  // 0x3f
      sprintf(buf, "%sfinal/misc/%s", prefix, name);
      break;

      // possible minimap/bigmap data
    case MAP_FILE_TYPE:
      sprintf(buf, "%sfinal/map%d/%s", prefix, 1, name);  // v1
      break;

      // jak 3 cloth animation file.
    case CL_FILE_TYPE:  // 0x41
      sprintf(buf, "%sdb/artdata%d/%s-cl.go", prefix, ART_FILE_VERSION, name);
      break;

      // no idea
    case REFPLANT_FILE_TYPE:  // 0x301
      sprintf(buf, "%sdb/refplant/%s", prefix, name);
      break;
    default:
      printf("UNKNOWN FILE TYPE %d\n", type);
  }

  char* result;
  if (!new_string) {
    // return pointer to static filename buffer
    result = buffer_633;
  } else {
    // or create a new string on the global heap.
    int l = (int)strlen(buffer_633);
    result = (char*)kmalloc(kglobalheap, l + 1, 0, "filename").c();
    kstrcpy(result, buffer_633);
  }

  return result;
}

}  // namespace jak3
