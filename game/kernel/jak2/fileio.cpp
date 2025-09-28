#include "fileio.h"

#include <cstdio>
#include <cstring>

#include "common/versions/versions.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kmalloc.h"

namespace jak2 {

/*!
 * Turn file name into file's path.
 * DONE, EXACT
 */
char* DecodeFileName(const char* name) {
  char* result;
  // names starting with $ are special:
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
    // if no special prefix is given, assume $CODE
    result = MakeFileName(CODE_FILE_TYPE, name, 0);
  }
  return result;
}

/*!
 * Build a file name based on type.
 * @param type: the file type.
 * @param name: the file name
 * @param new_string: if true, allocate a new global string for file name.
 *  will otherwise use a static buffer.
 * DONE, Had unused int, char*, and MakeFileNameInfo params.
 * PC PORT NOTE : Changed some paths so that they work for us (namely, got rid of 'host')
 */
char* MakeFileName(int type, const char* name, int new_string) {
  using namespace versions::jak2;
  // start with network filesystem
  // kstrcpy(buffer_633, "host:");
  kstrcpy(buffer_633, "");
  char* buf = strend(buffer_633);

  // prefix to build directory
  char prefix[64];
  kstrcpy(prefix, FOLDER_PREFIX);

  // build file name
  if (type == LISTENER_TO_KERNEL_FILE_TYPE) {
    kstrcpy(buf,
            "kernel/LISTENERTOKERNEL");  // unused (I guess this is an old method to transfer data?)
  } else if (type == KERNEL_TO_LISTENER_FILE_TYPE) {
    kstrcpy(buf,
            "kernel/KERNELTOLISTENER");  // unused (I guess this is an old method to transfer data?)
  } else if (type == CODE_FILE_TYPE) {
    sprintf(buf, "game/obj/%s.o", name);  // game object file (CODE)
  } else if (type == GAMEPAD_FILE_TYPE) {
    sprintf(buffer_633, "pad:0");  // I guess the gamepad could be opened like a file at some point?
  } else if (type == LISTENER_TO_KERNEL_LOCK_FILE_TYPE) {
    kstrcpy(buf, "kernel/LISTENERTOKERNEL_LOCK");  // unused (likely used for LISTENERTOKERNEL?)
  } else if (type == KERNEL_TO_LISTENER_LOCK_FILE_TYPE) {
    kstrcpy(buf, "kernel/KERNELTOLISTENER_LOCK");  //  unused (likley used for KERNELTOLISTENER?)
  } else if (type == IOP_MODULE_FILE_TYPE) {       // IOP module, overwrite the whole thing.
    // this is unused, even by the remaining code to load IOP modules from the network.
    // note this uses host0, which I believe is the PS2 TOOL's built in Linux SBC.
    sprintf(buffer_633, "host0:/usr/local/sce/iop/modules/%s.irx", name);
  } else if (type == DATA_FILE_TYPE) {
    // GOAL object file, but containing data instead of code.
    // likely packed by a tool that isn't the GOAL compiler.
    // sprintf(buf, "%sfinal/%s.go", prefix, name);
    sprintf(buf, "%sout/jak2/obj/%s.go", prefix, name);
  } else if (type == TX_PAGE_FILE_TYPE) {
    // Texture Page
    // part of level files, so it has a version number.
    // sprintf(buf, "%sdata/texture-page%d/%s.go", prefix, TX_PAGE_VERSION, name);
    sprintf(buf, "%sout/jak2/obj/%s.go", prefix, name);
  } else if (type == JA_FILE_TYPE) {
    // Art JA (joint animation? no idea)
    // part of level files, so it has a version number
    sprintf(buf, "%sdb/artdata%d/%s-ja.go", prefix, ART_FILE_VERSION, name);
  } else if (type == JG_FILE_TYPE) {
    // Art JG (joint group? no idea)
    // part of level files, so it has a version number
    sprintf(buf, "%sdb/artdata%d/%s-jg.go", prefix, ART_FILE_VERSION, name);
  } else if (type == MA_FILE_TYPE) {
    // Art MA (??)
    // part of level files, so it has a version number
    sprintf(buf, "%sdb/artdata%d/%s-ma.go", prefix, ART_FILE_VERSION, name);
  } else if (type == MG_FILE_TYPE) {
    // Art MG (??)
    // part of level files, so it has a version number
    sprintf(buf, "%sdb/artdata%d/%s-mg.go", prefix, ART_FILE_VERSION, name);
  } else if (type == TG_FILE_TYPE) {
    // unused, DATA TG file
    sprintf(buf, "%sdb/%s-tg.go", prefix, name);
  } else if (type == LEVEL_FILE_TYPE) {
    // Level main file.
    // part of level files, so it has a version number (a high one, 30!)
    sprintf(buf, "%sdb/level%d/%s-bt.go", prefix, LEVEL_FILE_VERSION, name);
    // -ag missing in jak2????
    //  } else if (type == ART_GROUP_FILE_TYPE) {
    //    // Level art group file.
    //    // part of level files, so it has a version number
    //    sprintf(buf, "%sdata/art-group%d/%s-ag.go", prefix, ART_FILE_VERSION, name);
  } else if (type == VS_FILE_TYPE) {
    // Level vs file, unused, unknown
    // possibly early visibility file?
    sprintf(buf, "%sfinal/level%d/%s-vs.go", prefix, LEVEL_FILE_VERSION, name);
  } else if (type == TX_FILE_TYPE) {
    // Resource?  TX file?  some sort of texture?
    sprintf(buf, "%sfinal/res%d/%s-tx.go", prefix, 1, name);
  } else if (type == VS_BIN_FILE_TYPE) {
    // level VS bin
    // perhaps another format of early visibility data
    sprintf(buf, "%sfinal/level%d/%s-vs.bin", prefix, LEVEL_FILE_VERSION, name);
  } else if (type == DGO_TXT_FILE_TYPE) {
    // Text file in the DGO directory?
    // Could have contained a list of files inside the DGO.
    sprintf(buf, "%sfinal/dgo%d/%s.txt", prefix, DGO_FILE_VERSION, name);
  } else if (type == LEVEL_WITH_EXTENSION_FILE_TYPE) {
    // Level file, but with an extension already on it.
    sprintf(buf, "%sfinal/level%d/%s", prefix, LEVEL_FILE_VERSION, name);
  } else if (type == DATA_DGO_FILE_TYPE) {
    // data DGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "%sfinal/dgo%d/%s.dgo", prefix, DGO_FILE_VERSION, name);
  } else if (type == GAME_DGO_FILE_TYPE) {
    // game DGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "game/dgo%d/%s.dgo", DGO_FILE_VERSION, name);
  } else if (type == DATA_CGO_FILE_TYPE) {
    // data CGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "%sfinal/dgo%d/%s.cgo", prefix, DGO_FILE_VERSION, name);
  } else if (type == GAME_CGO_FILE_TYPE) {
    // game CGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "game/dgo%d/%s.cgo", DGO_FILE_VERSION, name);
  } else if (type == CNT_FILE_TYPE) {
    // game cnt file. leftover from jak 1.
    sprintf(buf, "%sfinal/res%d/game-cnt.go", prefix, 1);
  } else if (type == RES_FILE_TYPE) {
    // RES go file?
    sprintf(buf, "%sfinal/res%d/%s.go", prefix, 1, name);
  } else if (type == SND_BNK_FILE_TYPE) {
    // sound bank
    sprintf(buf, "%sfinal/sound%d/%s.bnk", prefix, 1, name);  // v1
  } else if (type == MUSIC_BNK_FILE_TYPE) {
    // sound bank
    sprintf(buf, "%sfinal/music%d/%s.bnk", prefix, 1, name);  // v1
  } else if (type == VAG_FILE_TYPE) {
    // vagwad file (why the 2?)
    sprintf(buf, "%sfinal/vagwad2/%s.%s", prefix, name, "<INVALID>");  // v1, memory bug here
  } else if (type == MISC_FILE_TYPE) {
    // sound bank
    sprintf(buf, "%sfinal/misc/%s", prefix, name);
  } else if (type == MAP_FILE_TYPE) {
    // map
    sprintf(buf, "%sfinal/map%d/%s", prefix, 1, name);  // v1
  } else if (type == REFPLANT_FILE_TYPE) {
    // REFPLANT? no idea. it's different in jak2.
    sprintf(buf, "%sdb/refplant/%s", prefix, name);
  } else if (type == ISO_FILE_TYPE) {
    sprintf(buffer_633, "/out/iso/%s", name);
  } else {
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
}  // namespace jak2
