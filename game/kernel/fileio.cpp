/*!
 * @file fileio.cpp
 * GOAL Low-Level File I/O and String Utilities
 * DONE!
 */

#include <cassert>
#include <cstring>
#include <cstdio>
#include "game/sce/stubs.h"
#include "fileio.h"
#include "kprint.h"

namespace {
// buffer for file paths.  This might be static char buffer[512]. Maybe 633 is the line number?
char buffer_633[512];
}  // namespace

void fileio_init_globals() {
  memset(buffer_633, 0, 512);
}

using namespace ee;

/*!
 * Return pointer to null terminator of string.
 * const is for losers.
 * DONE, EXACT
 */
char* strend(char* str) {
  while (*str)
    str++;
  return str;
}

/*!
 * An implementation of Huffman decoding.
 * In this limited decoder, your data must have lower two bits equal to zero.
 * @param loc_ptr pointer to pointer to data to read (will be modified to point to next word)
 * @return decoded word
 * UNUSED, EXACT
 */
u32 ReadHufWord(u8** loc_ptr) {
  u8* loc = *loc_ptr;      // pointer to data to read
  u32 value = *(u32*)loc;  // read word
  u8* next_loc = loc + 1;  // next data to read
  u32 length = value & 3;  // length of word is stored in lower two bits.
  switch (length) {
    case 0:  // already all set.
      break;

    case 1:
      value = (value & 0xfc) | (loc[1] << 8);
      next_loc = loc + 2;
      break;

    case 2:
      value = (value & 0xfc) | (loc[1] << 8) | (loc[2] << 0x10);
      next_loc = loc + 3;
      break;

    case 3:
      value = (value & 0xfc) | (loc[1] << 8) | (loc[2] << 0x10) | (loc[3] << 0x18);
      next_loc = loc + 4;
      break;

    default:
      assert(false);
  }

  // update location pointer
  *loc_ptr = next_loc;
  return value;
}

/*!
 * Copy a string from src to dst. The null terminator is copied too.
 * This is identical to normal strcpy.
 * DONE, EXACT
 */
void kstrcpy(char* dst, const char* src) {
  char* dst_ptr = dst;
  const char* src_ptr = src;

  while (*src_ptr != 0) {
    *dst_ptr = *src_ptr;
    src_ptr++;
    dst_ptr++;
  }
  *dst_ptr = 0;
}

/*!
 * Copy a string from src to dst, making all letters upper case.
 * The null terminator is copied too.
 * DONE, EXACT
 */
void kstrcpyup(char* dst, const char* src) {
  while (*src) {
    char c = *src;
    if (c >= 'a' && c <= 'z') {  // A-Z,a-z
      c -= 0x20;
    }
    *dst = c;
    dst++;
    src++;
  }
  *dst = 0;
}

/*!
 * Concatenate two strings.  Src is added to dest.
 * The new string is null terminated.  No bounds checking is done.
 * DONE, EXACT
 */
void kstrcat(char* dest, const char* src) {
  // seek to end of first string
  while (*dest) {
    dest++;
  }
  // copy second string
  while (*src) {
    *dest = *src;
    src++;
    dest++;
  }
  // null terminate
  *dest = 0;
}

/*!
 * Concatenate two strings with a maximum length for the resulting string
 * The maximum length should be larger than the length of the original string.
 * The resulting string will be truncated when it reaches the given length.
 * The null terminator is added, but doesn't count toward the length.
 * DONE, EXACT
 */
void kstrncat(char* dest, const char* src, s32 count) {
  // seek to null terminator of first string, count length
  s32 i = 0;
  while (*dest) {
    dest++;
    i++;
  }

  // append second string, not exceeding length
  while (*src && (i < count)) {
    *dest = *src;
    src++;
    dest++;
    i++;
  }

  // null terminate
  *dest = 0;
}

/*!
 * Insert the pad char at the beginning of a string, count times.
 * DONE, EXACT
 */
char* kstrinsert(char* str, char pad, s32 count) {
  // shift string+null terminator to the right.
  s32 len = strlen(str);
  while (len > -1) {
    str[len + count] = str[len];
    len--;
  }

  // pad
  len = 0;
  while (len < count) {
    str[len++] = pad;
  }
  return str;
}

/*!
 * Get filename from path.
 * This function is renamed to basename_goal so it doesn't conflict with "basename" that is
 * already defined on my computer.
 * For example:
 *   a/b/c.e will return c.e
 *   a\b\c.e will return c.e
 *   asdf.asdf will return asdf.asdf
 *   DONE, EXACT
 */
char* basename_goal(char* s) {
  char* input = s;
  char* pt = s;

  // seek to end
  for (;;) {
    char c = *pt;
    if (c) {
      pt++;
    } else {
      break;
    }
  }

  // back up...
  for (;;) {
    if (pt < input) {
      return input;
    }
    pt--;
    char c = *pt;
    // until we hit a slash.
    if (c == '\\' || c == '/') {  // slashes
      return pt + 1;              // and return one past
    }
  }
}

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
    } else if (!strncmp(name, "$DATA/", 6)) {
      result = MakeFileName(DATA_FILE_TYPE, name + 6, 0);
    } else if (!strncmp(name, "$CODE/", 6)) {
      result = MakeFileName(CODE_FILE_TYPE, name + 6, 0);
    } else if (!strncmp(name, "$RES/", 5)) {
      result = MakeFileName(RES_FILE_TYPE, name + 5, 0);
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
 */
char* MakeFileName(int type, const char* name, int new_string) {
  // start with network filesystem
  kstrcpy(buffer_633, "host:");
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
    sprintf(buf, "%sdata/%s.go", prefix, name);
  } else if (type == TX_PAGE_FILE_TYPE) {
    // Texture Page
    // part of level files, so it has a version number.
    sprintf(buf, "%sdata/texture-page%d/%s.go", prefix, TX_PAGE_VERSION, name);
  } else if (type == JA_FILE_TYPE) {
    // Art JA (joint animation? no idea)
    // part of level files, so it has a version number
    sprintf(buf, "%sdd_next/artdata%d/%s-ja.go", prefix, ART_FILE_VERSION, name);
  } else if (type == JG_FILE_TYPE) {
    // Art JG (joint group? no idea)
    // part of level files, so it has a version number
    sprintf(buf, "%sdd_next/artdata%d/%s-jg.go", prefix, ART_FILE_VERSION, name);
  } else if (type == MA_FILE_TYPE) {
    // Art MA (??)
    // part of level files, so it has a version number
    sprintf(buf, "%sdd_next/artdata%d/%s-ma.go", prefix, ART_FILE_VERSION, name);
  } else if (type == MG_FILE_TYPE) {
    // Art MG (??)
    // part of level files, so it has a version number
    sprintf(buf, "%sdd_next/artdata%d/%s-mg.go", prefix, ART_FILE_VERSION, name);
  } else if (type == TG_FILE_TYPE) {
    // unused, DATA TG file
    sprintf(buf, "%sdata/%s-tg.go", prefix, name);
  } else if (type == LEVEL_FILE_TYPE) {
    // Level main file.
    // part of level files, so it has a version number (a high one, 30!)
    sprintf(buf, "%sdata/level%d/%s-bt.go", prefix, LEVEL_FILE_VERSION, name);
  } else if (type == ART_GROUP_FILE_TYPE) {
    // Level art group file.
    // part of level files, so it has a version number
    sprintf(buf, "%sdata/art-group%d/%s-ag.go", prefix, ART_FILE_VERSION, name);
  } else if (type == VS_FILE_TYPE) {
    // Level vs file, unused, unknown
    // possibly early visibility file?
    sprintf(buf, "%sdata/level%d/%s-vs.go", prefix, LEVEL_FILE_VERSION, name);
  } else if (type == TX_FILE_TYPE) {
    // Resource?  TX file?  some sort of texture?
    sprintf(buf, "%sdata/res%d/%s-tx.go", prefix, RES_FILE_VERSION, name);
  } else if (type == VS_BIN_FILE_TYPE) {
    // level VS bin
    // perhaps another format of early visibility data
    sprintf(buf, "%sdata/level%d/%s-vs.bin", prefix, LEVEL_FILE_VERSION, name);
  } else if (type == DGO_TXT_FILE_TYPE) {
    // Text file in the DGO directory?
    // Could have contained a list of files inside the DGO.
    sprintf(buf, "%sdata/dgo%d/%s.txt", prefix, DGO_FILE_VERSION, name);
  } else if (type == LEVEL_WITH_EXTENSION_FILE_TYPE) {
    // Level file, but with an extension already on it.
    sprintf(buf, "%sdata/level%d/%s", prefix, LEVEL_FILE_VERSION, name);
  } else if (type == DATA_DGO_FILE_TYPE) {
    // data DGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "%sdata/dgo%d/%s.dgo", prefix, DGO_FILE_VERSION, name);
  } else if (type == GAME_DGO_FILE_TYPE) {
    // game DGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "game/dgo%d/%s.dgo", DGO_FILE_VERSION, name);
  } else if (type == DATA_CGO_FILE_TYPE) {
    // data CGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "%sdata/dgo%d/%s.cgo", prefix, DGO_FILE_VERSION, name);
  } else if (type == GAME_CGO_FILE_TYPE) {
    // game CGO file (unused, all DGO/CGOs loaded through IOP)
    sprintf(buf, "game/dgo%d/%s.cgo", DGO_FILE_VERSION, name);
  } else if (type == CNT_FILE_TYPE) {
    // game cnt file (continue point?)
    sprintf(buf, "%sdata/res%d/game-cnt.go", prefix, RES_FILE_VERSION);
  } else if (type == RES_FILE_TYPE) {
    // RES go file?
    sprintf(buf, "%sdata/res%d/%s.go", prefix, RES_FILE_VERSION, name);
  } else if (type == REFPLANT_FILE_TYPE) {
    // REFPLANT? no idea
    static char nextDir[] = "/";
    sprintf(buf, "%sconfig_data/refplant/%s", nextDir, name);
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

/*!
 * Does the file exist?  No.  It doesn't.
 * @return 0 always, even if the file exists.
 * DONE, EXACT, UNUSED
 */
u32 FileExists(const char* name) {
  (void)name;
  return 0;
}

/*!
 * Does nothing. Likely is supposed to delete a file.
 * @param name
 * DONE, EXACT, UNUSED
 */
void FileDelete(const char* name) {
  (void)name;
}

/*!
 * Does nothing. Likely is supposed to copy a file.
 * @param a
 * @param b
 * DONE, EXACT, UNUSED
 */
void FileCopy(const char* a, const char* b) {
  (void)a;
  (void)b;
}

/*!
 * Determine the file length in bytes.
 * DONE, EXACT
 */
s32 FileLength(char* filename) {
  s32 fd = sceOpen(filename, SCE_RDONLY);
  if (fd < 0) {
    MsgErr("dkernel: file length !open \'%s\' (%d)\n", filename, fd);
    sceClose(fd);
    return 0xfffffffb;
  } else {
    s32 rv = sceLseek(fd, 0, SCE_SEEK_END);
    sceClose(fd);
    return rv;
  }
}

/*!
 * Load a file into memory
 * @param name : file name
 * @param heap : heap to allocate into, if memory is null
 * @param memory : memory to load into. If null, allocates on the given kheap (with 64 extra bytes)
 * @param malloc_flags : flags for the kmalloc
 * @param size_out : file size is written here, if it's not null
 * @return pointer to file data
 * DONE, EXACT
 */
Ptr<u8> FileLoad(char* name, Ptr<kheapinfo> heap, Ptr<u8> memory, u32 malloc_flags, s32* size_out) {
  s32 fd = sceOpen(name, SCE_RDONLY);
  if (fd < 0) {
    MsgErr("dkernel: file read !open \'%s\' (%d)\n", name, fd);
    sceClose(fd);
    return Ptr<u8>(0xfffffffb);
  }

  // determine size
  s32 initial_pos = sceLseek(fd, 0, SCE_SEEK_CUR);
  s32 size = sceLseek(fd, 0, SCE_SEEK_END);
  sceLseek(fd, initial_pos, SCE_SEEK_SET);

  if (size > 0) {
    if (memory.offset == 0) {
      memory = kmalloc(heap, size + 0x40, malloc_flags, name);
    }
    if (memory.offset == 0) {
      MsgErr("dkernel: mem full for file read: '%s' (%d bytes)\n", name, size);
      return Ptr<u8>(0xfffffffd);
    }

    s32 read_amount = sceRead(fd, memory.c(), size);
    if (read_amount == size) {
      sceClose(fd);
      if (size_out)
        *size_out = size;
      return memory;
    } else {
      MsgErr("dkernel: can't read full file (%d of %d): '%s'\n", read_amount, size, name);
      sceClose(fd);
      return Ptr<u8>(0xfffffffb);
    }
  } else {
    return Ptr<u8>(0);
  }
}

/*!
 * Write a file.
 * DONE, EXACT
 */
s32 FileSave(char* name, u8* data, s32 size) {
  s32 fd = sceOpen(name, SCE_WRONLY | SCE_TRUNC | SCE_CREAT);
  if (fd < 0) {
    MsgErr("dkernel: file write !open '%s'\n", name);
    sceClose(fd);
    return 0xfffffffa;
  }

  if (size != 0) {
    s32 written = sceWrite(fd, data, size);
    if (written != size) {
      MsgErr("dkernel: can't write full file '%s'\n", name);
      sceClose(fd);
      return 0xfffffffa;
    }
  }

  sceClose(fd);
  return 0;
}