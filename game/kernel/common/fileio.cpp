#include "fileio.h"

#include <cstring>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kprint.h"
#include "game/sce/sif_ee.h"

using namespace ee;

// buffer for file paths.  This might be static char buffer[512]. Maybe 633 is the line number?
char buffer_633[512];

void fileio_init_globals() {
  memset(buffer_633, 0, 512);
}

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
      ASSERT(false);
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

  /* Original code, has memory bug.
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
   */

  // back up...
  for (;;) {
    if (pt <= input) {
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
    // in jak 3, this became a loop over smaller writes for some reason.
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
