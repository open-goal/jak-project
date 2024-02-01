#include "kprint.h"

#include <cmath>
#include <cstdarg>
#include <cstdio>
#include <cstring>

#include "common/cross_os_debug/xdbg.h"
#include "common/listener_common.h"
#include "common/log/log.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kscheme.h"

// Pointer set to something in the middle of the output buffer, if there is something in the buffer.
Ptr<u8> OutputPending;

// Pointer set to something in the middle of the print buffer, if there is something in the buffer
Ptr<u8> PrintPending;

// Size of incoming message.
s32 MessCount;

// Pointer to message buffer, the compiler to target buffer
Ptr<u8> MessBufArea;

// Pointer to the output buffer, the runtime to compiler buffer
Ptr<u8> OutputBufArea;

// Pointer to print buffer, the buffer for printing and string formatting.
Ptr<u8> PrintBufArea;

// integer printing conversion table
char ConvertTable[16];

// buffer for sending an "acknowledge" message to the compiler
char AckBufArea[40];

/*!
 * Initialize global variables for kprint
 */
void kprint_init_globals_common() {
  OutputPending.offset = 0;
  PrintPending.offset = 0;
  MessCount = 0;
  MessBufArea.offset = 0;
  OutputBufArea.offset = 0;
  PrintBufArea.offset = 0;
  memcpy(ConvertTable, "0123456789abcdef", 16);
  memset(AckBufArea, 0, sizeof(AckBufArea));
}

/*!
 * Initialize GOAL Kernel printing/messaging system.
 * Allocates buffers.
 */
void init_output() {
  // Note: slightly different behavior in jak 1/jak 2 here.
  // I think the jak 2 version makes clear_wrong do the wrong thing if you are masterdebug but not
  // debugsegment.
  bool use_debug;
  switch (g_game_version) {
    case GameVersion::Jak1:
      use_debug = MasterDebug;
      break;
    case GameVersion::Jak2:
      use_debug = MasterDebug && DebugSegment;
      break;
    case GameVersion::Jak3:
      use_debug = MasterDebug || DebugSegment;
      break;
    default:
      ASSERT(false);
  }

  if (use_debug) {
    MessBufArea = kmalloc(kdebugheap, DEBUG_MESSAGE_BUFFER_SIZE, KMALLOC_MEMSET | KMALLOC_ALIGN_256,
                          "mess-buf");
    OutputBufArea = kmalloc(kdebugheap, DEBUG_OUTPUT_BUFFER_SIZE,
                            KMALLOC_MEMSET | KMALLOC_ALIGN_256, "output-buf");
    PrintBufArea = kmalloc(kdebugheap, DEBUG_PRINT_BUFFER_SIZE, KMALLOC_MEMSET | KMALLOC_ALIGN_256,
                           "print-buf");
  } else {
    // no compiler connection, so we do not allocate buffers
    MessBufArea = Ptr<u8>(0);
    OutputBufArea = Ptr<u8>(0);

    // we still need a (small) print buffer for string maniuplation and debugging prints.
    PrintBufArea =
        kmalloc(kglobalheap, PRINT_BUFFER_SIZE, KMALLOC_MEMSET | KMALLOC_ALIGN_256, "print-buf");
  }
}

/*!
 * Empty output buffer (only if MasterDebug)
 */
void clear_output() {
  if (MasterDebug) {
    kstrcpy((char*)Ptr<u8>(OutputBufArea + sizeof(ListenerMessageHeader)).c(), "");
    OutputPending = Ptr<u8>(0);
  }
}

/*!
 * Clear all data in the print buffer
 */
void clear_print() {
  *Ptr<u8>(PrintBufArea + sizeof(ListenerMessageHeader)) = 0;
  PrintPending = Ptr<u8>(0);
}

/*!
 * Buffer message to compiler indicating the target has reset.
 * Write to the beginning of the output buffer.
 */
void reset_output() {
  if (MasterDebug) {
// original GOAL:
// sprintf(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader), "reset #x%x\n",
// s7.offset);

// modified for OpenGOAL:
#ifdef _WIN32
    sprintf(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader),
            "reset #x%x #x%llx %s\n", s7.offset, (unsigned long long)g_ee_main_mem,  // grr
            xdbg::get_current_thread_id().to_string().c_str());
#else
    sprintf(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader), "reset #x%x #x%lx %s\n",
            s7.offset, (uintptr_t)g_ee_main_mem, xdbg::get_current_thread_id().to_string().c_str());
#endif
    OutputPending = OutputBufArea + sizeof(ListenerMessageHeader);
  }
}

/*!
 * Buffer message to compiler indicating some object file has been unloaded.
 * DONE, EXACT
 */
void output_unload(const char* name) {
  if (MasterDebug) {
    sprintf(strend(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader)),
            "unload \"%s\"\n", name);
    OutputPending = OutputBufArea + sizeof(ListenerMessageHeader);
  }
}

/*!
 * Buffer message to compiler indicating some object file has been loaded.
 */
void output_segment_load(const char* name, Ptr<u8> link_block, u32 flags) {
  if (MasterDebug) {
    char* buffer = strend(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader));
    char true_str[] = "t";
    char false_str[] = "nil";
    char* flag_str = (flags & LINK_FLAG_OUTPUT_TRUE) ? true_str : false_str;
    auto lbp = link_block.cast<ObjectFileHeader>();
    // modified to also include segment sizes, and work from opengoal linker
    // original game used link_block_v3's here.
    sprintf(buffer, "load \"%s\" %s #x%x #x%x #x%x #x%x #x%x #x%x\n", name, flag_str,
            lbp->code_infos[0].offset, lbp->code_infos[1].offset, lbp->code_infos[2].offset,
            lbp->code_infos[0].size, lbp->code_infos[1].size, lbp->code_infos[2].size);
    OutputPending = OutputBufArea + sizeof(ListenerMessageHeader);
  }
}

/*!
 * Print to the GOAL print buffer from C
 * seeks PrintPending to begining of what was just printed.
 * This is a different behavior from all the other prints!
 * DONE, EXACT
 */
void cprintf(const char* format, ...) {
  va_list args;
  va_start(args, format);
  char* str = PrintPending.cast<char>().c();
  if (!PrintPending.offset)
    str = PrintBufArea.cast<char>().c() + sizeof(ListenerMessageHeader);
  PrintPending = make_ptr(strend(str)).cast<u8>();
  vsprintf((char*)PrintPending.c(), format, args);

  va_end(args);
}

/*!
 * Print directly to the C stdout
 * The "k" parameter is ignored, so this is just like printf
 * DONE, changed vprintf to lg::printstd
 */
void Msg(s32 k, const char* format, ...) {
  (void)k;
  va_list args;
  va_start(args, format);
  lg::printstd(format, args);
  va_end(args);
}

/*!
 * Print directly to the C stdout
 * This is idential to Msg
 * DONE, changed vprintf to lg::printstd
 */
void MsgWarn(const char* format, ...) {
  va_list args;
  va_start(args, format);
  lg::printstd(format, args);
  va_end(args);
}

/*!
 * Print directly to the C stdout
 * This is idential to Msg
 * DONE, changed vprintf to lg::printstd
 */
void MsgErr(const char* format, ...) {
  va_list args;
  va_start(args, format);
  lg::printstd(format, args);
  va_end(args);
}

/*!
 * Reverse string in place.
 * DONE, EXACT
 */
void reverse(char* str) {
  s32 i = 0;
  s32 end = (s32)strlen(str);
  while (end--, i < end) {
    char c = str[i];
    str[i] = str[end];
    str[end] = c;
    i++;
  }
}

/*!
 * Some sort of rounding for printing floating point numbers.
 * It is unused and believed to be not correct.
 * Currently copy-pasta from GHIDRA
 * (not checked in jak2)
 */
char* round(float x, s32* param1, char* start, char* sEnd, char padchar, s32* param4) {
  char cVar1;
  char* local_58;
  float f;

  if (x == 0.00000000) {
    f = padchar - '0';
  } else {
    modff(x * 10.00000000, &f);
  }
  if (4.00000000 < f) {
    while (true) {
      if (*sEnd == '.') {
        sEnd = sEnd + -1;
      }
      cVar1 = *sEnd;
      *sEnd = cVar1 + 1;
      if ((char)(cVar1 + 1) <= '9') {
        return start;
      }
      *sEnd = '0';
      if (sEnd == start)
        break;
      sEnd = sEnd + -1;
    }
    if (param1 == (int*)0x0) {
      sEnd[-1] = '1';
      local_58 = start + -1;
    } else {
      *sEnd = '1';
      *param1 = *param1 + 1;
      local_58 = start;
    }
  } else {
    local_58 = start;
    if (*param4 == 0x2d) {
      while (true) {
        if (*sEnd == '.') {
          sEnd = sEnd + -1;
        }
        local_58 = start;
        if (*sEnd != '0')
          break;
        if (sEnd == start) {
          *param4 = 0;
        }
        sEnd = sEnd + -1;
      }
    }
  }
  return local_58;
}

/*!
 * Convert floating point to string intermediate function.
 * Places a null character as the first character, then the integers, decimal, fraction digits
 * Presumably, if rounding worked, it would sometimes add an additional digit in front instead of
 * the null but rounding doesn't, so you get a null in front every time. There is no null terminator
 * added, you have to do it yourself. returned value is length : buff[last_char] - buff[1] (not
 * including the leading null) if the number is negative, *lead_char = '-'.  Otherwise it's '\0'.
 * The negative is not inserted for you.
 * The precision is how many digits after decimal to print.
 * The flag could have a 1 to enable rounding, but this doesn't work, so don't use it.
 * Without rounding the printing is a little bit off but you don't notice unless you look too
 * closely.
 *
 * DONE, added some sanity checks and removed support for "rounding" as round isn't implemented and
 * rounding is never used in the game.
 *
 * Not checked closely in jak 2.
 */
s32 cvt_float(float x, s32 precision, s32* lead_char, char* buff_start, char* buff_end, u32 flags) {
  // put a null at the beginning of the output
  *buff_start = 0;
  s32 forward_count = 0;

  // compute absolute value and set lead char to `-` if needed.
  float abs_x;
  if (x < 0.0f) {
    abs_x = -x;
    *lead_char = '-';
  } else {
    *lead_char = 0;
    abs_x = x;
  }

  // nan check
  u32 abs_x_u32 = *(u32*)&abs_x;
  if ((abs_x_u32 & 0x7fffffff) == 0x7fffffff) {
    kstrcpy(buff_start, "NaN");
    return 3;  // the length of NaN
  }

  // find fraction and integer parts of absolute value.
  float integer_part;
  float fraction_part = std::modf(abs_x, &integer_part);

  char* start_ptr = buff_start + 1;  // the null terminator is at buff_start[0].
  char* end_ptr = buff_end - 1;      // the last char we can write to.

  // loop over integer digits (increasing significance)
  while (start_ptr <= end_ptr && (integer_part != 0.0f)) {
    // the fractional part will be the lowest place integer divided by 10
    float next_int = std::modf(integer_part / 10.f, &integer_part);

    // the float to round to get the integer
    float rounder = next_int * 10.f + 0.5f;

    // but wait, the PS2 is stupid and maybe has weird floats that might not convert well.
    // this checks for very large exponent or negative sign bit, which would only happen if the
    // float is all messed up.
    u32 ru32 = *(u32*)&rounder;
    s32 value;
    if (((ru32 >> 0x17) & 0xff) < 0x9e) {  // exponent
      value = (char)rounder;
    } else if (!(ru32 >> 31)) {  // sign bit
      value = 0;
      // ASSERT(false);  // not sure what happens here.
    } else {
      value = -1;  // happens on NaN's
    }

    // place number at the end of the buffer and move pointer back
    *end_ptr = '0' + value;
    end_ptr--;

    // track how many integers we've written
    forward_count++;
  }

  char* count_chrp = start_ptr;   // one after the buffer start (null)
  if (forward_count == 0) {       // no integers
    *start_ptr = '0';             // put leading zero
    count_chrp = buff_start + 2;  // and set count to two after buffer start
  } else {
    while (end_ptr = end_ptr + 1,
           end_ptr < buff_end) {  // copy back to the beginning (will be in right order)
      *count_chrp = *end_ptr;
      count_chrp++;
    }
  }

  // if we have digits after decimal, place decimal.
  if (precision) {
    *count_chrp = '.';
    count_chrp++;
  }

  s32 prec = precision;

  // if we have a fractional part, we may need to either print it, or do rounding.
  if (fraction_part != 0.0f) {
    prec = precision;
    if (precision) {
      // same loop as before, but only over the number of digits we actually want to print.
      do {
        float next_int;
        fraction_part = std::modf(fraction_part * 10.f, &next_int);
        u32 ru32 = *(u32*)&next_int;
        s32 value;
        if (((ru32 >> 0x17) & 0xff) < 0x9e) {
          value = (char)next_int;
        } else if (!(ru32 >> 0x1f)) {
          value = 0;
          // ASSERT(false);  // not sure what happens here.
        } else {
          value = -1;  // happens on NaN's
        }
        *count_chrp = value + '0';
        count_chrp++;
        prec--;
      } while ((prec) && (fraction_part != 0.f));
    }

    // if the rounding flag is enabled, we would round here.
    // however, the rounding flag is always disabled and the rounding code doesn't work.
    if ((fraction_part != 0.f) && ((flags & 1) != 0)) {
      start_ptr = round(fraction_part, nullptr, start_ptr, count_chrp - 1, 0, lead_char);
      ASSERT(false);
    }
  }

  // if there are any left over digits (fraction part = 0 before we got to the end), append zeros.
  while (prec = prec - 1, prec != -1) {
    *count_chrp = '0';
    count_chrp++;
  }
  // return length. not including the null character at the beginning.
  return count_chrp - start_ptr;
}

/*!
 * Convert floating point to a string.
 * Don't set the precision too high or you get NaN (if float is longer than 31 digits without
 * negative sign) Don't set the flags. Rounding is a little bit off, but it adds character. Highly
 * recommended to pad with spaces, not zeros. If you pad with spaces you get "    -1.23" If you pad
 * with zeros, you get "0000000-1.23" which most people consider to be wrong.
 * @param out_str : output buffer to write null terminated string into
 * @param x : number
 * @param desired_len : length (will pad if under, nothing if over)
 * @param pad_char : character to pad with
 * @param precision
 * @param flags
 *
 * DONE
 * Not checked super closely in jak 2.
 */
void ftoa(char* out_str, float x, s32 desired_len, char pad_char, s32 precision, u32 flags) {
  char buff[0x100];
  char* current_buff = buff;
  s32 lead_char;

  // do conversion, but only write into the first half of the buffer (128 chars)
  s32 count = cvt_float(x, precision, &lead_char, current_buff, buff + 0x7f, flags);

  // if it ends up larger than 31 characters, it's probably gone horribly wrong and we should just
  // put NaN instead. Or maybe somebody requested a lot of precision.  That would be stupid and they
  // deserve to see NaN.
  if (count > 0x3f) {
    kstrcpy(current_buff, "NaN");
    count = 3;
    lead_char = 0;
  }

  // always true because we don't round,
  if (buff[0] == 0) {
    current_buff = buff + 1;
  }

  // length, including the leading negative (if we need it).
  s32 real_count = (lead_char != 0) + count;

  char* out_ptr = out_str;

  // pad
  if ((desired_len > 0) && (desired_len > real_count)) {
    for (s32 i = 0; i < (desired_len - real_count); i++) {
      *out_ptr = pad_char;
      out_ptr++;
    }
  }

  // leading
  if (lead_char) {
    *out_ptr = lead_char;
    out_ptr++;
  }

  // copy numbers
  for (s32 i = 0; i < count; i++) {
    *out_ptr = *current_buff;
    out_ptr++;
    current_buff++;
  }

  // null terminate!
  *out_ptr = 0;
}

/*!
 * Convert integer to string.
 * @param buffer : buffer to print into. Must be at least as long as the longest possible number to
 * print
 * @param value  : value to print.
 * @param base   : base to print in (2, 10, 16 supported)
 * @param length : length.  if shorter than length, pad with pad.  If longer, only truncate leading
 * f's 1's in base 16 or 2
 * @param pad    : character to pad with
 * @param flags  : flag.  Only the 2nd bit is used, which will disable negative sings on
 * binary/hexadecimal truncated numbers.  Something like -1 (0xffffffff) will print as -fffffff....
 * Not checked super closely in jak 2
 */
char* kitoa(char* buffer, s64 value, u64 base, s32 length, char pad, u32 flag) {
  s64 negativeValue = 0;
  s64 value_to_print = value;

  // if negative and base ten, we print the opposite of the value and add a negative sign
  if ((value < 0) && base == 10) {
    negativeValue = value;
    value_to_print = -value;
  }

  // write number in reverse
  int count = 0;
  do {
    buffer[count++] = ConvertTable[(u64)value_to_print % (u64)base];
    value_to_print = (u64)value_to_print / (u64)base;
  } while (value_to_print);

  // append negative if we need to
  if (negativeValue < 0) {
    buffer[count++] = '-';
  }

  // pad (probably some sort of for loop)
  s32 rLen = length;
  if (0 < length - count) {
    rLen = length - count;
    while (0 < rLen) {
      buffer[count++] = pad;
      rLen--;
    }
  }

  // truncate f's / 1's
  if (rLen > 0 && value < 0 && (base == 2 || base == 16) && rLen < count) {
    char c = (base == 16) ? 'f' : '1';

    while (rLen < count && (buffer[count - 1] == c)) {
      count--;
    }

    if (!(flag & 2)) {
      buffer[count++] = '-';
    }
  }

  // null terminate, reverse, return!
  buffer[count] = 0;
  reverse(buffer);
  return buffer;
}

/*!
 * Convert 128-bit integer to string.  Not implemented because it is never used in the game.
 * It would also require passing 128-bit values between GOAL and C++ and this is not worth
 * implementing. It is only used by the "format" function, which cannot use it properly. "format"
 * uses C varags, but 128-bit varags don't work, so "format" always passes 0 for quadword printing.
 */
void kqtoa() {
  ASSERT(false);
}
