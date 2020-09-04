/*!
 * @file kprint.cpp
 * GOAL Print.  Contains GOAL I/O, Print, Format...
 */

#include <cstring>
#include <cmath>
#include <stdarg.h>
#include <stdio.h>

#include "common/goal_constants.h"
#include "common/common_types.h"
#include "kprint.h"
#include "kmachine.h"
#include "kboot.h"
#include "kmalloc.h"
#include "kdsnetm.h"
#include "fileio.h"
#include "kscheme.h"
#include "klisten.h"
#include "klink.h"
#include "common/symbols.h"

///////////
// SDATA
///////////

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
void kprint_init_globals() {
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
 * DONE, EXACT
 */
void init_output() {
  if (MasterDebug) {
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
 * DONE
 * EXACT
 */
void clear_output() {
  if (MasterDebug) {
    kstrcpy((char*)Ptr<u8>(OutputBufArea + sizeof(GoalMessageHeader)).c(), "");
    OutputPending = Ptr<u8>(0);
  }
}

/*!
 * Clear all data in the print buffer
 * DONE
 * EXACT
 */
void clear_print() {
  *Ptr<u8>(PrintBufArea + sizeof(GoalMessageHeader)) = 0;
  PrintPending = Ptr<u8>(0);
}

/*!
 * Buffer message to compiler indicating the target has reset.
 * Write to the beginning of the output buffer.
 * DONE, EXACT
 */
void reset_output() {
  if (MasterDebug) {
    sprintf(OutputBufArea.cast<char>().c() + sizeof(GoalMessageHeader), "reset #x%x\n", s7.offset);
    OutputPending = OutputBufArea + sizeof(GoalMessageHeader);
  }
}

/*!
 * Buffer message to compiler indicating some object file has been unloaded.
 * DONE, EXACT
 */
void output_unload(const char* name) {
  if (!MasterDebug) {
    sprintf(strend(OutputBufArea.cast<char>().c() + sizeof(GoalMessageHeader)), "unload \"%s\"\n",
            name);
    OutputPending = OutputBufArea + sizeof(GoalMessageHeader);
  }
}

/*!
 * Buffer message to compiler indicating some object file has been loaded.
 */
void output_segment_load(const char* name, Ptr<u8> link_block, u32 flags) {
  if (MasterDebug) {
    char* buffer = strend(OutputBufArea.cast<char>().c() + sizeof(GoalMessageHeader));
    char true_str[] = "t";
    char false_str[] = "nil";
    char* flag_str = (flags & LINK_FLAG_OUTPUT_TRUE) ? true_str : false_str;
    auto lbp = link_block.cast<ObjectFileHeader>();
    sprintf(buffer, "load \"%s\" %s #x%x #x%x #x%x\n", name, flag_str, lbp->code_infos[0].offset,
            lbp->code_infos[1].offset, lbp->code_infos[2].offset);
    OutputPending = OutputBufArea + sizeof(GoalMessageHeader);
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
    str = PrintBufArea.cast<char>().c() + sizeof(GoalMessageHeader);
  PrintPending = make_ptr(strend(str)).cast<u8>();
  vsprintf((char*)PrintPending.c(), format, args);

  va_end(args);
}

/*!
 * Print directly to the C stdout
 * The "k" parameter is ignored, so this is just like printf
 * DONE, EXACT
 */
void Msg(s32 k, const char* format, ...) {
  (void)k;
  va_list args;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
}

/*!
 * Print directly to the C stdout
 * This is idential to Msg
 * DONE, EXACT
 */
void MsgWarn(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
}

/*!
 * Print directly to the C stdout
 * This is idential to Msg
 * DONE, EXACT
 */
void MsgErr(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vprintf(format, args);
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
      throw std::runtime_error("got very large exponent in rounding calculation");
    } else {
      value = -1;  // happens on NaN's
      //      throw std::runtime_error("got negative sign bit in rounding calculation");
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
          throw std::runtime_error("got very large exponent in rounding calculation");
        } else {
          value = -1;  // happens on NaN's
          //          throw std::runtime_error("got negative sign bit in rounding calculation");
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
      throw std::runtime_error("cvt_float called round!");
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
 *
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
  throw std::runtime_error("kqtoa not implemented");
}

struct format_struct {
  char data[0x40];
  void reset() {
    for (auto& c : data)
      c = -1;
  }
};

/*!
 * The GOAL "format" function.  The actual function is named "format".  However, GOAL's calling
 * convention differs from x86-64, so GOAL cannot directly call format.  There is an assembly
 * function in format_wrapper.nasm named format. It takes the GOAL argument registers, stores them
 * in an array on the stack, and calls this function with a pointer to that array.
 *
 * This function is a disaster. The Ghidra analyzer completely fails on it, so this is done by hand.
 *
 * To make this work correctly from GOAL with up to 8 arguments, there is an assembly function
 * defined in format_wrapper.nasm that places the GOAL arguments on the stack and calls this
 * format_impl function with a single argument that is a pointer to the argument array.
 */
s32 format_impl(uint64_t* args) {
  // first two args are dest, format string
  uint64_t* arg_regs = args + 2;

  // data for arguments in a format command
  format_struct argument_data[8];

  u32 arg_reg_idx = 0;

  // the gstring
  char* format_gstring = Ptr<char>(args[1]).c();

  u32 original_dest = args[0];

  // set up print pending
  char* print_temp = PrintPending.cast<char>().c();
  if (!PrintPending.offset) {
    print_temp = PrintBufArea.cast<char>().c() + sizeof(GoalMessageHeader);
  }
  PrintPending = make_ptr(strend(print_temp)).cast<u8>();

  // what we write to
  char* output_ptr = PrintPending.cast<char>().c();

  // convert gstring to cstring
  char* format_cstring = format_gstring + 4;

  // mysteries
  char* PrintPendingLocal2 = PrintPending.cast<char>().c();
  char* PrintPendingLocal3 = output_ptr;

  // start by computing indentation
  u32 indentation = 0;

  // read goal binteger
  if (print_column.offset) {
    // added the if check so we can format even if the kernel didn't load right.
    indentation = (*print_column) >> 3;
  }

  // which arg we're on
  u32 arg_idx = 0;

  // if last char was newline and we have tabs, do tabs
  if (indentation && output_ptr[-1] == '\n') {
    for (u32 i = 0; i < indentation; i++) {
      *output_ptr = ' ';
      output_ptr++;
    }
  }

  // input pointer
  char* format_ptr = format_cstring;

  // loop over the format string
  while (*format_ptr) {
    // got a command?
    if (*format_ptr == '~') {
      char* arg_start = format_ptr;
      // get some arguments
      arg_idx = 0;
      u8 justify = 0;
      for (auto& x : argument_data) {
        x.reset();
      }

      // read arguments
      while ((u8)(format_ptr[1] - '0') < 10 ||  // number 0 to 10
             format_ptr[1] == ',' ||            // comma
             format_ptr[1] == '\'' ||           // quote
             format_ptr[1] == '`' ||            // backtick
             (argument_data[arg_idx].data[0] == -1 &&
              (format_ptr[1] == '-' || format_ptr[1] == '+')  // flags1 == -1 && +/-
              )) {
        // here format_ptr[1] points to next unread character in argument
        // format_ptr[0] is originally the ~
        // should exit loop with format_ptr[1] == the command character
        char arg_char = format_ptr[1];  // gVar1

        if (arg_char == ',') {
          // advance to next argument
          arg_idx++;     // increment which argument we're on
          format_ptr++;  // increment past comma, and try again
          continue;
        }

        // character argument
        if (arg_char == '\'') {  // 0x27
          argument_data[arg_idx].data[0] = format_ptr[2];
          format_ptr += 2;
          continue;
        }

        // string argument
        if (arg_char == '`') {  // 0x60
          u32 i = 0;
          format_ptr += 2;
          // read string
          while (*format_ptr != '`') {
            argument_data[arg_idx].data[i] = *format_ptr;
            i++;
            format_ptr++;
          }
          // null terminate
          argument_data[arg_idx].data[i] = 0;
          continue;
        }

        if (arg_char == '-') {  // 0x2d
          // negative flag
          argument_data[arg_idx].data[1] = 1;
          format_ptr++;
          continue;
        }

        if (arg_char == '+') {  // 0x2b
          // positive flag does nothing
          format_ptr++;
          continue;
        }

        // otherwise:

        // null terminate if we got no args
        if (argument_data[arg_idx].data[0] == -1) {
          argument_data[arg_idx].data[0] = 0;
        }

        // otherwise it's a number
        argument_data[arg_idx].data[0] = argument_data[arg_idx].data[0] * 10 + arg_char - '0';
        format_ptr++;
      }  // end argument while

      // switch on command
      switch (format_ptr[1]) {
          // offset of 0x25

        case '%':  // newline
          *output_ptr = '\n';
          output_ptr++;
          // indent the next line if there is one
          if (indentation && format_ptr[2]) {
            for (u32 i = 0; i < indentation; i++) {
              *output_ptr = ' ';
              output_ptr++;
            }
          }
          break;

        case '~':  // tilde escape
          *output_ptr = '~';
          output_ptr++;
          break;

          // pass through arguments
        case 'H':  // 23 -> 48, H
        case 'J':  // 25 -> 4A, J
        case 'K':  // 26 -> 4B, K
        case 'L':  // 27 -> 4C, L
        case 'N':  // 29 -> 4E, N
        case 'V':  // 31 -> 56, V
        case 'W':  // 32 -> 57, W
        case 'Y':  // 34 -> 59, Y
        case 'Z':  // 35 -> 5A, Z
        case 'h':
        case 'j':
        case 'k':
        case 'l':
        case 'n':
        case 'v':
        case 'w':
        case 'y':
        case 'z':
          while (arg_start < format_ptr + 1) {
            *output_ptr = *arg_start;
            arg_start++;
            output_ptr++;
          }
          *output_ptr = format_ptr[1];
          output_ptr++;
          break;

        case 'G':  // like %s, prints a C string
        case 'g': {
          *output_ptr = 0;
          u32 in = arg_regs[arg_reg_idx++];
          kstrcat(output_ptr, Ptr<char>(in).c());
          output_ptr = strend(output_ptr);
        } break;

        case 'A':  // print a boxed object
        case 'a':  // pad,padchar (like ) ~8,'0A
        {
          s8 arg0 = argument_data[0].data[0];
          s32 desired_length = arg0;
          *output_ptr = 0;
          u32 in = arg_regs[arg_reg_idx++];
          print_object(in);
          if (desired_length != -1) {
            s32 print_len = strlen(output_ptr);
            if (desired_length < print_len) {
              // too long!
              if (desired_length > 1) {  // mark with tilde that we will truncate
                output_ptr[desired_length - 1] = '~';
              }
              output_ptr[desired_length] = 0;  // and truncate
            } else if (print_len < desired_length) {
              // too short
              if (justify == 0) {
                char pad = ' ';
                if (argument_data[1].data[0] != -1) {
                  pad = argument_data[1].data[0];
                }
                kstrinsert(output_ptr, pad, desired_length - print_len);
              } else {
                throw std::runtime_error("unsupported justify in format");
                //                output_ptr = strend(output_ptr);
                //                while(0 < (desired_length - print_len)) {
                //                  char pad = ' ';
                //                  if(argument_data[0].data[1] != -1) {
                //                    pad = argument_data[0].data[1];
                //                  }
                //                  output_ptr[0] = pad;
                //                  output_ptr++;
                //
                //                }
                //                *output_ptr = 0;
              }
            }
          }
          output_ptr = strend(output_ptr);

        } break;

        case 'S':  // like A, but strings are printed without quotes
        case 's': {
          s8 arg0 = argument_data[0].data[0];
          s32 desired_length = arg0;
          *output_ptr = 0;
          u32 in = arg_regs[arg_reg_idx++];

          // if it's a string
          if (((in & 0x7) == 0x4) && *Ptr<u32>(in - 4) == *(s7 + FIX_SYM_STRING_TYPE)) {
            cprintf("%s", Ptr<char>(in).c() + 4);
          } else {
            print_object(in);
          }

          if (desired_length != -1) {
            s32 print_len = strlen(output_ptr);
            if (desired_length < print_len) {
              // too long!
              if (desired_length > 1) {  // mark with tilde that we will truncate
                output_ptr[desired_length - 1] = '~';
              }
              output_ptr[desired_length] = 0;  // and truncate
            } else if (print_len < desired_length) {
              // too short
              if (justify == 0) {
                char pad = ' ';
                if (argument_data[1].data[0] != -1) {
                  pad = argument_data[1].data[0];
                }
                kstrinsert(output_ptr, pad, desired_length - print_len);

              } else {
                throw std::runtime_error("unsupported justify in format");
                //                output_ptr = strend(output_ptr);
                //                u32 l140 = 0;
                //                while(l140 < (desired_length - print_len)) {
                //                  char* l108 = output_ptr;
                //
                //                  char pad = ' ';
                //                  if(argument_data[0].data[1] != -1) {
                //                    pad = argument_data[0].data[1];
                //                  }
                //                  output_ptr[0] = pad;
                //                  output_ptr++;
                //                }
                //                *output_ptr = 0;
              }
            }
          }
          output_ptr = strend(output_ptr);
        } break;

        case 'C':  // character
        case 'c':
          *output_ptr = arg_regs[arg_reg_idx++];
          output_ptr++;
          break;

        case 'P':  // like ~A, but can specify type explicitly
        case 'p': {
          *output_ptr = 0;
          s8 arg0 = argument_data[0].data[0];
          u32 in = arg_regs[arg_reg_idx++];
          if (arg0 == -1) {
            print_object(in);
          } else {
            auto sym = find_symbol_from_c(argument_data[0].data);
            if (sym.offset) {
              Ptr<Type> type = *sym.cast<Ptr<Type>>();
              if (type.offset) {
                call_method_of_type(in, type, GOAL_PRINT_METHOD);
              }
            } else {
              throw std::runtime_error("failed to find symbol in format!");
            }
          }
          output_ptr = strend(output_ptr);
        } break;

        case 'I':  // like ~P, but calls inpsect
        case 'i': {
          *output_ptr = 0;
          s8 arg0 = argument_data[0].data[0];
          u32 in = arg_regs[arg_reg_idx++];
          if (arg0 == -1) {
            inspect_object(in);
          } else {
            auto sym = find_symbol_from_c(argument_data[0].data);
            if (sym.offset) {
              Ptr<Type> type = *sym.cast<Ptr<Type>>();
              if (type.offset) {
                call_method_of_type(in, type, GOAL_INSPECT_METHOD);
              }
            } else {
              throw std::runtime_error("failed to find symbol in format!");
            }
          }
          output_ptr = strend(output_ptr);
        } break;

        case 'Q':  // not yet implemented.  hopefully andy gavin finishes this one soon.
        case 'q':
          throw std::runtime_error("nyi q format string");
          break;

        case 'X':  // hex, 64 bit, pad padchar
        case 'x': {
          char pad = '0';
          if (argument_data[1].data[0] != -1) {
            pad = argument_data[1].data[0];
          }
          u64 in = arg_regs[arg_reg_idx++];
          kitoa(output_ptr, in, 16, argument_data[0].data[0], pad, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'D':  // integer 64, pad padchar
        case 'd': {
          char pad = ' ';
          if (argument_data[1].data[0] != -1) {
            pad = argument_data[1].data[0];
          }
          u64 in = arg_regs[arg_reg_idx++];
          kitoa(output_ptr, in, 10, argument_data[0].data[0], pad, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'B':  // integer 64, pad padchar
        case 'b': {
          char pad = '0';
          if (argument_data[1].data[0] != -1) {
            pad = argument_data[1].data[0];
          }
          u64 in = arg_regs[arg_reg_idx++];
          kitoa(output_ptr, in, 2, argument_data[0].data[0], pad, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'F':  // float 12 pad, 4 precision
        {
          float in = *(float*)&arg_regs[arg_reg_idx++];
          ftoa(output_ptr, in, 0xc, ' ', 4, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'f':  // float with args
        {
          float in = *(float*)&arg_regs[arg_reg_idx++];
          s8 pad_length = argument_data[0].data[0];
          s8 pad_char = argument_data[1].data[0];
          if (pad_char == -1)
            pad_char = ' ';
          s8 precision = argument_data[2].data[0];
          if (precision == -1)
            precision = 4;
          ftoa(output_ptr, in, pad_length, pad_char, precision, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'R':  // rotation degrees
        case 'r': {
          float in = *(float*)&arg_regs[arg_reg_idx++];
          s8 pad_length = argument_data[0].data[0];
          s8 pad_char = argument_data[1].data[0];
          if (pad_char == -1)
            pad_char = ' ';
          s8 precision = argument_data[2].data[0];
          if (precision == -1)
            precision = 4;
          ftoa(output_ptr, in * 360.f / 65536.f, pad_length, pad_char, precision, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'M':  // distance meters
        case 'm': {
          float in = *(float*)&arg_regs[arg_reg_idx++];
          s8 pad_length = argument_data[0].data[0];
          s8 pad_char = argument_data[1].data[0];
          if (pad_char == -1)
            pad_char = ' ';
          s8 precision = argument_data[2].data[0];
          if (precision == -1)
            precision = 4;
          ftoa(output_ptr, in / 4096.f, pad_length, pad_char, precision, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'E':  // time seconds
        case 'e': {
          s64 in = arg_regs[arg_reg_idx++];
          s8 pad_length = argument_data[0].data[0];
          s8 pad_char = argument_data[0].data[1];
          if (pad_char == -1)
            pad_char = ' ';
          s8 precision = argument_data[0].data[2];
          if (precision == -1)
            precision = 4;
          float value;
          if (in < 0) {
            throw std::runtime_error("time seconds format error negative.\n");
          } else {
            value = in;
          }
          ftoa(output_ptr, value / 300.f, pad_length, pad_char, precision, 0);
          output_ptr = strend(output_ptr);
        } break;

        case 'T':
        case 't': {
          sprintf(output_ptr, "\t");
          output_ptr = strend(output_ptr);
        } break;

        default:
          MsgErr("format: unknown code 0x%02x\n", format_ptr[1]);
          throw std::runtime_error("format error");
          break;
      }
      format_ptr++;
    } else {
      // got normal char, just copy it
      *output_ptr = *format_ptr;
      output_ptr++;
    }
    format_ptr++;
  }  // end format string while

  // end
  *output_ptr = 0;
  output_ptr++;

  if (original_dest == s7.offset + FIX_SYM_TRUE) {
    // do nothing, we're done
    return 0;
  } else if (original_dest == s7.offset + FIX_SYM_FALSE) {
    // #f means print to new string
    u32 string = make_string_from_c(PrintPendingLocal3);
    PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
    *PrintPendingLocal3 = 0;
    return string;
  } else if (original_dest == 0) {
    printf("%s", PrintPendingLocal3);
    fflush(stdout);
    PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
    *PrintPendingLocal3 = 0;
    return 0;
  } else {
    if ((original_dest & OFFSET_MASK) == BASIC_OFFSET) {
      Ptr<Type> type = *Ptr<Ptr<Type>>(original_dest - 4);
      if (type == *Ptr<Ptr<Type>>(s7.offset + FIX_SYM_STRING_TYPE)) {
        u32 len = *Ptr<u32>(original_dest);
        char* str = Ptr<char>(original_dest + 4).c();
        kstrncat(str, PrintPendingLocal3, len);
        PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
        *PrintPendingLocal3 = 0;
        return 0;
      } else if (type == *Ptr<Ptr<Type>>(s7.offset + FIX_SYM_FILE_STREAM_TYPE)) {
        throw std::runtime_error("FORMAT into a file stream not supported");
      }
    }
    throw std::runtime_error("unknown format destination");
    return 0;
  }

  throw std::runtime_error("how did we get here?");
  return 7;
}