#include "kprint.h"

#include <cstdio>
#include <cstring>

#include "common/goal_constants.h"
#include "common/listener_common.h"
#include "common/log/log.h"
#include "common/symbols.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kboot.h"
#include "game/kernel/common/klisten.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/jak2/kscheme.h"
#include "game/sce/sif_ee.h"

namespace jak2 {

void output_sql_query(char* query_name) {
  if (MasterDebug) {
    sprintf(strend(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader)), "sql-query \"");

    char* buffer_ptr = strend(OutputBufArea.cast<char>().c() + sizeof(ListenerMessageHeader));
    char query_char = *query_name;
    char query_char_to_add = *query_name;
    while (query_char != 0) {
      if (query_char_to_add == '\"') {
        *buffer_ptr = '\\';
        buffer_ptr[1] = *query_name;
        buffer_ptr += 2;
      } else {
        *buffer_ptr = query_char_to_add;
        buffer_ptr++;
      }
      query_name = query_name + 1;
      query_char = *query_name;
      query_char_to_add = *query_name;
    }
    *buffer_ptr = '\"';
    buffer_ptr[1] = '\n';
    buffer_ptr[2] = 0;
    OutputPending = OutputBufArea + sizeof(ListenerMessageHeader);
  }
}
}  // namespace jak2

/*!
 * The GOAL "format" function.  The actual function is named "format".  However, GOAL's calling
 * convention differs from x86-64, so GOAL cannot directly call format.  There is an assembly
 * function in format_wrapper.nasm named format. It takes the GOAL argument registers, stores them
 * in an array on the stack, and calls this function with a pointer to that array.
 *
 * This function is a disaster. For now, it's copied from jak1, with the obvious fixes made, but
 * it's probably worth another pass.
 */
s32 format_impl_jak2(uint64_t* args) {
  using namespace jak2_symbols;
  using namespace jak2;

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
    print_temp = PrintBufArea.cast<char>().c() + sizeof(ListenerMessageHeader);
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
    indentation = (*(print_column - 1)) >> 3;
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
      while ((u8)(format_ptr[1] - '0') < 10 ||  // number 0 to 9
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

        case 'O':
        case 'o': {
          *output_ptr = '~';
          output_ptr++;
          kitoa(output_ptr, arg_regs[arg_reg_idx++], 10, 0, ' ', 0);
          output_ptr = strend(output_ptr);
          *output_ptr = 'u';
          output_ptr++;
        } break;

        case 'A':  // print a boxed object
        case 'a':  // pad,padchar (like ) ~8,'0A
        {
          s8 arg0 = argument_data[0].data[0];
          s32 desired_length = arg0;
          *output_ptr = 0;
          u32 in = arg_regs[arg_reg_idx++];
          jak2::print_object(in);
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
                ASSERT(false);
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
          if (((in & 0x7) == 0x4) && *Ptr<u32>(in - 4) == *(s7 + FIX_SYM_STRING_TYPE - 1)) {
            cprintf("%s", Ptr<char>(in).c() + 4);
          } else {
            jak2::print_object(in);
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
                ASSERT(false);
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
          u64 in = arg_regs[arg_reg_idx++];
          if (arg0 == -1) {
            jak2::print_object(in);
          } else {
            auto sym = jak2::find_symbol_from_c(argument_data[0].data);
            if (sym.offset) {
              Ptr<Type> type(sym->value());
              if (type.offset) {
                call_method_of_type(in, type, GOAL_PRINT_METHOD);
              }
            } else {
              ASSERT(false);  // bad type.
            }
          }
          output_ptr = strend(output_ptr);
        } break;

        case 'I':  // like ~P, but calls inpsect
        case 'i': {
          *output_ptr = 0;
          s8 arg0 = argument_data[0].data[0];
          u64 in = arg_regs[arg_reg_idx++];
          if (arg0 == -1) {
            inspect_object(in);
          } else {
            auto sym = find_symbol_from_c(argument_data[0].data);
            if (sym.offset) {
              Ptr<Type> type(sym->value());
              if (type.offset) {
                call_method_of_type(in, type, GOAL_INSPECT_METHOD);
              }
            } else {
              ASSERT(false);  // bad type
            }
          }
          output_ptr = strend(output_ptr);
        } break;

        case 'Q':  // not yet implemented.  hopefully andy gavin finishes this one soon.
        case 'q':
          ASSERT(false);
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
            ASSERT(false);  // i don't get this one
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
          MsgErr("input was %s\n", format_cstring);
          // ASSERT(false);
          goto copy_char_hack;
          break;
      }
      format_ptr++;
    } else {
    // got normal char, just copy it
    copy_char_hack:  // we goto here if we get a bad code for ~, which sort of backtracks and falls
                     // back to regular character copying
      *output_ptr = *format_ptr;
      output_ptr++;
    }
    format_ptr++;
  }  // end format string while

  // end
  *output_ptr = 0;
  output_ptr++;

  if (original_dest == s7.offset + FIX_SYM_TRUE) {
    // #t means to put it in the print buffer

    // change for Jak 2: if we are disk-booting and do a (format #t, immediately flush to stdout.
    // we'd get these eventually in ClearPending, but for some reason they flush these here.
    // This is nicer because we may crash in between here and flushing the print buffer.
    if (DiskBoot) {
      // however, we are going to disable it anyway because it spams the console and is annoying
      if (false) {
        lg::print("{}", PrintPendingLocal3);
        // printf("%s", PrintPendingLocal3);
        // fflush(stdout);
      }
      PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
      // if we don't comment this line, our output gets cleared
      // *PrintPendingLocal3 = 0;
    }

    return 0;
  } else if (original_dest == s7.offset + FIX_SYM_FALSE) {
    // #f means print to new string
    u32 string = make_string_from_c(PrintPendingLocal3);
    PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
    *PrintPendingLocal3 = 0;
    return string;
  } else if (original_dest == 0) {
    lg::print("{}", PrintPendingLocal3);
    // printf("%s", PrintPendingLocal3);
    // fflush(stdout);
    PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
    *PrintPendingLocal3 = 0;
    return 0;
  } else {
    if ((original_dest & OFFSET_MASK) == BASIC_OFFSET) {
      Ptr<Type> type = *Ptr<Ptr<Type>>(original_dest - 4);
      if (type == *Ptr<Ptr<Type>>(s7.offset + FIX_SYM_STRING_TYPE - 1)) {
        u32 len = *Ptr<u32>(original_dest);
        char* str = Ptr<char>(original_dest + 4).c();
        kstrncat(str, PrintPendingLocal3, len);
        PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
        *PrintPendingLocal3 = 0;
        return 0;
      } else if (type == *Ptr<Ptr<Type>>(s7.offset + FIX_SYM_FILE_STREAM - 1)) {
        size_t len = strlen(PrintPendingLocal3);
        // sceWrite
        ee::sceWrite(*Ptr<s32>(original_dest + 12), PrintPendingLocal3, len);

        PrintPending = make_ptr(PrintPendingLocal2).cast<u8>();
        *PrintPendingLocal3 = 0;
        return 0;
      }
    }
    ASSERT(false);  // unknown destination
    return 0;
  }

  ASSERT(false);  // ??????
  return 7;
}
