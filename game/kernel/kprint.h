/*!
 * @file kprint.h
 * GOAL Print.  Contains GOAL I/O, Print, Format...
 */

#ifndef RUNTIME_KPRINT_H
#define RUNTIME_KPRINT_H

#include "kmachine.h"

constexpr u32 DEBUG_MESSAGE_BUFFER_SIZE = 0x80000;
constexpr u32 DEBUG_OUTPUT_BUFFER_SIZE = 0x80000;
constexpr u32 DEBUG_PRINT_BUFFER_SIZE = 0x200000;
constexpr u32 PRINT_BUFFER_SIZE = 0x2000;

///////////
// SDATA
///////////
extern Ptr<u8> OutputPending;
extern Ptr<u8> PrintPending;
extern s32 MessCount;

extern char AckBufArea[40];
extern Ptr<u8> MessBufArea;
extern Ptr<u8> OutputBufArea;
extern Ptr<u8> PrintBufArea;

/*!
 * Initialize global variables for kprint
 */
void kprint_init_globals();

/*!
 * Initialize GOAL Kernel printing/messaging system.
 * Allocates buffers.
 */
void init_output();

/*!
 * Empty output buffer (only if MasterDebug)
 */
void clear_output();

/*!
 * Clear all data in the print buffer
 */
void clear_print();

/*!
 * Buffer message to compiler indicating the target has reset.
 * Write to the beginning of the output buffer.
 */
void reset_output();

/*!
 * Buffer message to compiler indicating some object file has been unloaded.
 */
void output_unload(const char* name);

/*!
 * Buffer message to compiler indicating some object file has been loaded.
 */
void output_segment_load(const char* name, Ptr<u8> link_block, u32 flags);

/*!
 * Print to the GOAL print buffer from C
 */
void cprintf(const char* format, ...) __attribute__((format(printf, 1, 2)));

/*!
 * Print directly to the C stdout
 * The "k" parameter is ignored, so this is just like printf
 */
void Msg(s32 k, const char* format, ...) __attribute__((format(printf, 2, 3)));

/*!
 * Print directly to the C stdout
 * This is identical to Msg.
 */
void MsgWarn(const char* format, ...) __attribute__((format(printf, 1, 2)));

/*!
 * Print directly to the C stdout
 * This is identical to Msg.
 */
void MsgErr(const char* format, ...) __attribute__((format(printf, 1, 2)));

/*!
 * Reverse string in place.
 */
void reverse(char* s);

/*!
 * Helper function for floating point to string conversion.
 */
s32 cvt_float(float x, s32 precision, s32* lead_char, char* buff_start, char* buff_end, u32 flags);

/*!
 * Convert floating point to a string.
 */
void ftoa(char* out_str, float x, s32 desired_len, char pad_char, s32 precision, u32 flags);

/*!
 * Convert integer to a string.
 */
char* kitoa(char* buffer, s64 value, u64 base, s32 length, char pad, u32 flag);

/*!
 * Convert 128-bit integer to string.  Not implemented because it is never used in the game.
 * The format function does have the ability to call it, but it always passes a zero because
 * getting a 128-bit integer in PS2 gcc's varargs doesn't work.
 */
void kqtoa();

extern "C" {
s32 format_impl(uint64_t* args);
}

#endif  // RUNTIME_KPRINT_H
