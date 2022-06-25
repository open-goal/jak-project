#pragma once

/*!
 * @file kprint.h
 * GOAL Print.  Contains GOAL I/O, Print, Format...
 */

#include "common/common_types.h"
#include "game/kernel/common/Ptr.h"

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

#ifdef __linux__
/*!
 * Print to the GOAL print buffer from C
 */
void cprintf(const char* format, ...) __attribute__((format(printf, 1, 2)));
#elif _WIN32
/*!
 * Print to the GOAL print buffer from C
 */
void cprintf(const char* format, ...);
#endif

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
