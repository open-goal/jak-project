#pragma once

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"
///////////
// SDATA
///////////
extern Ptr<u8> OutputPending;
extern Ptr<u8> PrintPending;
extern s32 MessCount;

extern char AckBufArea[40];
extern char ConvertTable[16];  // todo rm
extern Ptr<u8> MessBufArea;
extern Ptr<u8> OutputBufArea;
extern Ptr<u8> PrintBufArea;

constexpr u32 DEBUG_MESSAGE_BUFFER_SIZE = 0x80000;
constexpr u32 DEBUG_OUTPUT_BUFFER_SIZE = 0x80000;
constexpr u32 DEBUG_PRINT_BUFFER_SIZE = 0x200000;
constexpr u32 PRINT_BUFFER_SIZE = 0x2000;

void kprint_init_globals_common();

#ifdef __linux__
/*!
 * Print directly to the C stdout
 * The "k" parameter is ignored, so this is just like printf
 */
void Msg(s32 k, const char* format, ...) __attribute__((format(printf, 2, 3)));
#elif _WIN32
/*!
 * Print directly to the C stdout
 * The "k" parameter is ignored, so this is just like printf
 */
void Msg(s32 k, const char* format, ...);
#endif

#ifdef __linux__
/*!
 * Print directly to the C stdout
 * This is identical to Msg.
 */
void MsgWarn(const char* format, ...) __attribute__((format(printf, 1, 2)));
#elif _WIN32
/*!
 * Print directly to the C stdout
 * This is identical to Msg.
 */
void MsgWarn(const char* format, ...);
#endif
#ifdef __linux__
/*!
 * Print directly to the C stdout
 * This is identical to Msg.
 */
void MsgErr(const char* format, ...) __attribute__((format(printf, 1, 2)));
#elif _WIN32
/*!
 * Print directly to the C stdout
 * This is identical to Msg.
 */
void MsgErr(const char* format, ...);
#endif