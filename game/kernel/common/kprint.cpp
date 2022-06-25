#include "kprint.h"

#include <cstdarg>
#include <cstdio>
#include <cstring>

#include "game/kernel/common/Ptr.h"

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