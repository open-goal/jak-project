#pragma once

/*!
 * @file libsd.h
 * Stub implementation of the IOP sound library
 */

#include "common/common_types.h"

// Batch
#define SD_BATCH_SETPARAM 0x1
#define SD_BATCH_SETSWITCH 0x2
#define SD_BATCH_SETADDR 0x3
#define SD_BATCH_SETCORE 0x4
#define SD_BATCH_WRITEIOP 0x5
#define SD_BATCH_WRITEEE 0x6
#define SD_BATCH_EERETURN 0x7
#define SD_BATCH_GETPARAM 0x10
#define SD_BATCH_GETSWITCH 0x12
#define SD_BATCH_GETADDR 0x13
#define SD_BATCH_GETCORE 0x14

namespace iop {

struct sceSdBatch {
  u16 func;
  u16 entry;
  u32 value;
};

void sceSdSetSwitch(u16 entry, u32 value);
u32 sceSdGetSwitch(u16 entry);

u32 sceSdGetAddr(u16 reg);
void sceSdSetAddr(u16 entry, u32 value);

u16 sceSdGetParam(u16 entry);
void sceSdSetParam(u16 reg, u16 val);

int sceSdProcBatch(sceSdBatch* batch, u32* rets, u32 num);
int sceSdProcBatchEx(sceSdBatch* batch, u32* rets, u32 num, u32 voice);
}  // namespace iop
