#pragma once

#include "isocommon.h"

s32 LoadISOFileToIOP(FileRecord* file, void* addr, uint32_t length);
s32 LoadISOFileToEE(FileRecord* file, uint32_t ee_addr, uint32_t length);
s32 LoadISOFileChunkToEE(FileRecord* file, uint32_t dest_addr, uint32_t length, uint32_t offset);
