#pragma once

#include "common/common_types.h"

namespace ee {

#define sceMcExecIdle (-1)
#define sceMcExecRun 0
#define sceMcExecFinish 1

#define sceMcResSucceed 0
#define sceMcResNoFormat (-2)
#define sceMcResNoEntry (-4)
#define sceMcResDeniedPermit (-5)
#define sceMcResNotEmpty (-6)

#define sceMcFuncNoCardInfo 1
#define sceMcFuncNoOpen 2
#define sceMcFuncNoClose 3
#define sceMcFuncNoRead 5
#define sceMcFuncNoWrite 6
#define sceMcFuncNoGetDir 13
#define sceMcFuncNoDelete 15
#define sceMcFuncNoFormat 16
#define sceMcFuncNoUnformat 17

#define sceMcTypePS2 2

struct sceMcStDateTime {
  u8 unk;
  u8 sec;
  u8 min;
  u8 hour;
  u8 day;
  u8 month;
  u16 year;
};

struct sceMcTblGetDir {
  sceMcStDateTime created, modified;
  u32 file_size;
  u16 attr_file;
  u16 unk;
  u32 unk2;
  u32 pad_apl_no;
  char name[32];
};

s32 sceMcMkdir(s32 port, s32 slot, const char* name);
s32 sceMcSync(s32 mode, s32* cmd, s32* result);
s32 sceMcOpen(s32 port, s32 slot, const char* name, s32 mode);
s32 sceMcWrite(s32 fd, const void* buff, s32 size);
s32 sceMcClose(s32 fd);

s32 sceMcGetInfo(s32 port, s32 slot, s32* type, s32* free, s32* format);
s32 sceMcFormat(s32 port, s32 slot);
s32 sceMcUnformat(s32 port, s32 slot);
s32 sceMcDelete(s32 port, s32 slot, const char* name);

s32 sceMcGetDir(s32 port, int slot, const char* name, u32 mode, s32 maxent, sceMcTblGetDir* table);
s32 sceMcRead(s32 fd, void* buff, s32 size);

void flush_memory_card_to_file();
void read_memory_card_from_file();
}  // namespace ee