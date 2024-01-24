#ifndef SBANK_H_
#define SBANK_H_

#include "common/common_types.h"

namespace jak3 {
struct SoundBankInfo {
  char bank_name[16];
  char slot_name[16];
  u32 spu_loc;
  u32 spu_size;
  u32 unk;
  bool in_use;
  u8 unk2;
  u8 unk3;
  u8 index;
  u32 unk4;
};
void InitBanks();
}  // namespace jak3

#endif  // SBANK_H_
