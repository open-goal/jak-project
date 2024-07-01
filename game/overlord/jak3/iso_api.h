#ifndef ISO_API_H_
#define ISO_API_H_

#include "common/common_types.h"

#include "game/overlord/jak3/iso_structs.h"

namespace jak3 {
u32 LoadISOFileToIOP(const ISOFileDef* file, void* addr, u32 length);
}

#endif  // ISO_API_H_
