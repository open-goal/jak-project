#ifndef JAK_V2_ISO_API_H
#define JAK_V2_ISO_API_H
#include "isocommon.h"

void LoadISOFileToIOP(FileRecord *file, void *addr, uint32_t length);
void LoadISOFileToEE(FileRecord *file, uint32_t ee_addr, uint32_t length);

#endif //JAK_V2_ISO_API_H
