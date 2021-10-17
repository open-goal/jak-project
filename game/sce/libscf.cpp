#include "libscf.h"

namespace ee {
int sceScfGetAspect() {
  return SCE_ASPECT_43;
}

int sceScfGetLanguage() {
  return SCE_ENGLISH_LANGUAGE;
}

void sceCdReadClock(sceCdCLOCK* result) {
  result->stat = 0;  // ??
  result->second = 1;
  result->minute = 0x92;
  result->hour = 0x76;
  result->week = 13;
  result->day = 0x99;
  result->month = 0x16;
  result->year = 0x19;
}
}  // namespace ee