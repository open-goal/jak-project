#include "libscf.h"

#include <ctime>

namespace ee {
int sceScfGetAspect() {
  return SCE_ASPECT_43;
}

int sceScfGetLanguage() {
  return SCE_ENGLISH_LANGUAGE;
}

void sceCdReadClock(sceCdCLOCK* result) {
  time_t t = time(0);
  std::tm* date = localtime(&t);

  // convert decimal value into hex value with identical digit representation
  // e.g. 60 -> 0x60
  auto convert = [](u8 val) -> u8 { return ((val % 10) * 0x01) + ((val / 10) * 0x10); };

  result->stat = 0;  // ??
  result->second = convert(date->tm_sec);
  result->minute = convert(date->tm_min);
  result->hour = convert(date->tm_hour);
  result->week = convert((date->tm_yday - date->tm_wday + 7) / 7);
  result->day = convert(date->tm_mday);
  result->month = convert(date->tm_mon + 1);
  result->year = convert(date->tm_year - 100);
}
}  // namespace ee