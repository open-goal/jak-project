#include "libscf.h"

#include <cstdlib>
#include <cstring>
#include <ctime>

#ifdef _WIN32
// clang-format off
#include <Windows.h>
#include <WinNls.h>
#elif __linux__
#include <clocale>
// clang-format on
#endif

namespace ee {
int sceScfGetAspect() {
  return SCE_ASPECT_43;
}

int sceScfGetLanguage() {
#ifdef _WIN32
  // method 1: GetUserDefaultUILanguage
  LANGID curLang = GetUserDefaultUILanguage();
  auto curLangMain = curLang & 0x3ff;        // "base" language
  auto curLangSub = (curLang >> 10) & 0x3f;  // "sub"-language
  if (curLangMain == LANG_JAPANESE) {
    return SCE_JAPANESE_LANGUAGE;
  } else if (curLangMain == LANG_ENGLISH) {
    return SCE_ENGLISH_LANGUAGE;
  } else if (curLangMain == LANG_FRENCH) {
    return SCE_FRENCH_LANGUAGE;
  } else if (curLangMain == LANG_SPANISH) {
    // would non-European Spanish speakers prefer this over English?
    // I'll wait for someone to complain first
    return SCE_SPANISH_LANGUAGE;
  } else if (curLangMain == LANG_GERMAN) {
    return SCE_GERMAN_LANGUAGE;
  } else if (curLangMain == LANG_ITALIAN) {
    return SCE_ITALIAN_LANGUAGE;
  } else if (curLangMain == LANG_PORTUGUESE) {
    if (curLangSub == SUBLANG_PORTUGUESE) {
      return SCE_ENGLISH_LANGUAGE;  // SCE_PORTUGUESE_LANGUAGE;
    } else {
      return SCE_ENGLISH_LANGUAGE;
    }
  } else if (curLangMain == LANG_DUTCH) {
    return SCE_DUTCH_LANGUAGE;
  } else if (curLangMain == LANG_KOREAN) {
    return SCE_KOREAN_LANGUAGE;
  }
#elif __linux__
  const char* lang = std::getenv("LANG");
  if (!lang)
    return SCE_ENGLISH_LANGUAGE;

  if (!std::strncmp(lang, "ja", 2)) {
    return SCE_JAPANESE_LANGUAGE;
  }
  if (!std::strncmp(lang, "en", 2)) {
    return SCE_ENGLISH_LANGUAGE;
  }
  if (!std::strncmp(lang, "fr", 2)) {
    return SCE_FRENCH_LANGUAGE;
  }
  if (!std::strncmp(lang, "es", 2)) {
    return SCE_SPANISH_LANGUAGE;
  }
  if (!std::strncmp(lang, "de", 2)) {
    return SCE_GERMAN_LANGUAGE;
  }
  if (!std::strncmp(lang, "it", 2)) {
    return SCE_ITALIAN_LANGUAGE;
  }
  if (!std::strncmp(lang, "pt", 2)) {
    return SCE_PORTUGUESE_LANGUAGE;
  }
  if (!std::strncmp(lang, "nl", 2)) {
    return SCE_DUTCH_LANGUAGE;
  }
  if (!std::strncmp(lang, "ko", 2)) {
    return SCE_KOREAN_LANGUAGE;
  }
#endif
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
