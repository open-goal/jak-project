#pragma once

/*!
 * @file FontUtils.h
 *
 * Code for handling text and strings in Jak 1's "large font" format.
 *
 * MAKE SURE THIS FILE IS ENCODED IN UTF-8!!! The various strings here depend on it.
 * Always verify the encoding if string detection suddenly goes awry.
 */

#include "common/common_types.h"

#include <string>
#include <vector>

struct RemapInfo {
  std::string chars;
  std::vector<u8> bytes;
};
struct ReplaceInfo {
  std::string from;
  std::string to;
};

/*!
 * Remaps UTF-8 characters to the appropriate character fit for the game's large font.
 * It is unfortunately quite large.
 */
extern std::vector<RemapInfo> g_font_large_char_remap;
/*!
 * Replaces specific UTF-8 strings with more readable variants.
 */
extern std::vector<ReplaceInfo> g_font_large_string_replace;

RemapInfo* jak1_bytes_to_utf8(const char* in);
std::string& jak1_trans_to_utf8(std::string& str);

std::string convert_to_jak1_encoding(std::string str);
std::string convert_from_jak1_encoding(const char* in);
