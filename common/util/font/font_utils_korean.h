/*
    MIT License

    Copyright (c) 2017 Jonghwan Hyeon

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

    https://github.com/jonghwanhyeon/hangul-jamo/blob/main/LICENSE

    Code converted to C++
*/

#pragma once

#include <vector>

#include "common/util/json_util.h"

struct KoreanLookupEntry {
  // glyph to use if no relevant alternative exists
  std::string defaultGlyph;
  // context=>glyph
  // ie. "<G>,\u1166"
  // when wanting to draw a specific jamo, it's spot is indicated by the <G>
  std::unordered_map<std::string, std::string> alternatives;
};
void from_json(const json& j, KoreanLookupEntry& obj);

typedef std::vector<KoreanLookupEntry> KoreanLookupOrientations;

namespace font_util {
bool is_language_id_korean(const int language_id);
std::string compose_korean_containing_text(const std::string& text);
std::string encode_korean_containing_text_to_game(
    const std::string& text,
    const std::unordered_map<std::string, KoreanLookupOrientations> db);
};  // namespace font_util
