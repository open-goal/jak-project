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

#include <deque>
#include <iostream>
#include <iterator>
#include <memory>
#include <optional>
#include <tuple>
#include <vector>

// TODO - code not yet verified
template <typename Iterator>
class tee_iterator {
  using value_type = typename std::iterator_traits<Iterator>::value_type;
  Iterator source;
  std::shared_ptr<std::deque<value_type>> buffer;
  size_t index;

 public:
  tee_iterator(Iterator src, std::shared_ptr<std::deque<value_type>> buf, size_t idx)
      : source(src), buffer(buf), index(idx) {}

  std::optional<value_type> next() {
    if (index < buffer->size()) {
      return (*buffer)[index++];
    } else {
      if (source != Iterator{}) {
        auto val = *source;
        ++source;
        buffer->push_back(val);
        ++index;
        return val;
      } else {
        return std::nullopt;
      }
    }
  }
};

struct Syllable {
  std::string leading_consonant;
  std::string vowel;
  std::optional<std::string> trailing_consonant;
};

std::string compose_korean_containing_text(const std::string& text);
std::string decompose_korean_containing_text(const std::string& text);