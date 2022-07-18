/*
  https://github.com/svenstaro/glsl-language-server/blob/master/LICENSE

  MIT License

  Copyright (c) 2017 Sven-Hendrik Haase

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
*/

#pragma once

#include <string>
#include <tuple>

#include "third-party/json.hpp"

using json = nlohmann::json;

class MessageBuffer {
 public:
  MessageBuffer();
  virtual ~MessageBuffer();
  void handle_char(char c);
  const std::map<std::string, std::string>& headers() const;
  const json& body() const;
  const std::string& raw() const;
  bool message_completed();
  void clear();

 private:
  std::tuple<std::string, std::string> try_parse_header(std::string& message) const;

  std::string m_raw_message;
  std::map<std::string, std::string> m_headers;
  json m_body;

  // This is set once a sole \r\n is encountered because it denotes that the
  // header is done.
  bool m_is_header_done = false;
  bool m_reading_content = false;
};
