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

#include "stdio.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"

MessageBuffer::MessageBuffer() {}
MessageBuffer::~MessageBuffer() {}

void MessageBuffer::handle_char(char c) {
  m_raw_message += c;

  if (!m_reading_content) {
    auto [header_name, header_value] = try_parse_header(m_raw_message);
    // Check whether we were actually able to parse a header.
    // If so, add it to our known headers.
    // We'll also reset our string then.
    if (!header_name.empty()) {
      lg::trace("found header!");
      m_headers[header_name] = header_value;
      m_raw_message.clear();
    }
  }

  // A sole \r\n is the separator between the header block and the body block
  // but we don't need it.
  if (m_raw_message == "\r\n") {
    m_raw_message.clear();
    m_is_header_done = true;
    m_reading_content = true;
    lg::trace("Header complete, content length: {}", m_headers["Content-Length"]);
  }

  if (m_is_header_done) {
    // Now that we know that we're in the body, we just have to count until
    // we reach the length of the body as provided in the Content-Length
    // header.
    auto content_length = std::stoi(m_headers["Content-Length"]);
    if (m_raw_message.length() == (size_t)content_length) {
      m_body = json::parse(m_raw_message);
      m_reading_content = false;
    }
  }
}

const std::map<std::string, std::string>& MessageBuffer::headers() const {
  return m_headers;
}

const json& MessageBuffer::body() const {
  return m_body;
}

const std::string& MessageBuffer::raw() const {
  return m_raw_message;
}

bool MessageBuffer::message_completed() {
  if (m_is_header_done && !m_body.empty()) {
    return true;
  }
  return false;
}

std::tuple<std::string, std::string> MessageBuffer::try_parse_header(std::string& message) const {
  auto eol_pos = message.find("\r\n");
  if (eol_pos != std::string::npos) {
    std::string header_string = message.substr(0, eol_pos);
    auto delim_pos = header_string.find(":");
    if (delim_pos != std::string::npos) {
      std::string header_name = header_string.substr(0, delim_pos);
      std::string header_value = header_string.substr(delim_pos + 1);
      return std::make_tuple(header_name, header_value);
    }
  }
  return std::make_tuple(std::string{}, std::string{});
}

void MessageBuffer::clear() {
  m_raw_message.clear();
  m_headers.clear();
  m_body.clear();
  m_is_header_done = false;
}
