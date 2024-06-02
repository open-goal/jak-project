#pragma once

/*!
 * @file Reader.h
 *
 * The Reader converts text into GOOS object, for interpreting or compiling.
 * The Reader also stores the GOOS symbol table, and is able to figure out where forms
 * came from, for printing error messages about forms.
 *
 * The reader also know where the source folder is, through an environment variable set when
 * launching the compiler or the compiler test.
 */

#include <memory>
#include <optional>
#include <unordered_map>
#include <utility>

#include "common/goos/Object.h"
#include "common/goos/TextDB.h"
#include "common/repl/repl_wrapper.h"
#include "common/util/Assert.h"

namespace goos {

/*!
 * Wrapper around a source of text that allows reading/peeking.
 */
struct TextStream {
  explicit TextStream(std::shared_ptr<SourceText> ptr) { text = std::move(ptr); }

  std::shared_ptr<SourceText> text;
  int seek = 0;
  int line_count = 0;

  char peek() {
    ASSERT(seek < text->get_size());
    return text->get_text()[seek];
  }

  char peek(int i) {
    ASSERT(seek + i < text->get_size());
    return text->get_text()[seek + i];
  }

  char read() {
    ASSERT(seek < text->get_size());
    char c = text->get_text()[seek++];
    if (c == '\n')
      line_count++;
    return c;
  }

  bool text_remains() { return seek < text->get_size(); }
  bool text_remains(int i) { return seek + i < text->get_size(); }
  void seek_past_whitespace_and_comments();
  void read_utf8_encoding(bool throw_on_error);
};

/*!
 * A Token used for parsing.
 */
struct Token {
  std::shared_ptr<SourceText> source_text;
  int source_offset;
  int source_line;
  std::string text;
};

class Reader {
 public:
  Reader();
  Object read_from_string(const std::string& str,
                          bool add_top_level = true,
                          const std::optional<std::string>& string_name = {});
  std::optional<Object> read_from_stdin(const std::string& prompt, REPL::Wrapper& repl);
  Object read_from_file(const std::vector<std::string>& file_path, bool check_encoding = false);
  bool check_string_is_valid(const std::string& str) const;

  SymbolTable symbolTable;
  TextDb db;

 private:
  Object internal_read(std::shared_ptr<SourceText> text,
                       bool check_encoding,
                       bool add_top_level = true);
  Object read_list(TextStream& stream, bool expect_close_paren = true);
  bool read_object(Token& tok, TextStream& ts, Object& obj);
  bool read_array(TextStream& stream, Object& o);

  void throw_reader_error(TextStream& here, const std::string& err, int seek_offset);
  Token get_next_token(TextStream& stream);

  bool try_token_as_symbol(const Token& tok, Object& obj);
  bool try_token_as_char(const Token& tok, Object& obj);
  bool try_token_as_float(const Token& tok, Object& obj);
  bool try_token_as_binary(const Token& tok, Object& obj);
  bool try_token_as_hex(const Token& tok, Object& obj);
  bool try_token_as_integer(const Token& tok, Object& obj);
  bool read_string(TextStream& stream, Object& obj);
  void add_reader_macro(const std::string& shortcut, std::string replacement);

  bool m_valid_symbols_chars[256];
  bool m_valid_source_text_chars[256];

  bool is_valid_source_char(char c) const;

  std::unordered_map<std::string, std::string> m_reader_macros;
};

std::string get_readable_string(const char* in);
}  // namespace goos
