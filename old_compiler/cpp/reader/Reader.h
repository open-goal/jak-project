#ifndef COMPILER_READER_H
#define COMPILER_READER_H

#include <memory>
#include <cassert>
#include "goos/Object.h"
#include "reader/TextDb.h"

struct TextStream {
  explicit TextStream(std::shared_ptr<ITextFragment> ptr) { text = ptr; }

  std::shared_ptr<ITextFragment> text;
  int seek = 0;
  int line_count = 0;

  char peek() {
    assert(seek < text->get_size());
    return text->get_text()[seek];
  }

  char peek(int i) {
    assert(seek + i < text->get_size());
    return text->get_text()[seek + i];
  }

  char read() {
    assert(seek < text->get_size());
    char c = text->get_text()[seek++];
    if (c == '\n')
      line_count++;
    return c;
  }

  bool text_remains() { return seek < text->get_size(); }

  bool text_remains(int i) { return seek + i < text->get_size(); }
};

struct Token {
  std::shared_ptr<ITextFragment> source_text;
  int source_offset;
  int source_line;
  std::string text;
};

class Reader {
 public:
  Reader();
  Object read_from_string(const std::string& str);
  Object read_from_stdin(const std::string& prompt_name);
  Object read_from_file(const std::string& filename);

  std::string get_next_dir();

  SymbolTable symbolTable;
  TextDb db;

 private:
  Object internal_read(std::shared_ptr<ITextFragment> text);
  Object read_list(TextStream& stream, bool expect_close_paren = true);

  void seek_past_whitespace_and_comments(TextStream& stream);
  void display_error_info(TextStream& here, const std::string& err);
  Token get_next_token(TextStream& stream);

  bool try_token_as_symbol(const Token& tok, Object& obj);
  bool try_token_as_float(const Token& tok, Object& obj);
  bool try_token_as_binary(const Token& tok, Object& obj);
  bool try_token_as_hex(const Token& tok, Object& obj);
  bool try_token_as_integer(const Token& tok, Object& obj);
  bool try_token_as_boolean(const Token& tok, Object& obj);
  bool read_string(TextStream& stream, Object& obj);

  char valid_symbols_chars[256];
};

#endif  // COMPILER_READER_H
