#include <iostream>
#include "Reader.h"
#include "TextDb.h"
#include "third-party/linenoise.h"

Reader::Reader() {
  linenoise::SetHistoryMaxLen(400);

  for (auto& x : valid_symbols_chars) {
    x = false;
  }

  for (char x = 'a'; x <= 'z'; x++) {
    valid_symbols_chars[(int)x] = true;
  }

  for (char x = 'A'; x <= 'Z'; x++) {
    valid_symbols_chars[(int)x] = true;
  }

  for (char x = '0'; x <= '9'; x++) {
    valid_symbols_chars[(int)x] = true;
  }

  const char bonus[] = "!$%&*+-/\\.,@^_-;:<>?~=#";

  for (const char* c = bonus; *c; c++) {
    valid_symbols_chars[(int)*c] = true;
  }
}
Object Reader::read_from_stdin(const std::string& prompt_name) {
  //  // display prompt
  //  printf("%s> ", prompt_name.c_str());
  //
  //  // read text
  //  std::string line;
  //  std::getline(std::cin, line);

  std::string line;
  std::string prompt_full = "\033[0m" + prompt_name + "> ";
  linenoise::Readline(prompt_full.c_str(), line);
  linenoise::AddHistory(line.c_str());
  // todo, decide if we should keep reading or not.

  // create text fragment and add to the DB
  auto textFrag = std::make_shared<ReplText>(line);
  db.insert(textFrag);

  // perform read
  auto result = internal_read(textFrag);
  db.link(result, textFrag, 0);
  return result;
}

Object Reader::read_from_string(const std::string& str) {
  // create text fragment and add to the DB
  auto textFrag = std::make_shared<ProgramString>(str);
  db.insert(textFrag);

  // perform read
  auto result = internal_read(textFrag);
  db.link(result, textFrag, 0);
  return result;
}

Object Reader::read_from_file(const std::string& filename) {
  auto textFrag = std::make_shared<FileText>(get_next_dir() + "/" + filename);
  db.insert(textFrag);

  auto result = internal_read(textFrag);
  db.link(result, textFrag, 0);
  return result;
}

Object Reader::internal_read(std::shared_ptr<ITextFragment> text) {
  // first create stream
  TextStream ts(text);

  // clean up first whitespace
  seek_past_whitespace_and_comments(ts);

  // read list!
  auto objs = read_list(ts, false);
  return PairObject::make_new(SymbolObject::make_new(symbolTable, "top-level"), objs);
}

void Reader::seek_past_whitespace_and_comments(TextStream& stream) {
  while (stream.text_remains()) {
    char c = stream.peek();
    switch (c) {
      case ' ':
      case '\t':
      case '\n':
        // just a whitespace, eat it!
        stream.read();
        break;

      case ';':
        // line comment.
        while (stream.text_remains() && stream.read() != '\n') {
        }
        break;

      case '#':
        if (stream.text_remains(1) && stream.peek(1) == '|') {
          assert(stream.read() == '#');  // #
          assert(stream.read() == '|');  // |

          bool found_end = false;
          // find |#
          while (stream.text_remains() && !found_end) {
            // find |
            while (stream.text_remains() && stream.read() != '|') {
            }
            if (stream.text_remains() && stream.read() == '#') {
              found_end = true;
            }
          }
          return;

        } else {
          // not a line comment
          return;
        }
        break;

      default:
        return;
    }
  }
}

// given a stream starting at first character of the token,
// return the token. does not consume any whitespace at the end.
Token Reader::get_next_token(TextStream& stream) {
  assert(stream.text_remains());
  Token t;
  t.source_line = stream.line_count;
  t.source_offset = stream.seek;
  t.source_text = stream.text;

  char first = stream.read();
  t.text.push_back(first);

  // paren/double quote is its own token.
  if (first == '(' || first == ')' || first == '"' || first == '\'' || first == '`')
    return t;

  if (first == ',' && stream.text_remains() && stream.peek() == '@') {
    t.text.push_back(stream.read());
    return t;
  } else if (first == ',') {
    return t;
  }

  while (stream.text_remains()) {
    char next = stream.peek();
    if (next == ' ' || next == '\n' || next == '\t' || next == ')' ||
        next == ';' /*|| next == '#'*/ || next == '(') {
      return t;
    } else {
      t.text.push_back(stream.read());
    }
  }

  return t;
}

const static std::unordered_map<std::string, std::string> reader_macros = {
    {"'", "quote"},
    {"`", "quasiquote"},
    {",", "unquote"},
    {",@", "unquote-splicing"}};

// call on char after open paren.
Object Reader::read_list(TextStream& ts, bool expect_close_paren) {
  seek_past_whitespace_and_comments(ts);
  std::vector<Object> objects;

  bool got_close_paren = false;
  int start_offset = ts.seek;

  while (ts.text_remains()) {
    auto tok = get_next_token(ts);

    // reader macro thing:
    bool got_reader_macro = false;
    std::string reader_macro_string;
    auto kv = reader_macros.find(tok.text);
    if (kv != reader_macros.end()) {
      got_reader_macro = true;
      reader_macro_string = kv->second;
      tok = get_next_token(ts);
    }

    auto insert_object = [&](Object o) {
      if (got_reader_macro) {
        objects.push_back(
            build_list({SymbolObject::make_new(symbolTable, reader_macro_string), o}));
      } else {
        objects.push_back(o);
      }
    };

    if (tok.text.empty()) {
      // empty list
      break;
    } else if (tok.text[0] == '(') {
      assert(tok.text.length() == 1);
      insert_object(read_list(ts, true));
      seek_past_whitespace_and_comments(ts);
      continue;
    } else if (tok.text[0] == ')') {
      got_close_paren = true;
      assert(tok.text.length() == 1);
      break;
    } else {
      Object obj;
      // try as an array
      // try as char

      // try as integer
      if (try_token_as_integer(tok, obj)) {
        seek_past_whitespace_and_comments(ts);
        insert_object(obj);
        continue;
      }

      // try as hex
      if (try_token_as_hex(tok, obj)) {
        seek_past_whitespace_and_comments(ts);
        insert_object(obj);
        continue;
      }

      // try as binary
      if (try_token_as_binary(tok, obj)) {
        seek_past_whitespace_and_comments(ts);
        insert_object(obj);
        continue;
      }

      // try as float
      if (try_token_as_float(tok, obj)) {
        seek_past_whitespace_and_comments(ts);
        insert_object(obj);
        continue;
      }

      // try as bool
      if (try_token_as_boolean(tok, obj)) {
        seek_past_whitespace_and_comments(ts);
        insert_object(obj);
        continue;
      }

      // try as string
      if (tok.text[0] == '"') {
        // it's a string.
        assert(tok.text.length() == 1);
        if (read_string(ts, obj)) {
          seek_past_whitespace_and_comments(ts);
          insert_object(obj);
        } else {
          display_error_info(ts, "failed to read string, close quote not found");
        }
        continue;
      }

      // try as symbol
      if (try_token_as_symbol(tok, obj)) {
        insert_object(obj);
        seek_past_whitespace_and_comments(ts);
        continue;
      }
      display_error_info(ts, "invalid token encountered in reader: " + tok.text);
    }
  }

  if (expect_close_paren && !got_close_paren) {
    display_error_info(ts, "failed to find close paren");
  }

  if (got_close_paren && !expect_close_paren) {
    display_error_info(ts, "found an unexpected close paren");
  }

  auto rv = build_list(objects);
  db.link(rv, ts.text, start_offset);
  return rv;
}

bool Reader::try_token_as_symbol(const Token& tok, Object& obj) {
  // check start character is valid:
  assert(!tok.text.empty());
  char start = tok.text[0];
  // if(start == '!' || start == '*' || start == '/' || start == '+' || start == '-' || start == '='
  // || start == '<' || start == '>' || start == '?' || (start >= 'A' && start < 'Z') || (start >=
  // 'a' && start <= 'z')) {
  if (valid_symbols_chars[(int)start]) {
    obj = SymbolObject::make_new(symbolTable, tok.text);
    return true;
  } else {
    return false;
  }
}

// todo, handle \n in a string.
bool Reader::read_string(TextStream& stream, Object& obj) {
  bool got_close_quote = false;
  std::string str;

  while (stream.text_remains()) {
    char c = stream.read();
    if (c == '"') {
      obj = StringObject::make_new(str);
      got_close_quote = true;
      break;
    }
    str.push_back(c);
  }

  return got_close_quote;
}

static bool number_start(char c) {
  return (c >= '0' && c <= '9') || c == '-';
}

static bool str_contains(const std::string& str, char c) {
  for (auto& x : str)
    if (x == c)
      return true;
  return false;
}

bool Reader::try_token_as_float(const Token& tok, Object& obj) {
  if (number_start(tok.text[0]) && str_contains(tok.text, '.')) {
    try {
      std::size_t end = 0;
      double v = std::stod(tok.text, &end);
      if (end != tok.text.size())
        return false;
      obj = Object::make_float(v);
      return true;
    } catch (std::exception& e) {
      return false;
    }
  }
  return false;
}

bool Reader::try_token_as_binary(const Token& tok, Object& obj) {
  if (tok.text.size() >= 3 && tok.text[0] == '#' && tok.text[1] == 'b') {
    uint64_t value = 0;
    if (tok.text.size() > 64 + 2)
      return false;
    for (uint32_t i = 2; i < tok.text.size(); i++) {
      value <<= 1u;
      if (tok.text[i] == '1')
        value++;
      else if (tok.text[i] != '0')
        return false;
    }
    obj = Object::make_integer((int64_t)value);
    return true;
  }
  return false;
}

bool Reader::try_token_as_hex(const Token& tok, Object& obj) {
  if (tok.text.size() >= 3 && tok.text[0] == '#' && tok.text[1] == 'x') {
    uint64_t v = 0;
    try {
      std::size_t end = 0;
      v = std::stoll(tok.text.substr(2), &end, 16);
      if (end + 2 != tok.text.size())
        return false;
      obj = Object::make_integer(v);
      return true;
    } catch (std::exception& e) {
      return false;
    }
  }
  return false;
}

bool Reader::try_token_as_integer(const Token& tok, Object& obj) {
  if (number_start(tok.text[0]) && !str_contains(tok.text, '.')) {
    uint64_t v = 0;
    try {
      std::size_t end = 0;
      v = std::stoll(tok.text, &end);
      if (end != tok.text.size())
        return false;
      obj = Object::make_integer(v);
      return true;
    } catch (std::exception& e) {
      return false;
    }
  }
  return false;
}

bool Reader::try_token_as_boolean(const Token& tok, Object& obj) {
  if (tok.text.size() != 2)
    return false;
  if (tok.text[0] == '#') {
    if (tok.text[1] == 'f') {
      obj = SymbolObject::make_new(symbolTable, "#f");
      return true;
    } else if (tok.text[1] == 't') {
      obj = SymbolObject::make_new(symbolTable, "#t");
      return true;
    }
  }
  return false;
}

void Reader::display_error_info(TextStream& here, const std::string& err) {
  printf("Reader error:\n%s\nat %s", err.c_str(), db.get_info_for(here.text, here.seek).c_str());
  throw std::runtime_error(err);
}

std::string Reader::get_next_dir() {
  auto result = std::getenv("NEXT_DIR");
  if (!result)
    throw std::runtime_error(
        "Environment variable NEXT_DIR is not set.  Please set this to point to next/");
  return {result};
}