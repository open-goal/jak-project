/*!
 * @file Reader.cpp
 *
 * The Reader converts text into GOOS object, for interpreting or compiling.
 * The Reader also stores the GOOS symbol table, and is able to figure out where forms
 * came from, for printing error messages about forms.
 *
 * The reader also know where the source folder is, through an environment variable set when
 * launching the compiler or the compiler test.
 */

#include "Reader.h"
#include "common/util/FileUtil.h"
#include "third-party/fmt/core.h"
#include <filesystem>
#include "ReplUtils.h"

namespace goos {

namespace {
/*!
 * Is this a valid character to start a decimal integer number?
 */
bool decimal_start(char c) {
  return (c >= '0' && c <= '9') || c == '-';
}

/*!
 * Is this a valid character to start a floating point number?
 */
bool float_start(char c) {
  return (c >= '0' && c <= '9') || c == '-' || c == '.';
}

/*!
 * Is this a valid character for a hex number?
 */
bool hex_char(char c) {
  return !((c < '0' || c > '9') && (c < 'a' || c > 'f') && (c < 'A' || c > 'F'));
}

/*!
 * Does the given string contain c?
 */
bool str_contains(const std::string& str, char c) {
  for (auto& x : str) {
    if (x == c) {
      return true;
    }
  }
  return false;
}
}  // namespace

/*!
 * Advance a TextStream through any comments or whitespace.
 * This will leave the stream at the next non-whitespace character (or at the end)
 */
void TextStream::seek_past_whitespace_and_comments() {
  while (text_remains()) {
    char c = peek();
    switch (c) {
      case ' ':
      case '\t':
      case '\n':
        // just a whitespace, eat it!
        read();
        break;

      case ';':
        // line comment.
        while (text_remains() && read() != '\n') {
        }
        break;

      case '#':
        if (text_remains(1) && peek(1) == '|') {
          assert(read() == '#');  // #
          assert(read() == '|');  // |

          bool found_end = false;
          // find |#
          while (text_remains() && !found_end) {
            // find |
            while (text_remains() && read() != '|') {
            }
            if (text_remains() && read() == '#') {
              found_end = true;
            }
          }
          continue;

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

Reader::Reader() {
  // add default macros
  add_reader_macro("'", "quote");
  add_reader_macro("`", "quasiquote");
  add_reader_macro(",", "unquote");
  add_reader_macro(",@", "unquote-splicing");

  // setup table of which characters are valid for starting a symbol
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

/*!
 * Prompt the user and read the result.
 */
Object Reader::read_from_stdin(const std::string& prompt, ReplWrapper& repl) {
  // escape code will make sure that we remove any color
  std::string prompt_full = "\033[0m" + prompt;
  std::string line = repl.readline(prompt_full);
  repl.add_to_history(line.c_str());
  // todo, decide if we should keep reading or not.

  // create text fragment and add to the DB
  auto textFrag = std::make_shared<ReplText>(line);
  db.insert(textFrag);

  // perform read
  auto result = internal_read(textFrag);
  db.link(result, textFrag, 0);
  return result;
}

/*!
 * Read a string.
 */
Object Reader::read_from_string(const std::string& str, bool add_top_level) {
  // create text fragment and add to the DB
  auto textFrag = std::make_shared<ProgramString>(str);
  db.insert(textFrag);

  // perform read
  auto result = internal_read(textFrag, add_top_level);
  db.link(result, textFrag, 0);
  return result;
}

/*!
 * Read a file
 */
Object Reader::read_from_file(const std::vector<std::string>& file_path) {
  auto textFrag = std::make_shared<FileText>(file_util::get_file_path(file_path));
  db.insert(textFrag);

  auto result = internal_read(textFrag);
  db.link(result, textFrag, 0);
  return result;
}

/*!
 * Common read for a SourceText
 */
Object Reader::internal_read(std::shared_ptr<SourceText> text, bool add_top_level) {
  // first create stream
  TextStream ts(text);

  // clean up first whitespace
  ts.seek_past_whitespace_and_comments();

  // read list!
  auto objs = read_list(ts, false);
  if (add_top_level) {
    return PairObject::make_new(SymbolObject::make_new(symbolTable, "top-level"), objs);
  } else {
    return objs;
  }
}

/*!
 * Given a stream starting at the first character of a token, get the token. Doesn't consume
 * whitespace at the end and leaves the stream on the first character after the token.
 */
Token Reader::get_next_token(TextStream& stream) {
  assert(stream.text_remains());
  Token t;
  t.source_line = stream.line_count;
  t.source_offset = stream.seek;
  t.source_text = stream.text;

  char first = stream.read();
  t.text.push_back(first);

  // First - look for special tokens which end early:

  // parens, double quotes, quotes, and backticks are tokens.
  if (first == '(' || first == ')' || first == '"' || first == '\'' || first == '`')
    return t;

  // ",@" is its own token
  if (first == ',' && stream.text_remains() && stream.peek() == '@') {
    t.text.push_back(stream.read());
    return t;
  } else if (first == ',') {
    // "," is its own token.
    return t;
  } else if (first == '#' && stream.text_remains() && stream.peek() == '(') {
    t.text.push_back(stream.read());
    return t;
  }

  // Second - not a special token, so we read until we get a character that ends the token.
  while (stream.text_remains()) {
    char next = stream.peek();
    if (next == ' ' || next == '\n' || next == '\t' || next == ')' || next == ';' || next == '#' ||
        next == '(') {
      return t;
    } else {
      // not the end, so add to token.
      t.text.push_back(stream.read());
    }
  }

  return t;
}

/*!
 * Add a macro that replaces the sequence of [shortcut, other_token] with
 * (replacement other_token) <- a list with two objects, replacement is a symbol.
 * These are used to make 'x turn into (quote x) and similar.
 */
void Reader::add_reader_macro(const std::string& shortcut, std::string replacement) {
  reader_macros[shortcut] = std::move(replacement);
}

/*!
 * Try to read an object.
 */
bool Reader::read_object(Token& tok, TextStream& ts, Object& obj) {
  try {
    // try as integer
    if (try_token_as_integer(tok, obj)) {
      return true;
    }

    // try as hex
    if (try_token_as_hex(tok, obj)) {
      return true;
    }

    // try as binary
    if (try_token_as_binary(tok, obj)) {
      return true;
    }

    // try as float
    if (try_token_as_float(tok, obj)) {
      return true;
    }

    // try as string
    if (tok.text[0] == '"') {
      // it's a string.
      assert(tok.text.length() == 1);
      if (read_string(ts, obj)) {
        return true;
      } else {
        throw_reader_error(ts, "failed to read string, close quote not found", -1);
        return false;
      }
    }

    if (tok.text[0] == '#' && tok.text.size() >= 2 && tok.text[1] == '(') {
      if (read_array(ts, obj)) {
        return true;
      }
    }

    if (try_token_as_char(tok, obj)) {
      return true;
    }

    // try as symbol
    if (try_token_as_symbol(tok, obj)) {
      return true;
    }
  } catch (std::exception& e) {
    throw_reader_error(ts, "parsing token " + tok.text + " failed: " + e.what(), -1);
  }

  return false;
}

bool Reader::read_array(TextStream& stream, Object& o) {
  //  assert(stream.read() == '(');
  stream.seek_past_whitespace_and_comments();
  std::vector<Object> objects;

  bool got_close_paren = false;
  while (stream.text_remains()) {
    auto tok = get_next_token(stream);
    assert(!tok.text.empty());

    if (tok.text[0] == '(') {
      assert(tok.text.length() == 1);
      objects.push_back(read_list(stream, true));
      stream.seek_past_whitespace_and_comments();
      continue;
    } else if (tok.text[0] == ')') {
      assert(tok.text.length() == 1);
      got_close_paren = true;
      break;
    } else {
      Object next_obj;
      if (read_object(tok, stream, next_obj)) {
        stream.seek_past_whitespace_and_comments();
        objects.push_back(next_obj);
      } else {
        throw_reader_error(stream, "invalid token encountered in array reader: " + tok.text,
                           -int(tok.text.size()));
      }
    }
  }

  if (!got_close_paren) {
    throw_reader_error(stream, "An array must end in a close parenthesis", -1);
    return false;
  }

  o = ArrayObject::make_new(objects);
  return true;
}

/*!
 * Call this on the character after the open paren.
 */
Object Reader::read_list(TextStream& ts, bool expect_close_paren) {
  ts.seek_past_whitespace_and_comments();
  std::vector<Object> objects;

  bool got_close_paren = false;      // does this list end?
  bool got_dot = false;              // did we get a . ?
  bool got_thing_after_dot = false;  // did we get an object after the . ?
  int start_offset = ts.seek;

  // loop over tokens
  while (ts.text_remains()) {
    auto tok = get_next_token(ts);

    // reader macro thing:
    bool got_reader_macro = false;

    std::string reader_macro_string;
    auto kv = reader_macros.find(tok.text);
    if (kv != reader_macros.end()) {
      // we found a reader macro! Remember this, and get the next token.
      got_reader_macro = true;
      reader_macro_string = kv->second;
      tok = get_next_token(ts);
    } else {
      // no reader macro
      if (tok.text == ".") {
        // list dot notation (ex, (1 . 2))
        if (got_dot) {
          throw_reader_error(ts, "A list cannot have multiple dots.", -1);
        }
        ts.seek_past_whitespace_and_comments();
        if (!ts.text_remains()) {
          throw_reader_error(ts, "A list cannot end in a dot", -1);
        }
        tok = get_next_token(ts);
        got_dot = true;
      }
    }

    // inserter function, used to properly insert a next object
    auto insert_object = [&](Object o) {
      if (got_thing_after_dot) {
        throw_reader_error(ts, "A list cannot have multiple entries after the dot", -1);
      }

      // create child list if we got a reader macro (ex 'x -> (quote x))
      if (got_reader_macro) {
        objects.push_back(
            build_list({SymbolObject::make_new(symbolTable, reader_macro_string), o}));
      } else {
        objects.push_back(o);
      }

      // remember if we got an object after the dot
      if (got_dot) {
        got_thing_after_dot = true;
      }
    };

    if (tok.text.empty()) {
      assert(false);
      // empty list
      break;
    } else if (tok.text[0] == '(') {
      // nested list
      assert(tok.text.length() == 1);
      insert_object(read_list(ts, true));
      ts.seek_past_whitespace_and_comments();
      continue;
    } else if (tok.text[0] == ')') {
      // end of this list
      got_close_paren = true;
      assert(tok.text.length() == 1);
      break;
    } else {
      // try to get an object
      Object obj;

      if (read_object(tok, ts, obj)) {
        ts.seek_past_whitespace_and_comments();
        insert_object(obj);
      } else {
        throw_reader_error(ts, "invalid token encountered in reader: " + tok.text,
                           -int(tok.text.size()));
      }
    }
  }

  // done getting objects.  Check close paren and dot
  if (expect_close_paren && !got_close_paren) {
    throw_reader_error(ts, "failed to find close paren", -1);
  }

  if (got_close_paren && !expect_close_paren) {
    throw_reader_error(ts, "found an unexpected close paren", -1);
  }

  if (got_dot && !got_thing_after_dot) {
    throw_reader_error(ts, "A list must have an entry after the dot", -1);
  }

  // build up list or improper list, link it, and return!
  if (got_thing_after_dot) {
    if (objects.size() < 2) {
      throw_reader_error(ts, "A list with a dot must have at least one thing before the dot", -1);
    }
    auto back = objects.back();
    objects.pop_back();
    auto rv = build_list(objects);

    auto lst = rv;
    while (true) {
      if (lst.as_pair()->cdr.is_empty_list()) {
        lst.as_pair()->cdr = back;
        break;
      } else {
        lst = lst.as_pair()->cdr;
      }
    }
    db.link(rv, ts.text, start_offset);
    return rv;
  } else {
    auto rv = build_list(objects);
    db.link(rv, ts.text, start_offset);
    return rv;
  }
}

/*!
 * Try decoding as symbol. Returns success.
 */
bool Reader::try_token_as_symbol(const Token& tok, Object& obj) {
  // check start character is valid:
  assert(!tok.text.empty());
  char start = tok.text[0];
  if (valid_symbols_chars[(int)start]) {
    obj = SymbolObject::make_new(symbolTable, tok.text);
    return true;
  } else {
    return false;
  }
}

/*!
 * Read a string and escape. Start on the first char after the first double quote.
 * Supported escapes are \n, \t, \\ and work like they do in C.
 * An arbitrary character can be entered as \c12 where the "12" is hexadecimal.
 */
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

    if (c == '\\') {
      if (!stream.text_remains()) {
        throw_reader_error(stream, "incomplete string escape code", -1);
      }
      if (stream.peek() == 'n') {
        stream.read();
        str.push_back('\n');
      } else if (stream.peek() == 't') {
        stream.read();
        str.push_back('\t');
      } else if (stream.peek() == '\\') {
        stream.read();
        str.push_back('\\');
      } else if (stream.peek() == '"') {
        stream.read();
        str.push_back('"');
      } else if (stream.peek() == 'c') {
        stream.read();
        if (!stream.text_remains(2)) {
          throw_reader_error(stream, "incomplete string escape code", -1);
        }
        auto first = stream.read();
        auto second = stream.read();
        if (!hex_char(first) || !hex_char(second)) {
          throw_reader_error(stream, "invalid character escape hex number", -3);
        }
        char hex_num[3] = {first, second, '\0'};
        std::size_t end = 0;
        auto value = std::stoul(hex_num, &end, 16);
        if (end != 2) {
          throw_reader_error(stream, "invalid character escape", -2);
        }
        assert(value < 256);
        str.push_back(char(value));
      } else {
        throw_reader_error(stream, "unknown string escape code", -1);
      }
    } else {
      str.push_back(c);
    }
  }

  return got_close_quote;
}

/*!
 * Try decoding as a float.  Must have a "." in it.
 * Otherwise all combinations of leading zeros, "."'s, negative signs, etc are ok.
 * Trailing zeros not required.
 */
bool Reader::try_token_as_float(const Token& tok, Object& obj) {
  if (float_start(tok.text[0]) && str_contains(tok.text, '.')) {
    size_t offset = tok.text[0] == '-' ? 1 : 0;
    for (; offset < tok.text.size(); offset++) {
      char c = tok.text.at(offset);
      if ((c < '0' || c > '9') && (c != '.')) {
        return false;
      }
    }

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

/*!
 * Try decoding as binary. Looks like #b101010 ...
 * 64-bit unsigned
 */
bool Reader::try_token_as_binary(const Token& tok, Object& obj) {
  if (tok.text.size() >= 3 && tok.text[0] == '#' && tok.text[1] == 'b') {
    for (size_t offset = 2; offset < tok.text.size(); offset++) {
      char c = tok.text.at(offset);
      if (c != '0' && c != '1') {
        return false;
      }
    }

    uint64_t value = 0;

    for (uint32_t i = 2; i < tok.text.size(); i++) {
      if (value & (0x8000000000000000)) {
        throw std::runtime_error("overflow in binary constant: " + tok.text);
      }

      value <<= 1u;
      if (tok.text[i] == '1') {
        value++;
      } else if (tok.text[i] != '0') {
        return false;
      }
    }
    obj = Object::make_integer((int64_t)value);
    return true;
  }
  return false;
}

/*!
 * Try decoding as hex. Looks like #xdeadBEEF . Don't care about case.
 * 64-bit unsigned
 */
bool Reader::try_token_as_hex(const Token& tok, Object& obj) {
  if (tok.text.size() >= 3 && tok.text[0] == '#' && tok.text[1] == 'x') {
    // determine if we look like a number or not. If we look like a number, but stoll fails,
    // it means that the number is too big or too small, and we should error
    for (size_t offset = 2; offset < tok.text.size(); offset++) {
      char c = tok.text.at(offset);
      if (!hex_char(c)) {
        return false;
      }
    }

    uint64_t v = 0;
    try {
      std::size_t end = 0;
      v = std::stoull(tok.text.substr(2), &end, 16);
      if (end + 2 != tok.text.size())
        return false;
      obj = Object::make_integer(v);
      return true;
    } catch (std::exception& e) {
      throw std::runtime_error("The number " + tok.text + " cannot be a hexadecimal constant");
    }
  }
  return false;
}

/*!
 * Try decoding as integer. No decimals points allowed.
 * 64-bit signed. Won't accept values between INT64_MAX and UINT64_MAX.
 */
bool Reader::try_token_as_integer(const Token& tok, Object& obj) {
  if (decimal_start(tok.text[0]) && !str_contains(tok.text, '.')) {
    // determine if we look like a number or not. If we look like a number, but stoll fails,
    // it means that the number is too big or too small, and we should error
    size_t offset = tok.text[0] == '-' ? 1 : 0;
    if (offset == 1 && tok.text.size() == 1) {
      return false;  // - by itself is not a number!
    }
    for (; offset < tok.text.size(); offset++) {
      char c = tok.text.at(offset);
      if (c < '0' || c > '9') {
        return false;
      }
    }
    uint64_t v = 0;
    try {
      std::size_t end = 0;
      v = std::stoll(tok.text, &end);
      if (end != tok.text.size()) {
        return false;
      }
      obj = Object::make_integer(v);
      return true;
    } catch (std::exception& e) {
      throw std::runtime_error("The number " + tok.text + " cannot be an integer constant");
    }
  }
  return false;
}

bool Reader::try_token_as_char(const Token& tok, Object& obj) {
  if (tok.text.size() >= 3 && tok.text[0] == '#' && tok.text[1] == '\\') {
    if (tok.text.size() == 3 && file_util::is_printable_char(tok.text[2]) && tok.text[2] != ' ') {
      obj = Object::make_char(tok.text[2]);
      return true;
    }

    if (tok.text.size() == 4 && tok.text[2] == '\\') {
      switch (tok.text[3]) {
        case 'n':
          obj = Object::make_char('\n');
          return true;
        case 's':
          obj = Object::make_char(' ');
          return true;
        case 't':
          obj = Object::make_char('\t');
          return true;
      }
    }
  }
  return false;
}

/*!
 * Throw an exception with useful information because of an error in the text stream.
 * Used for reader errors, like "missing close paren" or similar.
 */
void Reader::throw_reader_error(TextStream& here, const std::string& err, int seek_offset) {
  throw std::runtime_error("Reader error:\n" + err + "\nat " +
                           db.get_info_for(here.text, here.seek + seek_offset));
}

/*!
 * Get the source directory of the current project.
 */
std::string Reader::get_source_dir() {
  return file_util::get_project_path();
}

/*!
 * Convert any string into one that can be read.
 * Unprintable characters become escape sequences, including tab and newline.
 */
std::string get_readable_string(const char* in) {
  std::string result;
  while (*in) {
    if (file_util::is_printable_char(*in) && *in != '\\' && *in != '"') {
      result.push_back(*in);
    } else if (*in == '\n') {
      result += "\\n";
    } else if (*in == '\t') {
      result += "\\t";
    } else if (*in == '\\') {
      result += "\\\\";
    } else if (*in == '"') {
      result += "\\\"";
    } else {
      result += fmt::format("\\c{:02x}", uint8_t(*in));
    }
    in++;
  }
  return result;
}
}  // namespace goos
