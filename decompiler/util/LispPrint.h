#ifndef JAK2_DISASSEMBLER_LISPPRINT_H
#define JAK2_DISASSEMBLER_LISPPRINT_H

#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

/*!
 * What type of thing is it?
 */
enum class FormKind {
  SYMBOL,
  HEX_NUMBER,
  DECIMAL_NUMBER,
  BINARY_NUMBER,
  SIGNED_NUMBER,
  STRING,
  EMPTY_LIST,
  PAIR
};

/*!
 * Tokens in a textual representation
 */
enum class TokenKind {
  WHITESPACE,
  SYMBOL,
  OPEN_PAREN,
  DOT,
  CLOSE_PAREN,
  EMPTY_PAIR,
  SPECIAL_SYMBOL
};

/*!
 * Token in a text representation
 */
struct FormToken {
  explicit FormToken(TokenKind _kind, std::string* _str = nullptr) : kind(_kind), str(_str) {}

  TokenKind kind;
  union {
    std::string* str;
  };

  std::string toString() {
    std::string s;
    switch (kind) {
      case TokenKind::WHITESPACE:
        s.push_back(' ');
        break;
      case TokenKind::SYMBOL:
        s.append(*str);
        break;
      case TokenKind::OPEN_PAREN:
        s.push_back('(');
        break;
      case TokenKind::DOT:
        s.push_back('.');
        break;
      case TokenKind::CLOSE_PAREN:
        s.push_back(')');
        break;
      case TokenKind::EMPTY_PAIR:
        s.append("()");
        break;
      case TokenKind::SPECIAL_SYMBOL:
        s.append(*str);
        break;
      default:
        throw std::runtime_error("toString unknown token kind");
    }
    return s;
  }
};

/*!
 * S-Expression Form
 */
class Form {
 public:
  FormKind kind;

  std::string* symbol;
  std::shared_ptr<Form> pair[2];

  std::string toStringSimple();
  std::string toStringPretty(int indent = 0, int line_length = 80);
  void toTokenList(std::vector<FormToken>& tokens);

 private:
  void buildStringSimple(std::string& str);
};

/*!
 * Symbol table to reduce the number of strings everywhere.
 */
class SymbolTable {
 public:
  SymbolTable();
  std::string* intern(const std::string& str);
  ~SymbolTable();
  std::shared_ptr<Form> getEmptyPair() { return empty_pair; }

 private:
  std::unordered_map<std::string, std::string*> map;
  std::shared_ptr<Form> empty_pair;
};

/*!
 * Global symbol table used for the compiler/decompiler
 */
extern SymbolTable gSymbolTable;

std::shared_ptr<Form> toForm(const std::string& str);  //

std::shared_ptr<Form> buildList(const std::string& str);
std::shared_ptr<Form> buildList(std::shared_ptr<Form> form);
std::shared_ptr<Form> buildList(std::vector<std::shared_ptr<Form>>& forms);
std::shared_ptr<Form> buildList(std::shared_ptr<Form>* forms, int count);

template <typename... Args>
std::shared_ptr<Form> buildList(const std::string& str, Args... rest) {
  auto f = std::make_shared<Form>();
  f->kind = FormKind::PAIR;
  f->pair[0] = toForm(str);
  f->pair[1] = buildList(rest...);
  return f;
}

template <typename... Args>
std::shared_ptr<Form> buildList(std::shared_ptr<Form> car, Args... rest) {
  auto f = std::make_shared<Form>();
  f->kind = FormKind::PAIR;
  f->pair[0] = car;
  f->pair[1] = buildList(rest...);
  return f;
}

#endif  // JAK2_DISASSEMBLER_LISPPRINT_H
