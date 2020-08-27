#include "LispPrint.h"

#include <cassert>
#include <iostream>
#include <vector>

//////// HACK - symbol table now looks up by string, which makes it really stupid and store
// all strings twice.
// should probably just remove it

/*!
 * String interning
 */
std::string* SymbolTable::intern(const std::string& str) {
  if (map.find(str) == map.end()) {
    auto* new_string = new std::string(str);
    map[str] = new_string;
    return new_string;
  } else {
    return map[str];
  }
}

/*!
 * Global interned string table
 */
SymbolTable gSymbolTable;

SymbolTable::SymbolTable() {
  empty_pair = std::make_shared<Form>();
  empty_pair->kind = FormKind::EMPTY_LIST;
}

SymbolTable::~SymbolTable() {
  for (const auto& kv : map)
    delete kv.second;
}

/*!
 * Convert a form to a one-line string.
 */
std::string Form::toStringSimple() {
  std::string result;
  buildStringSimple(result);
  return result;
}

void Form::buildStringSimple(std::string &str) {
  std::vector<FormToken> tokens;
  toTokenList(tokens);
  for(auto& token : tokens) {
    switch(token.kind) {
      case TokenKind::WHITESPACE:
        str.push_back(' ');
        break;
      case TokenKind::SYMBOL:
        str.append(*token.str);
        break;
      case TokenKind::OPEN_PAREN:
        str.push_back('(');
        break;
      case TokenKind::DOT:
        str.push_back('.');
        break;
      case TokenKind::CLOSE_PAREN:
        str.push_back(')');
        break;
      case TokenKind::EMPTY_PAIR:
        str.append("()");
        break;
      case TokenKind::SPECIAL_SYMBOL:
        str.append(*token.str);
        break;
      default:
        throw std::runtime_error("buildStringSimple unknown token kind");
    }
  }
}

void Form::toTokenList(std::vector<FormToken> &tokens) {
  switch(kind) {
    case FormKind::SYMBOL:
      tokens.emplace_back(TokenKind::SYMBOL, symbol);
      break;
    case FormKind::PAIR:
    {
      tokens.emplace_back(TokenKind::OPEN_PAREN);
      Form* toPrint = this;
      for(;;) {
        if(toPrint->kind == FormKind::PAIR) {
          toPrint->pair[0]->toTokenList(tokens); // print CAR
          toPrint = toPrint->pair[1].get();
          if(toPrint->kind == FormKind::EMPTY_LIST) {
            tokens.emplace_back(TokenKind::CLOSE_PAREN);
            return;
          } else {
            tokens.emplace_back(TokenKind::WHITESPACE);
          }
        } else { // not a proper list!
          tokens.emplace_back(TokenKind::DOT);
          tokens.emplace_back(TokenKind::WHITESPACE);
          toPrint->toTokenList(tokens);
          tokens.emplace_back(TokenKind::CLOSE_PAREN);
          return;
        }
      }
    }
      break;
    case FormKind::EMPTY_LIST:
      tokens.emplace_back(TokenKind::EMPTY_PAIR);
      break;
    default:
      throw std::runtime_error("unhandled form type in buildSimpleString");
      break;
  }
}

///////////////////
// Pretty Printer
///////////////////

/*!
 * Linked list node representing a token in the output (whitespace, paren, newline, etc)
 */
struct PrettyPrinterNode {
  FormToken* tok = nullptr; // if we aren't a newline, we will have a token.
  int line = -1;            // line that token occurs on. undef for newlines
  int lineIndent = -1;      // indent of line.  only valid for first token in the line
  int offset = -1;          // offset of beginning of token from left margin
  int specialIndentDelta = 0;
  bool is_line_separator = false; // true if line separator (not a token)
  PrettyPrinterNode *next = nullptr, *prev = nullptr; // linked list
  PrettyPrinterNode *paren = nullptr; // pointer to open paren if in parens.  open paren points to close and vice versa
  explicit PrettyPrinterNode(FormToken& _tok) {
    tok = &_tok;
  }
  PrettyPrinterNode() = default;
};

/*!
 * Splice in a line break after the given node, it there isn't one already and if it isn't the last node.
 */
static void insertNewlineAfter(PrettyPrinterNode* node, int specialIndentDelta) {
  if(node->next && !node->next->is_line_separator) {
    auto* nl = new PrettyPrinterNode;
    auto* next = node->next;
    node->next = nl;
    nl->prev = node;
    nl->next = next;
    next->prev = nl;
    nl->is_line_separator = true;
    nl->specialIndentDelta = specialIndentDelta;
  }
}

/*!
 * Splice in a line break before the given node, if there isn't one already and if it isn't the first node.
 */
static void insertNewlineBefore(PrettyPrinterNode* node, int specialIndentDelta) {
  if(node->prev && !node->prev->is_line_separator) {
    auto* nl = new PrettyPrinterNode;
    auto* prev = node->prev;
    prev->next = nl;
    nl->prev = prev;
    nl->next = node;
    node->prev = nl;
    nl->is_line_separator = true;
    nl->specialIndentDelta = specialIndentDelta;
  }
}

/*!
 * Break a list across multiple lines. This is the fundamental reducing operation of this algorithm
 */
static void breakList(PrettyPrinterNode* leftParen) {
  assert(!leftParen->is_line_separator);
  assert(leftParen->tok->kind == TokenKind::OPEN_PAREN);
  auto* rp = leftParen->paren;
  assert(rp->tok->kind == TokenKind::CLOSE_PAREN);

  for(auto* n = leftParen->next; n && n != rp; n = n->next) {
    if(!n->is_line_separator) {
      if(n->tok->kind == TokenKind::OPEN_PAREN) {
        n = n->paren;
        assert(n->tok->kind == TokenKind::CLOSE_PAREN);
        insertNewlineAfter(n, 0);
      } else if(n->tok->kind != TokenKind::WHITESPACE) {
        assert(n->tok->kind != TokenKind::CLOSE_PAREN);
        insertNewlineAfter(n, 0);
      }
    }
  }
}

/*!
 * Compute proper line numbers, offsets, and indents for a list of tokens with newlines
 * Will add newlines for close parens if needed.
 */
static PrettyPrinterNode* propagatePretty(PrettyPrinterNode* list, int line_length) {
  // propagate line numbers
  PrettyPrinterNode* rv = nullptr;
  int line = list->line;
  for(auto* n = list; n; n = n->next) {
    if(n->is_line_separator) {
      line++;
    } else {
      n->line = line;
      // add the weird newline.
      if(n->tok->kind == TokenKind::CLOSE_PAREN) {
        if(n->line != n->paren->line) {
          if(n->prev && !n->prev->is_line_separator) {
            insertNewlineBefore(n, 0);
            line++;
          }
          if(n->next && !n->next->is_line_separator) {
            insertNewlineAfter(n, 0);
          }
        }
      }
    }
  }

  // compute offsets and indents
  std::vector<int> indentStack;
  indentStack.push_back(0);
  int offset = 0;
  PrettyPrinterNode* line_start = list;
  bool previous_line_sep = false;
  for(auto* n = list; n; n = n->next) {
    if(n->is_line_separator) {
      previous_line_sep = true;
      offset = indentStack.back() += n->specialIndentDelta;
    } else {
      if(previous_line_sep) {
        line_start = n;
        n->lineIndent = offset;
        previous_line_sep = false;
      }

      n->offset = offset;
      offset += n->tok->toString().length();
      if(offset > line_length && !rv) rv = line_start;
      if(n->tok->kind == TokenKind::OPEN_PAREN) {
        if(!n->prev || n->prev->is_line_separator) {
          indentStack.push_back(offset + 1);
        } else {
          indentStack.push_back(offset - 1);
        }

      }

      if(n->tok->kind == TokenKind::CLOSE_PAREN) {
        indentStack.pop_back();
      }
    }

  }
  return rv;
}

/*!
 * Get the token on the start of the next line. nullptr if we're the last line.
 */
static PrettyPrinterNode* getNextLine(PrettyPrinterNode* start) {
  assert(!start->is_line_separator);
  int line = start->line;
  for(;;) {
    if(start->is_line_separator || start->line == line) {
      if(start->next)
        start = start->next;
      else
        return nullptr;
    } else {
      break;
    }
  }
  return start;
}

/*!
 * Get the next open paren on the current line (can start in the middle of line, not inclusive of start)
 * nullptr if there's no open parens on the rest of this line.
 */
static PrettyPrinterNode* getNextListOnLine(PrettyPrinterNode* start) {
  int line = start->line;
  assert(!start->is_line_separator);
  if(!start->next || start->next->is_line_separator) return nullptr;
  start = start->next;
  while(!start->is_line_separator && start->line == line) {
    if(start->tok->kind == TokenKind::OPEN_PAREN) return start;
    if(!start->next) return nullptr;
    start = start->next;
  }
  return nullptr;
}

/*!
 * Get the first open paren on the current line (can start in the middle of line, inclusive of start)
 * nullptr if there's no open parens on the rest of this line
 */
static PrettyPrinterNode* getFirstListOnLine(PrettyPrinterNode* start) {
  int line = start->line;
  assert(!start->is_line_separator);
  while(!start->is_line_separator && start->line == line) {
    if(start->tok->kind == TokenKind::OPEN_PAREN) return start;
    if(!start->next) return nullptr;
    start = start->next;
  }
  return nullptr;
}

/*!
 * Get the first token on the first line which exceeds the max length
 */
static PrettyPrinterNode* getFirstBadLine(PrettyPrinterNode* start, int line_length) {
  assert(!start->is_line_separator);
  int currentLine = start->line;
  auto* currentLineNode = start;
  for(;;) {
    if(start->is_line_separator) {
      assert(start->next);
      start = start->next;
    } else {
      if(start->line != currentLine) {
        currentLine = start->line;
        currentLineNode = start;
      }
      if(start->offset > line_length) {
        return currentLineNode;
      }
      if(!start->next) {
        return nullptr;
      }
      start = start->next;
    }
  }
}

/*!
 * Break insertion algorithm.
 */
static void insertBreaksAsNeeded(PrettyPrinterNode* head, int line_length) {
  PrettyPrinterNode* last_line_complete = nullptr;
  PrettyPrinterNode* line_to_start_line_search = head;

  // loop over lines
  for(;;) {

    // compute lines as needed
    propagatePretty(head, line_length);

    // search for a bad line starting at the last line we fixed
    PrettyPrinterNode* candidate_line = getFirstBadLine(line_to_start_line_search, line_length);
    // if we got the same line we started on, this means we couldn't fix it.
    if(candidate_line == last_line_complete) {
      candidate_line = nullptr; // so we say our candidate was bad and try to find another
      PrettyPrinterNode* next_line = getNextLine(line_to_start_line_search);
      if(next_line) {
        candidate_line = getFirstBadLine(next_line, line_length);
      }
    }
    if(!candidate_line) break;

    // okay, we have a line which needs fixing.
    assert(!candidate_line->prev || candidate_line->prev->is_line_separator);
    PrettyPrinterNode* form_to_start = getFirstListOnLine(candidate_line);
    for(;;) {
      if(!form_to_start) {
        printf("pretty printer has failed. Fix the bug or increase the the line length.\n");
        assert(false);
      }
      breakList(form_to_start);
      propagatePretty(head, line_length);
      if(getFirstBadLine(candidate_line, line_length) != candidate_line) {
        break;
      }

      form_to_start = getNextListOnLine(form_to_start);
      if(!form_to_start) break;

    }


    last_line_complete = candidate_line;
    line_to_start_line_search = candidate_line;
  }
}

static void insertSpecialBreaks(PrettyPrinterNode* node) {
  for(; node; node = node->next) {
    if(!node->is_line_separator && node->tok->kind == TokenKind::SYMBOL) {
      std::string& name = *node->tok->str;
      if(name == "deftype") {
        auto* parent_type_dec = getNextListOnLine(node);
        if(parent_type_dec) {
          insertNewlineAfter(parent_type_dec->paren, 0);
        }
      }
    }
  }
}

std::string Form::toStringPretty(int indent, int line_length) {
  (void)indent;
  (void)line_length;
  std::vector<FormToken> tokens;
  toTokenList(tokens);
  assert(!tokens.empty());
  std::string pretty;

  // build linked list of nodes
  PrettyPrinterNode* head = new PrettyPrinterNode(tokens[0]);
  PrettyPrinterNode* node = head;
  head->line = 0;
  head->offset = 0;
  head->lineIndent = 0;
  int offset = head->tok->toString().length();
  for(size_t i = 1; i < tokens.size(); i++) {
    node->next = new PrettyPrinterNode(tokens[i]);
    node->next->prev = node;
    node = node->next;
    node->line = 0;
    node->offset = offset;
    offset += node->tok->toString().length();
    node->lineIndent = 0;
  }

  // attach parens.
  std::vector<PrettyPrinterNode*> parenStack;
  parenStack.push_back(nullptr);
  for(PrettyPrinterNode* n = head; n; n = n->next) {
    if(n->tok->kind == TokenKind::OPEN_PAREN) {
      parenStack.push_back(n);
    } else if(n->tok->kind == TokenKind::CLOSE_PAREN) {
      n->paren = parenStack.back();
      parenStack.back()->paren = n;
      parenStack.pop_back();
    } else {
      n->paren = parenStack.back();
    }
  }
  assert(parenStack.size() == 1);
  assert(!parenStack.back());

  insertSpecialBreaks(head);
  propagatePretty(head, line_length);
  insertBreaksAsNeeded(head, line_length);


  // write to string
  bool newline_prev = true;
  for(PrettyPrinterNode* n = head; n; n = n->next) {
    if(n->is_line_separator){
      pretty.push_back('\n');
      newline_prev = true;
    } else {
      if(newline_prev) {
        pretty.append(n->lineIndent, ' ');
        newline_prev = false;
        if(n->tok->kind == TokenKind::WHITESPACE) continue;
      }
      pretty.append(n->tok->toString());
    }
  }

  for(;;) {
    if(!head) break;
    auto* next = head->next;
    delete head;
    head = next;
  }


  return pretty;
}

std::shared_ptr<Form> toForm(const std::string& str) {
  auto f = std::make_shared<Form>();
  f->kind = FormKind::SYMBOL;
  f->symbol = gSymbolTable.intern(str);
  return f;
}

std::shared_ptr<Form> buildList(std::shared_ptr<Form> form) {
  auto f = std::make_shared<Form>();
  f->kind = FormKind::PAIR;
  f->pair[0] = form;
  f->pair[1] = gSymbolTable.getEmptyPair();
  return f;
}

std::shared_ptr<Form> buildList(const std::string& str) {
  return buildList(toForm(str));
}

std::shared_ptr<Form> buildList(std::shared_ptr<Form>* forms, int count) {
  auto f = std::make_shared<Form>();
  f->kind = FormKind::PAIR;
  f->pair[0] = forms[0];
  if(count - 1) {
    f->pair[1] = buildList(forms + 1, count - 1);
  } else {
    f->pair[1] = gSymbolTable.getEmptyPair();
  }

  return f;
}

std::shared_ptr<Form> buildList(std::vector<std::shared_ptr<Form>>& forms) {
  if(forms.empty()) {
    return gSymbolTable.getEmptyPair();
  }
  return buildList(forms.data(), forms.size());
}
