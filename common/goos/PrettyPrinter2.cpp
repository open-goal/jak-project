#include "PrettyPrinter2.h"

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "third-party/fmt/core.h"
#include "third-party/fmt/format.h"

namespace pretty_print {

namespace v2 {

// Note: there's some recursive stuff, but we only recurse once per list depth.
// The previous issues we had with stack overflow only happened when there was a stack frame per
// element in a list.

// The main node type.
// unlike v1, this nests lists.
// these have pointers to parents, so generally not safe to copy.
struct Node {
  Node() = default;

  Node(const std::string& str) : kind(Kind::ATOM), atom_str(str) {}

  Node(std::vector<Node>&& list, bool is_list)
      : kind(is_list ? Kind::LIST : Kind::IMPROPER_LIST), child_nodes(std::move(list)) {}
  enum class Kind : u8 { ATOM, LIST, IMPROPER_LIST, INVALID } kind = Kind::INVALID;

  std::vector<Node> child_nodes;
  std::string atom_str;

  // number of quotes this is wrapped in.
  enum class QuoteKind { QUOTE, UNQUOTE, QUASIQUOTE, UNQUOTE_SPLICING };
  std::vector<QuoteKind> quotes;

  Node* parent = nullptr;
  u32 my_depth = 0;

  int get_quote_length() const;

  void link(Node* this_parent, std::vector<Node*>* bfs_order, u32 depth) {
    parent = this_parent;
    my_depth = depth;
    bfs_order->push_back(this);
    switch (kind) {
      case Kind::ATOM:
        break;
      case Kind::LIST:
      case Kind::IMPROPER_LIST:
        ASSERT(!child_nodes.empty());
        for (auto& child : child_nodes) {
          child.link(this, bfs_order, depth + 1);
        }
        break;
      default:
        ASSERT(false);
    }
  }

  bool needs_end_paren_newline() const {
    if (break_list) {
      return true;
    }

    if (!child_nodes.empty()) {
      return child_nodes.back().needs_end_paren_newline();
    }

    return false;
  }

  std::string debug_to_string() const {
    switch (kind) {
      case Kind::ATOM:
        return fmt::format("[atom {}]", atom_str);
      case Kind::LIST:
        return "[list]";
      case Kind::IMPROPER_LIST:
        return "[improper list]";
      default:
        ASSERT(false);
    }
  }

  // how wide is this text? not including the indentation of this subtree.
  u32 text_len = 0;

  bool break_list = false;
  u8 top_line_count = 0;
  u8 sub_elt_indent = 0;
};

inline const std::string quote_symbol(Node::QuoteKind kind) {
  switch (kind) {
    case Node::QuoteKind::QUOTE:
      return "'";
    case Node::QuoteKind::QUASIQUOTE:
      return "`";
    case Node::QuoteKind::UNQUOTE:
      return ",";
    case Node::QuoteKind::UNQUOTE_SPLICING:
      return ",@";
    default:
      ASSERT_MSG(false, fmt::format("invalid quote kind {}", fmt::underlying(kind)));
      return "[invalid]";
  }
}

int Node::get_quote_length() const {
  int out = 0;
  for (auto& q : quotes) {
    out += quote_symbol(q).length();
  }
  return out;
}

Node to_node(const goos::Object& obj) {
  switch (obj.type) {
    case goos::ObjectType::EMPTY_LIST:
      // just treat this as a printing "atom"
      return Node("()");
    case goos::ObjectType::INTEGER:
    case goos::ObjectType::FLOAT:
    case goos::ObjectType::CHAR:
    case goos::ObjectType::SYMBOL:
    case goos::ObjectType::STRING:
      // these are all atoms that the pretty printer should just treat as a blob.
      return Node(obj.print());

    case goos::ObjectType::PAIR: {
      // we've got four cases: quoted thing, unquoted thing, proper list, improper list.

      // there's probably a better way to do this but i am lazy
      auto& first = obj.as_pair()->car;
      if (first.is_symbol("quote")) {
        auto& second = obj.as_pair()->cdr;
        if (second.is_pair() && second.as_pair()->cdr.is_empty_list()) {
          Node result = to_node(second.as_pair()->car);
          result.quotes.push_back(Node::QuoteKind::QUOTE);
          return result;
        }
      } else if (first.is_symbol("unquote")) {
        auto& second = obj.as_pair()->cdr;
        if (second.is_pair() && second.as_pair()->cdr.is_empty_list()) {
          Node result = to_node(second.as_pair()->car);
          result.quotes.push_back(Node::QuoteKind::UNQUOTE);
          return result;
        }
      }

      // not quoted, so either list or pair
      std::vector<Node> children;
      auto* to_print = &obj;
      for (;;) {
        if (to_print->is_pair()) {
          // first print the car:
          children.push_back(to_node(to_print->as_pair()->car));
          // then load up the cdr as the next thing to print
          to_print = &to_print->as_pair()->cdr;
          if (to_print->is_empty_list()) {
            // we're done, add a close paren and finish
            return Node(std::move(children), true);
          }
        } else {
          children.push_back(to_node(*to_print));
          return Node(std::move(children), false);
        }
      }
    } break;

      // these are unsupported by the pretty printer.
    case goos::ObjectType::ARRAY:  // todo, we should probably handle arrays.
    case goos::ObjectType::LAMBDA:
    case goos::ObjectType::MACRO:
    case goos::ObjectType::ENVIRONMENT:
      throw std::runtime_error("tried to pretty print a goos object kind which is not supported.");
    default:
      ASSERT(false);
  }
}

void recompute_lengths(const std::vector<Node*>& bfs_order) {
  // iterate from leaves up
  for (auto it = bfs_order.rbegin(); it != bfs_order.rend(); it++) {
    Node* node = *it;
    switch (node->kind) {
      case Node::Kind::ATOM:
        node->text_len = node->atom_str.length() + node->get_quote_length();
        break;
      case Node::Kind::IMPROPER_LIST:
      case Node::Kind::LIST: {
        if (node->break_list) {
          // special case compute first line length
          int first_line_len = 1 + node->get_quote_length();  // open paren + quotes
          int nodes_on_first_line =
              std::min(int(node->child_nodes.size()), int(node->top_line_count));
          if (nodes_on_first_line > 0) {
            for (int node_idx = 0; node_idx < nodes_on_first_line; node_idx++) {
              first_line_len += node->child_nodes.at(node_idx).text_len;
              first_line_len++;  // trailing space
            }
            first_line_len--;  // last one doesn't have a trailing space
          }

          int max_line_len = first_line_len;

          // now the length of all the things below
          for (u32 node_idx = nodes_on_first_line; node_idx < node->child_nodes.size();
               node_idx++) {
            int line_len = node->sub_elt_indent + node->child_nodes.at(node_idx).text_len;
            max_line_len = std::max(max_line_len, line_len);
          }

          node->text_len = max_line_len;
        } else {
          node->text_len = 1 + node->get_quote_length();  // open paren + quotes
          for (auto& child : node->child_nodes) {
            node->text_len += (child.text_len + 1);  // space or close paren.
          }
        }
      } break;
      default:
        ASSERT(false);
    }
  }
}

/*!
 * Note: this has special cases for how to insert breaks.
 * These rules will be used if the printer decides it should break up the list.
 * If you want to force a form to always be broken up, see insert_required_breaks
 */
void break_list(Node* node) {
  ASSERT(!node->break_list);
  node->break_list = true;
  node->sub_elt_indent = 2;
  node->top_line_count = 1;

  const std::unordered_set<std::string> sameline_splitters = {
      "if",
      "<",
      ">",
      "<=",
      ">=",
      "set!",
      "=",
      "!=",
      "+",
      "-",
      "*",
      "/",
      "the",
      "->",
      "and",
      "or",
      "logand",
      "logior",
      "logxor",
      "+!",
      "*!",
      "logtest?",
      "not",
      "zero?",
      "nonzero?",
      "dma-buffer-add-gs-set",
      "dma-buffer-add-gs-set-flusha",
  };

  if (node->child_nodes.at(0).kind == Node::Kind::LIST) {
    // ((foo
    //    bar
    node->sub_elt_indent = 1;
  } else if (node->child_nodes.at(0).kind == Node::Kind::ATOM) {
    auto& name = node->child_nodes[0].atom_str;
    if (name == "defun" || name == "defun-debug" || name == "defbehavior" || name == "defstate") {
      // things with three things in the top line: (defun <name> <args>
      node->top_line_count = 3;
    } else if (name == "defskelgroup") {
      // things with 5 things in the top line: (defskelgroup <name> <art> jgeo janim
      node->top_line_count = 5;
      node->sub_elt_indent += name.size();
    } else if (name == "process-new") {
      // things with 3 things in the top line
      node->top_line_count = 3;
      node->sub_elt_indent += name.size();
    } else if (name == "ja" || name == "ja-no-eval") {
      node->top_line_count = 3;
      node->sub_elt_indent += name.size();
    } else if (name == "defmethod") {
      // things with 4 things in the top line: (defmethod <method> <type> <args>
      // or just 3 things in the top line: (defmethod <method> <args>
      node->top_line_count = 3;
      if (node->child_nodes.size() >= 4 && node->child_nodes[2].kind == Node::Kind::ATOM) {
        node->top_line_count = 4;
      }
    } else if (name == "until" || name == "while" || name == "dotimes" || name == "countdown" ||
               name == "when" || name == "behavior" || name == "lambda" || name == "defpart" ||
               name == "define") {
      node->top_line_count = 2;
    } else if (name == "let" || name == "let*" || name == "rlet" ||
               name == "with-dma-buffer-add-bucket") {
      // special case for things like let.
      node->top_line_count = 2;  // (let <defs>
      if (node->child_nodes.size() > 1 && node->child_nodes[1].child_nodes.size() > 1 &&
          !node->child_nodes[1].break_list) {
        // and break the defs.
        break_list(&node->child_nodes[1]);
      }
    } else if (sameline_splitters.count(name) > 0) {
      // if has a special indent rule:
      node->top_line_count = 2;
      node->sub_elt_indent += name.size();
    } else if (name == "cond") {
      // cond should always be broken up
      for (size_t i = 1; i < node->child_nodes.size(); i++) {
        auto& cond_body = node->child_nodes[i];
        if (cond_body.kind == Node::Kind::LIST && !cond_body.break_list) {
          break_list(&cond_body);
        }
      }
    } else if (name == "case") {
      // case gets a second thing on top, plus break up everything.
      node->top_line_count = 2;
      for (size_t i = 2; i < node->child_nodes.size(); i++) {
        auto& cond_body = node->child_nodes[i];
        if (cond_body.kind == Node::Kind::LIST && !cond_body.break_list) {
          break_list(&cond_body);
        }
      }
    }
  }

  Node* child = node;
  for (Node* p = node->parent; p; p = p->parent) {
    if (!p->break_list && &p->child_nodes.back() != child) {
      break_list(p);
    }
    child = p;
  }
}

void insert_required_breaks(const std::vector<Node*>& bfs_order) {
  const std::unordered_set<std::string> always_break = {
      "when",    "defun-debug", "countdown", "case",     "defun",   "defmethod", "let",
      "until",   "while",       "if",        "dotimes",  "cond",    "else",      "defbehavior",
      "with-pp", "rlet",        "defstate",  "behavior", "defpart", "loop",      "let*"};
  for (auto node : bfs_order) {
    if (!node->break_list && node->kind == Node::Kind::LIST &&
        node->child_nodes.at(0).kind == Node::Kind::ATOM) {
      if (always_break.count(node->child_nodes[0].atom_str) > 0) {
        break_list(node);
      }
    }
  }
}

int run_algorithm(const std::vector<Node*>& bfs_order, int line_length) {
  // our approach is to go in reverse order and find the first list node that is:
  // - too long
  // - not already split.
  // the "magic" of v2 is:
  // the "too long" check above ignores the sublist.

  int num_broken = 0;
  std::optional<s32> min_depth;
  for (auto it = bfs_order.rbegin(); it != bfs_order.rend(); it++) {
    Node* node = *it;
    if (min_depth && node->my_depth < min_depth) {
      break;
    }

    if (node->kind != Node::Kind::ATOM && (int)node->text_len > line_length &&
        node->break_list == false) {
      break_list(node);
      num_broken++;
      if (!min_depth) {
        min_depth = node->my_depth;
      }
    }
  }
  recompute_lengths(bfs_order);
  return num_broken;
}

int compute_extra_offset(const std::string& str, int s0, int ei) {
  ASSERT(!str.empty());
  for (size_t i = str.length(); i-- > 0;) {
    if ((int)i == s0) {
      return ei + str.length() - s0;
    } else if (i == '\n') {
      return str.length() - i;
    }
  }
  return ei + str.length() - s0;
}

void append_node_to_string(const Node* node,
                           std::string& str,
                           int init_indent_level,
                           int next_indent_level) {
  for (int i = 0; i < init_indent_level; i++) {
    str.push_back(' ');
  }
  for (auto q : node->quotes) {
    str.append(quote_symbol(q));
  }
  switch (node->kind) {
    case Node::Kind::ATOM:
      str.append(node->atom_str);
      break;
    case Node::Kind::IMPROPER_LIST:
    case Node::Kind::LIST:
      if (node->break_list) {
        str.push_back('(');
        size_t node_idx = 0;

        int listing_indent = next_indent_level + node->get_quote_length() + node->sub_elt_indent;
        int extra_indent = 0;
        int old_indent = listing_indent;
        if (node->top_line_count) {
          listing_indent -= node->sub_elt_indent;
          listing_indent += node->child_nodes.front().kind == Node::Kind::LIST ? 1 : 2;
        }
        for (; node_idx < node->top_line_count; node_idx++) {
          size_t s0 = str.length();
          if (node->kind == Node::Kind::IMPROPER_LIST &&
              &node->child_nodes.at(node_idx) == &node->child_nodes.back()) {
            str.append(". ");
          }
          // so, if these need to break, they should have a bigger indent.
          append_node_to_string(&node->child_nodes.at(node_idx), str, 0,
                                listing_indent + extra_indent);
          extra_indent = compute_extra_offset(str, s0, extra_indent);
          str.push_back(' ');
        }
        if (node->top_line_count) {
          listing_indent = old_indent;
        }
        if (node->top_line_count > 0) {
          str.pop_back();
        }
        str.push_back('\n');
        bool after_key = false;
        for (; node_idx < node->child_nodes.size(); node_idx++) {
          if (node->kind == Node::Kind::IMPROPER_LIST &&
              &node->child_nodes.at(node_idx) == &node->child_nodes.back()) {
            for (int i = 0; i < listing_indent; i++) {
              str.push_back(' ');
            }
            str.append(".\n");
          }
          append_node_to_string(&node->child_nodes.at(node_idx), str,
                                after_key ? 0 : listing_indent, listing_indent);
          if (node->child_nodes.at(node_idx).kind == Node::Kind::ATOM &&
              node->child_nodes.at(node_idx).atom_str.at(0) == ':' &&
              node->child_nodes.at(node_idx).atom_str.find(' ') == std::string::npos) {
            str.push_back(' ');
            after_key = true;
          } else {
            str.push_back('\n');
            after_key = false;
          }
        }
        for (int i = 0; i < listing_indent; i++) {
          str.push_back(' ');
        }
        str.push_back(')');
      } else {
        str.push_back('(');
        ASSERT(!node->child_nodes.empty());
        int listing_indent = next_indent_level + node->get_quote_length();
        int extra_indent = 1;
        int c0 = 0;
        for (auto& child : node->child_nodes) {
          if (node->kind == Node::Kind::IMPROPER_LIST && &child == &node->child_nodes.back()) {
            str.append(". ");
          }
          size_t s0 = str.length();
          append_node_to_string(&child, str, 0, listing_indent + extra_indent);
          str.push_back(' ');
          extra_indent += (str.length() - s0);
          if (&child == &node->child_nodes.at(0) && !child.break_list) {
            //
            if (child.kind == Node::Kind::LIST) {
              c0 = 0;
            } else {
              c0 = str.length() - s0;
            }
          }
        }
        str.pop_back();
        if (node->needs_end_paren_newline()) {
          str.push_back('\n');
          for (int i = 0; i < listing_indent + c0 + 1; i++) {
            str.push_back(' ');
          }
        }
        str.push_back(')');
      }
      break;
    default:
      ASSERT(false);
  }
}

std::string node_to_string(const Node* node) {
  std::string result;
  append_node_to_string(node, result, 0, 0);
  return result;
}

}  // namespace v2

std::string to_string(const goos::Object& obj, int line_length) {
  using namespace v2;

  // construct the tree
  Node root = to_node(obj);

  // create tree links and order by depth
  std::vector<Node*> bfs_order;
  root.link(nullptr, &bfs_order, 0);

  insert_required_breaks(bfs_order);

  // compute subtree lengths
  recompute_lengths(bfs_order);

  int max_depth = 0;
  for (auto node : bfs_order) {
    max_depth = std::max((int)node->my_depth, max_depth);
  }

  int num_broken = 1;
  while (num_broken) {
    num_broken = run_algorithm(bfs_order, line_length);
  }

  return node_to_string(&root);
}
}  // namespace pretty_print
