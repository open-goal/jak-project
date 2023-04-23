#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 37
#define LARGE_STATE_COUNT 15
#define SYMBOL_COUNT 41
#define ALIAS_COUNT 0
#define TOKEN_COUNT 24
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 5
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 10

enum {
  sym__ws = 1,
  sym_comment = 2,
  anon_sym_POUND_PIPE = 3,
  aux_sym_comment_multiline_token1 = 4,
  aux_sym_comment_multiline_token2 = 5,
  aux_sym_comment_multiline_token3 = 6,
  aux_sym_comment_multiline_token4 = 7,
  anon_sym_PIPE_POUND = 8,
  sym_num_lit = 9,
  aux_sym__kwd_unqualified_token1 = 10,
  anon_sym_COLON = 11,
  sym_str_lit = 12,
  sym_char_lit = 13,
  sym_null_lit = 14,
  sym_bool_lit = 15,
  anon_sym_SLASH = 16,
  aux_sym__sym_unqualified_token1 = 17,
  anon_sym_LPAREN = 18,
  anon_sym_RPAREN = 19,
  anon_sym_SQUOTE = 20,
  anon_sym_BQUOTE = 21,
  anon_sym_COMMA_AT = 22,
  anon_sym_COMMA = 23,
  sym_source = 24,
  sym__gap = 25,
  sym_comment_multiline = 26,
  sym__form = 27,
  sym_kwd_lit = 28,
  sym__kwd_marker = 29,
  sym_sym_lit = 30,
  sym_list_lit = 31,
  sym__bare_list_lit = 32,
  sym_quoting_lit = 33,
  sym_quasi_quoting_lit = 34,
  sym_unquote_splicing_lit = 35,
  sym_unquoting_lit = 36,
  aux_sym_source_repeat1 = 37,
  aux_sym_comment_multiline_repeat1 = 38,
  aux_sym__bare_list_lit_repeat1 = 39,
  aux_sym_quoting_lit_repeat1 = 40,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym__ws] = "_ws",
  [sym_comment] = "comment",
  [anon_sym_POUND_PIPE] = "#|",
  [aux_sym_comment_multiline_token1] = "comment_multiline_token1",
  [aux_sym_comment_multiline_token2] = "comment_multiline_token2",
  [aux_sym_comment_multiline_token3] = "comment_multiline_token3",
  [aux_sym_comment_multiline_token4] = "comment_multiline_token4",
  [anon_sym_PIPE_POUND] = "|#",
  [sym_num_lit] = "num_lit",
  [aux_sym__kwd_unqualified_token1] = "kwd_name",
  [anon_sym_COLON] = ":",
  [sym_str_lit] = "str_lit",
  [sym_char_lit] = "char_lit",
  [sym_null_lit] = "null_lit",
  [sym_bool_lit] = "bool_lit",
  [anon_sym_SLASH] = "sym_name",
  [aux_sym__sym_unqualified_token1] = "sym_name",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_SQUOTE] = "'",
  [anon_sym_BQUOTE] = "`",
  [anon_sym_COMMA_AT] = ",@",
  [anon_sym_COMMA] = ",",
  [sym_source] = "source",
  [sym__gap] = "_gap",
  [sym_comment_multiline] = "comment_multiline",
  [sym__form] = "_form",
  [sym_kwd_lit] = "kwd_lit",
  [sym__kwd_marker] = "_kwd_marker",
  [sym_sym_lit] = "sym_lit",
  [sym_list_lit] = "list_lit",
  [sym__bare_list_lit] = "_bare_list_lit",
  [sym_quoting_lit] = "quoting_lit",
  [sym_quasi_quoting_lit] = "quasi_quoting_lit",
  [sym_unquote_splicing_lit] = "unquote_splicing_lit",
  [sym_unquoting_lit] = "unquoting_lit",
  [aux_sym_source_repeat1] = "source_repeat1",
  [aux_sym_comment_multiline_repeat1] = "comment_multiline_repeat1",
  [aux_sym__bare_list_lit_repeat1] = "_bare_list_lit_repeat1",
  [aux_sym_quoting_lit_repeat1] = "quoting_lit_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym__ws] = sym__ws,
  [sym_comment] = sym_comment,
  [anon_sym_POUND_PIPE] = anon_sym_POUND_PIPE,
  [aux_sym_comment_multiline_token1] = aux_sym_comment_multiline_token1,
  [aux_sym_comment_multiline_token2] = aux_sym_comment_multiline_token2,
  [aux_sym_comment_multiline_token3] = aux_sym_comment_multiline_token3,
  [aux_sym_comment_multiline_token4] = aux_sym_comment_multiline_token4,
  [anon_sym_PIPE_POUND] = anon_sym_PIPE_POUND,
  [sym_num_lit] = sym_num_lit,
  [aux_sym__kwd_unqualified_token1] = aux_sym__kwd_unqualified_token1,
  [anon_sym_COLON] = anon_sym_COLON,
  [sym_str_lit] = sym_str_lit,
  [sym_char_lit] = sym_char_lit,
  [sym_null_lit] = sym_null_lit,
  [sym_bool_lit] = sym_bool_lit,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [aux_sym__sym_unqualified_token1] = anon_sym_SLASH,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [anon_sym_BQUOTE] = anon_sym_BQUOTE,
  [anon_sym_COMMA_AT] = anon_sym_COMMA_AT,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [sym_source] = sym_source,
  [sym__gap] = sym__gap,
  [sym_comment_multiline] = sym_comment_multiline,
  [sym__form] = sym__form,
  [sym_kwd_lit] = sym_kwd_lit,
  [sym__kwd_marker] = sym__kwd_marker,
  [sym_sym_lit] = sym_sym_lit,
  [sym_list_lit] = sym_list_lit,
  [sym__bare_list_lit] = sym__bare_list_lit,
  [sym_quoting_lit] = sym_quoting_lit,
  [sym_quasi_quoting_lit] = sym_quasi_quoting_lit,
  [sym_unquote_splicing_lit] = sym_unquote_splicing_lit,
  [sym_unquoting_lit] = sym_unquoting_lit,
  [aux_sym_source_repeat1] = aux_sym_source_repeat1,
  [aux_sym_comment_multiline_repeat1] = aux_sym_comment_multiline_repeat1,
  [aux_sym__bare_list_lit_repeat1] = aux_sym__bare_list_lit_repeat1,
  [aux_sym_quoting_lit_repeat1] = aux_sym_quoting_lit_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym__ws] = {
    .visible = false,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_POUND_PIPE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_comment_multiline_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_comment_multiline_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_comment_multiline_token3] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_comment_multiline_token4] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_PIPE_POUND] = {
    .visible = true,
    .named = false,
  },
  [sym_num_lit] = {
    .visible = true,
    .named = true,
  },
  [aux_sym__kwd_unqualified_token1] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [sym_str_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_char_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_null_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_bool_lit] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = true,
  },
  [aux_sym__sym_unqualified_token1] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SQUOTE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BQUOTE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA_AT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [sym_source] = {
    .visible = true,
    .named = true,
  },
  [sym__gap] = {
    .visible = false,
    .named = true,
  },
  [sym_comment_multiline] = {
    .visible = true,
    .named = true,
  },
  [sym__form] = {
    .visible = false,
    .named = true,
  },
  [sym_kwd_lit] = {
    .visible = true,
    .named = true,
  },
  [sym__kwd_marker] = {
    .visible = false,
    .named = true,
  },
  [sym_sym_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_list_lit] = {
    .visible = true,
    .named = true,
  },
  [sym__bare_list_lit] = {
    .visible = false,
    .named = true,
  },
  [sym_quoting_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_quasi_quoting_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_unquote_splicing_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_unquoting_lit] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_comment_multiline_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__bare_list_lit_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_quoting_lit_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_close = 1,
  field_marker = 2,
  field_name = 3,
  field_open = 4,
  field_value = 5,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_close] = "close",
  [field_marker] = "marker",
  [field_name] = "name",
  [field_open] = "open",
  [field_value] = "value",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 3},
  [3] = {.index = 4, .length = 2},
  [4] = {.index = 6, .length = 1},
  [5] = {.index = 7, .length = 2},
  [6] = {.index = 9, .length = 2},
  [7] = {.index = 11, .length = 3},
  [8] = {.index = 14, .length = 2},
  [9] = {.index = 16, .length = 2},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_close, 0, .inherited = true},
    {field_open, 0, .inherited = true},
    {field_value, 0, .inherited = true},
  [4] =
    {field_close, 1},
    {field_open, 0},
  [6] =
    {field_value, 0},
  [7] =
    {field_marker, 0},
    {field_value, 1},
  [9] =
    {field_marker, 0},
    {field_name, 1},
  [11] =
    {field_close, 2},
    {field_open, 0},
    {field_value, 1, .inherited = true},
  [14] =
    {field_value, 0, .inherited = true},
    {field_value, 1, .inherited = true},
  [16] =
    {field_marker, 0},
    {field_value, 2},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
};

static inline bool aux_sym__kwd_unqualified_token1_character_set_1(int32_t c) {
  return (c < '['
    ? (c < '('
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || c == '"'))
      : (c <= ')' || (c < ':'
        ? (c < '/'
          ? c == ','
          : c <= '/')
        : (c <= ';' || c == '@'))))
    : (c <= '^' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == '`'
          : c <= '{')
        : (c <= '~' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__kwd_unqualified_token1_character_set_2(int32_t c) {
  return (c < '['
    ? (c < '('
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || c == '"'))
      : (c <= ')' || (c < ';'
        ? (c < '/'
          ? c == ','
          : c <= '/')
        : (c <= ';' || c == '@'))))
    : (c <= '^' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == '`'
          : c <= '{')
        : (c <= '~' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_1(int32_t c) {
  return (c < '['
    ? (c < '('
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || c == '"'))
      : (c <= ')' || (c < ';'
        ? (c < '/'
          ? c == ','
          : c <= '9')
        : (c <= ';' || c == '@'))))
    : (c <= '^' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == '`'
          : c <= '{')
        : (c <= '~' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_2(int32_t c) {
  return (c < '['
    ? (c < '('
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || c == '"'))
      : (c <= ')' || (c < ';'
        ? (c < '/'
          ? c == ','
          : c <= '9')
        : (c <= ';' || (c >= '@' && c <= 'F')))))
    : (c <= '^' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? (c >= '`' && c <= 'f')
          : c <= '{')
        : (c <= '~' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(18);
      if (lookahead == '"') ADVANCE(1);
      if (lookahead == '#') ADVANCE(6);
      if (lookahead == '\'') ADVANCE(54);
      if (lookahead == '(') ADVANCE(52);
      if (lookahead == ')') ADVANCE(53);
      if (lookahead == ',') ADVANCE(57);
      if (lookahead == '/') ADVANCE(43);
      if (lookahead == ':') ADVANCE(36);
      if (lookahead == ';') ADVANCE(22);
      if (lookahead == '`') ADVANCE(55);
      if (lookahead == 'n') ADVANCE(10);
      if (lookahead == '|') ADVANCE(2);
      if (lookahead == '\n' ||
          lookahead == '\r') ADVANCE(19);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(4);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      if (('\t' <= lookahead && lookahead <= '\f') ||
          (28 <= lookahead && lookahead <= ' ') ||
          lookahead == 5760 ||
          (8192 <= lookahead && lookahead <= 8198) ||
          (8200 <= lookahead && lookahead <= 8202) ||
          lookahead == 8232 ||
          lookahead == 8233 ||
          lookahead == 8287 ||
          lookahead == 12288) ADVANCE(20);
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(37);
      if (lookahead == '\\') ADVANCE(14);
      if (lookahead != 0) ADVANCE(1);
      END_STATE();
    case 2:
      if (lookahead == '#') ADVANCE(30);
      END_STATE();
    case 3:
      if (lookahead == '#') ADVANCE(30);
      if (lookahead == '|') ADVANCE(29);
      END_STATE();
    case 4:
      if (lookahead == '#') ADVANCE(7);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      END_STATE();
    case 5:
      if (lookahead == '#') ADVANCE(16);
      if (lookahead == '|') ADVANCE(3);
      if (lookahead == '\n' ||
          lookahead == '\r') ADVANCE(24);
      if (lookahead != 0) ADVANCE(25);
      END_STATE();
    case 6:
      if (lookahead == '\\') ADVANCE(15);
      if (lookahead == 'b') ADVANCE(11);
      if (lookahead == 'f' ||
          lookahead == 't') ADVANCE(42);
      if (lookahead == 'x') ADVANCE(12);
      if (lookahead == '|') ADVANCE(23);
      END_STATE();
    case 7:
      if (lookahead == 'b') ADVANCE(11);
      if (lookahead == 'x') ADVANCE(12);
      END_STATE();
    case 8:
      if (lookahead == 'e') ADVANCE(40);
      END_STATE();
    case 9:
      if (lookahead == 'n') ADVANCE(8);
      END_STATE();
    case 10:
      if (lookahead == 'o') ADVANCE(9);
      END_STATE();
    case 11:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(32);
      END_STATE();
    case 12:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(34);
      END_STATE();
    case 13:
      if (!aux_sym__kwd_unqualified_token1_character_set_1(lookahead)) ADVANCE(35);
      END_STATE();
    case 14:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(1);
      END_STATE();
    case 15:
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(38);
      if (lookahead == '\\') ADVANCE(39);
      END_STATE();
    case 16:
      if (lookahead != 0 &&
          lookahead != '|') ADVANCE(28);
      END_STATE();
    case 17:
      if (eof) ADVANCE(18);
      if (lookahead == '"') ADVANCE(1);
      if (lookahead == '#') ADVANCE(6);
      if (lookahead == '\'') ADVANCE(54);
      if (lookahead == '(') ADVANCE(52);
      if (lookahead == ')') ADVANCE(53);
      if (lookahead == ',') ADVANCE(57);
      if (lookahead == '/') ADVANCE(43);
      if (lookahead == ':') ADVANCE(36);
      if (lookahead == ';') ADVANCE(22);
      if (lookahead == '`') ADVANCE(55);
      if (lookahead == 'n') ADVANCE(48);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(44);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          (28 <= lookahead && lookahead <= ' ') ||
          lookahead == 5760 ||
          (8192 <= lookahead && lookahead <= 8198) ||
          (8200 <= lookahead && lookahead <= 8202) ||
          lookahead == 8232 ||
          lookahead == 8233 ||
          lookahead == 8287 ||
          lookahead == 12288) ADVANCE(20);
      if (lookahead != 0 &&
          lookahead != '@' &&
          (lookahead < '[' || '^' < lookahead) &&
          lookahead != '{' &&
          lookahead != '}' &&
          lookahead != '~') ADVANCE(51);
      END_STATE();
    case 18:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 19:
      ACCEPT_TOKEN(sym__ws);
      if (lookahead == '\n' ||
          lookahead == '\r') ADVANCE(19);
      if (('\t' <= lookahead && lookahead <= '\f') ||
          (28 <= lookahead && lookahead <= ' ') ||
          lookahead == 5760 ||
          (8192 <= lookahead && lookahead <= 8198) ||
          (8200 <= lookahead && lookahead <= 8202) ||
          lookahead == 8232 ||
          lookahead == 8233 ||
          lookahead == 8287 ||
          lookahead == 12288) ADVANCE(20);
      END_STATE();
    case 20:
      ACCEPT_TOKEN(sym__ws);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          (28 <= lookahead && lookahead <= ' ') ||
          lookahead == 5760 ||
          (8192 <= lookahead && lookahead <= 8198) ||
          (8200 <= lookahead && lookahead <= 8202) ||
          lookahead == 8232 ||
          lookahead == 8233 ||
          lookahead == 8287 ||
          lookahead == 12288) ADVANCE(20);
      END_STATE();
    case 21:
      ACCEPT_TOKEN(sym_comment);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '\n') ADVANCE(21);
      if (lookahead != 0) ADVANCE(22);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(anon_sym_POUND_PIPE);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(aux_sym_comment_multiline_token1);
      if (lookahead == '|') ADVANCE(29);
      if (lookahead == '\n' ||
          lookahead == '\r') ADVANCE(26);
      if (lookahead != 0 &&
          lookahead != '#') ADVANCE(27);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(aux_sym_comment_multiline_token1);
      if (lookahead == '|') ADVANCE(29);
      if (lookahead != 0 &&
          lookahead != '#') ADVANCE(27);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(aux_sym_comment_multiline_token1);
      if (lookahead == '\n' ||
          lookahead == '\r') ADVANCE(26);
      if (lookahead != 0 &&
          lookahead != '#' &&
          lookahead != '|') ADVANCE(27);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(aux_sym_comment_multiline_token1);
      if (lookahead != 0 &&
          lookahead != '#' &&
          lookahead != '|') ADVANCE(27);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(aux_sym_comment_multiline_token2);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(aux_sym_comment_multiline_token3);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_PIPE_POUND);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym_num_lit);
      if (lookahead == '.') ADVANCE(33);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(sym_num_lit);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(32);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(sym_num_lit);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(33);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(sym_num_lit);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(34);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym__kwd_unqualified_token1);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(35);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(sym_str_lit);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(sym_char_lit);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(sym_char_lit);
      if (lookahead == 'n' ||
          lookahead == 's' ||
          lookahead == 't') ADVANCE(38);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(sym_null_lit);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym_null_lit);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(sym_bool_lit);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(45);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(51);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(31);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'b') ADVANCE(49);
      if (lookahead == 'x') ADVANCE(50);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'e') ADVANCE(41);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'n') ADVANCE(46);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'o') ADVANCE(47);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(32);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(34);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(51);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_BQUOTE);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_COMMA_AT);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_COMMA);
      if (lookahead == '@') ADVANCE(56);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 17},
  [2] = {.lex_state = 17},
  [3] = {.lex_state = 17},
  [4] = {.lex_state = 17},
  [5] = {.lex_state = 17},
  [6] = {.lex_state = 17},
  [7] = {.lex_state = 17},
  [8] = {.lex_state = 17},
  [9] = {.lex_state = 17},
  [10] = {.lex_state = 17},
  [11] = {.lex_state = 17},
  [12] = {.lex_state = 17},
  [13] = {.lex_state = 17},
  [14] = {.lex_state = 17},
  [15] = {.lex_state = 17},
  [16] = {.lex_state = 17},
  [17] = {.lex_state = 17},
  [18] = {.lex_state = 17},
  [19] = {.lex_state = 17},
  [20] = {.lex_state = 17},
  [21] = {.lex_state = 17},
  [22] = {.lex_state = 17},
  [23] = {.lex_state = 17},
  [24] = {.lex_state = 17},
  [25] = {.lex_state = 17},
  [26] = {.lex_state = 17},
  [27] = {.lex_state = 17},
  [28] = {.lex_state = 17},
  [29] = {.lex_state = 17},
  [30] = {.lex_state = 17},
  [31] = {.lex_state = 17},
  [32] = {.lex_state = 5},
  [33] = {.lex_state = 5},
  [34] = {.lex_state = 5},
  [35] = {.lex_state = 13},
  [36] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym__ws] = ACTIONS(1),
    [sym_comment] = ACTIONS(1),
    [anon_sym_POUND_PIPE] = ACTIONS(1),
    [aux_sym_comment_multiline_token4] = ACTIONS(1),
    [anon_sym_PIPE_POUND] = ACTIONS(1),
    [sym_num_lit] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [sym_str_lit] = ACTIONS(1),
    [sym_char_lit] = ACTIONS(1),
    [sym_null_lit] = ACTIONS(1),
    [sym_bool_lit] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [anon_sym_BQUOTE] = ACTIONS(1),
    [anon_sym_COMMA_AT] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
  },
  [1] = {
    [sym_source] = STATE(36),
    [sym__gap] = STATE(6),
    [sym_comment_multiline] = STATE(6),
    [sym__form] = STATE(6),
    [sym_kwd_lit] = STATE(6),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(6),
    [sym_list_lit] = STATE(6),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(6),
    [sym_quasi_quoting_lit] = STATE(6),
    [sym_unquote_splicing_lit] = STATE(6),
    [sym_unquoting_lit] = STATE(6),
    [aux_sym_source_repeat1] = STATE(6),
    [ts_builtin_sym_end] = ACTIONS(3),
    [sym__ws] = ACTIONS(5),
    [sym_comment] = ACTIONS(5),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(5),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(5),
    [sym_char_lit] = ACTIONS(5),
    [sym_null_lit] = ACTIONS(11),
    [sym_bool_lit] = ACTIONS(5),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [2] = {
    [sym__gap] = STATE(3),
    [sym_comment_multiline] = STATE(3),
    [sym__form] = STATE(31),
    [sym_kwd_lit] = STATE(31),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(31),
    [sym_list_lit] = STATE(31),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(31),
    [sym_quasi_quoting_lit] = STATE(31),
    [sym_unquote_splicing_lit] = STATE(31),
    [sym_unquoting_lit] = STATE(31),
    [aux_sym__bare_list_lit_repeat1] = STATE(3),
    [sym__ws] = ACTIONS(27),
    [sym_comment] = ACTIONS(27),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(29),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(29),
    [sym_char_lit] = ACTIONS(29),
    [sym_null_lit] = ACTIONS(31),
    [sym_bool_lit] = ACTIONS(29),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_RPAREN] = ACTIONS(33),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [3] = {
    [sym__gap] = STATE(3),
    [sym_comment_multiline] = STATE(3),
    [sym__form] = STATE(31),
    [sym_kwd_lit] = STATE(31),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(31),
    [sym_list_lit] = STATE(31),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(31),
    [sym_quasi_quoting_lit] = STATE(31),
    [sym_unquote_splicing_lit] = STATE(31),
    [sym_unquoting_lit] = STATE(31),
    [aux_sym__bare_list_lit_repeat1] = STATE(3),
    [sym__ws] = ACTIONS(35),
    [sym_comment] = ACTIONS(35),
    [anon_sym_POUND_PIPE] = ACTIONS(38),
    [sym_num_lit] = ACTIONS(41),
    [anon_sym_COLON] = ACTIONS(44),
    [sym_str_lit] = ACTIONS(41),
    [sym_char_lit] = ACTIONS(41),
    [sym_null_lit] = ACTIONS(47),
    [sym_bool_lit] = ACTIONS(41),
    [anon_sym_SLASH] = ACTIONS(50),
    [aux_sym__sym_unqualified_token1] = ACTIONS(53),
    [anon_sym_LPAREN] = ACTIONS(56),
    [anon_sym_RPAREN] = ACTIONS(59),
    [anon_sym_SQUOTE] = ACTIONS(61),
    [anon_sym_BQUOTE] = ACTIONS(64),
    [anon_sym_COMMA_AT] = ACTIONS(67),
    [anon_sym_COMMA] = ACTIONS(70),
  },
  [4] = {
    [sym__gap] = STATE(2),
    [sym_comment_multiline] = STATE(2),
    [sym__form] = STATE(31),
    [sym_kwd_lit] = STATE(31),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(31),
    [sym_list_lit] = STATE(31),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(31),
    [sym_quasi_quoting_lit] = STATE(31),
    [sym_unquote_splicing_lit] = STATE(31),
    [sym_unquoting_lit] = STATE(31),
    [aux_sym__bare_list_lit_repeat1] = STATE(2),
    [sym__ws] = ACTIONS(73),
    [sym_comment] = ACTIONS(73),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(29),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(29),
    [sym_char_lit] = ACTIONS(29),
    [sym_null_lit] = ACTIONS(31),
    [sym_bool_lit] = ACTIONS(29),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_RPAREN] = ACTIONS(75),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [5] = {
    [sym__gap] = STATE(5),
    [sym_comment_multiline] = STATE(5),
    [sym__form] = STATE(5),
    [sym_kwd_lit] = STATE(5),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(5),
    [sym_list_lit] = STATE(5),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(5),
    [sym_quasi_quoting_lit] = STATE(5),
    [sym_unquote_splicing_lit] = STATE(5),
    [sym_unquoting_lit] = STATE(5),
    [aux_sym_source_repeat1] = STATE(5),
    [ts_builtin_sym_end] = ACTIONS(77),
    [sym__ws] = ACTIONS(79),
    [sym_comment] = ACTIONS(79),
    [anon_sym_POUND_PIPE] = ACTIONS(82),
    [sym_num_lit] = ACTIONS(79),
    [anon_sym_COLON] = ACTIONS(85),
    [sym_str_lit] = ACTIONS(79),
    [sym_char_lit] = ACTIONS(79),
    [sym_null_lit] = ACTIONS(88),
    [sym_bool_lit] = ACTIONS(79),
    [anon_sym_SLASH] = ACTIONS(91),
    [aux_sym__sym_unqualified_token1] = ACTIONS(94),
    [anon_sym_LPAREN] = ACTIONS(97),
    [anon_sym_SQUOTE] = ACTIONS(100),
    [anon_sym_BQUOTE] = ACTIONS(103),
    [anon_sym_COMMA_AT] = ACTIONS(106),
    [anon_sym_COMMA] = ACTIONS(109),
  },
  [6] = {
    [sym__gap] = STATE(5),
    [sym_comment_multiline] = STATE(5),
    [sym__form] = STATE(5),
    [sym_kwd_lit] = STATE(5),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(5),
    [sym_list_lit] = STATE(5),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(5),
    [sym_quasi_quoting_lit] = STATE(5),
    [sym_unquote_splicing_lit] = STATE(5),
    [sym_unquoting_lit] = STATE(5),
    [aux_sym_source_repeat1] = STATE(5),
    [ts_builtin_sym_end] = ACTIONS(112),
    [sym__ws] = ACTIONS(114),
    [sym_comment] = ACTIONS(114),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(114),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(114),
    [sym_char_lit] = ACTIONS(114),
    [sym_null_lit] = ACTIONS(116),
    [sym_bool_lit] = ACTIONS(114),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [7] = {
    [sym__gap] = STATE(14),
    [sym_comment_multiline] = STATE(14),
    [sym__form] = STATE(16),
    [sym_kwd_lit] = STATE(16),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(16),
    [sym_list_lit] = STATE(16),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(16),
    [sym_quasi_quoting_lit] = STATE(16),
    [sym_unquote_splicing_lit] = STATE(16),
    [sym_unquoting_lit] = STATE(16),
    [aux_sym_quoting_lit_repeat1] = STATE(14),
    [sym__ws] = ACTIONS(118),
    [sym_comment] = ACTIONS(118),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(120),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(120),
    [sym_char_lit] = ACTIONS(120),
    [sym_null_lit] = ACTIONS(122),
    [sym_bool_lit] = ACTIONS(120),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [8] = {
    [sym__gap] = STATE(13),
    [sym_comment_multiline] = STATE(13),
    [sym__form] = STATE(18),
    [sym_kwd_lit] = STATE(18),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(18),
    [sym_list_lit] = STATE(18),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(18),
    [sym_quasi_quoting_lit] = STATE(18),
    [sym_unquote_splicing_lit] = STATE(18),
    [sym_unquoting_lit] = STATE(18),
    [aux_sym_quoting_lit_repeat1] = STATE(13),
    [sym__ws] = ACTIONS(124),
    [sym_comment] = ACTIONS(124),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(126),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(126),
    [sym_char_lit] = ACTIONS(126),
    [sym_null_lit] = ACTIONS(128),
    [sym_bool_lit] = ACTIONS(126),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [9] = {
    [sym__gap] = STATE(12),
    [sym_comment_multiline] = STATE(12),
    [sym__form] = STATE(19),
    [sym_kwd_lit] = STATE(19),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(19),
    [sym_list_lit] = STATE(19),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(19),
    [sym_quasi_quoting_lit] = STATE(19),
    [sym_unquote_splicing_lit] = STATE(19),
    [sym_unquoting_lit] = STATE(19),
    [aux_sym_quoting_lit_repeat1] = STATE(12),
    [sym__ws] = ACTIONS(130),
    [sym_comment] = ACTIONS(130),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(132),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(132),
    [sym_char_lit] = ACTIONS(132),
    [sym_null_lit] = ACTIONS(134),
    [sym_bool_lit] = ACTIONS(132),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [10] = {
    [sym__gap] = STATE(11),
    [sym_comment_multiline] = STATE(11),
    [sym__form] = STATE(21),
    [sym_kwd_lit] = STATE(21),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(21),
    [sym_list_lit] = STATE(21),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(21),
    [sym_quasi_quoting_lit] = STATE(21),
    [sym_unquote_splicing_lit] = STATE(21),
    [sym_unquoting_lit] = STATE(21),
    [aux_sym_quoting_lit_repeat1] = STATE(11),
    [sym__ws] = ACTIONS(136),
    [sym_comment] = ACTIONS(136),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(138),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(138),
    [sym_char_lit] = ACTIONS(138),
    [sym_null_lit] = ACTIONS(140),
    [sym_bool_lit] = ACTIONS(138),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [11] = {
    [sym__gap] = STATE(15),
    [sym_comment_multiline] = STATE(15),
    [sym__form] = STATE(30),
    [sym_kwd_lit] = STATE(30),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(30),
    [sym_list_lit] = STATE(30),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(30),
    [sym_quasi_quoting_lit] = STATE(30),
    [sym_unquote_splicing_lit] = STATE(30),
    [sym_unquoting_lit] = STATE(30),
    [aux_sym_quoting_lit_repeat1] = STATE(15),
    [sym__ws] = ACTIONS(142),
    [sym_comment] = ACTIONS(142),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(144),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(144),
    [sym_char_lit] = ACTIONS(144),
    [sym_null_lit] = ACTIONS(146),
    [sym_bool_lit] = ACTIONS(144),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [12] = {
    [sym__gap] = STATE(15),
    [sym_comment_multiline] = STATE(15),
    [sym__form] = STATE(29),
    [sym_kwd_lit] = STATE(29),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(29),
    [sym_list_lit] = STATE(29),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(29),
    [sym_quasi_quoting_lit] = STATE(29),
    [sym_unquote_splicing_lit] = STATE(29),
    [sym_unquoting_lit] = STATE(29),
    [aux_sym_quoting_lit_repeat1] = STATE(15),
    [sym__ws] = ACTIONS(142),
    [sym_comment] = ACTIONS(142),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(148),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(148),
    [sym_char_lit] = ACTIONS(148),
    [sym_null_lit] = ACTIONS(150),
    [sym_bool_lit] = ACTIONS(148),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [13] = {
    [sym__gap] = STATE(15),
    [sym_comment_multiline] = STATE(15),
    [sym__form] = STATE(28),
    [sym_kwd_lit] = STATE(28),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(28),
    [sym_list_lit] = STATE(28),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(28),
    [sym_quasi_quoting_lit] = STATE(28),
    [sym_unquote_splicing_lit] = STATE(28),
    [sym_unquoting_lit] = STATE(28),
    [aux_sym_quoting_lit_repeat1] = STATE(15),
    [sym__ws] = ACTIONS(142),
    [sym_comment] = ACTIONS(142),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(152),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(152),
    [sym_char_lit] = ACTIONS(152),
    [sym_null_lit] = ACTIONS(154),
    [sym_bool_lit] = ACTIONS(152),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
  [14] = {
    [sym__gap] = STATE(15),
    [sym_comment_multiline] = STATE(15),
    [sym__form] = STATE(26),
    [sym_kwd_lit] = STATE(26),
    [sym__kwd_marker] = STATE(35),
    [sym_sym_lit] = STATE(26),
    [sym_list_lit] = STATE(26),
    [sym__bare_list_lit] = STATE(22),
    [sym_quoting_lit] = STATE(26),
    [sym_quasi_quoting_lit] = STATE(26),
    [sym_unquote_splicing_lit] = STATE(26),
    [sym_unquoting_lit] = STATE(26),
    [aux_sym_quoting_lit_repeat1] = STATE(15),
    [sym__ws] = ACTIONS(142),
    [sym_comment] = ACTIONS(142),
    [anon_sym_POUND_PIPE] = ACTIONS(7),
    [sym_num_lit] = ACTIONS(156),
    [anon_sym_COLON] = ACTIONS(9),
    [sym_str_lit] = ACTIONS(156),
    [sym_char_lit] = ACTIONS(156),
    [sym_null_lit] = ACTIONS(158),
    [sym_bool_lit] = ACTIONS(156),
    [anon_sym_SLASH] = ACTIONS(13),
    [aux_sym__sym_unqualified_token1] = ACTIONS(15),
    [anon_sym_LPAREN] = ACTIONS(17),
    [anon_sym_SQUOTE] = ACTIONS(19),
    [anon_sym_BQUOTE] = ACTIONS(21),
    [anon_sym_COMMA_AT] = ACTIONS(23),
    [anon_sym_COMMA] = ACTIONS(25),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 5,
    ACTIONS(163), 1,
      anon_sym_POUND_PIPE,
    ACTIONS(160), 2,
      sym__ws,
      sym_comment,
    ACTIONS(168), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    STATE(15), 3,
      sym__gap,
      sym_comment_multiline,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(166), 10,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [30] = 2,
    ACTIONS(172), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(170), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [53] = 2,
    ACTIONS(176), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(174), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [76] = 2,
    ACTIONS(180), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(178), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [99] = 2,
    ACTIONS(184), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(182), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [122] = 2,
    ACTIONS(188), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(186), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [145] = 2,
    ACTIONS(192), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(190), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [168] = 2,
    ACTIONS(196), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(194), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [191] = 2,
    ACTIONS(200), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(198), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [214] = 2,
    ACTIONS(204), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(202), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [237] = 2,
    ACTIONS(208), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(206), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [260] = 2,
    ACTIONS(212), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(210), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [283] = 2,
    ACTIONS(216), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(214), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [306] = 2,
    ACTIONS(220), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(218), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [329] = 2,
    ACTIONS(224), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(222), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [352] = 2,
    ACTIONS(228), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(226), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [375] = 2,
    ACTIONS(232), 3,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
      anon_sym_COMMA,
    ACTIONS(230), 14,
      sym__ws,
      sym_comment,
      anon_sym_POUND_PIPE,
      sym_num_lit,
      anon_sym_COLON,
      sym_str_lit,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [397] = 4,
    ACTIONS(238), 1,
      anon_sym_PIPE_POUND,
    STATE(33), 1,
      aux_sym_comment_multiline_repeat1,
    ACTIONS(234), 2,
      aux_sym_comment_multiline_token1,
      aux_sym_comment_multiline_token4,
    ACTIONS(236), 2,
      aux_sym_comment_multiline_token2,
      aux_sym_comment_multiline_token3,
  [412] = 4,
    ACTIONS(244), 1,
      anon_sym_PIPE_POUND,
    STATE(34), 1,
      aux_sym_comment_multiline_repeat1,
    ACTIONS(240), 2,
      aux_sym_comment_multiline_token1,
      aux_sym_comment_multiline_token4,
    ACTIONS(242), 2,
      aux_sym_comment_multiline_token2,
      aux_sym_comment_multiline_token3,
  [427] = 4,
    ACTIONS(252), 1,
      anon_sym_PIPE_POUND,
    STATE(34), 1,
      aux_sym_comment_multiline_repeat1,
    ACTIONS(246), 2,
      aux_sym_comment_multiline_token1,
      aux_sym_comment_multiline_token4,
    ACTIONS(249), 2,
      aux_sym_comment_multiline_token2,
      aux_sym_comment_multiline_token3,
  [442] = 1,
    ACTIONS(254), 1,
      aux_sym__kwd_unqualified_token1,
  [446] = 1,
    ACTIONS(256), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(15)] = 0,
  [SMALL_STATE(16)] = 30,
  [SMALL_STATE(17)] = 53,
  [SMALL_STATE(18)] = 76,
  [SMALL_STATE(19)] = 99,
  [SMALL_STATE(20)] = 122,
  [SMALL_STATE(21)] = 145,
  [SMALL_STATE(22)] = 168,
  [SMALL_STATE(23)] = 191,
  [SMALL_STATE(24)] = 214,
  [SMALL_STATE(25)] = 237,
  [SMALL_STATE(26)] = 260,
  [SMALL_STATE(27)] = 283,
  [SMALL_STATE(28)] = 306,
  [SMALL_STATE(29)] = 329,
  [SMALL_STATE(30)] = 352,
  [SMALL_STATE(31)] = 375,
  [SMALL_STATE(32)] = 397,
  [SMALL_STATE(33)] = 412,
  [SMALL_STATE(34)] = 427,
  [SMALL_STATE(35)] = 442,
  [SMALL_STATE(36)] = 446,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [35] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(3),
  [38] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(32),
  [41] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(31),
  [44] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(35),
  [47] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(31),
  [50] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(27),
  [53] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(27),
  [56] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(4),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8),
  [61] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(7),
  [64] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(8),
  [67] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(9),
  [70] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 8), SHIFT_REPEAT(10),
  [73] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2),
  [79] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(5),
  [82] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(32),
  [85] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(35),
  [88] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(5),
  [91] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(27),
  [94] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(27),
  [97] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(4),
  [100] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(7),
  [103] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(8),
  [106] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(9),
  [109] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(10),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 1),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [116] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [118] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [120] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [122] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [128] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [134] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [136] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [140] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [146] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [150] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [154] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [158] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [160] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(15),
  [163] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(32),
  [166] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [168] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [170] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 2, .production_id = 5),
  [172] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 2, .production_id = 5),
  [174] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 2, .production_id = 3),
  [176] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 2, .production_id = 3),
  [178] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 5),
  [180] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 5),
  [182] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 5),
  [184] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 5),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment_multiline, 2),
  [188] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comment_multiline, 2),
  [190] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 2, .production_id = 5),
  [192] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 2, .production_id = 5),
  [194] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [196] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_kwd_lit, 2, .production_id = 6),
  [200] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_kwd_lit, 2, .production_id = 6),
  [202] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_comment_multiline, 3),
  [204] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_comment_multiline, 3),
  [206] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 3, .production_id = 7),
  [208] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 3, .production_id = 7),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 3, .production_id = 9),
  [212] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 3, .production_id = 9),
  [214] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [216] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [218] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 9),
  [220] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 9),
  [222] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 9),
  [224] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 9),
  [226] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 3, .production_id = 9),
  [228] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 3, .production_id = 9),
  [230] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 4),
  [232] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 4),
  [234] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [236] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [238] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [240] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [242] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [244] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [246] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_comment_multiline_repeat1, 2), SHIFT_REPEAT(34),
  [249] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_comment_multiline_repeat1, 2), SHIFT_REPEAT(34),
  [252] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_comment_multiline_repeat1, 2),
  [254] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [256] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_opengoal(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
