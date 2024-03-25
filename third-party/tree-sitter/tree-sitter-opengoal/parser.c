#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 60
#define LARGE_STATE_COUNT 5
#define SYMBOL_COUNT 72
#define ALIAS_COUNT 0
#define TOKEN_COUNT 50
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 7
#define MAX_ALIAS_SEQUENCE_LENGTH 4
#define PRODUCTION_ID_COUNT 12

enum ts_symbol_identifiers {
  sym__ws = 1,
  sym_comment = 2,
  sym_block_comment = 3,
  aux_sym_num_lit_token1 = 4,
  sym_kwd_lit = 5,
  anon_sym_SQUOTE = 6,
  aux_sym__format_token_token1 = 7,
  anon_sym_v = 8,
  anon_sym_V = 9,
  anon_sym_POUND = 10,
  anon_sym_COMMA = 11,
  anon_sym_AT = 12,
  anon_sym_AT_COLON = 13,
  anon_sym_COLON = 14,
  anon_sym_COLON_AT = 15,
  anon_sym_TILDE = 16,
  anon_sym_PERCENT = 17,
  anon_sym_AMP = 18,
  anon_sym_PIPE = 19,
  aux_sym_format_directive_type_token1 = 20,
  aux_sym_format_directive_type_token2 = 21,
  anon_sym_LF = 22,
  anon_sym_CR = 23,
  aux_sym_format_directive_type_token3 = 24,
  aux_sym_format_directive_type_token4 = 25,
  aux_sym_format_directive_type_token5 = 26,
  aux_sym_format_directive_type_token6 = 27,
  anon_sym__ = 28,
  aux_sym_format_directive_type_token7 = 29,
  aux_sym_format_directive_type_token8 = 30,
  aux_sym_format_directive_type_token9 = 31,
  aux_sym_format_directive_type_token10 = 32,
  anon_sym_SEMI = 33,
  anon_sym_BQUOTE = 34,
  anon_sym_STAR = 35,
  anon_sym_QMARK = 36,
  anon_sym_Newline = 37,
  aux_sym_format_directive_type_token11 = 38,
  anon_sym_DQUOTE = 39,
  aux_sym_str_lit_token1 = 40,
  aux_sym_str_lit_token2 = 41,
  sym_char_lit = 42,
  sym_null_lit = 43,
  sym_bool_lit = 44,
  anon_sym_SLASH = 45,
  aux_sym__sym_unqualified_token1 = 46,
  anon_sym_LPAREN = 47,
  anon_sym_RPAREN = 48,
  anon_sym_COMMA_AT = 49,
  sym_source = 50,
  sym__gap = 51,
  sym__form = 52,
  sym_num_lit = 53,
  sym__format_token = 54,
  sym_format_prefix_parameters = 55,
  sym_format_modifiers = 56,
  sym_format_directive_type = 57,
  sym_format_specifier = 58,
  sym_str_lit = 59,
  sym_sym_lit = 60,
  sym_list_lit = 61,
  sym__bare_list_lit = 62,
  sym_quoting_lit = 63,
  sym_quasi_quoting_lit = 64,
  sym_unquote_splicing_lit = 65,
  sym_unquoting_lit = 66,
  aux_sym_source_repeat1 = 67,
  aux_sym_format_modifiers_repeat1 = 68,
  aux_sym_str_lit_repeat1 = 69,
  aux_sym__bare_list_lit_repeat1 = 70,
  aux_sym_quoting_lit_repeat1 = 71,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym__ws] = "_ws",
  [sym_comment] = "comment",
  [sym_block_comment] = "block_comment",
  [aux_sym_num_lit_token1] = "num_lit_token1",
  [sym_kwd_lit] = "kwd_lit",
  [anon_sym_SQUOTE] = "'",
  [aux_sym__format_token_token1] = "char_lit",
  [anon_sym_v] = "v",
  [anon_sym_V] = "V",
  [anon_sym_POUND] = "#",
  [anon_sym_COMMA] = ",",
  [anon_sym_AT] = "@",
  [anon_sym_AT_COLON] = "@:",
  [anon_sym_COLON] = ":",
  [anon_sym_COLON_AT] = ":@",
  [anon_sym_TILDE] = "~",
  [anon_sym_PERCENT] = "%",
  [anon_sym_AMP] = "&",
  [anon_sym_PIPE] = "|",
  [aux_sym_format_directive_type_token1] = "format_directive_type_token1",
  [aux_sym_format_directive_type_token2] = "format_directive_type_token2",
  [anon_sym_LF] = "\n",
  [anon_sym_CR] = "\r",
  [aux_sym_format_directive_type_token3] = "format_directive_type_token3",
  [aux_sym_format_directive_type_token4] = "format_directive_type_token4",
  [aux_sym_format_directive_type_token5] = "format_directive_type_token5",
  [aux_sym_format_directive_type_token6] = "format_directive_type_token6",
  [anon_sym__] = "_",
  [aux_sym_format_directive_type_token7] = "format_directive_type_token7",
  [aux_sym_format_directive_type_token8] = "format_directive_type_token8",
  [aux_sym_format_directive_type_token9] = "format_directive_type_token9",
  [aux_sym_format_directive_type_token10] = "format_directive_type_token10",
  [anon_sym_SEMI] = ";",
  [anon_sym_BQUOTE] = "`",
  [anon_sym_STAR] = "*",
  [anon_sym_QMARK] = "\?",
  [anon_sym_Newline] = "Newline",
  [aux_sym_format_directive_type_token11] = "format_directive_type_token11",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_str_lit_token1] = "str_lit_token1",
  [aux_sym_str_lit_token2] = "str_lit_token2",
  [sym_char_lit] = "char_lit",
  [sym_null_lit] = "null_lit",
  [sym_bool_lit] = "bool_lit",
  [anon_sym_SLASH] = "sym_name",
  [aux_sym__sym_unqualified_token1] = "sym_name",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_COMMA_AT] = ",@",
  [sym_source] = "source",
  [sym__gap] = "_gap",
  [sym__form] = "_form",
  [sym_num_lit] = "num_lit",
  [sym__format_token] = "_format_token",
  [sym_format_prefix_parameters] = "format_prefix_parameters",
  [sym_format_modifiers] = "format_modifiers",
  [sym_format_directive_type] = "format_directive_type",
  [sym_format_specifier] = "format_specifier",
  [sym_str_lit] = "str_lit",
  [sym_sym_lit] = "sym_lit",
  [sym_list_lit] = "list_lit",
  [sym__bare_list_lit] = "_bare_list_lit",
  [sym_quoting_lit] = "quoting_lit",
  [sym_quasi_quoting_lit] = "quasi_quoting_lit",
  [sym_unquote_splicing_lit] = "unquote_splicing_lit",
  [sym_unquoting_lit] = "unquoting_lit",
  [aux_sym_source_repeat1] = "source_repeat1",
  [aux_sym_format_modifiers_repeat1] = "format_modifiers_repeat1",
  [aux_sym_str_lit_repeat1] = "str_lit_repeat1",
  [aux_sym__bare_list_lit_repeat1] = "_bare_list_lit_repeat1",
  [aux_sym_quoting_lit_repeat1] = "quoting_lit_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym__ws] = sym__ws,
  [sym_comment] = sym_comment,
  [sym_block_comment] = sym_block_comment,
  [aux_sym_num_lit_token1] = aux_sym_num_lit_token1,
  [sym_kwd_lit] = sym_kwd_lit,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym__format_token_token1] = sym_char_lit,
  [anon_sym_v] = anon_sym_v,
  [anon_sym_V] = anon_sym_V,
  [anon_sym_POUND] = anon_sym_POUND,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_AT] = anon_sym_AT,
  [anon_sym_AT_COLON] = anon_sym_AT_COLON,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_COLON_AT] = anon_sym_COLON_AT,
  [anon_sym_TILDE] = anon_sym_TILDE,
  [anon_sym_PERCENT] = anon_sym_PERCENT,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [aux_sym_format_directive_type_token1] = aux_sym_format_directive_type_token1,
  [aux_sym_format_directive_type_token2] = aux_sym_format_directive_type_token2,
  [anon_sym_LF] = anon_sym_LF,
  [anon_sym_CR] = anon_sym_CR,
  [aux_sym_format_directive_type_token3] = aux_sym_format_directive_type_token3,
  [aux_sym_format_directive_type_token4] = aux_sym_format_directive_type_token4,
  [aux_sym_format_directive_type_token5] = aux_sym_format_directive_type_token5,
  [aux_sym_format_directive_type_token6] = aux_sym_format_directive_type_token6,
  [anon_sym__] = anon_sym__,
  [aux_sym_format_directive_type_token7] = aux_sym_format_directive_type_token7,
  [aux_sym_format_directive_type_token8] = aux_sym_format_directive_type_token8,
  [aux_sym_format_directive_type_token9] = aux_sym_format_directive_type_token9,
  [aux_sym_format_directive_type_token10] = aux_sym_format_directive_type_token10,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_BQUOTE] = anon_sym_BQUOTE,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_QMARK] = anon_sym_QMARK,
  [anon_sym_Newline] = anon_sym_Newline,
  [aux_sym_format_directive_type_token11] = aux_sym_format_directive_type_token11,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_str_lit_token1] = aux_sym_str_lit_token1,
  [aux_sym_str_lit_token2] = aux_sym_str_lit_token2,
  [sym_char_lit] = sym_char_lit,
  [sym_null_lit] = sym_null_lit,
  [sym_bool_lit] = sym_bool_lit,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [aux_sym__sym_unqualified_token1] = anon_sym_SLASH,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_COMMA_AT] = anon_sym_COMMA_AT,
  [sym_source] = sym_source,
  [sym__gap] = sym__gap,
  [sym__form] = sym__form,
  [sym_num_lit] = sym_num_lit,
  [sym__format_token] = sym__format_token,
  [sym_format_prefix_parameters] = sym_format_prefix_parameters,
  [sym_format_modifiers] = sym_format_modifiers,
  [sym_format_directive_type] = sym_format_directive_type,
  [sym_format_specifier] = sym_format_specifier,
  [sym_str_lit] = sym_str_lit,
  [sym_sym_lit] = sym_sym_lit,
  [sym_list_lit] = sym_list_lit,
  [sym__bare_list_lit] = sym__bare_list_lit,
  [sym_quoting_lit] = sym_quoting_lit,
  [sym_quasi_quoting_lit] = sym_quasi_quoting_lit,
  [sym_unquote_splicing_lit] = sym_unquote_splicing_lit,
  [sym_unquoting_lit] = sym_unquoting_lit,
  [aux_sym_source_repeat1] = aux_sym_source_repeat1,
  [aux_sym_format_modifiers_repeat1] = aux_sym_format_modifiers_repeat1,
  [aux_sym_str_lit_repeat1] = aux_sym_str_lit_repeat1,
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
  [sym_block_comment] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_num_lit_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_kwd_lit] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym__format_token_token1] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_v] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_V] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_POUND] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AT_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON_AT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_TILDE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PERCENT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_format_directive_type_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_format_directive_type_token2] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_LF] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_CR] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_format_directive_type_token3] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_format_directive_type_token4] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_format_directive_type_token5] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_format_directive_type_token6] = {
    .visible = false,
    .named = false,
  },
  [anon_sym__] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_format_directive_type_token7] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_format_directive_type_token8] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_format_directive_type_token9] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_format_directive_type_token10] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BQUOTE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_QMARK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_Newline] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_format_directive_type_token11] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_str_lit_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_str_lit_token2] = {
    .visible = false,
    .named = false,
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
  [anon_sym_COMMA_AT] = {
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
  [sym__form] = {
    .visible = false,
    .named = true,
  },
  [sym_num_lit] = {
    .visible = true,
    .named = true,
  },
  [sym__format_token] = {
    .visible = false,
    .named = true,
  },
  [sym_format_prefix_parameters] = {
    .visible = true,
    .named = true,
  },
  [sym_format_modifiers] = {
    .visible = true,
    .named = true,
  },
  [sym_format_directive_type] = {
    .visible = true,
    .named = true,
  },
  [sym_format_specifier] = {
    .visible = true,
    .named = true,
  },
  [sym_str_lit] = {
    .visible = true,
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
  [aux_sym_format_modifiers_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_str_lit_repeat1] = {
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

enum ts_field_identifiers {
  field_close = 1,
  field_marker = 2,
  field_name = 3,
  field_numberOfArgs = 4,
  field_open = 5,
  field_repetitions = 6,
  field_value = 7,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_close] = "close",
  [field_marker] = "marker",
  [field_name] = "name",
  [field_numberOfArgs] = "numberOfArgs",
  [field_open] = "open",
  [field_repetitions] = "repetitions",
  [field_value] = "value",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 3},
  [3] = {.index = 4, .length = 2},
  [4] = {.index = 6, .length = 2},
  [5] = {.index = 8, .length = 1},
  [6] = {.index = 9, .length = 2},
  [8] = {.index = 11, .length = 3},
  [9] = {.index = 14, .length = 2},
  [10] = {.index = 16, .length = 1},
  [11] = {.index = 17, .length = 1},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_close, 0, .inherited = true},
    {field_open, 0, .inherited = true},
    {field_value, 0, .inherited = true},
  [4] =
    {field_marker, 0},
    {field_value, 1},
  [6] =
    {field_close, 1},
    {field_open, 0},
  [8] =
    {field_value, 0},
  [9] =
    {field_marker, 0},
    {field_value, 2},
  [11] =
    {field_close, 2},
    {field_open, 0},
    {field_value, 1, .inherited = true},
  [14] =
    {field_value, 0, .inherited = true},
    {field_value, 1, .inherited = true},
  [16] =
    {field_repetitions, 0},
  [17] =
    {field_numberOfArgs, 0},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [7] = {
    [0] = sym_num_lit,
  },
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
  [37] = 37,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 44,
  [45] = 45,
  [46] = 46,
  [47] = 47,
  [48] = 48,
  [49] = 49,
  [50] = 50,
  [51] = 51,
  [52] = 52,
  [53] = 53,
  [54] = 54,
  [55] = 55,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 59,
};

static inline bool sym_kwd_lit_character_set_1(int32_t c) {
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

static inline bool sym_kwd_lit_character_set_2(int32_t c) {
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

static inline bool aux_sym_format_directive_type_token11_character_set_1(int32_t c) {
  return (c < 'R'
    ? (c < 'G'
      ? (c < 'B'
        ? c == '$'
        : c <= 'E')
      : (c <= 'L' || c == 'O'))
    : (c <= 'T' || (c < 'r'
      ? (c < 'b'
        ? (c >= 'X' && c <= 'Z')
        : c <= 'o')
      : (c <= 't' || (c >= 'x' && c <= 'z')))));
}

static inline bool aux_sym_format_directive_type_token11_character_set_2(int32_t c) {
  return (c < 'R'
    ? (c < 'G'
      ? (c < 'B'
        ? c == '$'
        : c <= 'E')
      : (c <= 'L' || c == 'O'))
    : (c <= 'T' || (c < 'r'
      ? (c < 'b'
        ? (c >= 'V' && c <= 'Z')
        : c <= 'o')
      : (c <= 't' || (c >= 'x' && c <= 'z')))));
}

static inline bool aux_sym_format_directive_type_token11_character_set_3(int32_t c) {
  return (c < 'R'
    ? (c < 'G'
      ? (c < 'B'
        ? c == '$'
        : (c <= 'B' || (c >= 'D' && c <= 'E')))
      : (c <= 'H' || (c < 'N'
        ? (c >= 'J' && c <= 'L')
        : c <= 'O')))
    : (c <= 'T' || (c < 'j'
      ? (c < 'b'
        ? (c >= 'V' && c <= 'Z')
        : (c <= 'b' || (c >= 'd' && c <= 'h')))
      : (c <= 'o' || (c < 'w'
        ? (c >= 'r' && c <= 't')
        : c <= 'z')))));
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
      if (eof) ADVANCE(23);
      if (lookahead == '\n') ADVANCE(68);
      if (lookahead == '\r') ADVANCE(68);
      if (lookahead == '"') ADVANCE(67);
      if (lookahead == '#') ADVANCE(68);
      if (lookahead == '%') ADVANCE(68);
      if (lookahead == '&') ADVANCE(68);
      if (lookahead == '\'') ADVANCE(68);
      if (lookahead == '(') ADVANCE(68);
      if (lookahead == ')') ADVANCE(68);
      if (lookahead == '*') ADVANCE(68);
      if (lookahead == ',') ADVANCE(68);
      if (lookahead == '/') ADVANCE(68);
      if (lookahead == ':') ADVANCE(68);
      if (lookahead == ';') ADVANCE(68);
      if (lookahead == '?') ADVANCE(68);
      if (lookahead == '@') ADVANCE(68);
      if (lookahead == 'V') ADVANCE(68);
      if (lookahead == '\\') ADVANCE(33);
      if (lookahead == '^') ADVANCE(68);
      if (lookahead == '_') ADVANCE(68);
      if (lookahead == '`') ADVANCE(68);
      if (lookahead == 'v') ADVANCE(68);
      if (lookahead == '|') ADVANCE(68);
      if (lookahead == '~') ADVANCE(43);
      if (lookahead == '<' ||
          lookahead == '>') ADVANCE(68);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(68);
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(68);
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(68);
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(68);
      if (lookahead == 'W' ||
          lookahead == 'w') ADVANCE(68);
      if (('[' <= lookahead && lookahead <= ']')) ADVANCE(68);
      if (('{' <= lookahead && lookahead <= '}')) ADVANCE(68);
      if (lookahead == '$' ||
          ('B' <= lookahead && lookahead <= 'E') ||
          ('G' <= lookahead && lookahead <= 'L') ||
          lookahead == 'N' ||
          lookahead == 'O' ||
          ('R' <= lookahead && lookahead <= 'T') ||
          ('X' <= lookahead && lookahead <= 'o') ||
          ('r' <= lookahead && lookahead <= 't') ||
          ('x' <= lookahead && lookahead <= 'z')) ADVANCE(68);
      if (lookahead != 0) ADVANCE(68);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == '\r') ADVANCE(50);
      if (lookahead == '"') ADVANCE(67);
      if (lookahead == '#') ADVANCE(36);
      if (lookahead == '%') ADVANCE(44);
      if (lookahead == '&') ADVANCE(45);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == ',') ADVANCE(37);
      if (lookahead == ':') ADVANCE(41);
      if (lookahead == ';') ADVANCE(60);
      if (lookahead == '?') ADVANCE(63);
      if (lookahead == '@') ADVANCE(39);
      if (lookahead == 'N') ADVANCE(66);
      if (lookahead == 'V') ADVANCE(35);
      if (lookahead == '^') ADVANCE(48);
      if (lookahead == '_') ADVANCE(55);
      if (lookahead == '`') ADVANCE(61);
      if (lookahead == 'v') ADVANCE(34);
      if (lookahead == '|') ADVANCE(46);
      if (lookahead == '~') ADVANCE(43);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(7);
      if (lookahead == '<' ||
          lookahead == '>') ADVANCE(59);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(54);
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(47);
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(52);
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(51);
      if (lookahead == 'W' ||
          lookahead == 'w') ADVANCE(53);
      if (lookahead == '[' ||
          lookahead == ']') ADVANCE(58);
      if (('{' <= lookahead && lookahead <= '}')) ADVANCE(57);
      if (lookahead == '(' ||
          lookahead == ')') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (aux_sym_format_directive_type_token11_character_set_1(lookahead)) ADVANCE(65);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == '\r') ADVANCE(50);
      if (lookahead == '#') ADVANCE(9);
      if (lookahead == '%') ADVANCE(44);
      if (lookahead == '&') ADVANCE(45);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == ',') ADVANCE(37);
      if (lookahead == ':') ADVANCE(41);
      if (lookahead == ';') ADVANCE(60);
      if (lookahead == '?') ADVANCE(63);
      if (lookahead == '@') ADVANCE(39);
      if (lookahead == 'N') ADVANCE(66);
      if (lookahead == '^') ADVANCE(48);
      if (lookahead == '_') ADVANCE(55);
      if (lookahead == '`') ADVANCE(61);
      if (lookahead == '|') ADVANCE(46);
      if (lookahead == '~') ADVANCE(43);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(7);
      if (lookahead == '<' ||
          lookahead == '>') ADVANCE(59);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(54);
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(47);
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(52);
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(51);
      if (lookahead == 'W' ||
          lookahead == 'w') ADVANCE(53);
      if (lookahead == '[' ||
          lookahead == ']') ADVANCE(58);
      if (('{' <= lookahead && lookahead <= '}')) ADVANCE(57);
      if (lookahead == '(' ||
          lookahead == ')') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (aux_sym_format_directive_type_token11_character_set_2(lookahead)) ADVANCE(65);
      END_STATE();
    case 3:
      if (lookahead == '"') ADVANCE(67);
      if (lookahead == '\\') ADVANCE(19);
      if (lookahead == '~') ADVANCE(43);
      if (lookahead != 0) ADVANCE(68);
      END_STATE();
    case 4:
      if (lookahead == '#') ADVANCE(21);
      if (lookahead == '|') ADVANCE(5);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 5:
      if (lookahead == '#') ADVANCE(26);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 6:
      if (lookahead == '#') ADVANCE(9);
      if (lookahead == '%') ADVANCE(44);
      if (lookahead == '&') ADVANCE(45);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == '*') ADVANCE(62);
      if (lookahead == ',') ADVANCE(37);
      if (lookahead == ':') ADVANCE(41);
      if (lookahead == '@') ADVANCE(39);
      if (lookahead == '|') ADVANCE(46);
      if (lookahead == '~') ADVANCE(43);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(7);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (aux_sym_format_directive_type_token11_character_set_3(lookahead)) ADVANCE(65);
      END_STATE();
    case 7:
      if (lookahead == '#') ADVANCE(9);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      END_STATE();
    case 8:
      if (lookahead == '\\') ADVANCE(20);
      if (lookahead == 'b') ADVANCE(15);
      if (lookahead == 'f' ||
          lookahead == 't') ADVANCE(73);
      if (lookahead == 'x') ADVANCE(16);
      if (lookahead == '|') ADVANCE(4);
      END_STATE();
    case 9:
      if (lookahead == 'b') ADVANCE(15);
      if (lookahead == 'x') ADVANCE(16);
      END_STATE();
    case 10:
      if (lookahead == 'e') ADVANCE(64);
      END_STATE();
    case 11:
      if (lookahead == 'i') ADVANCE(13);
      END_STATE();
    case 12:
      if (lookahead == 'l') ADVANCE(11);
      END_STATE();
    case 13:
      if (lookahead == 'n') ADVANCE(10);
      END_STATE();
    case 14:
      if (lookahead == 'w') ADVANCE(12);
      END_STATE();
    case 15:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(28);
      END_STATE();
    case 16:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(30);
      END_STATE();
    case 17:
      if (!sym_kwd_lit_character_set_1(lookahead)) ADVANCE(31);
      END_STATE();
    case 18:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(33);
      END_STATE();
    case 19:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(69);
      END_STATE();
    case 20:
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(70);
      if (lookahead == '\\') ADVANCE(71);
      END_STATE();
    case 21:
      if (lookahead != 0 &&
          lookahead != '|') ADVANCE(4);
      END_STATE();
    case 22:
      if (eof) ADVANCE(23);
      if (lookahead == '"') ADVANCE(67);
      if (lookahead == '#') ADVANCE(8);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == '(') ADVANCE(83);
      if (lookahead == ')') ADVANCE(84);
      if (lookahead == ',') ADVANCE(38);
      if (lookahead == '/') ADVANCE(74);
      if (lookahead == ':') ADVANCE(17);
      if (lookahead == ';') ADVANCE(25);
      if (lookahead == '`') ADVANCE(61);
      if (lookahead == 'n') ADVANCE(79);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(75);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          (28 <= lookahead && lookahead <= ' ') ||
          lookahead == 5760 ||
          (8192 <= lookahead && lookahead <= 8198) ||
          (8200 <= lookahead && lookahead <= 8202) ||
          lookahead == 8232 ||
          lookahead == 8233 ||
          lookahead == 8287 ||
          lookahead == 12288) ADVANCE(24);
      if (lookahead != 0 &&
          lookahead != '@' &&
          (lookahead < '[' || '^' < lookahead) &&
          lookahead != '{' &&
          lookahead != '}' &&
          lookahead != '~') ADVANCE(82);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym__ws);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          (28 <= lookahead && lookahead <= ' ') ||
          lookahead == 5760 ||
          (8192 <= lookahead && lookahead <= 8198) ||
          (8200 <= lookahead && lookahead <= 8202) ||
          lookahead == 8232 ||
          lookahead == 8233 ||
          lookahead == 8287 ||
          lookahead == 12288) ADVANCE(24);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(25);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_block_comment);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (lookahead == '.') ADVANCE(29);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(29);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(sym_kwd_lit);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(aux_sym__format_token_token1);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_v);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_V);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_POUND);
      if (lookahead == 'b') ADVANCE(15);
      if (lookahead == 'x') ADVANCE(16);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_COMMA);
      if (lookahead == '@') ADVANCE(85);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_AT);
      if (lookahead == ':') ADVANCE(40);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_AT_COLON);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_COLON);
      if (lookahead == '@') ADVANCE(42);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_COLON_AT);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_PIPE);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token1);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token2);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_LF);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_CR);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token3);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token4);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token5);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token6);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym__);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token7);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token8);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token9);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token10);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_BQUOTE);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_QMARK);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_Newline);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token11);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token11);
      if (lookahead == 'e') ADVANCE(14);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(aux_sym_str_lit_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\' &&
          lookahead != '~') ADVANCE(68);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(aux_sym_str_lit_token2);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(sym_char_lit);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(sym_char_lit);
      if (lookahead == 'n' ||
          lookahead == 's' ||
          lookahead == 't') ADVANCE(70);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(sym_null_lit);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(82);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_bool_lit);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(76);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(82);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'b') ADVANCE(80);
      if (lookahead == 'x') ADVANCE(81);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(82);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'e') ADVANCE(72);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(82);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'n') ADVANCE(77);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(82);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'o') ADVANCE(78);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(82);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(28);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(82);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_2(lookahead)) ADVANCE(82);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(30);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(82);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(anon_sym_COMMA_AT);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 22},
  [2] = {.lex_state = 1},
  [3] = {.lex_state = 1},
  [4] = {.lex_state = 1},
  [5] = {.lex_state = 2},
  [6] = {.lex_state = 22},
  [7] = {.lex_state = 22},
  [8] = {.lex_state = 2},
  [9] = {.lex_state = 22},
  [10] = {.lex_state = 22},
  [11] = {.lex_state = 22},
  [12] = {.lex_state = 22},
  [13] = {.lex_state = 2},
  [14] = {.lex_state = 22},
  [15] = {.lex_state = 22},
  [16] = {.lex_state = 22},
  [17] = {.lex_state = 22},
  [18] = {.lex_state = 2},
  [19] = {.lex_state = 22},
  [20] = {.lex_state = 22},
  [21] = {.lex_state = 22},
  [22] = {.lex_state = 2},
  [23] = {.lex_state = 2},
  [24] = {.lex_state = 22},
  [25] = {.lex_state = 22},
  [26] = {.lex_state = 22},
  [27] = {.lex_state = 22},
  [28] = {.lex_state = 22},
  [29] = {.lex_state = 22},
  [30] = {.lex_state = 22},
  [31] = {.lex_state = 22},
  [32] = {.lex_state = 22},
  [33] = {.lex_state = 22},
  [34] = {.lex_state = 22},
  [35] = {.lex_state = 22},
  [36] = {.lex_state = 22},
  [37] = {.lex_state = 22},
  [38] = {.lex_state = 22},
  [39] = {.lex_state = 22},
  [40] = {.lex_state = 22},
  [41] = {.lex_state = 22},
  [42] = {.lex_state = 6},
  [43] = {.lex_state = 6},
  [44] = {.lex_state = 6},
  [45] = {.lex_state = 6},
  [46] = {.lex_state = 6},
  [47] = {.lex_state = 3},
  [48] = {.lex_state = 3},
  [49] = {.lex_state = 3},
  [50] = {.lex_state = 6},
  [51] = {.lex_state = 3},
  [52] = {.lex_state = 3},
  [53] = {.lex_state = 3},
  [54] = {.lex_state = 3},
  [55] = {.lex_state = 3},
  [56] = {.lex_state = 3},
  [57] = {.lex_state = 3},
  [58] = {.lex_state = 0},
  [59] = {.lex_state = 18},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [aux_sym__format_token_token1] = ACTIONS(1),
    [anon_sym_v] = ACTIONS(1),
    [anon_sym_V] = ACTIONS(1),
    [anon_sym_POUND] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_AT] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_PERCENT] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [aux_sym_format_directive_type_token1] = ACTIONS(1),
    [aux_sym_format_directive_type_token2] = ACTIONS(1),
    [anon_sym_LF] = ACTIONS(1),
    [anon_sym_CR] = ACTIONS(1),
    [aux_sym_format_directive_type_token3] = ACTIONS(1),
    [aux_sym_format_directive_type_token4] = ACTIONS(1),
    [aux_sym_format_directive_type_token5] = ACTIONS(1),
    [aux_sym_format_directive_type_token6] = ACTIONS(1),
    [anon_sym__] = ACTIONS(1),
    [aux_sym_format_directive_type_token7] = ACTIONS(1),
    [aux_sym_format_directive_type_token8] = ACTIONS(1),
    [aux_sym_format_directive_type_token9] = ACTIONS(1),
    [aux_sym_format_directive_type_token10] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_BQUOTE] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_QMARK] = ACTIONS(1),
    [aux_sym_format_directive_type_token11] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [aux_sym_str_lit_token1] = ACTIONS(1),
    [sym_char_lit] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
  },
  [1] = {
    [sym_source] = STATE(58),
    [sym__gap] = STATE(6),
    [sym__form] = STATE(6),
    [sym_num_lit] = STATE(6),
    [sym_str_lit] = STATE(6),
    [sym_sym_lit] = STATE(6),
    [sym_list_lit] = STATE(6),
    [sym__bare_list_lit] = STATE(33),
    [sym_quoting_lit] = STATE(6),
    [sym_quasi_quoting_lit] = STATE(6),
    [sym_unquote_splicing_lit] = STATE(6),
    [sym_unquoting_lit] = STATE(6),
    [aux_sym_source_repeat1] = STATE(6),
    [ts_builtin_sym_end] = ACTIONS(3),
    [sym__ws] = ACTIONS(5),
    [sym_comment] = ACTIONS(5),
    [sym_block_comment] = ACTIONS(5),
    [aux_sym_num_lit_token1] = ACTIONS(7),
    [sym_kwd_lit] = ACTIONS(5),
    [anon_sym_SQUOTE] = ACTIONS(9),
    [anon_sym_COMMA] = ACTIONS(11),
    [anon_sym_BQUOTE] = ACTIONS(13),
    [anon_sym_DQUOTE] = ACTIONS(15),
    [sym_char_lit] = ACTIONS(5),
    [sym_null_lit] = ACTIONS(17),
    [sym_bool_lit] = ACTIONS(5),
    [anon_sym_SLASH] = ACTIONS(19),
    [aux_sym__sym_unqualified_token1] = ACTIONS(21),
    [anon_sym_LPAREN] = ACTIONS(23),
    [anon_sym_COMMA_AT] = ACTIONS(25),
  },
  [2] = {
    [sym__format_token] = STATE(42),
    [sym_format_prefix_parameters] = STATE(5),
    [sym_format_modifiers] = STATE(13),
    [sym_format_directive_type] = STATE(55),
    [aux_sym_format_modifiers_repeat1] = STATE(46),
    [aux_sym_num_lit_token1] = ACTIONS(27),
    [anon_sym_SQUOTE] = ACTIONS(29),
    [anon_sym_v] = ACTIONS(31),
    [anon_sym_V] = ACTIONS(31),
    [anon_sym_POUND] = ACTIONS(33),
    [anon_sym_COMMA] = ACTIONS(35),
    [anon_sym_AT] = ACTIONS(37),
    [anon_sym_AT_COLON] = ACTIONS(39),
    [anon_sym_COLON] = ACTIONS(37),
    [anon_sym_COLON_AT] = ACTIONS(39),
    [anon_sym_TILDE] = ACTIONS(41),
    [anon_sym_PERCENT] = ACTIONS(41),
    [anon_sym_AMP] = ACTIONS(41),
    [anon_sym_PIPE] = ACTIONS(41),
    [aux_sym_format_directive_type_token1] = ACTIONS(41),
    [aux_sym_format_directive_type_token2] = ACTIONS(41),
    [anon_sym_LF] = ACTIONS(41),
    [anon_sym_CR] = ACTIONS(41),
    [aux_sym_format_directive_type_token3] = ACTIONS(41),
    [aux_sym_format_directive_type_token4] = ACTIONS(41),
    [aux_sym_format_directive_type_token5] = ACTIONS(41),
    [aux_sym_format_directive_type_token6] = ACTIONS(41),
    [anon_sym__] = ACTIONS(41),
    [aux_sym_format_directive_type_token7] = ACTIONS(41),
    [aux_sym_format_directive_type_token8] = ACTIONS(41),
    [aux_sym_format_directive_type_token9] = ACTIONS(41),
    [aux_sym_format_directive_type_token10] = ACTIONS(41),
    [anon_sym_SEMI] = ACTIONS(41),
    [anon_sym_BQUOTE] = ACTIONS(41),
    [anon_sym_QMARK] = ACTIONS(41),
    [anon_sym_Newline] = ACTIONS(41),
    [aux_sym_format_directive_type_token11] = ACTIONS(43),
    [anon_sym_DQUOTE] = ACTIONS(45),
  },
  [3] = {
    [sym__format_token] = STATE(42),
    [sym_format_prefix_parameters] = STATE(5),
    [sym_format_modifiers] = STATE(13),
    [sym_format_directive_type] = STATE(55),
    [aux_sym_format_modifiers_repeat1] = STATE(46),
    [aux_sym_num_lit_token1] = ACTIONS(27),
    [anon_sym_SQUOTE] = ACTIONS(29),
    [anon_sym_v] = ACTIONS(31),
    [anon_sym_V] = ACTIONS(31),
    [anon_sym_POUND] = ACTIONS(33),
    [anon_sym_COMMA] = ACTIONS(35),
    [anon_sym_AT] = ACTIONS(37),
    [anon_sym_AT_COLON] = ACTIONS(39),
    [anon_sym_COLON] = ACTIONS(37),
    [anon_sym_COLON_AT] = ACTIONS(39),
    [anon_sym_TILDE] = ACTIONS(41),
    [anon_sym_PERCENT] = ACTIONS(41),
    [anon_sym_AMP] = ACTIONS(41),
    [anon_sym_PIPE] = ACTIONS(41),
    [aux_sym_format_directive_type_token1] = ACTIONS(41),
    [aux_sym_format_directive_type_token2] = ACTIONS(41),
    [anon_sym_LF] = ACTIONS(41),
    [anon_sym_CR] = ACTIONS(41),
    [aux_sym_format_directive_type_token3] = ACTIONS(41),
    [aux_sym_format_directive_type_token4] = ACTIONS(41),
    [aux_sym_format_directive_type_token5] = ACTIONS(41),
    [aux_sym_format_directive_type_token6] = ACTIONS(41),
    [anon_sym__] = ACTIONS(41),
    [aux_sym_format_directive_type_token7] = ACTIONS(41),
    [aux_sym_format_directive_type_token8] = ACTIONS(41),
    [aux_sym_format_directive_type_token9] = ACTIONS(41),
    [aux_sym_format_directive_type_token10] = ACTIONS(41),
    [anon_sym_SEMI] = ACTIONS(41),
    [anon_sym_BQUOTE] = ACTIONS(41),
    [anon_sym_QMARK] = ACTIONS(41),
    [anon_sym_Newline] = ACTIONS(41),
    [aux_sym_format_directive_type_token11] = ACTIONS(43),
    [anon_sym_DQUOTE] = ACTIONS(47),
  },
  [4] = {
    [sym__format_token] = STATE(42),
    [sym_format_prefix_parameters] = STATE(5),
    [sym_format_modifiers] = STATE(13),
    [sym_format_directive_type] = STATE(55),
    [aux_sym_format_modifiers_repeat1] = STATE(46),
    [aux_sym_num_lit_token1] = ACTIONS(27),
    [anon_sym_SQUOTE] = ACTIONS(29),
    [anon_sym_v] = ACTIONS(31),
    [anon_sym_V] = ACTIONS(31),
    [anon_sym_POUND] = ACTIONS(33),
    [anon_sym_COMMA] = ACTIONS(35),
    [anon_sym_AT] = ACTIONS(37),
    [anon_sym_AT_COLON] = ACTIONS(39),
    [anon_sym_COLON] = ACTIONS(37),
    [anon_sym_COLON_AT] = ACTIONS(39),
    [anon_sym_TILDE] = ACTIONS(41),
    [anon_sym_PERCENT] = ACTIONS(41),
    [anon_sym_AMP] = ACTIONS(41),
    [anon_sym_PIPE] = ACTIONS(41),
    [aux_sym_format_directive_type_token1] = ACTIONS(41),
    [aux_sym_format_directive_type_token2] = ACTIONS(41),
    [anon_sym_LF] = ACTIONS(41),
    [anon_sym_CR] = ACTIONS(41),
    [aux_sym_format_directive_type_token3] = ACTIONS(41),
    [aux_sym_format_directive_type_token4] = ACTIONS(41),
    [aux_sym_format_directive_type_token5] = ACTIONS(41),
    [aux_sym_format_directive_type_token6] = ACTIONS(41),
    [anon_sym__] = ACTIONS(41),
    [aux_sym_format_directive_type_token7] = ACTIONS(41),
    [aux_sym_format_directive_type_token8] = ACTIONS(41),
    [aux_sym_format_directive_type_token9] = ACTIONS(41),
    [aux_sym_format_directive_type_token10] = ACTIONS(41),
    [anon_sym_SEMI] = ACTIONS(41),
    [anon_sym_BQUOTE] = ACTIONS(41),
    [anon_sym_QMARK] = ACTIONS(41),
    [anon_sym_Newline] = ACTIONS(41),
    [aux_sym_format_directive_type_token11] = ACTIONS(43),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 11,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(35), 1,
      anon_sym_COMMA,
    ACTIONS(43), 1,
      aux_sym_format_directive_type_token11,
    STATE(18), 1,
      sym_format_modifiers,
    STATE(42), 1,
      sym__format_token,
    STATE(46), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(52), 1,
      sym_format_directive_type,
    ACTIONS(37), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(39), 2,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
    ACTIONS(41), 21,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      aux_sym_format_directive_type_token1,
      aux_sym_format_directive_type_token2,
      anon_sym_LF,
      anon_sym_CR,
      aux_sym_format_directive_type_token3,
      aux_sym_format_directive_type_token4,
      aux_sym_format_directive_type_token5,
      aux_sym_format_directive_type_token6,
      anon_sym__,
      aux_sym_format_directive_type_token7,
      aux_sym_format_directive_type_token8,
      aux_sym_format_directive_type_token9,
      aux_sym_format_directive_type_token10,
      anon_sym_SEMI,
      anon_sym_BQUOTE,
      anon_sym_QMARK,
      anon_sym_Newline,
  [56] = 14,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(49), 1,
      ts_builtin_sym_end,
    ACTIONS(53), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(51), 6,
      sym__ws,
      sym_comment,
      sym_block_comment,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(7), 11,
      sym__gap,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
      aux_sym_source_repeat1,
  [114] = 14,
    ACTIONS(55), 1,
      ts_builtin_sym_end,
    ACTIONS(60), 1,
      aux_sym_num_lit_token1,
    ACTIONS(63), 1,
      anon_sym_SQUOTE,
    ACTIONS(66), 1,
      anon_sym_COMMA,
    ACTIONS(69), 1,
      anon_sym_BQUOTE,
    ACTIONS(72), 1,
      anon_sym_DQUOTE,
    ACTIONS(75), 1,
      sym_null_lit,
    ACTIONS(78), 1,
      anon_sym_SLASH,
    ACTIONS(81), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(84), 1,
      anon_sym_LPAREN,
    ACTIONS(87), 1,
      anon_sym_COMMA_AT,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(57), 6,
      sym__ws,
      sym_comment,
      sym_block_comment,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(7), 11,
      sym__gap,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
      aux_sym_source_repeat1,
  [172] = 2,
    ACTIONS(92), 3,
      anon_sym_AT,
      anon_sym_COLON,
      aux_sym_format_directive_type_token11,
    ACTIONS(90), 26,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      aux_sym_format_directive_type_token1,
      aux_sym_format_directive_type_token2,
      anon_sym_LF,
      anon_sym_CR,
      aux_sym_format_directive_type_token3,
      aux_sym_format_directive_type_token4,
      aux_sym_format_directive_type_token5,
      aux_sym_format_directive_type_token6,
      anon_sym__,
      aux_sym_format_directive_type_token7,
      aux_sym_format_directive_type_token8,
      aux_sym_format_directive_type_token9,
      aux_sym_format_directive_type_token10,
      anon_sym_SEMI,
      anon_sym_BQUOTE,
      anon_sym_QMARK,
      anon_sym_Newline,
  [206] = 16,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(98), 1,
      sym_null_lit,
    ACTIONS(100), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(11), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    ACTIONS(94), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(96), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(41), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [268] = 16,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(98), 1,
      sym_null_lit,
    ACTIONS(104), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(9), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    ACTIONS(96), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    ACTIONS(102), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(41), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [330] = 16,
    ACTIONS(109), 1,
      aux_sym_num_lit_token1,
    ACTIONS(115), 1,
      anon_sym_SQUOTE,
    ACTIONS(118), 1,
      anon_sym_COMMA,
    ACTIONS(121), 1,
      anon_sym_BQUOTE,
    ACTIONS(124), 1,
      anon_sym_DQUOTE,
    ACTIONS(127), 1,
      sym_null_lit,
    ACTIONS(130), 1,
      anon_sym_SLASH,
    ACTIONS(133), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(136), 1,
      anon_sym_LPAREN,
    ACTIONS(139), 1,
      anon_sym_RPAREN,
    ACTIONS(141), 1,
      anon_sym_COMMA_AT,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(11), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    ACTIONS(106), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(112), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(41), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [392] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(148), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(144), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(146), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(31), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [451] = 8,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(43), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(150), 1,
      anon_sym_COMMA,
    STATE(42), 1,
      sym__format_token,
    STATE(50), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(52), 1,
      sym_format_directive_type,
    ACTIONS(41), 21,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      aux_sym_format_directive_type_token1,
      aux_sym_format_directive_type_token2,
      anon_sym_LF,
      anon_sym_CR,
      aux_sym_format_directive_type_token3,
      aux_sym_format_directive_type_token4,
      aux_sym_format_directive_type_token5,
      aux_sym_format_directive_type_token6,
      anon_sym__,
      aux_sym_format_directive_type_token7,
      aux_sym_format_directive_type_token8,
      aux_sym_format_directive_type_token9,
      aux_sym_format_directive_type_token10,
      anon_sym_SEMI,
      anon_sym_BQUOTE,
      anon_sym_QMARK,
      anon_sym_Newline,
  [496] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(154), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(144), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(152), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(28), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [555] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(160), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(14), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(156), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(158), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(38), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [614] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(164), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(144), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(162), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(24), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [673] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(170), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(21), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(166), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(168), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(27), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [732] = 8,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(43), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(150), 1,
      anon_sym_COMMA,
    STATE(42), 1,
      sym__format_token,
    STATE(50), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(57), 1,
      sym_format_directive_type,
    ACTIONS(41), 21,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      aux_sym_format_directive_type_token1,
      aux_sym_format_directive_type_token2,
      anon_sym_LF,
      anon_sym_CR,
      aux_sym_format_directive_type_token3,
      aux_sym_format_directive_type_token4,
      aux_sym_format_directive_type_token5,
      aux_sym_format_directive_type_token6,
      anon_sym__,
      aux_sym_format_directive_type_token7,
      aux_sym_format_directive_type_token8,
      aux_sym_format_directive_type_token9,
      aux_sym_format_directive_type_token10,
      anon_sym_SEMI,
      anon_sym_BQUOTE,
      anon_sym_QMARK,
      anon_sym_Newline,
  [777] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(176), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(12), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(172), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(174), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(39), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [836] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(182), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(16), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(178), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(180), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(40), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [895] = 15,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_SQUOTE,
    ACTIONS(11), 1,
      anon_sym_COMMA,
    ACTIONS(13), 1,
      anon_sym_BQUOTE,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(186), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(144), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(184), 3,
      sym_kwd_lit,
      sym_char_lit,
      sym_bool_lit,
    STATE(32), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [954] = 2,
    ACTIONS(190), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(188), 24,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      aux_sym_format_directive_type_token1,
      aux_sym_format_directive_type_token2,
      anon_sym_LF,
      anon_sym_CR,
      aux_sym_format_directive_type_token3,
      aux_sym_format_directive_type_token4,
      aux_sym_format_directive_type_token5,
      aux_sym_format_directive_type_token6,
      anon_sym__,
      aux_sym_format_directive_type_token7,
      aux_sym_format_directive_type_token8,
      aux_sym_format_directive_type_token9,
      aux_sym_format_directive_type_token10,
      anon_sym_SEMI,
      anon_sym_BQUOTE,
      anon_sym_QMARK,
      anon_sym_Newline,
  [984] = 2,
    ACTIONS(194), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(192), 24,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      aux_sym_format_directive_type_token1,
      aux_sym_format_directive_type_token2,
      anon_sym_LF,
      anon_sym_CR,
      aux_sym_format_directive_type_token3,
      aux_sym_format_directive_type_token4,
      aux_sym_format_directive_type_token5,
      aux_sym_format_directive_type_token6,
      anon_sym__,
      aux_sym_format_directive_type_token7,
      aux_sym_format_directive_type_token8,
      aux_sym_format_directive_type_token9,
      aux_sym_format_directive_type_token10,
      anon_sym_SEMI,
      anon_sym_BQUOTE,
      anon_sym_QMARK,
      anon_sym_Newline,
  [1014] = 2,
    ACTIONS(198), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(196), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1037] = 2,
    ACTIONS(202), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(200), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1060] = 2,
    ACTIONS(206), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(204), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1083] = 2,
    ACTIONS(210), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(208), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1106] = 2,
    ACTIONS(214), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(212), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1129] = 4,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(216), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(221), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(219), 10,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_COMMA_AT,
  [1156] = 2,
    ACTIONS(225), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(223), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1179] = 2,
    ACTIONS(229), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(227), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1202] = 2,
    ACTIONS(233), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(231), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1225] = 2,
    ACTIONS(237), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(235), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1248] = 2,
    ACTIONS(241), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(239), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1271] = 2,
    ACTIONS(245), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(243), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1294] = 2,
    ACTIONS(249), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(247), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1317] = 2,
    ACTIONS(253), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(251), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1340] = 2,
    ACTIONS(257), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(255), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1363] = 2,
    ACTIONS(261), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(259), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1386] = 2,
    ACTIONS(265), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(263), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1409] = 2,
    ACTIONS(269), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(267), 14,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1431] = 4,
    ACTIONS(277), 1,
      anon_sym_STAR,
    ACTIONS(273), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(275), 4,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(271), 6,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1453] = 2,
    ACTIONS(281), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(279), 11,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      anon_sym_STAR,
      aux_sym_format_directive_type_token11,
  [1471] = 2,
    ACTIONS(285), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(283), 11,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
      anon_sym_STAR,
      aux_sym_format_directive_type_token11,
  [1489] = 6,
    ACTIONS(287), 1,
      aux_sym_num_lit_token1,
    ACTIONS(290), 1,
      anon_sym_SQUOTE,
    ACTIONS(293), 1,
      anon_sym_COMMA,
    ACTIONS(296), 2,
      anon_sym_AT,
      anon_sym_COLON,
    STATE(45), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
    ACTIONS(298), 3,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1512] = 7,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(300), 1,
      anon_sym_COMMA,
    ACTIONS(306), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(302), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(304), 2,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
    STATE(45), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1537] = 4,
    ACTIONS(308), 1,
      anon_sym_TILDE,
    ACTIONS(311), 1,
      anon_sym_DQUOTE,
    ACTIONS(313), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(47), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1552] = 4,
    ACTIONS(45), 1,
      anon_sym_DQUOTE,
    ACTIONS(316), 1,
      anon_sym_TILDE,
    ACTIONS(318), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(47), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1567] = 4,
    ACTIONS(320), 1,
      anon_sym_TILDE,
    ACTIONS(322), 1,
      anon_sym_DQUOTE,
    ACTIONS(324), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(48), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1582] = 5,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(300), 1,
      anon_sym_COMMA,
    ACTIONS(306), 1,
      aux_sym_format_directive_type_token11,
    STATE(45), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1599] = 1,
    ACTIONS(326), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1606] = 1,
    ACTIONS(328), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1613] = 1,
    ACTIONS(330), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1620] = 1,
    ACTIONS(332), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1627] = 1,
    ACTIONS(334), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1634] = 1,
    ACTIONS(336), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1641] = 1,
    ACTIONS(338), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1648] = 1,
    ACTIONS(340), 1,
      ts_builtin_sym_end,
  [1652] = 1,
    ACTIONS(342), 1,
      aux_sym__format_token_token1,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(5)] = 0,
  [SMALL_STATE(6)] = 56,
  [SMALL_STATE(7)] = 114,
  [SMALL_STATE(8)] = 172,
  [SMALL_STATE(9)] = 206,
  [SMALL_STATE(10)] = 268,
  [SMALL_STATE(11)] = 330,
  [SMALL_STATE(12)] = 392,
  [SMALL_STATE(13)] = 451,
  [SMALL_STATE(14)] = 496,
  [SMALL_STATE(15)] = 555,
  [SMALL_STATE(16)] = 614,
  [SMALL_STATE(17)] = 673,
  [SMALL_STATE(18)] = 732,
  [SMALL_STATE(19)] = 777,
  [SMALL_STATE(20)] = 836,
  [SMALL_STATE(21)] = 895,
  [SMALL_STATE(22)] = 954,
  [SMALL_STATE(23)] = 984,
  [SMALL_STATE(24)] = 1014,
  [SMALL_STATE(25)] = 1037,
  [SMALL_STATE(26)] = 1060,
  [SMALL_STATE(27)] = 1083,
  [SMALL_STATE(28)] = 1106,
  [SMALL_STATE(29)] = 1129,
  [SMALL_STATE(30)] = 1156,
  [SMALL_STATE(31)] = 1179,
  [SMALL_STATE(32)] = 1202,
  [SMALL_STATE(33)] = 1225,
  [SMALL_STATE(34)] = 1248,
  [SMALL_STATE(35)] = 1271,
  [SMALL_STATE(36)] = 1294,
  [SMALL_STATE(37)] = 1317,
  [SMALL_STATE(38)] = 1340,
  [SMALL_STATE(39)] = 1363,
  [SMALL_STATE(40)] = 1386,
  [SMALL_STATE(41)] = 1409,
  [SMALL_STATE(42)] = 1431,
  [SMALL_STATE(43)] = 1453,
  [SMALL_STATE(44)] = 1471,
  [SMALL_STATE(45)] = 1489,
  [SMALL_STATE(46)] = 1512,
  [SMALL_STATE(47)] = 1537,
  [SMALL_STATE(48)] = 1552,
  [SMALL_STATE(49)] = 1567,
  [SMALL_STATE(50)] = 1582,
  [SMALL_STATE(51)] = 1599,
  [SMALL_STATE(52)] = 1606,
  [SMALL_STATE(53)] = 1613,
  [SMALL_STATE(54)] = 1620,
  [SMALL_STATE(55)] = 1627,
  [SMALL_STATE(56)] = 1634,
  [SMALL_STATE(57)] = 1641,
  [SMALL_STATE(58)] = 1648,
  [SMALL_STATE(59)] = 1652,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [43] = {.entry = {.count = 1, .reusable = false}}, SHIFT(56),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 1),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [53] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2),
  [57] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(7),
  [60] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(30),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(15),
  [66] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(20),
  [69] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(19),
  [72] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(49),
  [75] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(7),
  [78] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(37),
  [81] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(37),
  [84] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(10),
  [87] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(17),
  [90] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_prefix_parameters, 1),
  [92] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_prefix_parameters, 1),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [98] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [106] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(11),
  [109] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(30),
  [112] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(41),
  [115] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(15),
  [118] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(20),
  [121] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(19),
  [124] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(49),
  [127] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(41),
  [130] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(37),
  [133] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(37),
  [136] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(10),
  [139] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9),
  [141] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(17),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [146] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [148] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [154] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [160] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [162] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [164] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [166] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [170] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [176] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [182] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [186] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [188] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 1),
  [190] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_modifiers, 1),
  [192] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 2),
  [194] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_modifiers, 2),
  [196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 3, .production_id = 6),
  [198] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 3, .production_id = 6),
  [200] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 3),
  [202] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 3),
  [204] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 2, .production_id = 4),
  [206] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 2, .production_id = 4),
  [208] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 3),
  [210] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 3),
  [212] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 3, .production_id = 6),
  [214] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 3, .production_id = 6),
  [216] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(29),
  [219] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [221] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [223] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_num_lit, 1),
  [225] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_num_lit, 1),
  [227] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 6),
  [229] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 6),
  [231] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 6),
  [233] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 6),
  [235] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [237] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [239] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 3, .production_id = 8),
  [241] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 3, .production_id = 8),
  [243] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 2),
  [245] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 2),
  [247] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 4),
  [249] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 4),
  [251] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [253] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [255] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 2, .production_id = 3),
  [257] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 2, .production_id = 3),
  [259] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 3),
  [261] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 3),
  [263] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 2, .production_id = 3),
  [265] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 2, .production_id = 3),
  [267] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 5),
  [269] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 5),
  [271] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [273] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [275] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [277] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [279] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 1, .production_id = 7),
  [281] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 1, .production_id = 7),
  [283] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 2),
  [285] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 2),
  [287] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(43),
  [290] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(59),
  [293] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(45),
  [296] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [298] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [300] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [302] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [304] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [306] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [308] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(4),
  [311] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2),
  [313] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(47),
  [316] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [318] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [320] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [322] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [324] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [326] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 10),
  [328] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 3),
  [330] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 11),
  [332] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2),
  [334] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 2),
  [336] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 1),
  [338] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 4),
  [340] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [342] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_opengoal() {
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
