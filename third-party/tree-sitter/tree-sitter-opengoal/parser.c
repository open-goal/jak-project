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

static inline bool sym_block_comment_character_set_1(int32_t c) {
  return (c < '['
    ? (c < ','
      ? (c < '"'
        ? (c < 28
          ? (c >= '\t' && c <= '\r')
          : c <= ' ')
        : (c <= '"' || (c >= '(' && c <= ')')))
      : (c <= ',' || (c < ';'
        ? c == '/'
        : (c <= ';' || c == '@'))))
    : (c <= '^' || (c < 8200
      ? (c < 5760
        ? (c < '{'
          ? c == '`'
          : c <= '}')
        : (c <= 5760 || (c >= 8192 && c <= 8198)))
      : (c <= 8202 || (c < 8287
        ? (c >= 8232 && c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool sym_block_comment_character_set_2(int32_t c) {
  return (c < '`'
    ? (c < ','
      ? (c < '"'
        ? (c < 28
          ? (c >= '\t' && c <= '\r')
          : c <= ' ')
        : (c <= '"' || (c >= '(' && c <= ')')))
      : (c <= ',' || (c < '@'
        ? (c < ';'
          ? c == '/'
          : c <= ';')
        : (c <= '@' || (c >= '[' && c <= '^')))))
    : (c <= '`' || (c < 8200
      ? (c < 5760
        ? (c < '}'
          ? c == '{'
          : c <= '}')
        : (c <= 5760 || (c >= 8192 && c <= 8198)))
      : (c <= 8202 || (c < 8287
        ? (c >= 8232 && c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

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
  return (c < 'V'
    ? (c < 'D'
      ? (c < 'B'
        ? c == '$'
        : c <= 'B')
      : (c <= 'H' || (c < 'R'
        ? (c >= 'J' && c <= 'O')
        : c <= 'T')))
    : (c <= 'Z' || (c < 'j'
      ? (c < 'd'
        ? c == 'b'
        : c <= 'h')
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
          : c <= '/')
        : (c <= ';' || c == '@'))))
    : (c <= '^' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == '`'
          : c <= '{')
        : (c <= '}' || c == 5760))
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
        : (c <= ';' || c == '@'))))
    : (c <= '^' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == '`'
          : c <= '{')
        : (c <= '}' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_3(int32_t c) {
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
    : (c <= '^' || (c < 8200
      ? (c < 5760
        ? (c < '{'
          ? c == '`'
          : c <= '}')
        : (c <= 5760 || (c >= 8192 && c <= 8198)))
      : (c <= 8202 || (c < 8287
        ? (c >= 8232 && c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_4(int32_t c) {
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
        : (c <= '}' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_5(int32_t c) {
  return (c < 'b'
    ? (c < ','
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || (c < '('
          ? c == '"'
          : c <= ')')))
      : (c <= ',' || (c < '@'
        ? (c < ';'
          ? c == '/'
          : c <= ';')
        : (c <= '@' || (c < '`'
          ? (c >= '[' && c <= '^')
          : c <= '`')))))
    : (c <= 'b' || (c < 8192
      ? (c < 'x'
        ? (c < 't'
          ? c == 'f'
          : c <= 't')
        : (c <= 'x' || (c < 5760
          ? (c >= '{' && c <= '}')
          : c <= 5760)))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_6(int32_t c) {
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
          : c <= '1')
        : (c <= ';' || c == '@'))))
    : (c <= '^' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == '`'
          : c <= '{')
        : (c <= '}' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_7(int32_t c) {
  return (c < '`'
    ? (c < ','
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || (c < '('
          ? c == '"'
          : c <= ')')))
      : (c <= ',' || (c < '@'
        ? (c < ';'
          ? c == '/'
          : c <= ';')
        : (c <= '@' || (c >= '[' && c <= '^')))))
    : (c <= '`' || (c < 8192
      ? (c < '{'
        ? (c < 'x'
          ? c == 'b'
          : c <= 'x')
        : (c <= '{' || (c < 5760
          ? c == '}'
          : c <= 5760)))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_8(int32_t c) {
  return (c < '`'
    ? (c < ','
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || (c < '('
          ? c == '"'
          : c <= ')')))
      : (c <= ',' || (c < '@'
        ? (c < ';'
          ? c == '/'
          : c <= ';')
        : (c <= '@' || (c >= '[' && c <= '^')))))
    : (c <= '`' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == 'e'
          : c <= '{')
        : (c <= '}' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_9(int32_t c) {
  return (c < '`'
    ? (c < ','
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || (c < '('
          ? c == '"'
          : c <= ')')))
      : (c <= ',' || (c < '@'
        ? (c < ';'
          ? c == '/'
          : c <= ';')
        : (c <= '@' || (c >= '[' && c <= '^')))))
    : (c <= '`' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == 'n'
          : c <= '{')
        : (c <= '}' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_10(int32_t c) {
  return (c < '`'
    ? (c < ','
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || (c < '('
          ? c == '"'
          : c <= ')')))
      : (c <= ',' || (c < '@'
        ? (c < ';'
          ? c == '/'
          : c <= ';')
        : (c <= '@' || (c >= '[' && c <= '^')))))
    : (c <= '`' || (c < 8192
      ? (c < '}'
        ? (c < '{'
          ? c == 'o'
          : c <= '{')
        : (c <= '}' || c == 5760))
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
      if (eof) ADVANCE(22);
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
          ('B' <= lookahead && lookahead <= 'O') ||
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
      if (lookahead == '$' ||
          ('B' <= lookahead && lookahead <= 'O') ||
          ('R' <= lookahead && lookahead <= 'T') ||
          ('X' <= lookahead && lookahead <= 'Z') ||
          ('b' <= lookahead && lookahead <= 'o') ||
          ('r' <= lookahead && lookahead <= 't') ||
          ('x' <= lookahead && lookahead <= 'z')) ADVANCE(65);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == '\r') ADVANCE(50);
      if (lookahead == '#') ADVANCE(8);
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
      if (lookahead == '$' ||
          ('B' <= lookahead && lookahead <= 'O') ||
          ('R' <= lookahead && lookahead <= 'T') ||
          ('V' <= lookahead && lookahead <= 'Z') ||
          ('b' <= lookahead && lookahead <= 'o') ||
          ('r' <= lookahead && lookahead <= 't') ||
          ('x' <= lookahead && lookahead <= 'z')) ADVANCE(65);
      END_STATE();
    case 3:
      if (lookahead == '"') ADVANCE(67);
      if (lookahead == '\\') ADVANCE(18);
      if (lookahead == '~') ADVANCE(43);
      if (lookahead != 0) ADVANCE(68);
      END_STATE();
    case 4:
      if (lookahead == '#') ADVANCE(20);
      if (lookahead == '|') ADVANCE(5);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 5:
      if (lookahead == '#') ADVANCE(25);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 6:
      if (lookahead == '#') ADVANCE(8);
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
      if (aux_sym_format_directive_type_token11_character_set_1(lookahead)) ADVANCE(65);
      END_STATE();
    case 7:
      if (lookahead == '#') ADVANCE(8);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      END_STATE();
    case 8:
      if (lookahead == 'b') ADVANCE(14);
      if (lookahead == 'x') ADVANCE(15);
      END_STATE();
    case 9:
      if (lookahead == 'e') ADVANCE(64);
      END_STATE();
    case 10:
      if (lookahead == 'i') ADVANCE(12);
      END_STATE();
    case 11:
      if (lookahead == 'l') ADVANCE(10);
      END_STATE();
    case 12:
      if (lookahead == 'n') ADVANCE(9);
      END_STATE();
    case 13:
      if (lookahead == 'w') ADVANCE(11);
      END_STATE();
    case 14:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(28);
      END_STATE();
    case 15:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(30);
      END_STATE();
    case 16:
      if (!sym_kwd_lit_character_set_1(lookahead)) ADVANCE(31);
      END_STATE();
    case 17:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(33);
      END_STATE();
    case 18:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(69);
      END_STATE();
    case 19:
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(70);
      if (lookahead == '\\') ADVANCE(71);
      END_STATE();
    case 20:
      if (lookahead != 0 &&
          lookahead != '|') ADVANCE(4);
      END_STATE();
    case 21:
      if (eof) ADVANCE(22);
      if (lookahead == '"') ADVANCE(67);
      if (lookahead == '#') ADVANCE(79);
      if (lookahead == '\'') ADVANCE(32);
      if (lookahead == '(') ADVANCE(87);
      if (lookahead == ')') ADVANCE(88);
      if (lookahead == ',') ADVANCE(38);
      if (lookahead == '/') ADVANCE(74);
      if (lookahead == ':') ADVANCE(16);
      if (lookahead == ';') ADVANCE(24);
      if (lookahead == '`') ADVANCE(61);
      if (lookahead == 'n') ADVANCE(84);
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
          lookahead == 12288) ADVANCE(23);
      if (lookahead != 0 &&
          lookahead != '@' &&
          (lookahead < '[' || '^' < lookahead) &&
          lookahead != '{' &&
          lookahead != '}' &&
          lookahead != '~') ADVANCE(86);
      END_STATE();
    case 22:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 23:
      ACCEPT_TOKEN(sym__ws);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          (28 <= lookahead && lookahead <= ' ') ||
          lookahead == 5760 ||
          (8192 <= lookahead && lookahead <= 8198) ||
          (8200 <= lookahead && lookahead <= 8202) ||
          lookahead == 8232 ||
          lookahead == 8233 ||
          lookahead == 8287 ||
          lookahead == 12288) ADVANCE(23);
      END_STATE();
    case 24:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(24);
      END_STATE();
    case 25:
      ACCEPT_TOKEN(sym_block_comment);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(sym_block_comment);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(86);
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
      if (lookahead == 'b') ADVANCE(14);
      if (lookahead == 'x') ADVANCE(15);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_COMMA);
      if (lookahead == '@') ADVANCE(89);
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
      if (lookahead == 'e') ADVANCE(13);
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
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(86);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(sym_bool_lit);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(86);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(81);
      if (!aux_sym__sym_unqualified_token1_character_set_2(lookahead)) ADVANCE(86);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(85);
      if (lookahead == '|') ADVANCE(77);
      if (!aux_sym__sym_unqualified_token1_character_set_3(lookahead)) ADVANCE(76);
      if (sym_block_comment_character_set_1(lookahead)) ADVANCE(4);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(26);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(76);
      if (sym_block_comment_character_set_2(lookahead)) ADVANCE(4);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_4(lookahead)) ADVANCE(86);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(30);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_5(lookahead)) ADVANCE(86);
      if (lookahead == '\\') ADVANCE(19);
      if (lookahead == 'b') ADVANCE(80);
      if (lookahead == 'f' ||
          lookahead == 't') ADVANCE(73);
      if (lookahead == 'x') ADVANCE(78);
      if (lookahead == '|') ADVANCE(76);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_6(lookahead)) ADVANCE(86);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(28);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_7(lookahead)) ADVANCE(86);
      if (lookahead == 'b') ADVANCE(80);
      if (lookahead == 'x') ADVANCE(78);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_8(lookahead)) ADVANCE(86);
      if (lookahead == 'e') ADVANCE(72);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_9(lookahead)) ADVANCE(86);
      if (lookahead == 'n') ADVANCE(82);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_10(lookahead)) ADVANCE(86);
      if (lookahead == 'o') ADVANCE(83);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_3(lookahead)) ADVANCE(76);
      if (lookahead == '|') ADVANCE(86);
      if (sym_block_comment_character_set_1(lookahead)) ADVANCE(4);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(86);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_COMMA_AT);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 21},
  [2] = {.lex_state = 1},
  [3] = {.lex_state = 1},
  [4] = {.lex_state = 1},
  [5] = {.lex_state = 2},
  [6] = {.lex_state = 21},
  [7] = {.lex_state = 21},
  [8] = {.lex_state = 2},
  [9] = {.lex_state = 21},
  [10] = {.lex_state = 21},
  [11] = {.lex_state = 21},
  [12] = {.lex_state = 21},
  [13] = {.lex_state = 2},
  [14] = {.lex_state = 21},
  [15] = {.lex_state = 21},
  [16] = {.lex_state = 21},
  [17] = {.lex_state = 21},
  [18] = {.lex_state = 2},
  [19] = {.lex_state = 21},
  [20] = {.lex_state = 21},
  [21] = {.lex_state = 21},
  [22] = {.lex_state = 2},
  [23] = {.lex_state = 2},
  [24] = {.lex_state = 21},
  [25] = {.lex_state = 21},
  [26] = {.lex_state = 21},
  [27] = {.lex_state = 21},
  [28] = {.lex_state = 21},
  [29] = {.lex_state = 21},
  [30] = {.lex_state = 21},
  [31] = {.lex_state = 21},
  [32] = {.lex_state = 21},
  [33] = {.lex_state = 21},
  [34] = {.lex_state = 21},
  [35] = {.lex_state = 21},
  [36] = {.lex_state = 21},
  [37] = {.lex_state = 21},
  [38] = {.lex_state = 21},
  [39] = {.lex_state = 21},
  [40] = {.lex_state = 21},
  [41] = {.lex_state = 21},
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
  [59] = {.lex_state = 17},
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
    [sym_block_comment] = ACTIONS(7),
    [aux_sym_num_lit_token1] = ACTIONS(9),
    [sym_kwd_lit] = ACTIONS(5),
    [anon_sym_SQUOTE] = ACTIONS(11),
    [anon_sym_COMMA] = ACTIONS(13),
    [anon_sym_BQUOTE] = ACTIONS(15),
    [anon_sym_DQUOTE] = ACTIONS(17),
    [sym_char_lit] = ACTIONS(5),
    [sym_null_lit] = ACTIONS(7),
    [sym_bool_lit] = ACTIONS(7),
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
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
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
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(53), 3,
      sym_block_comment,
      sym_null_lit,
      sym_bool_lit,
    ACTIONS(51), 4,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      sym_char_lit,
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
    ACTIONS(63), 1,
      aux_sym_num_lit_token1,
    ACTIONS(66), 1,
      anon_sym_SQUOTE,
    ACTIONS(69), 1,
      anon_sym_COMMA,
    ACTIONS(72), 1,
      anon_sym_BQUOTE,
    ACTIONS(75), 1,
      anon_sym_DQUOTE,
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
    ACTIONS(60), 3,
      sym_block_comment,
      sym_null_lit,
      sym_bool_lit,
    ACTIONS(57), 4,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      sym_char_lit,
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
  [206] = 17,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(96), 1,
      sym_block_comment,
    ACTIONS(102), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(94), 2,
      sym__ws,
      sym_comment,
    ACTIONS(98), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(100), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(11), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
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
  [270] = 17,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(106), 1,
      sym_block_comment,
    ACTIONS(108), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(98), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(100), 2,
      sym_null_lit,
      sym_bool_lit,
    ACTIONS(104), 2,
      sym__ws,
      sym_comment,
    STATE(9), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
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
  [334] = 17,
    ACTIONS(113), 1,
      sym_block_comment,
    ACTIONS(116), 1,
      aux_sym_num_lit_token1,
    ACTIONS(122), 1,
      anon_sym_SQUOTE,
    ACTIONS(125), 1,
      anon_sym_COMMA,
    ACTIONS(128), 1,
      anon_sym_BQUOTE,
    ACTIONS(131), 1,
      anon_sym_DQUOTE,
    ACTIONS(137), 1,
      anon_sym_SLASH,
    ACTIONS(140), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(143), 1,
      anon_sym_LPAREN,
    ACTIONS(146), 1,
      anon_sym_RPAREN,
    ACTIONS(148), 1,
      anon_sym_COMMA_AT,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(110), 2,
      sym__ws,
      sym_comment,
    ACTIONS(119), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(134), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(11), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
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
  [398] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(153), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(151), 2,
      sym__ws,
      sym_comment,
    ACTIONS(155), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(157), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [459] = 8,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(43), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(159), 1,
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
  [504] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(153), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(151), 2,
      sym__ws,
      sym_comment,
    ACTIONS(161), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(163), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [565] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(167), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(165), 2,
      sym__ws,
      sym_comment,
    ACTIONS(169), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(171), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(14), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [626] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(153), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(151), 2,
      sym__ws,
      sym_comment,
    ACTIONS(173), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(175), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [687] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(179), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(177), 2,
      sym__ws,
      sym_comment,
    ACTIONS(181), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(183), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(21), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [748] = 8,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(43), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(159), 1,
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
  [793] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(187), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(185), 2,
      sym__ws,
      sym_comment,
    ACTIONS(189), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(191), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(12), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [854] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(195), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(193), 2,
      sym__ws,
      sym_comment,
    ACTIONS(197), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(199), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(16), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [915] = 16,
    ACTIONS(9), 1,
      aux_sym_num_lit_token1,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_BQUOTE,
    ACTIONS(17), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_COMMA_AT,
    ACTIONS(153), 1,
      sym_block_comment,
    STATE(33), 1,
      sym__bare_list_lit,
    ACTIONS(151), 2,
      sym__ws,
      sym_comment,
    ACTIONS(201), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(203), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [976] = 2,
    ACTIONS(207), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(205), 24,
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
  [1006] = 2,
    ACTIONS(211), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(209), 24,
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
  [1036] = 2,
    ACTIONS(215), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(213), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1059] = 2,
    ACTIONS(219), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(217), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1082] = 2,
    ACTIONS(223), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(221), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1105] = 2,
    ACTIONS(227), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(225), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1128] = 2,
    ACTIONS(231), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(229), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1151] = 5,
    ACTIONS(236), 1,
      sym_block_comment,
    ACTIONS(233), 2,
      sym__ws,
      sym_comment,
    STATE(29), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(241), 4,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(239), 9,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_COMMA_AT,
  [1180] = 2,
    ACTIONS(245), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(243), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1203] = 2,
    ACTIONS(249), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(247), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1226] = 2,
    ACTIONS(253), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(251), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1249] = 2,
    ACTIONS(257), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(255), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1272] = 2,
    ACTIONS(261), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(259), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1295] = 2,
    ACTIONS(265), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(263), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1318] = 2,
    ACTIONS(269), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(267), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1341] = 2,
    ACTIONS(273), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(271), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1364] = 2,
    ACTIONS(277), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(275), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1387] = 2,
    ACTIONS(281), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(279), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1410] = 2,
    ACTIONS(285), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(283), 13,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1433] = 2,
    ACTIONS(289), 5,
      sym_block_comment,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(287), 12,
      sym__ws,
      sym_comment,
      aux_sym_num_lit_token1,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1455] = 4,
    ACTIONS(297), 1,
      anon_sym_STAR,
    ACTIONS(293), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(295), 4,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(291), 6,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1477] = 2,
    ACTIONS(301), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(299), 11,
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
  [1495] = 2,
    ACTIONS(305), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(303), 11,
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
  [1513] = 6,
    ACTIONS(307), 1,
      aux_sym_num_lit_token1,
    ACTIONS(310), 1,
      anon_sym_SQUOTE,
    ACTIONS(313), 1,
      anon_sym_COMMA,
    ACTIONS(316), 2,
      anon_sym_AT,
      anon_sym_COLON,
    STATE(45), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
    ACTIONS(318), 3,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1536] = 7,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(320), 1,
      anon_sym_COMMA,
    ACTIONS(326), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(322), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(324), 2,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
    STATE(45), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1561] = 4,
    ACTIONS(328), 1,
      anon_sym_TILDE,
    ACTIONS(331), 1,
      anon_sym_DQUOTE,
    ACTIONS(333), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(47), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1576] = 4,
    ACTIONS(45), 1,
      anon_sym_DQUOTE,
    ACTIONS(336), 1,
      anon_sym_TILDE,
    ACTIONS(338), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(47), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1591] = 4,
    ACTIONS(340), 1,
      anon_sym_TILDE,
    ACTIONS(342), 1,
      anon_sym_DQUOTE,
    ACTIONS(344), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(48), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1606] = 5,
    ACTIONS(27), 1,
      aux_sym_num_lit_token1,
    ACTIONS(29), 1,
      anon_sym_SQUOTE,
    ACTIONS(320), 1,
      anon_sym_COMMA,
    ACTIONS(326), 1,
      aux_sym_format_directive_type_token11,
    STATE(45), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1623] = 1,
    ACTIONS(346), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1630] = 1,
    ACTIONS(348), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1637] = 1,
    ACTIONS(350), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1644] = 1,
    ACTIONS(352), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1651] = 1,
    ACTIONS(354), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1658] = 1,
    ACTIONS(356), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1665] = 1,
    ACTIONS(358), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1672] = 1,
    ACTIONS(360), 1,
      ts_builtin_sym_end,
  [1676] = 1,
    ACTIONS(362), 1,
      aux_sym__format_token_token1,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(5)] = 0,
  [SMALL_STATE(6)] = 56,
  [SMALL_STATE(7)] = 114,
  [SMALL_STATE(8)] = 172,
  [SMALL_STATE(9)] = 206,
  [SMALL_STATE(10)] = 270,
  [SMALL_STATE(11)] = 334,
  [SMALL_STATE(12)] = 398,
  [SMALL_STATE(13)] = 459,
  [SMALL_STATE(14)] = 504,
  [SMALL_STATE(15)] = 565,
  [SMALL_STATE(16)] = 626,
  [SMALL_STATE(17)] = 687,
  [SMALL_STATE(18)] = 748,
  [SMALL_STATE(19)] = 793,
  [SMALL_STATE(20)] = 854,
  [SMALL_STATE(21)] = 915,
  [SMALL_STATE(22)] = 976,
  [SMALL_STATE(23)] = 1006,
  [SMALL_STATE(24)] = 1036,
  [SMALL_STATE(25)] = 1059,
  [SMALL_STATE(26)] = 1082,
  [SMALL_STATE(27)] = 1105,
  [SMALL_STATE(28)] = 1128,
  [SMALL_STATE(29)] = 1151,
  [SMALL_STATE(30)] = 1180,
  [SMALL_STATE(31)] = 1203,
  [SMALL_STATE(32)] = 1226,
  [SMALL_STATE(33)] = 1249,
  [SMALL_STATE(34)] = 1272,
  [SMALL_STATE(35)] = 1295,
  [SMALL_STATE(36)] = 1318,
  [SMALL_STATE(37)] = 1341,
  [SMALL_STATE(38)] = 1364,
  [SMALL_STATE(39)] = 1387,
  [SMALL_STATE(40)] = 1410,
  [SMALL_STATE(41)] = 1433,
  [SMALL_STATE(42)] = 1455,
  [SMALL_STATE(43)] = 1477,
  [SMALL_STATE(44)] = 1495,
  [SMALL_STATE(45)] = 1513,
  [SMALL_STATE(46)] = 1536,
  [SMALL_STATE(47)] = 1561,
  [SMALL_STATE(48)] = 1576,
  [SMALL_STATE(49)] = 1591,
  [SMALL_STATE(50)] = 1606,
  [SMALL_STATE(51)] = 1623,
  [SMALL_STATE(52)] = 1630,
  [SMALL_STATE(53)] = 1637,
  [SMALL_STATE(54)] = 1644,
  [SMALL_STATE(55)] = 1651,
  [SMALL_STATE(56)] = 1658,
  [SMALL_STATE(57)] = 1665,
  [SMALL_STATE(58)] = 1672,
  [SMALL_STATE(59)] = 1676,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
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
  [60] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(7),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(30),
  [66] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(15),
  [69] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(20),
  [72] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(19),
  [75] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(49),
  [78] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(37),
  [81] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(37),
  [84] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(10),
  [87] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(17),
  [90] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_prefix_parameters, 1),
  [92] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_prefix_parameters, 1),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [96] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [100] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [106] = {.entry = {.count = 1, .reusable = false}}, SHIFT(9),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [110] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(11),
  [113] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(11),
  [116] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(30),
  [119] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(41),
  [122] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(15),
  [125] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(20),
  [128] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(19),
  [131] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(49),
  [134] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(41),
  [137] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(37),
  [140] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(37),
  [143] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(10),
  [146] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9),
  [148] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 9), SHIFT_REPEAT(17),
  [151] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [153] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [155] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [157] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [159] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [161] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [163] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [165] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [167] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [169] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [171] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [173] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [175] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [177] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [179] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [181] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [183] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [185] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [187] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [189] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [191] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [193] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [195] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [197] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [199] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [201] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [203] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [205] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 1),
  [207] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_modifiers, 1),
  [209] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 2),
  [211] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_modifiers, 2),
  [213] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 3, .production_id = 6),
  [215] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 3, .production_id = 6),
  [217] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 3),
  [219] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 3),
  [221] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 2, .production_id = 4),
  [223] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 2, .production_id = 4),
  [225] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 3),
  [227] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 3),
  [229] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 3, .production_id = 6),
  [231] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 3, .production_id = 6),
  [233] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(29),
  [236] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(29),
  [239] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [241] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [243] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_num_lit, 1),
  [245] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_num_lit, 1),
  [247] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 6),
  [249] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 6),
  [251] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 6),
  [253] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 6),
  [255] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [257] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [259] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 3, .production_id = 8),
  [261] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 3, .production_id = 8),
  [263] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 2),
  [265] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 2),
  [267] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 4),
  [269] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 4),
  [271] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [273] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [275] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 2, .production_id = 3),
  [277] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 2, .production_id = 3),
  [279] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 3),
  [281] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 3),
  [283] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 2, .production_id = 3),
  [285] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 2, .production_id = 3),
  [287] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 5),
  [289] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 5),
  [291] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [293] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [295] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [297] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [299] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 1, .production_id = 7),
  [301] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 1, .production_id = 7),
  [303] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 2),
  [305] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 2),
  [307] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(43),
  [310] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(59),
  [313] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(45),
  [316] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [318] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [320] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [322] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [324] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [326] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [328] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(4),
  [331] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2),
  [333] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(47),
  [336] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [338] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [340] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [342] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [344] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [346] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 10),
  [348] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 3),
  [350] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 11),
  [352] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2),
  [354] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 2),
  [356] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 1),
  [358] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 4),
  [360] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [362] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
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
