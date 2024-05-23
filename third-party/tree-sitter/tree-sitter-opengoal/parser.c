#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 62
#define LARGE_STATE_COUNT 4
#define SYMBOL_COUNT 74
#define ALIAS_COUNT 0
#define TOKEN_COUNT 51
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 7
#define MAX_ALIAS_SEQUENCE_LENGTH 4
#define PRODUCTION_ID_COUNT 13

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
  aux_sym__digit_sym_token1 = 45,
  anon_sym_SLASH = 46,
  aux_sym__sym_unqualified_token1 = 47,
  anon_sym_LPAREN = 48,
  anon_sym_RPAREN = 49,
  anon_sym_COMMA_AT = 50,
  sym_source = 51,
  sym__gap = 52,
  sym__form = 53,
  sym_num_lit = 54,
  sym__format_token = 55,
  sym_format_prefix_parameters = 56,
  sym_format_modifiers = 57,
  sym_format_directive_type = 58,
  sym_format_specifier = 59,
  sym_str_lit = 60,
  sym_sym_lit = 61,
  sym__digit_sym = 62,
  sym_list_lit = 63,
  sym__bare_list_lit = 64,
  sym_quoting_lit = 65,
  sym_quasi_quoting_lit = 66,
  sym_unquote_splicing_lit = 67,
  sym_unquoting_lit = 68,
  aux_sym_source_repeat1 = 69,
  aux_sym_format_modifiers_repeat1 = 70,
  aux_sym_str_lit_repeat1 = 71,
  aux_sym__bare_list_lit_repeat1 = 72,
  aux_sym_quoting_lit_repeat1 = 73,
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
  [aux_sym__digit_sym_token1] = "sym_name",
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
  [sym__digit_sym] = "_digit_sym",
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
  [aux_sym__digit_sym_token1] = aux_sym__digit_sym_token1,
  [anon_sym_SLASH] = aux_sym__digit_sym_token1,
  [aux_sym__sym_unqualified_token1] = aux_sym__digit_sym_token1,
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
  [sym__digit_sym] = sym__digit_sym,
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
  [aux_sym__digit_sym_token1] = {
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
  [sym__digit_sym] = {
    .visible = false,
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
  [2] = {.index = 1, .length = 1},
  [3] = {.index = 2, .length = 3},
  [4] = {.index = 5, .length = 2},
  [5] = {.index = 7, .length = 2},
  [6] = {.index = 9, .length = 1},
  [7] = {.index = 10, .length = 2},
  [9] = {.index = 12, .length = 3},
  [10] = {.index = 15, .length = 2},
  [11] = {.index = 17, .length = 1},
  [12] = {.index = 18, .length = 1},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_name, 0, .inherited = true},
  [2] =
    {field_close, 0, .inherited = true},
    {field_open, 0, .inherited = true},
    {field_value, 0, .inherited = true},
  [5] =
    {field_marker, 0},
    {field_value, 1},
  [7] =
    {field_close, 1},
    {field_open, 0},
  [9] =
    {field_value, 0},
  [10] =
    {field_marker, 0},
    {field_value, 2},
  [12] =
    {field_close, 2},
    {field_open, 0},
    {field_value, 1, .inherited = true},
  [15] =
    {field_value, 0, .inherited = true},
    {field_value, 1, .inherited = true},
  [17] =
    {field_repetitions, 0},
  [18] =
    {field_numberOfArgs, 0},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [8] = {
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
  [60] = 60,
  [61] = 61,
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

static inline bool aux_sym__sym_unqualified_token1_character_set_3(int32_t c) {
  return (c < '['
    ? (c < '('
      ? (c < 28
        ? (c < '\t'
          ? c == 0
          : c <= '\r')
        : (c <= ' ' || c == '"'))
      : (c <= ')' || (c < ';'
        ? (c < '.'
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

static inline bool aux_sym__sym_unqualified_token1_character_set_5(int32_t c) {
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

static inline bool aux_sym__sym_unqualified_token1_character_set_7(int32_t c) {
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
          ? c == 'e'
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
          ? c == 'n'
          : c <= '{')
        : (c <= '}' || c == 5760))
      : (c <= 8198 || (c < 8287
        ? (c < 8232
          ? (c >= 8200 && c <= 8202)
          : c <= 8233)
        : (c <= 8287 || c == 12288))))));
}

static inline bool aux_sym__sym_unqualified_token1_character_set_11(int32_t c) {
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
      if (lookahead == '\n') ADVANCE(74);
      if (lookahead == '\r') ADVANCE(74);
      if (lookahead == '"') ADVANCE(73);
      if (lookahead == '#') ADVANCE(74);
      if (lookahead == '%') ADVANCE(74);
      if (lookahead == '&') ADVANCE(74);
      if (lookahead == '\'') ADVANCE(74);
      if (lookahead == '(') ADVANCE(74);
      if (lookahead == ')') ADVANCE(74);
      if (lookahead == '*') ADVANCE(74);
      if (lookahead == ',') ADVANCE(74);
      if (lookahead == '/') ADVANCE(74);
      if (lookahead == ':') ADVANCE(74);
      if (lookahead == ';') ADVANCE(74);
      if (lookahead == '?') ADVANCE(74);
      if (lookahead == '@') ADVANCE(74);
      if (lookahead == 'V') ADVANCE(74);
      if (lookahead == '\\') ADVANCE(39);
      if (lookahead == '^') ADVANCE(74);
      if (lookahead == '_') ADVANCE(74);
      if (lookahead == '`') ADVANCE(74);
      if (lookahead == 'v') ADVANCE(74);
      if (lookahead == '|') ADVANCE(74);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead == '<' ||
          lookahead == '>') ADVANCE(74);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(74);
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(74);
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(74);
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(74);
      if (lookahead == 'W' ||
          lookahead == 'w') ADVANCE(74);
      if (('[' <= lookahead && lookahead <= ']')) ADVANCE(74);
      if (('{' <= lookahead && lookahead <= '}')) ADVANCE(74);
      if (lookahead == '$' ||
          ('B' <= lookahead && lookahead <= 'O') ||
          ('R' <= lookahead && lookahead <= 'T') ||
          ('X' <= lookahead && lookahead <= 'o') ||
          ('r' <= lookahead && lookahead <= 't') ||
          ('x' <= lookahead && lookahead <= 'z')) ADVANCE(74);
      if (lookahead != 0) ADVANCE(74);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(55);
      if (lookahead == '\r') ADVANCE(56);
      if (lookahead == '"') ADVANCE(73);
      if (lookahead == '#') ADVANCE(42);
      if (lookahead == '%') ADVANCE(50);
      if (lookahead == '&') ADVANCE(51);
      if (lookahead == '\'') ADVANCE(38);
      if (lookahead == ',') ADVANCE(43);
      if (lookahead == ':') ADVANCE(47);
      if (lookahead == ';') ADVANCE(66);
      if (lookahead == '?') ADVANCE(69);
      if (lookahead == '@') ADVANCE(45);
      if (lookahead == 'N') ADVANCE(72);
      if (lookahead == 'V') ADVANCE(41);
      if (lookahead == '^') ADVANCE(54);
      if (lookahead == '_') ADVANCE(61);
      if (lookahead == '`') ADVANCE(67);
      if (lookahead == 'v') ADVANCE(40);
      if (lookahead == '|') ADVANCE(52);
      if (lookahead == '~') ADVANCE(49);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(7);
      if (lookahead == '<' ||
          lookahead == '>') ADVANCE(65);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(60);
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(53);
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(58);
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(57);
      if (lookahead == 'W' ||
          lookahead == 'w') ADVANCE(59);
      if (lookahead == '[' ||
          lookahead == ']') ADVANCE(64);
      if (('{' <= lookahead && lookahead <= '}')) ADVANCE(63);
      if (lookahead == '(' ||
          lookahead == ')') ADVANCE(62);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (lookahead == '$' ||
          ('B' <= lookahead && lookahead <= 'O') ||
          ('R' <= lookahead && lookahead <= 'T') ||
          ('X' <= lookahead && lookahead <= 'Z') ||
          ('b' <= lookahead && lookahead <= 'o') ||
          ('r' <= lookahead && lookahead <= 't') ||
          ('x' <= lookahead && lookahead <= 'z')) ADVANCE(71);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(55);
      if (lookahead == '\r') ADVANCE(56);
      if (lookahead == '#') ADVANCE(8);
      if (lookahead == '%') ADVANCE(50);
      if (lookahead == '&') ADVANCE(51);
      if (lookahead == '\'') ADVANCE(38);
      if (lookahead == ',') ADVANCE(43);
      if (lookahead == ':') ADVANCE(47);
      if (lookahead == ';') ADVANCE(66);
      if (lookahead == '?') ADVANCE(69);
      if (lookahead == '@') ADVANCE(45);
      if (lookahead == 'N') ADVANCE(72);
      if (lookahead == '^') ADVANCE(54);
      if (lookahead == '_') ADVANCE(61);
      if (lookahead == '`') ADVANCE(67);
      if (lookahead == '|') ADVANCE(52);
      if (lookahead == '~') ADVANCE(49);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(7);
      if (lookahead == '<' ||
          lookahead == '>') ADVANCE(65);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(60);
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(53);
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(58);
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(57);
      if (lookahead == 'W' ||
          lookahead == 'w') ADVANCE(59);
      if (lookahead == '[' ||
          lookahead == ']') ADVANCE(64);
      if (('{' <= lookahead && lookahead <= '}')) ADVANCE(63);
      if (lookahead == '(' ||
          lookahead == ')') ADVANCE(62);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (lookahead == '$' ||
          ('B' <= lookahead && lookahead <= 'O') ||
          ('R' <= lookahead && lookahead <= 'T') ||
          ('V' <= lookahead && lookahead <= 'Z') ||
          ('b' <= lookahead && lookahead <= 'o') ||
          ('r' <= lookahead && lookahead <= 't') ||
          ('x' <= lookahead && lookahead <= 'z')) ADVANCE(71);
      END_STATE();
    case 3:
      if (lookahead == '"') ADVANCE(73);
      if (lookahead == '\\') ADVANCE(18);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead != 0) ADVANCE(74);
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
      if (lookahead == '%') ADVANCE(50);
      if (lookahead == '&') ADVANCE(51);
      if (lookahead == '\'') ADVANCE(38);
      if (lookahead == '*') ADVANCE(68);
      if (lookahead == ',') ADVANCE(43);
      if (lookahead == ':') ADVANCE(47);
      if (lookahead == '@') ADVANCE(45);
      if (lookahead == '|') ADVANCE(52);
      if (lookahead == '~') ADVANCE(49);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(7);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(27);
      if (aux_sym_format_directive_type_token11_character_set_1(lookahead)) ADVANCE(71);
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
      if (lookahead == 'e') ADVANCE(70);
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
      if (!sym_kwd_lit_character_set_1(lookahead)) ADVANCE(37);
      END_STATE();
    case 17:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(39);
      END_STATE();
    case 18:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(75);
      END_STATE();
    case 19:
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(76);
      if (lookahead == '\\') ADVANCE(77);
      END_STATE();
    case 20:
      if (lookahead != 0 &&
          lookahead != '|') ADVANCE(4);
      END_STATE();
    case 21:
      if (eof) ADVANCE(22);
      if (lookahead == '"') ADVANCE(73);
      if (lookahead == '#') ADVANCE(86);
      if (lookahead == '\'') ADVANCE(38);
      if (lookahead == '(') ADVANCE(94);
      if (lookahead == ')') ADVANCE(95);
      if (lookahead == ',') ADVANCE(44);
      if (lookahead == '/') ADVANCE(81);
      if (lookahead == ':') ADVANCE(16);
      if (lookahead == ';') ADVANCE(24);
      if (lookahead == '`') ADVANCE(67);
      if (lookahead == 'n') ADVANCE(91);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(82);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(33);
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
          lookahead != '~') ADVANCE(93);
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
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(93);
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
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_2(lookahead)) ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(31);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_3(lookahead)) ADVANCE(93);
      if (lookahead == '.') ADVANCE(34);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_3(lookahead)) ADVANCE(80);
      if (lookahead == '.') ADVANCE(35);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(33);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_4(lookahead)) ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(34);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_4(lookahead)) ADVANCE(80);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(35);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_5(lookahead)) ADVANCE(93);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(36);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(sym_kwd_lit);
      if (!sym_kwd_lit_character_set_2(lookahead)) ADVANCE(37);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(aux_sym__format_token_token1);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_v);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_V);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_POUND);
      if (lookahead == 'b') ADVANCE(14);
      if (lookahead == 'x') ADVANCE(15);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_COMMA);
      if (lookahead == '@') ADVANCE(96);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_AT);
      if (lookahead == ':') ADVANCE(46);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_AT_COLON);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_COLON);
      if (lookahead == '@') ADVANCE(48);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_COLON_AT);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_PIPE);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token1);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token2);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_LF);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_CR);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token3);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token4);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token5);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token6);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym__);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token7);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token8);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token9);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token10);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_BQUOTE);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_QMARK);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_Newline);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token11);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token11);
      if (lookahead == 'e') ADVANCE(13);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(aux_sym_str_lit_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\' &&
          lookahead != '~') ADVANCE(74);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(aux_sym_str_lit_token2);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_char_lit);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_char_lit);
      if (lookahead == 'n' ||
          lookahead == 's' ||
          lookahead == 't') ADVANCE(76);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_null_lit);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(93);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_bool_lit);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(93);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(aux_sym__digit_sym_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(80);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(88);
      if (!aux_sym__sym_unqualified_token1_character_set_4(lookahead)) ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(32);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(92);
      if (lookahead == '|') ADVANCE(84);
      if (!aux_sym__sym_unqualified_token1_character_set_6(lookahead)) ADVANCE(83);
      if (sym_block_comment_character_set_1(lookahead)) ADVANCE(4);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(26);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(83);
      if (sym_block_comment_character_set_2(lookahead)) ADVANCE(4);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_2(lookahead)) ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(31);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_7(lookahead)) ADVANCE(93);
      if (lookahead == '\\') ADVANCE(19);
      if (lookahead == 'b') ADVANCE(87);
      if (lookahead == 'f' ||
          lookahead == 't') ADVANCE(79);
      if (lookahead == 'x') ADVANCE(85);
      if (lookahead == '|') ADVANCE(83);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_5(lookahead)) ADVANCE(93);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(36);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_8(lookahead)) ADVANCE(93);
      if (lookahead == 'b') ADVANCE(87);
      if (lookahead == 'x') ADVANCE(85);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_9(lookahead)) ADVANCE(93);
      if (lookahead == 'e') ADVANCE(78);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_10(lookahead)) ADVANCE(93);
      if (lookahead == 'n') ADVANCE(89);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_11(lookahead)) ADVANCE(93);
      if (lookahead == 'o') ADVANCE(90);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_6(lookahead)) ADVANCE(83);
      if (lookahead == '|') ADVANCE(93);
      if (sym_block_comment_character_set_1(lookahead)) ADVANCE(4);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(93);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 96:
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
  [8] = {.lex_state = 21},
  [9] = {.lex_state = 21},
  [10] = {.lex_state = 21},
  [11] = {.lex_state = 21},
  [12] = {.lex_state = 21},
  [13] = {.lex_state = 21},
  [14] = {.lex_state = 21},
  [15] = {.lex_state = 21},
  [16] = {.lex_state = 21},
  [17] = {.lex_state = 21},
  [18] = {.lex_state = 21},
  [19] = {.lex_state = 2},
  [20] = {.lex_state = 2},
  [21] = {.lex_state = 2},
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
  [42] = {.lex_state = 21},
  [43] = {.lex_state = 21},
  [44] = {.lex_state = 6},
  [45] = {.lex_state = 6},
  [46] = {.lex_state = 6},
  [47] = {.lex_state = 6},
  [48] = {.lex_state = 6},
  [49] = {.lex_state = 3},
  [50] = {.lex_state = 3},
  [51] = {.lex_state = 6},
  [52] = {.lex_state = 3},
  [53] = {.lex_state = 3},
  [54] = {.lex_state = 3},
  [55] = {.lex_state = 3},
  [56] = {.lex_state = 3},
  [57] = {.lex_state = 3},
  [58] = {.lex_state = 3},
  [59] = {.lex_state = 3},
  [60] = {.lex_state = 0},
  [61] = {.lex_state = 17},
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
    [sym_source] = STATE(60),
    [sym__gap] = STATE(9),
    [sym__form] = STATE(9),
    [sym_num_lit] = STATE(9),
    [sym_str_lit] = STATE(9),
    [sym_sym_lit] = STATE(9),
    [sym__digit_sym] = STATE(39),
    [sym_list_lit] = STATE(9),
    [sym__bare_list_lit] = STATE(42),
    [sym_quoting_lit] = STATE(9),
    [sym_quasi_quoting_lit] = STATE(9),
    [sym_unquote_splicing_lit] = STATE(9),
    [sym_unquoting_lit] = STATE(9),
    [aux_sym_source_repeat1] = STATE(9),
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
    [aux_sym__digit_sym_token1] = ACTIONS(19),
    [anon_sym_SLASH] = ACTIONS(21),
    [aux_sym__sym_unqualified_token1] = ACTIONS(23),
    [anon_sym_LPAREN] = ACTIONS(25),
    [anon_sym_COMMA_AT] = ACTIONS(27),
  },
  [2] = {
    [sym__format_token] = STATE(44),
    [sym_format_prefix_parameters] = STATE(5),
    [sym_format_modifiers] = STATE(20),
    [sym_format_directive_type] = STATE(53),
    [aux_sym_format_modifiers_repeat1] = STATE(47),
    [aux_sym_num_lit_token1] = ACTIONS(29),
    [anon_sym_SQUOTE] = ACTIONS(31),
    [anon_sym_v] = ACTIONS(33),
    [anon_sym_V] = ACTIONS(33),
    [anon_sym_POUND] = ACTIONS(35),
    [anon_sym_COMMA] = ACTIONS(37),
    [anon_sym_AT] = ACTIONS(39),
    [anon_sym_AT_COLON] = ACTIONS(41),
    [anon_sym_COLON] = ACTIONS(39),
    [anon_sym_COLON_AT] = ACTIONS(41),
    [anon_sym_TILDE] = ACTIONS(43),
    [anon_sym_PERCENT] = ACTIONS(43),
    [anon_sym_AMP] = ACTIONS(43),
    [anon_sym_PIPE] = ACTIONS(43),
    [aux_sym_format_directive_type_token1] = ACTIONS(43),
    [aux_sym_format_directive_type_token2] = ACTIONS(43),
    [anon_sym_LF] = ACTIONS(43),
    [anon_sym_CR] = ACTIONS(43),
    [aux_sym_format_directive_type_token3] = ACTIONS(43),
    [aux_sym_format_directive_type_token4] = ACTIONS(43),
    [aux_sym_format_directive_type_token5] = ACTIONS(43),
    [aux_sym_format_directive_type_token6] = ACTIONS(43),
    [anon_sym__] = ACTIONS(43),
    [aux_sym_format_directive_type_token7] = ACTIONS(43),
    [aux_sym_format_directive_type_token8] = ACTIONS(43),
    [aux_sym_format_directive_type_token9] = ACTIONS(43),
    [aux_sym_format_directive_type_token10] = ACTIONS(43),
    [anon_sym_SEMI] = ACTIONS(43),
    [anon_sym_BQUOTE] = ACTIONS(43),
    [anon_sym_QMARK] = ACTIONS(43),
    [anon_sym_Newline] = ACTIONS(43),
    [aux_sym_format_directive_type_token11] = ACTIONS(45),
    [anon_sym_DQUOTE] = ACTIONS(47),
  },
  [3] = {
    [sym__format_token] = STATE(44),
    [sym_format_prefix_parameters] = STATE(5),
    [sym_format_modifiers] = STATE(20),
    [sym_format_directive_type] = STATE(53),
    [aux_sym_format_modifiers_repeat1] = STATE(47),
    [aux_sym_num_lit_token1] = ACTIONS(29),
    [anon_sym_SQUOTE] = ACTIONS(31),
    [anon_sym_v] = ACTIONS(33),
    [anon_sym_V] = ACTIONS(33),
    [anon_sym_POUND] = ACTIONS(35),
    [anon_sym_COMMA] = ACTIONS(37),
    [anon_sym_AT] = ACTIONS(39),
    [anon_sym_AT_COLON] = ACTIONS(41),
    [anon_sym_COLON] = ACTIONS(39),
    [anon_sym_COLON_AT] = ACTIONS(41),
    [anon_sym_TILDE] = ACTIONS(43),
    [anon_sym_PERCENT] = ACTIONS(43),
    [anon_sym_AMP] = ACTIONS(43),
    [anon_sym_PIPE] = ACTIONS(43),
    [aux_sym_format_directive_type_token1] = ACTIONS(43),
    [aux_sym_format_directive_type_token2] = ACTIONS(43),
    [anon_sym_LF] = ACTIONS(43),
    [anon_sym_CR] = ACTIONS(43),
    [aux_sym_format_directive_type_token3] = ACTIONS(43),
    [aux_sym_format_directive_type_token4] = ACTIONS(43),
    [aux_sym_format_directive_type_token5] = ACTIONS(43),
    [aux_sym_format_directive_type_token6] = ACTIONS(43),
    [anon_sym__] = ACTIONS(43),
    [aux_sym_format_directive_type_token7] = ACTIONS(43),
    [aux_sym_format_directive_type_token8] = ACTIONS(43),
    [aux_sym_format_directive_type_token9] = ACTIONS(43),
    [aux_sym_format_directive_type_token10] = ACTIONS(43),
    [anon_sym_SEMI] = ACTIONS(43),
    [anon_sym_BQUOTE] = ACTIONS(43),
    [anon_sym_QMARK] = ACTIONS(43),
    [anon_sym_Newline] = ACTIONS(43),
    [aux_sym_format_directive_type_token11] = ACTIONS(45),
    [anon_sym_DQUOTE] = ACTIONS(49),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 14,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(35), 1,
      anon_sym_POUND,
    ACTIONS(37), 1,
      anon_sym_COMMA,
    ACTIONS(45), 1,
      aux_sym_format_directive_type_token11,
    STATE(5), 1,
      sym_format_prefix_parameters,
    STATE(20), 1,
      sym_format_modifiers,
    STATE(44), 1,
      sym__format_token,
    STATE(47), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(53), 1,
      sym_format_directive_type,
    ACTIONS(33), 2,
      anon_sym_v,
      anon_sym_V,
    ACTIONS(39), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(41), 2,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
    ACTIONS(43), 21,
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
  [66] = 11,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(37), 1,
      anon_sym_COMMA,
    ACTIONS(45), 1,
      aux_sym_format_directive_type_token11,
    STATE(21), 1,
      sym_format_modifiers,
    STATE(44), 1,
      sym__format_token,
    STATE(47), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(54), 1,
      sym_format_directive_type,
    ACTIONS(39), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(41), 2,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
    ACTIONS(43), 21,
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
  [122] = 19,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(53), 1,
      sym_block_comment,
    ACTIONS(59), 1,
      anon_sym_RPAREN,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(51), 2,
      sym__ws,
      sym_comment,
    ACTIONS(55), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(57), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(7), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    STATE(43), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [192] = 19,
    ACTIONS(64), 1,
      sym_block_comment,
    ACTIONS(67), 1,
      aux_sym_num_lit_token1,
    ACTIONS(73), 1,
      anon_sym_SQUOTE,
    ACTIONS(76), 1,
      anon_sym_COMMA,
    ACTIONS(79), 1,
      anon_sym_BQUOTE,
    ACTIONS(82), 1,
      anon_sym_DQUOTE,
    ACTIONS(88), 1,
      aux_sym__digit_sym_token1,
    ACTIONS(91), 1,
      anon_sym_SLASH,
    ACTIONS(94), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(100), 1,
      anon_sym_RPAREN,
    ACTIONS(102), 1,
      anon_sym_COMMA_AT,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(61), 2,
      sym__ws,
      sym_comment,
    ACTIONS(70), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(85), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(7), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    STATE(43), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [262] = 19,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(107), 1,
      sym_block_comment,
    ACTIONS(109), 1,
      anon_sym_RPAREN,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(55), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(57), 2,
      sym_null_lit,
      sym_bool_lit,
    ACTIONS(105), 2,
      sym__ws,
      sym_comment,
    STATE(6), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    STATE(43), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [332] = 16,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(111), 1,
      ts_builtin_sym_end,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(115), 3,
      sym_block_comment,
      sym_null_lit,
      sym_bool_lit,
    ACTIONS(113), 4,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      sym_char_lit,
    STATE(10), 11,
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
  [396] = 16,
    ACTIONS(117), 1,
      ts_builtin_sym_end,
    ACTIONS(125), 1,
      aux_sym_num_lit_token1,
    ACTIONS(128), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_COMMA,
    ACTIONS(134), 1,
      anon_sym_BQUOTE,
    ACTIONS(137), 1,
      anon_sym_DQUOTE,
    ACTIONS(140), 1,
      aux_sym__digit_sym_token1,
    ACTIONS(143), 1,
      anon_sym_SLASH,
    ACTIONS(146), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(149), 1,
      anon_sym_LPAREN,
    ACTIONS(152), 1,
      anon_sym_COMMA_AT,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(122), 3,
      sym_block_comment,
      sym_null_lit,
      sym_bool_lit,
    ACTIONS(119), 4,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      sym_char_lit,
    STATE(10), 11,
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
  [460] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(157), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(155), 2,
      sym__ws,
      sym_comment,
    ACTIONS(159), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(161), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(16), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
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
  [527] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(165), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(163), 2,
      sym__ws,
      sym_comment,
    ACTIONS(167), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(169), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(17), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    STATE(37), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [594] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(173), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(171), 2,
      sym__ws,
      sym_comment,
    ACTIONS(175), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(177), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(18), 2,
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
  [661] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(181), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(179), 2,
      sym__ws,
      sym_comment,
    ACTIONS(183), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(185), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(15), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    STATE(30), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [728] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(189), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(187), 2,
      sym__ws,
      sym_comment,
    ACTIONS(191), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(193), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    STATE(34), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [795] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(189), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(187), 2,
      sym__ws,
      sym_comment,
    ACTIONS(195), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(197), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(31), 2,
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
  [862] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(189), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(187), 2,
      sym__ws,
      sym_comment,
    ACTIONS(199), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(201), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(31), 2,
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
  [929] = 18,
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
      aux_sym__digit_sym_token1,
    ACTIONS(21), 1,
      anon_sym_SLASH,
    ACTIONS(23), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(25), 1,
      anon_sym_LPAREN,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(189), 1,
      sym_block_comment,
    STATE(39), 1,
      sym__digit_sym,
    STATE(42), 1,
      sym__bare_list_lit,
    ACTIONS(187), 2,
      sym__ws,
      sym_comment,
    ACTIONS(203), 2,
      sym_kwd_lit,
      sym_char_lit,
    ACTIONS(205), 2,
      sym_null_lit,
      sym_bool_lit,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    STATE(33), 9,
      sym__form,
      sym_num_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [996] = 2,
    ACTIONS(209), 3,
      anon_sym_AT,
      anon_sym_COLON,
      aux_sym_format_directive_type_token11,
    ACTIONS(207), 26,
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
  [1030] = 8,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(45), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(211), 1,
      anon_sym_COMMA,
    STATE(44), 1,
      sym__format_token,
    STATE(51), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(54), 1,
      sym_format_directive_type,
    ACTIONS(43), 21,
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
  [1075] = 8,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(45), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(211), 1,
      anon_sym_COMMA,
    STATE(44), 1,
      sym__format_token,
    STATE(51), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(59), 1,
      sym_format_directive_type,
    ACTIONS(43), 21,
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
  [1120] = 2,
    ACTIONS(215), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(213), 24,
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
  [1150] = 2,
    ACTIONS(219), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(217), 24,
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
  [1180] = 2,
    ACTIONS(223), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(221), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1204] = 2,
    ACTIONS(227), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(225), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1228] = 2,
    ACTIONS(231), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(229), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1252] = 2,
    ACTIONS(235), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(233), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1276] = 2,
    ACTIONS(239), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(237), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1300] = 2,
    ACTIONS(243), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(241), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1324] = 2,
    ACTIONS(247), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(245), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1348] = 5,
    ACTIONS(252), 1,
      sym_block_comment,
    ACTIONS(249), 2,
      sym__ws,
      sym_comment,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(255), 6,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(257), 8,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_COMMA_AT,
  [1378] = 2,
    ACTIONS(261), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(259), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1402] = 2,
    ACTIONS(265), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(263), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1426] = 2,
    ACTIONS(269), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(267), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1450] = 2,
    ACTIONS(273), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(271), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1474] = 2,
    ACTIONS(277), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(275), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1498] = 2,
    ACTIONS(281), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(279), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1522] = 2,
    ACTIONS(285), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(283), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1546] = 2,
    ACTIONS(289), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(287), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1570] = 2,
    ACTIONS(293), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(291), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1594] = 2,
    ACTIONS(297), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(295), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1618] = 2,
    ACTIONS(301), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(299), 12,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1642] = 2,
    ACTIONS(305), 7,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COMMA,
      sym_null_lit,
      sym_bool_lit,
      aux_sym__digit_sym_token1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(303), 11,
      sym__ws,
      sym_comment,
      sym_kwd_lit,
      anon_sym_SQUOTE,
      anon_sym_BQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_COMMA_AT,
  [1665] = 4,
    ACTIONS(313), 1,
      anon_sym_STAR,
    ACTIONS(309), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(311), 4,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(307), 6,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1687] = 2,
    ACTIONS(317), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(315), 11,
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
  [1705] = 2,
    ACTIONS(321), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(319), 11,
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
  [1723] = 7,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(323), 1,
      anon_sym_COMMA,
    ACTIONS(329), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(325), 2,
      anon_sym_AT,
      anon_sym_COLON,
    ACTIONS(327), 2,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
    STATE(48), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1748] = 6,
    ACTIONS(331), 1,
      aux_sym_num_lit_token1,
    ACTIONS(334), 1,
      anon_sym_SQUOTE,
    ACTIONS(337), 1,
      anon_sym_COMMA,
    ACTIONS(340), 2,
      anon_sym_AT,
      anon_sym_COLON,
    STATE(48), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
    ACTIONS(342), 3,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1771] = 4,
    ACTIONS(344), 1,
      anon_sym_TILDE,
    ACTIONS(347), 1,
      anon_sym_DQUOTE,
    ACTIONS(349), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(49), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1786] = 4,
    ACTIONS(352), 1,
      anon_sym_TILDE,
    ACTIONS(354), 1,
      anon_sym_DQUOTE,
    ACTIONS(356), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(52), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1801] = 5,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(323), 1,
      anon_sym_COMMA,
    ACTIONS(329), 1,
      aux_sym_format_directive_type_token11,
    STATE(48), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1818] = 4,
    ACTIONS(47), 1,
      anon_sym_DQUOTE,
    ACTIONS(358), 1,
      anon_sym_TILDE,
    ACTIONS(360), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(49), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1833] = 1,
    ACTIONS(362), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1840] = 1,
    ACTIONS(364), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1847] = 1,
    ACTIONS(366), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1854] = 1,
    ACTIONS(368), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1861] = 1,
    ACTIONS(370), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1868] = 1,
    ACTIONS(372), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1875] = 1,
    ACTIONS(374), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1882] = 1,
    ACTIONS(376), 1,
      ts_builtin_sym_end,
  [1886] = 1,
    ACTIONS(378), 1,
      aux_sym__format_token_token1,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(4)] = 0,
  [SMALL_STATE(5)] = 66,
  [SMALL_STATE(6)] = 122,
  [SMALL_STATE(7)] = 192,
  [SMALL_STATE(8)] = 262,
  [SMALL_STATE(9)] = 332,
  [SMALL_STATE(10)] = 396,
  [SMALL_STATE(11)] = 460,
  [SMALL_STATE(12)] = 527,
  [SMALL_STATE(13)] = 594,
  [SMALL_STATE(14)] = 661,
  [SMALL_STATE(15)] = 728,
  [SMALL_STATE(16)] = 795,
  [SMALL_STATE(17)] = 862,
  [SMALL_STATE(18)] = 929,
  [SMALL_STATE(19)] = 996,
  [SMALL_STATE(20)] = 1030,
  [SMALL_STATE(21)] = 1075,
  [SMALL_STATE(22)] = 1120,
  [SMALL_STATE(23)] = 1150,
  [SMALL_STATE(24)] = 1180,
  [SMALL_STATE(25)] = 1204,
  [SMALL_STATE(26)] = 1228,
  [SMALL_STATE(27)] = 1252,
  [SMALL_STATE(28)] = 1276,
  [SMALL_STATE(29)] = 1300,
  [SMALL_STATE(30)] = 1324,
  [SMALL_STATE(31)] = 1348,
  [SMALL_STATE(32)] = 1378,
  [SMALL_STATE(33)] = 1402,
  [SMALL_STATE(34)] = 1426,
  [SMALL_STATE(35)] = 1450,
  [SMALL_STATE(36)] = 1474,
  [SMALL_STATE(37)] = 1498,
  [SMALL_STATE(38)] = 1522,
  [SMALL_STATE(39)] = 1546,
  [SMALL_STATE(40)] = 1570,
  [SMALL_STATE(41)] = 1594,
  [SMALL_STATE(42)] = 1618,
  [SMALL_STATE(43)] = 1642,
  [SMALL_STATE(44)] = 1665,
  [SMALL_STATE(45)] = 1687,
  [SMALL_STATE(46)] = 1705,
  [SMALL_STATE(47)] = 1723,
  [SMALL_STATE(48)] = 1748,
  [SMALL_STATE(49)] = 1771,
  [SMALL_STATE(50)] = 1786,
  [SMALL_STATE(51)] = 1801,
  [SMALL_STATE(52)] = 1818,
  [SMALL_STATE(53)] = 1833,
  [SMALL_STATE(54)] = 1840,
  [SMALL_STATE(55)] = 1847,
  [SMALL_STATE(56)] = 1854,
  [SMALL_STATE(57)] = 1861,
  [SMALL_STATE(58)] = 1868,
  [SMALL_STATE(59)] = 1875,
  [SMALL_STATE(60)] = 1882,
  [SMALL_STATE(61)] = 1886,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(9),
  [9] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [39] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [45] = {.entry = {.count = 1, .reusable = false}}, SHIFT(58),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [49] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [53] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [57] = {.entry = {.count = 1, .reusable = false}}, SHIFT(43),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [61] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(7),
  [64] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(7),
  [67] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(36),
  [70] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(43),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(11),
  [76] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(12),
  [79] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(13),
  [82] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(50),
  [85] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(43),
  [88] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(27),
  [91] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(29),
  [94] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(29),
  [97] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(8),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10),
  [102] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(14),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [107] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [109] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 1),
  [113] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [115] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [117] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2),
  [119] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(10),
  [122] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(10),
  [125] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(36),
  [128] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(11),
  [131] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(12),
  [134] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(13),
  [137] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(50),
  [140] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(27),
  [143] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(29),
  [146] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(29),
  [149] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(8),
  [152] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(14),
  [155] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [157] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [159] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [161] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [163] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [165] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [167] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [169] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [171] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [173] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [175] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [177] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [179] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [181] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [183] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [185] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [187] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [189] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [191] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [193] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [195] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [197] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [199] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [201] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [203] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [205] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [207] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_prefix_parameters, 1),
  [209] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_prefix_parameters, 1),
  [211] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [213] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 1),
  [215] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_modifiers, 1),
  [217] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 2),
  [219] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_modifiers, 2),
  [221] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 3, .production_id = 7),
  [223] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 3, .production_id = 7),
  [225] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 3),
  [227] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 3),
  [229] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 2),
  [231] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 2),
  [233] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__digit_sym, 1, .production_id = 1),
  [235] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__digit_sym, 1, .production_id = 1),
  [237] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 2, .production_id = 5),
  [239] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 2, .production_id = 5),
  [241] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [243] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [245] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 4),
  [247] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 4),
  [249] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(31),
  [252] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(31),
  [255] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [257] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [259] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 3, .production_id = 7),
  [261] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 3, .production_id = 7),
  [263] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 7),
  [265] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 7),
  [267] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 7),
  [269] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 7),
  [271] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 3, .production_id = 9),
  [273] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 3, .production_id = 9),
  [275] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_num_lit, 1),
  [277] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_num_lit, 1),
  [279] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 2, .production_id = 4),
  [281] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 2, .production_id = 4),
  [283] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 4),
  [285] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 4),
  [287] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_sym_lit, 1, .production_id = 2),
  [289] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_sym_lit, 1, .production_id = 2),
  [291] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 4),
  [293] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 4),
  [295] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 2, .production_id = 4),
  [297] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 2, .production_id = 4),
  [299] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_lit, 1, .production_id = 3),
  [301] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list_lit, 1, .production_id = 3),
  [303] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 6),
  [305] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 6),
  [307] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [309] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [311] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [313] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [315] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 1, .production_id = 8),
  [317] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 1, .production_id = 8),
  [319] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 2),
  [321] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 2),
  [323] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [325] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [327] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [329] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [331] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(45),
  [334] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(61),
  [337] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(48),
  [340] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [342] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [344] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(4),
  [347] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2),
  [349] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(49),
  [352] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [354] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [356] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [358] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [360] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [362] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 2),
  [364] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 3),
  [366] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 11),
  [368] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 12),
  [370] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2),
  [372] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 1),
  [374] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 4),
  [376] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [378] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
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
