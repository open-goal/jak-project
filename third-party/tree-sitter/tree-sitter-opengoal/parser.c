#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 62
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 74
#define ALIAS_COUNT 0
#define TOKEN_COUNT 50
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 7
#define MAX_ALIAS_SEQUENCE_LENGTH 4
#define PRODUCTION_ID_COUNT 13

enum {
  sym__ws = 1,
  sym_comment = 2,
  sym_block_comment = 3,
  aux_sym_num_lit_token1 = 4,
  aux_sym__kwd_unqualified_token1 = 5,
  anon_sym_COLON = 6,
  anon_sym_SQUOTE = 7,
  aux_sym__format_token_token1 = 8,
  anon_sym_v = 9,
  anon_sym_V = 10,
  anon_sym_POUND = 11,
  anon_sym_COMMA = 12,
  anon_sym_AT = 13,
  anon_sym_AT_COLON = 14,
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
  anon_sym_STAR = 34,
  anon_sym_QMARK = 35,
  anon_sym_Newline = 36,
  aux_sym_format_directive_type_token11 = 37,
  anon_sym_DQUOTE = 38,
  aux_sym_str_lit_token1 = 39,
  aux_sym_str_lit_token2 = 40,
  sym_char_lit = 41,
  sym_null_lit = 42,
  sym_bool_lit = 43,
  anon_sym_SLASH = 44,
  aux_sym__sym_unqualified_token1 = 45,
  anon_sym_LPAREN = 46,
  anon_sym_RPAREN = 47,
  anon_sym_BQUOTE = 48,
  anon_sym_COMMA_AT = 49,
  sym_source = 50,
  sym__gap = 51,
  sym__form = 52,
  sym_num_lit = 53,
  sym_kwd_lit = 54,
  sym__kwd_marker = 55,
  sym__format_token = 56,
  sym_format_prefix_parameters = 57,
  sym_format_modifiers = 58,
  sym_format_directive_type = 59,
  sym_format_specifier = 60,
  sym_str_lit = 61,
  sym_sym_lit = 62,
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
  [aux_sym__kwd_unqualified_token1] = "kwd_name",
  [anon_sym_COLON] = ":",
  [anon_sym_SQUOTE] = "'",
  [aux_sym__format_token_token1] = "char_lit",
  [anon_sym_v] = "v",
  [anon_sym_V] = "V",
  [anon_sym_POUND] = "#",
  [anon_sym_COMMA] = ",",
  [anon_sym_AT] = "@",
  [anon_sym_AT_COLON] = "@:",
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
  [anon_sym_BQUOTE] = "`",
  [anon_sym_COMMA_AT] = ",@",
  [sym_source] = "source",
  [sym__gap] = "_gap",
  [sym__form] = "_form",
  [sym_num_lit] = "num_lit",
  [sym_kwd_lit] = "kwd_lit",
  [sym__kwd_marker] = "_kwd_marker",
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
  [aux_sym__kwd_unqualified_token1] = aux_sym__kwd_unqualified_token1,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym__format_token_token1] = sym_char_lit,
  [anon_sym_v] = anon_sym_v,
  [anon_sym_V] = anon_sym_V,
  [anon_sym_POUND] = anon_sym_POUND,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_AT] = anon_sym_AT,
  [anon_sym_AT_COLON] = anon_sym_AT_COLON,
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
  [anon_sym_BQUOTE] = anon_sym_BQUOTE,
  [anon_sym_COMMA_AT] = anon_sym_COMMA_AT,
  [sym_source] = sym_source,
  [sym__gap] = sym__gap,
  [sym__form] = sym__form,
  [sym_num_lit] = sym_num_lit,
  [sym_kwd_lit] = sym_kwd_lit,
  [sym__kwd_marker] = sym__kwd_marker,
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
  [aux_sym__kwd_unqualified_token1] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
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
  [anon_sym_BQUOTE] = {
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
  [sym_kwd_lit] = {
    .visible = true,
    .named = true,
  },
  [sym__kwd_marker] = {
    .visible = false,
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

enum {
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
  [7] = {.index = 11, .length = 2},
  [9] = {.index = 13, .length = 3},
  [10] = {.index = 16, .length = 2},
  [11] = {.index = 18, .length = 1},
  [12] = {.index = 19, .length = 1},
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
    {field_name, 1},
  [11] =
    {field_marker, 0},
    {field_value, 2},
  [13] =
    {field_close, 2},
    {field_open, 0},
    {field_value, 1, .inherited = true},
  [16] =
    {field_value, 0, .inherited = true},
    {field_value, 1, .inherited = true},
  [18] =
    {field_repetitions, 0},
  [19] =
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

static inline bool aux_sym_str_lit_token1_character_set_1(int32_t c) {
  return (c < 'X'
    ? (c < 'O'
      ? (c < 'B'
        ? c == '$'
        : c <= 'G')
      : (c <= 'O' || (c >= 'R' && c <= 'T')))
    : (c <= 'X' || (c < 'r'
      ? (c < 'o'
        ? (c >= 'b' && c <= 'g')
        : c <= 'o')
      : (c <= 't' || c == 'x'))));
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
      if (eof) ADVANCE(22);
      if (lookahead == '\n') ADVANCE(66);
      if (lookahead == '\r') ADVANCE(66);
      if (lookahead == '"') ADVANCE(65);
      if (lookahead == '#') ADVANCE(66);
      if (lookahead == '%') ADVANCE(66);
      if (lookahead == '&') ADVANCE(66);
      if (lookahead == '\'') ADVANCE(66);
      if (lookahead == '(') ADVANCE(66);
      if (lookahead == ')') ADVANCE(66);
      if (lookahead == '*') ADVANCE(66);
      if (lookahead == ',') ADVANCE(66);
      if (lookahead == '/') ADVANCE(66);
      if (lookahead == ':') ADVANCE(66);
      if (lookahead == ';') ADVANCE(66);
      if (lookahead == '?') ADVANCE(66);
      if (lookahead == '@') ADVANCE(66);
      if (lookahead == 'V') ADVANCE(66);
      if (lookahead == '\\') ADVANCE(34);
      if (lookahead == '^') ADVANCE(66);
      if (lookahead == '_') ADVANCE(66);
      if (lookahead == '`') ADVANCE(66);
      if (lookahead == 'v') ADVANCE(66);
      if (lookahead == '|') ADVANCE(66);
      if (lookahead == '~') ADVANCE(43);
      if (lookahead == '<' ||
          lookahead == '>') ADVANCE(66);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(66);
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(66);
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(66);
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(66);
      if (lookahead == 'W' ||
          lookahead == 'w') ADVANCE(66);
      if (('[' <= lookahead && lookahead <= ']')) ADVANCE(66);
      if (('{' <= lookahead && lookahead <= '}')) ADVANCE(66);
      if (aux_sym_str_lit_token1_character_set_1(lookahead)) ADVANCE(66);
      if (lookahead != 0) ADVANCE(66);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(49);
      if (lookahead == '\r') ADVANCE(50);
      if (lookahead == '"') ADVANCE(65);
      if (lookahead == '#') ADVANCE(37);
      if (lookahead == '%') ADVANCE(44);
      if (lookahead == '&') ADVANCE(45);
      if (lookahead == '\'') ADVANCE(33);
      if (lookahead == '*') ADVANCE(61);
      if (lookahead == ',') ADVANCE(38);
      if (lookahead == ':') ADVANCE(32);
      if (lookahead == ';') ADVANCE(60);
      if (lookahead == '?') ADVANCE(62);
      if (lookahead == '@') ADVANCE(40);
      if (lookahead == 'N') ADVANCE(7);
      if (lookahead == 'V') ADVANCE(36);
      if (lookahead == '^') ADVANCE(48);
      if (lookahead == '_') ADVANCE(55);
      if (lookahead == 'v') ADVANCE(35);
      if (lookahead == '|') ADVANCE(46);
      if (lookahead == '~') ADVANCE(43);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(4);
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
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      if (aux_sym_str_lit_token1_character_set_1(lookahead)) ADVANCE(64);
      END_STATE();
    case 2:
      if (lookahead == '"') ADVANCE(65);
      if (lookahead == '\\') ADVANCE(19);
      if (lookahead == '~') ADVANCE(43);
      if (lookahead != 0) ADVANCE(66);
      END_STATE();
    case 3:
      if (lookahead == '#') ADVANCE(25);
      END_STATE();
    case 4:
      if (lookahead == '#') ADVANCE(6);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      END_STATE();
    case 5:
      if (lookahead == '\\') ADVANCE(20);
      if (lookahead == 'b') ADVANCE(14);
      if (lookahead == 'f' ||
          lookahead == 't') ADVANCE(71);
      if (lookahead == 'x') ADVANCE(15);
      if (lookahead == '|') ADVANCE(17);
      END_STATE();
    case 6:
      if (lookahead == 'b') ADVANCE(14);
      if (lookahead == 'x') ADVANCE(15);
      END_STATE();
    case 7:
      if (lookahead == 'e') ADVANCE(12);
      END_STATE();
    case 8:
      if (lookahead == 'e') ADVANCE(63);
      END_STATE();
    case 9:
      if (lookahead == 'i') ADVANCE(11);
      END_STATE();
    case 10:
      if (lookahead == 'l') ADVANCE(9);
      END_STATE();
    case 11:
      if (lookahead == 'n') ADVANCE(8);
      END_STATE();
    case 12:
      if (lookahead == 'w') ADVANCE(10);
      END_STATE();
    case 13:
      if (lookahead == '|') ADVANCE(3);
      if (lookahead != 0 &&
          lookahead != '#') ADVANCE(13);
      END_STATE();
    case 14:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(27);
      END_STATE();
    case 15:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(29);
      END_STATE();
    case 16:
      if (!aux_sym__kwd_unqualified_token1_character_set_1(lookahead)) ADVANCE(30);
      END_STATE();
    case 17:
      if (lookahead != 0 &&
          lookahead != '#' &&
          lookahead != '|') ADVANCE(13);
      END_STATE();
    case 18:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(34);
      END_STATE();
    case 19:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(67);
      END_STATE();
    case 20:
      if (lookahead != 0 &&
          lookahead != '\\') ADVANCE(68);
      if (lookahead == '\\') ADVANCE(69);
      END_STATE();
    case 21:
      if (eof) ADVANCE(22);
      if (lookahead == '"') ADVANCE(65);
      if (lookahead == '#') ADVANCE(5);
      if (lookahead == '\'') ADVANCE(33);
      if (lookahead == '(') ADVANCE(81);
      if (lookahead == ')') ADVANCE(82);
      if (lookahead == ',') ADVANCE(39);
      if (lookahead == '/') ADVANCE(72);
      if (lookahead == ':') ADVANCE(31);
      if (lookahead == ';') ADVANCE(24);
      if (lookahead == '`') ADVANCE(83);
      if (lookahead == 'n') ADVANCE(77);
      if (('+' <= lookahead && lookahead <= '-')) ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
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
          lookahead != '~') ADVANCE(80);
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
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (lookahead == '.') ADVANCE(28);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(27);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(28);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(aux_sym_num_lit_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(29);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(aux_sym__kwd_unqualified_token1);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(30);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_COLON);
      if (lookahead == '@') ADVANCE(42);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(aux_sym__format_token_token1);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_v);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_V);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_POUND);
      if (lookahead == 'b') ADVANCE(14);
      if (lookahead == 'x') ADVANCE(15);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_COMMA);
      if (lookahead == '@') ADVANCE(84);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_AT);
      if (lookahead == ':') ADVANCE(41);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_AT_COLON);
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
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_QMARK);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_Newline);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(aux_sym_format_directive_type_token11);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(aux_sym_str_lit_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\' &&
          lookahead != '~') ADVANCE(66);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(aux_sym_str_lit_token2);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(sym_char_lit);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(sym_char_lit);
      if (lookahead == 'n' ||
          lookahead == 's' ||
          lookahead == 't') ADVANCE(68);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(sym_null_lit);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(sym_bool_lit);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '#') ADVANCE(74);
      if (!aux_sym__sym_unqualified_token1_character_set_1(lookahead)) ADVANCE(80);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(26);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'b') ADVANCE(78);
      if (lookahead == 'x') ADVANCE(79);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'e') ADVANCE(70);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'n') ADVANCE(75);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == 'o') ADVANCE(76);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(27);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__sym_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(29);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(aux_sym__sym_unqualified_token1);
      if (!aux_sym__kwd_unqualified_token1_character_set_2(lookahead)) ADVANCE(80);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_BQUOTE);
      END_STATE();
    case 84:
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
  [5] = {.lex_state = 1},
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
  [19] = {.lex_state = 1},
  [20] = {.lex_state = 1},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 1},
  [23] = {.lex_state = 1},
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
  [43] = {.lex_state = 1},
  [44] = {.lex_state = 1},
  [45] = {.lex_state = 1},
  [46] = {.lex_state = 1},
  [47] = {.lex_state = 1},
  [48] = {.lex_state = 2},
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 1},
  [51] = {.lex_state = 2},
  [52] = {.lex_state = 2},
  [53] = {.lex_state = 2},
  [54] = {.lex_state = 2},
  [55] = {.lex_state = 2},
  [56] = {.lex_state = 2},
  [57] = {.lex_state = 2},
  [58] = {.lex_state = 2},
  [59] = {.lex_state = 16},
  [60] = {.lex_state = 18},
  [61] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [aux_sym__format_token_token1] = ACTIONS(1),
    [anon_sym_v] = ACTIONS(1),
    [anon_sym_V] = ACTIONS(1),
    [anon_sym_POUND] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_AT] = ACTIONS(1),
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
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_QMARK] = ACTIONS(1),
    [aux_sym_format_directive_type_token11] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [aux_sym_str_lit_token1] = ACTIONS(1),
    [sym_char_lit] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_BQUOTE] = ACTIONS(1),
  },
  [1] = {
    [sym_source] = STATE(61),
    [sym__gap] = STATE(10),
    [sym__form] = STATE(10),
    [sym_num_lit] = STATE(10),
    [sym_kwd_lit] = STATE(10),
    [sym__kwd_marker] = STATE(59),
    [sym_str_lit] = STATE(10),
    [sym_sym_lit] = STATE(10),
    [sym_list_lit] = STATE(10),
    [sym__bare_list_lit] = STATE(33),
    [sym_quoting_lit] = STATE(10),
    [sym_quasi_quoting_lit] = STATE(10),
    [sym_unquote_splicing_lit] = STATE(10),
    [sym_unquoting_lit] = STATE(10),
    [aux_sym_source_repeat1] = STATE(10),
    [ts_builtin_sym_end] = ACTIONS(3),
    [sym__ws] = ACTIONS(5),
    [sym_comment] = ACTIONS(5),
    [sym_block_comment] = ACTIONS(5),
    [aux_sym_num_lit_token1] = ACTIONS(7),
    [anon_sym_COLON] = ACTIONS(9),
    [anon_sym_SQUOTE] = ACTIONS(11),
    [anon_sym_COMMA] = ACTIONS(13),
    [anon_sym_DQUOTE] = ACTIONS(15),
    [sym_char_lit] = ACTIONS(5),
    [sym_null_lit] = ACTIONS(17),
    [sym_bool_lit] = ACTIONS(5),
    [anon_sym_SLASH] = ACTIONS(19),
    [aux_sym__sym_unqualified_token1] = ACTIONS(21),
    [anon_sym_LPAREN] = ACTIONS(23),
    [anon_sym_BQUOTE] = ACTIONS(25),
    [anon_sym_COMMA_AT] = ACTIONS(27),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 14,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(37), 1,
      anon_sym_POUND,
    ACTIONS(39), 1,
      anon_sym_COMMA,
    ACTIONS(45), 1,
      anon_sym_DQUOTE,
    STATE(5), 1,
      sym_format_prefix_parameters,
    STATE(21), 1,
      sym_format_modifiers,
    STATE(44), 1,
      sym__format_token,
    STATE(46), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(55), 1,
      sym_format_directive_type,
    ACTIONS(31), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(35), 2,
      anon_sym_v,
      anon_sym_V,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [66] = 14,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(37), 1,
      anon_sym_POUND,
    ACTIONS(39), 1,
      anon_sym_COMMA,
    ACTIONS(47), 1,
      anon_sym_DQUOTE,
    STATE(5), 1,
      sym_format_prefix_parameters,
    STATE(21), 1,
      sym_format_modifiers,
    STATE(44), 1,
      sym__format_token,
    STATE(46), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(55), 1,
      sym_format_directive_type,
    ACTIONS(31), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(35), 2,
      anon_sym_v,
      anon_sym_V,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [132] = 13,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(37), 1,
      anon_sym_POUND,
    ACTIONS(39), 1,
      anon_sym_COMMA,
    STATE(5), 1,
      sym_format_prefix_parameters,
    STATE(21), 1,
      sym_format_modifiers,
    STATE(44), 1,
      sym__format_token,
    STATE(46), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(55), 1,
      sym_format_directive_type,
    ACTIONS(31), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(35), 2,
      anon_sym_v,
      anon_sym_V,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [195] = 10,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(39), 1,
      anon_sym_COMMA,
    STATE(20), 1,
      sym_format_modifiers,
    STATE(44), 1,
      sym__format_token,
    STATE(46), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(54), 1,
      sym_format_directive_type,
    ACTIONS(31), 2,
      anon_sym_COLON,
      anon_sym_AT,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [248] = 16,
    ACTIONS(49), 1,
      ts_builtin_sym_end,
    ACTIONS(54), 1,
      aux_sym_num_lit_token1,
    ACTIONS(57), 1,
      anon_sym_COLON,
    ACTIONS(60), 1,
      anon_sym_SQUOTE,
    ACTIONS(63), 1,
      anon_sym_COMMA,
    ACTIONS(66), 1,
      anon_sym_DQUOTE,
    ACTIONS(69), 1,
      sym_null_lit,
    ACTIONS(72), 1,
      anon_sym_SLASH,
    ACTIONS(75), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(78), 1,
      anon_sym_LPAREN,
    ACTIONS(81), 1,
      anon_sym_BQUOTE,
    ACTIONS(84), 1,
      anon_sym_COMMA_AT,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(51), 5,
      sym__ws,
      sym_comment,
      sym_block_comment,
      sym_char_lit,
      sym_bool_lit,
    STATE(6), 12,
      sym__gap,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
      aux_sym_source_repeat1,
  [312] = 18,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(91), 1,
      sym_null_lit,
    ACTIONS(93), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(89), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(9), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    ACTIONS(87), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(42), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [380] = 18,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(91), 1,
      sym_null_lit,
    ACTIONS(97), 1,
      anon_sym_RPAREN,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(89), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(7), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    ACTIONS(95), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(42), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [448] = 18,
    ACTIONS(102), 1,
      aux_sym_num_lit_token1,
    ACTIONS(105), 1,
      anon_sym_COLON,
    ACTIONS(108), 1,
      anon_sym_SQUOTE,
    ACTIONS(111), 1,
      anon_sym_COMMA,
    ACTIONS(114), 1,
      anon_sym_DQUOTE,
    ACTIONS(120), 1,
      sym_null_lit,
    ACTIONS(123), 1,
      anon_sym_SLASH,
    ACTIONS(126), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(129), 1,
      anon_sym_LPAREN,
    ACTIONS(132), 1,
      anon_sym_RPAREN,
    ACTIONS(134), 1,
      anon_sym_BQUOTE,
    ACTIONS(137), 1,
      anon_sym_COMMA_AT,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(117), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(9), 2,
      sym__gap,
      aux_sym__bare_list_lit_repeat1,
    ACTIONS(99), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(42), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [516] = 16,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(140), 1,
      ts_builtin_sym_end,
    ACTIONS(144), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(142), 5,
      sym__ws,
      sym_comment,
      sym_block_comment,
      sym_char_lit,
      sym_bool_lit,
    STATE(6), 12,
      sym__gap,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
      aux_sym_source_repeat1,
  [580] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(150), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(148), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(16), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(146), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(41), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [645] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(156), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(154), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(17), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(152), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(40), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [710] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(162), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(160), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(18), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(158), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(28), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [775] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(168), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(166), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(15), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(164), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(29), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [840] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(174), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(172), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(170), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(39), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [905] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(178), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(176), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(170), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(24), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [970] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(182), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(180), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(170), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(32), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [1035] = 17,
    ACTIONS(7), 1,
      aux_sym_num_lit_token1,
    ACTIONS(9), 1,
      anon_sym_COLON,
    ACTIONS(11), 1,
      anon_sym_SQUOTE,
    ACTIONS(13), 1,
      anon_sym_COMMA,
    ACTIONS(15), 1,
      anon_sym_DQUOTE,
    ACTIONS(19), 1,
      anon_sym_SLASH,
    ACTIONS(21), 1,
      aux_sym__sym_unqualified_token1,
    ACTIONS(23), 1,
      anon_sym_LPAREN,
    ACTIONS(25), 1,
      anon_sym_BQUOTE,
    ACTIONS(27), 1,
      anon_sym_COMMA_AT,
    ACTIONS(186), 1,
      sym_null_lit,
    STATE(33), 1,
      sym__bare_list_lit,
    STATE(59), 1,
      sym__kwd_marker,
    ACTIONS(184), 2,
      sym_char_lit,
      sym_bool_lit,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(170), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    STATE(27), 10,
      sym__form,
      sym_num_lit,
      sym_kwd_lit,
      sym_str_lit,
      sym_sym_lit,
      sym_list_lit,
      sym_quoting_lit,
      sym_quasi_quoting_lit,
      sym_unquote_splicing_lit,
      sym_unquoting_lit,
  [1100] = 2,
    ACTIONS(190), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(188), 26,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [1133] = 7,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(192), 1,
      anon_sym_COMMA,
    STATE(44), 1,
      sym__format_token,
    STATE(50), 1,
      aux_sym_format_modifiers_repeat1,
    STATE(58), 1,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [1175] = 7,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(192), 1,
      anon_sym_COMMA,
    STATE(44), 1,
      sym__format_token,
    STATE(50), 1,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [1217] = 1,
    ACTIONS(194), 24,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [1244] = 1,
    ACTIONS(196), 24,
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
      anon_sym_QMARK,
      anon_sym_Newline,
      aux_sym_format_directive_type_token11,
  [1271] = 2,
    ACTIONS(200), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(198), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1294] = 2,
    ACTIONS(204), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(202), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1317] = 2,
    ACTIONS(208), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(206), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1340] = 2,
    ACTIONS(212), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(210), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1363] = 2,
    ACTIONS(216), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(214), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1386] = 2,
    ACTIONS(220), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(218), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1409] = 2,
    ACTIONS(224), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(222), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1432] = 4,
    STATE(31), 2,
      sym__gap,
      aux_sym_quoting_lit_repeat1,
    ACTIONS(226), 3,
      sym__ws,
      sym_comment,
      sym_block_comment,
    ACTIONS(231), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(229), 10,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1459] = 2,
    ACTIONS(235), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(233), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1482] = 2,
    ACTIONS(239), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(237), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1505] = 2,
    ACTIONS(243), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(241), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1528] = 2,
    ACTIONS(247), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(245), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1551] = 2,
    ACTIONS(251), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(249), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1574] = 2,
    ACTIONS(255), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(253), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1597] = 2,
    ACTIONS(259), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(257), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1620] = 2,
    ACTIONS(263), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(261), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1643] = 2,
    ACTIONS(267), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(265), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1666] = 2,
    ACTIONS(271), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(269), 15,
      ts_builtin_sym_end,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1689] = 2,
    ACTIONS(275), 3,
      anon_sym_COMMA,
      sym_null_lit,
      aux_sym__sym_unqualified_token1,
    ACTIONS(273), 14,
      sym__ws,
      sym_comment,
      sym_block_comment,
      aux_sym_num_lit_token1,
      anon_sym_COLON,
      anon_sym_SQUOTE,
      anon_sym_DQUOTE,
      sym_char_lit,
      sym_bool_lit,
      anon_sym_SLASH,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_BQUOTE,
      anon_sym_COMMA_AT,
  [1711] = 2,
    ACTIONS(279), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(277), 11,
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
  [1729] = 4,
    ACTIONS(287), 1,
      anon_sym_STAR,
    ACTIONS(283), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(285), 4,
      anon_sym_TILDE,
      anon_sym_PERCENT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(281), 6,
      aux_sym_num_lit_token1,
      anon_sym_SQUOTE,
      anon_sym_COMMA,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1751] = 2,
    ACTIONS(291), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(289), 11,
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
  [1769] = 7,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(295), 1,
      anon_sym_COMMA,
    ACTIONS(299), 1,
      aux_sym_format_directive_type_token11,
    ACTIONS(293), 2,
      anon_sym_COLON,
      anon_sym_AT,
    ACTIONS(297), 2,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
    STATE(47), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1794] = 6,
    ACTIONS(301), 1,
      aux_sym_num_lit_token1,
    ACTIONS(306), 1,
      anon_sym_SQUOTE,
    ACTIONS(309), 1,
      anon_sym_COMMA,
    ACTIONS(304), 2,
      anon_sym_COLON,
      anon_sym_AT,
    STATE(47), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
    ACTIONS(312), 3,
      anon_sym_AT_COLON,
      anon_sym_COLON_AT,
      aux_sym_format_directive_type_token11,
  [1817] = 4,
    ACTIONS(314), 1,
      anon_sym_TILDE,
    ACTIONS(317), 1,
      anon_sym_DQUOTE,
    ACTIONS(319), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(48), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1832] = 4,
    ACTIONS(47), 1,
      anon_sym_DQUOTE,
    ACTIONS(322), 1,
      anon_sym_TILDE,
    ACTIONS(324), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(48), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1847] = 5,
    ACTIONS(29), 1,
      aux_sym_num_lit_token1,
    ACTIONS(33), 1,
      anon_sym_SQUOTE,
    ACTIONS(295), 1,
      anon_sym_COMMA,
    ACTIONS(299), 1,
      aux_sym_format_directive_type_token11,
    STATE(47), 2,
      sym__format_token,
      aux_sym_format_modifiers_repeat1,
  [1864] = 4,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(328), 1,
      anon_sym_DQUOTE,
    ACTIONS(330), 2,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
    STATE(49), 2,
      sym_format_specifier,
      aux_sym_str_lit_repeat1,
  [1879] = 1,
    ACTIONS(332), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1886] = 1,
    ACTIONS(334), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1893] = 1,
    ACTIONS(336), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1900] = 1,
    ACTIONS(338), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1907] = 1,
    ACTIONS(340), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1914] = 1,
    ACTIONS(342), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1921] = 1,
    ACTIONS(344), 4,
      anon_sym_TILDE,
      anon_sym_DQUOTE,
      aux_sym_str_lit_token1,
      aux_sym_str_lit_token2,
  [1928] = 1,
    ACTIONS(346), 1,
      aux_sym__kwd_unqualified_token1,
  [1932] = 1,
    ACTIONS(348), 1,
      aux_sym__format_token_token1,
  [1936] = 1,
    ACTIONS(350), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 66,
  [SMALL_STATE(4)] = 132,
  [SMALL_STATE(5)] = 195,
  [SMALL_STATE(6)] = 248,
  [SMALL_STATE(7)] = 312,
  [SMALL_STATE(8)] = 380,
  [SMALL_STATE(9)] = 448,
  [SMALL_STATE(10)] = 516,
  [SMALL_STATE(11)] = 580,
  [SMALL_STATE(12)] = 645,
  [SMALL_STATE(13)] = 710,
  [SMALL_STATE(14)] = 775,
  [SMALL_STATE(15)] = 840,
  [SMALL_STATE(16)] = 905,
  [SMALL_STATE(17)] = 970,
  [SMALL_STATE(18)] = 1035,
  [SMALL_STATE(19)] = 1100,
  [SMALL_STATE(20)] = 1133,
  [SMALL_STATE(21)] = 1175,
  [SMALL_STATE(22)] = 1217,
  [SMALL_STATE(23)] = 1244,
  [SMALL_STATE(24)] = 1271,
  [SMALL_STATE(25)] = 1294,
  [SMALL_STATE(26)] = 1317,
  [SMALL_STATE(27)] = 1340,
  [SMALL_STATE(28)] = 1363,
  [SMALL_STATE(29)] = 1386,
  [SMALL_STATE(30)] = 1409,
  [SMALL_STATE(31)] = 1432,
  [SMALL_STATE(32)] = 1459,
  [SMALL_STATE(33)] = 1482,
  [SMALL_STATE(34)] = 1505,
  [SMALL_STATE(35)] = 1528,
  [SMALL_STATE(36)] = 1551,
  [SMALL_STATE(37)] = 1574,
  [SMALL_STATE(38)] = 1597,
  [SMALL_STATE(39)] = 1620,
  [SMALL_STATE(40)] = 1643,
  [SMALL_STATE(41)] = 1666,
  [SMALL_STATE(42)] = 1689,
  [SMALL_STATE(43)] = 1711,
  [SMALL_STATE(44)] = 1729,
  [SMALL_STATE(45)] = 1751,
  [SMALL_STATE(46)] = 1769,
  [SMALL_STATE(47)] = 1794,
  [SMALL_STATE(48)] = 1817,
  [SMALL_STATE(49)] = 1832,
  [SMALL_STATE(50)] = 1847,
  [SMALL_STATE(51)] = 1864,
  [SMALL_STATE(52)] = 1879,
  [SMALL_STATE(53)] = 1886,
  [SMALL_STATE(54)] = 1893,
  [SMALL_STATE(55)] = 1900,
  [SMALL_STATE(56)] = 1907,
  [SMALL_STATE(57)] = 1914,
  [SMALL_STATE(58)] = 1921,
  [SMALL_STATE(59)] = 1928,
  [SMALL_STATE(60)] = 1932,
  [SMALL_STATE(61)] = 1936,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(10),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2),
  [51] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(6),
  [54] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(34),
  [57] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(59),
  [60] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(11),
  [63] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(12),
  [66] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(51),
  [69] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(6),
  [72] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(37),
  [75] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(37),
  [78] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(8),
  [81] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(13),
  [84] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_repeat1, 2), SHIFT_REPEAT(14),
  [87] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [91] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [97] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [99] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(9),
  [102] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(34),
  [105] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(59),
  [108] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(11),
  [111] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(12),
  [114] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(51),
  [117] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(42),
  [120] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(42),
  [123] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(37),
  [126] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(37),
  [129] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(8),
  [132] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10),
  [134] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(13),
  [137] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 2, .production_id = 10), SHIFT_REPEAT(14),
  [140] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source, 1),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [144] = {.entry = {.count = 1, .reusable = false}}, SHIFT(6),
  [146] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [150] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [154] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [156] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [162] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [166] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [168] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [170] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [174] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [178] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [182] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [186] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [188] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_prefix_parameters, 1),
  [190] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_format_prefix_parameters, 1),
  [192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [194] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 1),
  [196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_modifiers, 2),
  [198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 3, .production_id = 7),
  [200] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 3, .production_id = 7),
  [202] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 4),
  [204] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 4),
  [206] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 2, .production_id = 4),
  [208] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 2, .production_id = 4),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 7),
  [212] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 3, .production_id = 7),
  [214] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 3),
  [216] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quasi_quoting_lit, 2, .production_id = 3),
  [218] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 3),
  [220] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 2, .production_id = 3),
  [222] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_kwd_lit, 2, .production_id = 6),
  [224] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_kwd_lit, 2, .production_id = 6),
  [226] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2), SHIFT_REPEAT(31),
  [229] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [231] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_quoting_lit_repeat1, 2),
  [233] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 3, .production_id = 7),
  [235] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 3, .production_id = 7),
  [237] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [239] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list_lit, 1, .production_id = 2),
  [241] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_num_lit, 1),
  [243] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_num_lit, 1),
  [245] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__bare_list_lit, 3, .production_id = 9),
  [247] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__bare_list_lit, 3, .production_id = 9),
  [249] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 2),
  [251] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 2),
  [253] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [255] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_sym_lit, 1, .production_id = 1),
  [257] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_str_lit, 3),
  [259] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_str_lit, 3),
  [261] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 7),
  [263] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquote_splicing_lit, 3, .production_id = 7),
  [265] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoting_lit, 2, .production_id = 3),
  [267] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoting_lit, 2, .production_id = 3),
  [269] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoting_lit, 2, .production_id = 3),
  [271] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_quoting_lit, 2, .production_id = 3),
  [273] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 5),
  [275] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym__bare_list_lit_repeat1, 1, .production_id = 5),
  [277] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 2),
  [279] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 2),
  [281] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [283] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 1),
  [285] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [287] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [289] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__format_token, 1, .production_id = 8),
  [291] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__format_token, 1, .production_id = 8),
  [293] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [295] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [297] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [299] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [301] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(45),
  [304] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [306] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(60),
  [309] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2), SHIFT_REPEAT(47),
  [312] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_format_modifiers_repeat1, 2),
  [314] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(4),
  [317] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2),
  [319] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_str_lit_repeat1, 2), SHIFT_REPEAT(48),
  [322] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [324] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [326] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [328] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [330] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [332] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 11),
  [334] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2, .production_id = 12),
  [336] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 3),
  [338] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 2),
  [340] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 2),
  [342] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_directive_type, 1),
  [344] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_format_specifier, 4),
  [346] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [348] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [350] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
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
