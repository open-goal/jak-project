#include "kscheme.h"

// total number of symbols in the table
s32 NumSymbols;

// value of the GOAL s7 register, pointing to the middle of the symbol table
Ptr<u32> s7;

void kscheme_init_globals_common() {
  NumSymbols = 0;
  s7.offset = 0;
}