#include <string>
#include <unordered_map>
#include <unordered_set>

#include "common/goal_constants.h"
#include "common/listener_common.h"
#include "common/symbols.h"

#include "all_jak1_symbols.h"
#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kboot.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/common/memory_layout.h"
#include "game/kernel/jak1/fileio.h"
#include "game/kernel/jak1/kscheme.h"
#include "gtest/gtest.h"

using namespace jak1_symbols;
using namespace jak1;

TEST(Kernel, strend) {
  char test[] = "test";
  char* end = strend(test);
  EXPECT_TRUE(*end == 0);
  EXPECT_TRUE(end - test == 4);
}

TEST(Kernel, kstrcpy) {
  char buffer[24];
  memset(buffer, 1, 24);
  kstrcpy(buffer, "test");
  EXPECT_TRUE(buffer[0] == 't');
  EXPECT_TRUE(buffer[1] == 'e');
  EXPECT_TRUE(buffer[2] == 's');
  EXPECT_TRUE(buffer[3] == 't');
  EXPECT_TRUE(buffer[4] == '\0');
  EXPECT_TRUE(buffer[5] == 1);
}

TEST(Kernel, kstrcpyup) {
  char buffer[24];
  memset(buffer, 1, 24);
  kstrcpyup(buffer, "tesT");
  EXPECT_TRUE(buffer[0] == 'T');
  EXPECT_TRUE(buffer[1] == 'E');
  EXPECT_TRUE(buffer[2] == 'S');
  EXPECT_TRUE(buffer[3] == 'T');
  EXPECT_TRUE(buffer[4] == '\0');
  EXPECT_TRUE(buffer[5] == 1);
}

TEST(Kernel, kstrcat) {
  char buffer[24] = "test";
  kstrcat(buffer, "cat");
  const char expected[] = "testcat";
  for (int i = 0; i < 8; i++) {
    EXPECT_TRUE(expected[i] == buffer[i]);
  }
}

TEST(Kernel, kstrncat) {
  {
    char buffer[24] = "test";
    kstrncat(buffer, "cat", 6);
    const char expected[] = "testca";
    for (int i = 0; i < 7; i++) {
      EXPECT_TRUE(expected[i] == buffer[i]);
    }
  }

  {
    char buffer[24] = "test";
    kstrncat(buffer, "cat", 80);
    const char expected[] = "testcat";
    for (int i = 0; i < 8; i++) {
      EXPECT_TRUE(expected[i] == buffer[i]);
    }
  }
}

TEST(Kernel, kstrinsert) {
  char buffer[24];
  memset(buffer, 1, 24);
  buffer[0] = 't';
  buffer[1] = 'o';
  buffer[2] = '\0';

  EXPECT_TRUE(buffer == kstrinsert(buffer, 'a', 2));

  EXPECT_TRUE(buffer[0] == 'a');
  EXPECT_TRUE(buffer[1] == 'a');
  EXPECT_TRUE(buffer[2] == 't');
  EXPECT_TRUE(buffer[3] == 'o');
  EXPECT_TRUE(buffer[4] == '\0');
  EXPECT_TRUE(buffer[5] == 1);
}

TEST(Kernel, basename) {
  std::string x;

  char name0[] = "test1.asdf";
  x = basename_goal(name0);
  EXPECT_EQ(x, "test1.asdf");

  char name1[] = "a/b/test1.asdf";
  x = basename_goal(name1);
  EXPECT_EQ(x, "test1.asdf");

  char name2[] = "a\\b\\test1.asdf";
  x = basename_goal(name2);
  EXPECT_EQ(x, "test1.asdf");
}

TEST(Kernel, DecodeFileName) {
  std::string x;
  x = DecodeFileName("$TEXTURE/beans");
  EXPECT_EQ(x, "out/jak1/obj/beans.go");

  x = DecodeFileName("$ART_GROUP/stuff");
  EXPECT_EQ(x, "data/art-group6/stuff-ag.go");

  x = DecodeFileName("$LEVEL/my-level");
  EXPECT_EQ(x, "data/level30/my-level-bt.go");

  x = DecodeFileName("$LEVEL/my-level.123");
  EXPECT_EQ(x, "data/level30/my-level.123");

  x = DecodeFileName("$DATA/my-data");
  EXPECT_EQ(x, "out/jak1/obj/my-data.go");

  x = DecodeFileName("$CODE/my-code");
  EXPECT_EQ(x, "game/obj/my-code.o");

  x = DecodeFileName("$RES/my-res");
  EXPECT_EQ(x, "data/res1/my-res.go");

  x = DecodeFileName("asdf");
  EXPECT_EQ(x, "game/obj/asdf.o");
}

TEST(Kernel, reverse) {
  char in1[] = "asdf";
  reverse(in1);
  char in2[] = "asdfg";
  reverse(in2);
  EXPECT_EQ("fdsa", std::string(in1));
  EXPECT_EQ("gfdsa", std::string(in2));
}

TEST(Kernel, ftoa) {
  char buffer[128];
  ftoa(buffer, 1.23, 1, ' ', 4, 0);
  EXPECT_EQ("1.2300", std::string(buffer));

  ftoa(buffer, -1.23, 1, ' ', 4, 0);
  EXPECT_EQ("-1.2300", std::string(buffer));

  ftoa(buffer, 1., 1, ' ', 4, 0);
  EXPECT_EQ("1.0000", std::string(buffer));

  ftoa(buffer, 1., 1, ' ', 0, 0);
  EXPECT_EQ("1", std::string(buffer));

  ftoa(buffer, 1., 1, ' ', 1, 0);
  EXPECT_EQ("1.0", std::string(buffer));

  float zero = 0.0f;
  ftoa(buffer, 0.f / zero, 1, ' ', 4, 0);
  EXPECT_EQ("NaN", std::string(buffer));

  ftoa(buffer, 1., 8, ' ', 1, 0);
  EXPECT_EQ("     1.0", std::string(buffer));

  ftoa(buffer, -1., 8, '0', 1, 0);
  EXPECT_EQ("0000-1.0", std::string(buffer));

  ftoa(buffer, 0.f / zero, 8, ' ', 4, 0);
  EXPECT_EQ("     NaN", std::string(buffer));

  ftoa(buffer, 0.1, 1, ' ', 4, 0);
  EXPECT_EQ("0.1000", std::string(buffer));
}

TEST(Kernel, itoa_base_10) {
  char buffer[128];

  kprint_init_globals_common();

  // simple print 1
  kitoa(buffer, 1, 10, -1, ' ', 0);
  EXPECT_EQ("1", std::string(buffer));

  // simple print 0
  kitoa(buffer, 0, 10, -1, ' ', 0);
  EXPECT_EQ("0", std::string(buffer));

  // simple print -1
  kitoa(buffer, -1, 10, -1, ' ', 0);
  EXPECT_EQ("-1", std::string(buffer));

  // print negative which asks to be truncated, but shouldn't be
  kitoa(buffer, -123456, 10, 4, ' ', 0);
  EXPECT_EQ("-123456", std::string(buffer));

  // print an interesting number
  kitoa(buffer, 123321456789, 10, -1, ' ', 0);
  EXPECT_EQ("123321456789", std::string(buffer));

  // MAX
  kitoa(buffer, INT64_MAX, 10, -1, ' ', 0);
  EXPECT_EQ("9223372036854775807", std::string(buffer));

  // MIN
  kitoa(buffer, INT64_MIN, 10, -1, ' ', 0);
  EXPECT_EQ("-9223372036854775808", std::string(buffer));

  // Pad
  kitoa(buffer, 3, 10, 4, 'j', 0);
  EXPECT_EQ("jjj3", std::string(buffer));

  kitoa(buffer, 333, 10, 4, 'j', 0);
  EXPECT_EQ("j333", std::string(buffer));

  kitoa(buffer, 3333, 10, 4, 'j', 0);
  EXPECT_EQ("3333", std::string(buffer));

  kitoa(buffer, 33333, 10, 4, 'j', 0);
  EXPECT_EQ("33333", std::string(buffer));
}

TEST(Kernel, itoa_base_2) {
  char buffer[128];

  kprint_init_globals_common();
  kitoa(buffer, 1, 2, -1, ' ', 0);
  EXPECT_EQ("1", std::string(buffer));

  kitoa(buffer, -1, 2, -1, ' ', 0);
  EXPECT_EQ("1111111111111111111111111111111111111111111111111111111111111111",
            std::string(buffer));

  kitoa(buffer, -1, 2, 0, ' ', 0);
  EXPECT_EQ("1111111111111111111111111111111111111111111111111111111111111111",
            std::string(buffer));

  kitoa(buffer, -1, 2, 5, ' ', 0);
  EXPECT_EQ("-11111", std::string(buffer));

  kitoa(buffer, 0, 2, -1, ' ', 0);
  EXPECT_EQ("0", std::string(buffer));

  kitoa(buffer, INT64_MAX, 2, -1, ' ', 0);
  EXPECT_EQ("111111111111111111111111111111111111111111111111111111111111111", std::string(buffer));

  kitoa(buffer, 476, 2, -1, ' ', 0);
  EXPECT_EQ("111011100", std::string(buffer));

  kitoa(buffer, 476, 2, 11, 'j', 0);
  EXPECT_EQ("jj111011100", std::string(buffer));

  kitoa(buffer, INT64_MIN, 2, 0, ' ', 0);
  EXPECT_EQ("1000000000000000000000000000000000000000000000000000000000000000",
            std::string(buffer));
}

TEST(Kernel, itoa_base_16) {
  char buffer[128];

  kprint_init_globals_common();
  kitoa(buffer, 1, 16, -1, ' ', 0);
  EXPECT_EQ("1", std::string(buffer));

  kitoa(buffer, -1, 16, -1, ' ', 0);
  EXPECT_EQ("ffffffffffffffff", std::string(buffer));

  kitoa(buffer, -1, 16, 5, ' ', 0);
  EXPECT_EQ("-fffff", std::string(buffer));

  kitoa(buffer, 0, 16, -1, ' ', 0);
  EXPECT_EQ("0", std::string(buffer));

  kitoa(buffer, INT64_MAX, 16, -1, ' ', 0);
  EXPECT_EQ("7fffffffffffffff", std::string(buffer));

  kitoa(buffer, 195935983, 16, -1, ' ', 0);
  EXPECT_EQ("badbeef", std::string(buffer));

  kitoa(buffer, 195935983, 16, 11, 'j', 0);
  EXPECT_EQ("jjjjbadbeef", std::string(buffer));

  kitoa(buffer, INT64_MIN, 16, 0, ' ', 0);
  EXPECT_EQ("8000000000000000", std::string(buffer));
}

namespace {
void setup_hack_heaps(void* mem, int size) {
  g_ee_main_mem = (u8*)mem;
  int heap_size = (size - HEAP_START) / 2;
  MasterDebug = 1;
  EXPECT_TRUE(heap_size > 8 * 1024 * 1024);
  kmalloc_init_globals_common();
  kprint_init_globals_common();

  kinitheap(kglobalheap, Ptr<u8>(HEAP_START), heap_size);
  kinitheap(kdebugheap, Ptr<u8>(HEAP_START + heap_size), heap_size);
  init_output();
  init_crc();

  // create a fake symbol table
  auto symbol_table =
      kmalloc(kglobalheap, jak1::SYM_TABLE_MEM_SIZE, KMALLOC_MEMSET, "symbol-table").cast<u32>();
  s7 = symbol_table + (jak1::GOAL_MAX_SYMBOLS / 2) * 8 + BASIC_OFFSET;
  SymbolTable2 = symbol_table + BASIC_OFFSET;
  LastSymbol = symbol_table + 0xff00;
  NumSymbols = 0;

  // set up the empty pair (might not be needed?)
  *(s7 + FIX_SYM_EMPTY_CAR) = (s7 + FIX_SYM_EMPTY_PAIR).offset;
  *(s7 + FIX_SYM_EMPTY_CDR) = (s7 + FIX_SYM_EMPTY_PAIR).offset;

  // need to set up 'global fixed symbol so allocating memory works.
  *(s7 + FIX_SYM_GLOBAL_HEAP) = kglobalheap.offset;

  // allocate fundamental types
  alloc_and_init_type((s7 + FIX_SYM_TYPE_TYPE).cast<Symbol>(), 9);
  alloc_and_init_type((s7 + FIX_SYM_SYMBOL_TYPE).cast<Symbol>(), 9);
  alloc_and_init_type((s7 + FIX_SYM_STRING_TYPE).cast<Symbol>(), 9);
  alloc_and_init_type((s7 + FIX_SYM_FUNCTION_TYPE).cast<Symbol>(), 9);
  // booleans
  set_fixed_symbol(FIX_SYM_FALSE, "#f", s7.offset + FIX_SYM_FALSE);
  set_fixed_symbol(FIX_SYM_TRUE, "#t", s7.offset + FIX_SYM_TRUE);
  // heap symbols
  set_fixed_symbol(FIX_SYM_GLOBAL_HEAP, "global", kglobalheap.offset);
}
}  // namespace

TEST(Kernel, PrintBuffer) {
  // hack to setup
  constexpr int size = 32 * 1024 * 1024;
  auto mem = new u8[size];
  setup_hack_heaps(mem, size);
  clear_print();
  cprintf("test!\n");

  std::string result = PrintBufArea.cast<char>().c() + sizeof(ListenerMessageHeader);
  EXPECT_EQ(result, "test!\n");

  delete[] mem;

  // more complicated tests for format will be done from within GOAL.
}

TEST(Kernel, HashTable) {
  constexpr int size = 32 * 1024 * 1024;
  auto mem = new u8[size];
  setup_hack_heaps(mem, size);

  // check crc32 - the game has a hardcoded hash for _empty_ so we should check
  // that our implementation matches this hardcoded value.

  const char empty_sym_name[] = "_empty_";
  const char wrong_sym_name[] = "_Empty_";
  EXPECT_EQ(EMPTY_HASH, crc32((const u8*)empty_sym_name, strlen(empty_sym_name)));
  EXPECT_NE(EMPTY_HASH, crc32((const u8*)wrong_sym_name, strlen(wrong_sym_name)));

  std::unordered_map<std::string, u32> symbol_locations;
  std::unordered_set<u32> unique_locations;

  for (auto name : all_syms) {
    auto loc = intern_from_c(name).offset;
    symbol_locations[name] = loc;
    unique_locations.insert(loc);
  }

  EXPECT_EQ(7941, symbol_locations.size());
  EXPECT_EQ(7941, unique_locations.size());

  for (auto name : all_syms) {
    EXPECT_EQ(symbol_locations.at(name), intern_from_c(name).offset);
  }

  EXPECT_EQ(intern_from_c("global").offset - s7.offset, FIX_SYM_GLOBAL_HEAP);
  EXPECT_EQ(intern_from_c("#f").offset - s7.offset, 0);
  EXPECT_EQ(intern_from_c("#t").offset - s7.offset, 8);
  EXPECT_EQ(intern_from_c("_empty_").offset - s7.offset, FIX_SYM_EMPTY_PAIR);

  // expect no crc32 hash collisions. This doesn't matter, but it's nice to know.
  std::unordered_set<u32> crc32s;
  for (auto name : all_syms) {
    crc32s.insert(crc32((const u8*)name, strlen(name)));
  }
  EXPECT_EQ(7941, crc32s.size());

  delete[] mem;
}
