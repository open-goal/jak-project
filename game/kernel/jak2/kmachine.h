#pragma once

#include <optional>

#include "common/common_types.h"
#include "common/sqlite/sqlite.h"

namespace jak2 {
void InitParms(int argc, const char* const* argv);
void InitIOP();
int InitMachine();
int ShutdownMachine();
void InitMachineScheme();

extern sqlite::SQLiteDatabase sql_db;
void initialize_sql_db();
sqlite::GenericResponse run_sql_query(const std::string& query);

struct MouseInfo {
  //  ((active symbol :offset-assert 4)
  u32 active;
  //  (cursor basic :offset-assert 8)
  u32 cursor;
  //  (valid symbol :offset-assert 12)
  u32 valid;
  //  (id uint8 :offset-assert 16)
  u8 id;
  u8 pad;
  //  (status uint16 :offset-assert 18)
  u16 status;
  //  (button0 uint16 :offset-assert 20)
  u16 button0;
  //  (deltax int8 :offset-assert 22)
  s8 deltax;
  //  (deltay int8 :offset-assert 23)
  s8 deltay;
  //  (wheel uint8 :offset-assert 24)
  u8 wheel;
  u8 pad1[3];
  //  (change-time time-frame :offset-assert 32)
  //  (button0-abs uint32 3 :offset-assert 40)
  //  (button0-shadow-abs uint32 1 :offset-assert 52)
  //  (button0-rel uint32 3 :offset-assert 56)
  //  (pos vector 2 :inline :offset-assert 80)
  u32 pad2[13];
  //  (posx float :offset 80)
  float posx;
  //  (posy float :offset 84)
  float posy;
  //  (oldposx float :offset 96 :do-not-decompile)
  //  (oldposy float :offset 100)
  //  (speedx float :offset 92)
  //  (speedy float :offset 108)
};

enum class FocusStatus : u32 {
  Disable = 0,
  Dead = 1,
  Ignore = 2,
  Inactive = 3,
  Dangerous = 4,
  InAir = 5,
  Hit = 6,
  Grabbed = 7,
  InHead = 8,
  TouchWater = 9,
  OnWater = 10,
  UnderWater = 11,
  EdgeGrab = 12,
  Pole = 13,
  PilotRiding = 14,
  Flut = 15,
  Tube = 16,
  Ice = 17,
  Board = 18,
  Gun = 19,
  Pilot = 20,
  Mech = 21,
  Dark = 22,
  Rail = 23,
  Halfpipe = 24,
  Carry = 25,
  Super = 26,
  Shooting = 27,
  Indax = 28,
  Arrestable = 29,
  Teleporting = 30,
  FS31 = 31,
  Max = 32
};

#define FOCUS_TEST(status, foc) (status.test(static_cast<size_t>(foc)))

struct DiscordInfo {
  float orb_count;          // float
  float gem_count;          // float
  u32 death_count;          // int32
  u32 status;               // string
  u32 level;                // string
  u32 cutscene;             // symbol - bool
  float time_of_day;        // float
  float percent_completed;  // float
  u32 focus_status;         // uint32
  u32 task;                 // string
};
// To speedup finding the auto-splitter block in GOAL memory
// all this has is a marker for LiveSplit to find, and then the pointer
// to the symbol
struct AutoSplitterBlock {
  const char marker[20] = "UnLiStEdStRaTs_JaK2";
  u64 pointer_to_symbol = 0;
};

extern AutoSplitterBlock gAutoSplitterBlock;

}  // namespace jak2
