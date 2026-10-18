#pragma once
#include "common/common_types.h"
#include "common/sqlite/sqlite.h"

namespace jak3 {
void InitParms(int argc, const char* const* argv);
void InitMachineScheme();
int InitMachine();
int ShutdownMachine();

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

// (deftype keybd-info (basic)
//   ((active  symbol)
//    (valid   symbol)
//    (kdata   uint8  16)
//    (keys    uint8  256)
//    )
//   (:methods
//     (new (symbol type) _type_)
//     )
//   )
struct KeybdInfo {
  u32 active;
  u32 valid;
  u8 kdata[16];
  u8 keys[256];
};
}  // namespace jak3