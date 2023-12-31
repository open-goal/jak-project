#pragma once
#include <optional>
#include <string>

#include <common/common_types.h>

namespace kmachine_extras {
void update_discord_rpc(u32 discord_info);
void pc_set_levels(u32 lev_list);
void pc_set_active_levels(u32 lev_list);
u32 alloc_vagdir_names(u32 heap_sym);
inline u64 bool_to_symbol(const bool val);
// TODO - move to common
void encode_utf8_string(u32 src_str_ptr, u32 str_dest_ptr);
void init_autosplit_struct();
void callback_fetch_external_speedrun_times(bool success,
                                            const std::string& cache_id,
                                            std::optional<std::string> result);
void callback_fetch_external_race_times(bool success,
                                        const std::string& cache_id,
                                        std::optional<std::string> result);
void callback_fetch_external_highscores(bool success,
                                        const std::string& cache_id,
                                        std::optional<std::string> result);
void pc_fetch_external_speedrun_times(u32 speedrun_id_ptr);
void pc_fetch_external_race_times(u32 race_id_ptr);
void pc_fetch_external_highscores(u32 highscore_id_ptr);
void pc_get_external_speedrun_time(u32 speedrun_id_ptr,
                                   s32 index,
                                   u32 name_dest_ptr,
                                   u32 time_dest_ptr);
void pc_get_external_race_time(u32 race_id_ptr, s32 index, u32 name_dest_ptr, u32 time_dest_ptr);
void pc_get_external_highscore(u32 highscore_id_ptr,
                               s32 index,
                               u32 name_dest_ptr,
                               u32 time_dest_ptr);
s32 pc_get_num_external_speedrun_times(u32 speedrun_id_ptr);
s32 pc_get_num_external_race_times(u32 race_id_ptr);
s32 pc_get_num_external_highscores(u32 highscore_id_ptr);

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

// To speedup finding the auto-splitter block in GOAL memory
// all this has is a marker for LiveSplit to find, and then the pointer
// to the symbol
struct AutoSplitterBlock {
  const char marker[20] = "UnLiStEdStRaTs_JaK2";
  u64 pointer_to_symbol = 0;
};

extern AutoSplitterBlock g_auto_splitter_block_jak2;

}  // namespace kmachine_extras
