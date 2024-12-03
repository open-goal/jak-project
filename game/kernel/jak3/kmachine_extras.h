#pragma once
#include <optional>
#include <string>

#include "common/common_types.h"
#include "common/util/json_util.h"

namespace jak3 {
namespace kmachine_extras {
void update_discord_rpc(u32 discord_info);
void pc_set_levels(u32 lev_list);
void pc_set_active_levels(u32 lev_list);
u32 alloc_vagdir_names(u32 heap_sym);
inline u64 bool_to_symbol(const bool val);
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
s32 pc_sr_mode_get_practice_entries_amount();
void pc_sr_mode_get_practice_entry_name(s32 entry_index, u32 name_str_ptr);
void pc_sr_mode_get_practice_entry_continue_point(s32 entry_index, u32 name_str_ptr);
s32 pc_sr_mode_get_practice_entry_history_success(s32 entry_index);
s32 pc_sr_mode_get_practice_entry_history_attempts(s32 entry_index);
s32 pc_sr_mode_get_practice_entry_session_success(s32 entry_index);
s32 pc_sr_mode_get_practice_entry_session_attempts(s32 entry_index);
void pc_sr_mode_get_practice_entry_avg_time(s32 entry_index, u32 time_str_ptr);
void pc_sr_mode_get_practice_entry_fastest_time(s32 entry_index, u32 time_str_ptr);
u64 pc_sr_mode_record_practice_entry_attempt(s32 entry_index, u32 success_bool, u32 time);
void pc_sr_mode_init_practice_info(s32 entry_index, u32 speedrun_practice_obj_ptr);
s32 pc_sr_mode_get_custom_category_amount();
void pc_sr_mode_get_custom_category_name(s32 entry_index, u32 name_str_ptr);
void pc_sr_mode_get_custom_category_continue_point(s32 entry_index, u32 name_str_ptr);
void pc_sr_mode_init_custom_category_info(s32 entry_index, u32 speedrun_custom_category_ptr);
void pc_sr_mode_dump_new_custom_category(u32 speedrun_custom_category_ptr);

struct DiscordInfo {
  float orb_count;          // float
  float gem_count;          // float
  s32 death_count;          // int32
  u32 status;               // string
  u32 level;                // string
  u32 cutscene;             // symbol - bool
  float time_of_day;        // float
  float percent_completed;  // float
  u64 focus_status;         // uint64
  s32 current_vehicle;      // int32
  u32 task;                 // string
};

enum class VehicleType : s32 {
  h_bike_a = 0,
  h_bike_b = 1,
  h_bike_c = 2,
  h_car_a = 3,
  h_car_b = 4,
  h_car_c = 5,
  h_bike_d = 6,
  h_hellcat = 7,
  h_warf = 8,
  h_glider = 9,
  h_sled = 10,
  h_kg_pickup = 11,
  v_turtle = 12,
  v_snake = 13,
  v_scorpion = 14,
  v_toad = 15,
  v_fox = 16,
  v_rhino = 17,
  v_mirage = 18,
  v_x_ride = 19,
  v_marauder = 20,
  v_faccar = 21,
  v_catapult = 22,
  v_marauder_b = 23,
  test_car = 25,
  wbike_test = 26,
  vt27 = 27,
  evan_test_bike = 29,
  Max = 30
};

const std::map<VehicleType, std::string> vehicle_remap = {
    {VehicleType::v_turtle, "Tough Puppy"}, {VehicleType::v_snake, "Sand Shark"},
    {VehicleType::v_toad, "Dune Hopper"},   {VehicleType::v_scorpion, "Gila Stomper"},
    {VehicleType::v_fox, "Heat Seeker"},    {VehicleType::v_rhino, "Slam Dozer"},
    {VehicleType::v_mirage, "Dust Demon"},  {VehicleType::v_x_ride, "Desert Screamer"},
};

inline std::string VehicleTypeToString(VehicleType v) {
  return vehicle_remap.find(v) != vehicle_remap.end() ? vehicle_remap.at(v) : "Unknown";
}

enum class FocusStatus : u64 {
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
  Light = 17,
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
  Invulnerable = 31,
  Turret = 32,
  NoGravity = 33,
  GunNoTarget = 34,
  Max = 64
};

#define FOCUS_TEST(status, foc) (status.test(static_cast<size_t>(foc)))

// To speed up finding the auto-splitter block in GOAL memory
// all this has is a marker for LiveSplit to find, and then the pointer
// to the symbol
struct AutoSplitterBlock {
  const char marker[20] = "UnLiStEdStRaTs_JaK3";
  u64 pointer_to_symbol = 0;
};

extern AutoSplitterBlock g_auto_splitter_block_jak3;

struct SpeedrunPracticeEntryHistoryAttempt {
  std::optional<float> time;
};
void to_json(json& j, const SpeedrunPracticeEntryHistoryAttempt& obj);
void from_json(const json& j, SpeedrunPracticeEntryHistoryAttempt& obj);

struct SpeedrunPracticeEntry {
  std::string name;
  std::string continue_point_name;
  u64 flags;
  u64 completed_task;
  u64 features;
  u64 secrets;
  u64 vehicles;
  std::vector<float> starting_position;
  std::vector<float> starting_rotation;
  std::vector<float> starting_camera_position;
  std::vector<float> starting_camera_rotation;
  std::vector<float> start_zone_v1;
  std::vector<float> start_zone_v2;
  std::optional<std::vector<float>> end_zone_v1;
  std::optional<std::vector<float>> end_zone_v2;
  std::optional<u64> end_task;
  std::map<std::string, std::vector<SpeedrunPracticeEntryHistoryAttempt>> history;
};
void to_json(json& j, const SpeedrunPracticeEntry& obj);
void from_json(const json& j, SpeedrunPracticeEntry& obj);

struct SpeedrunPracticeState {
  s32 current_session_id;
  s32 total_attempts;
  s32 total_successes;
  s32 session_attempts;
  s32 session_successes;
  double total_time;
  float average_time;
  float fastest_time;
};

struct ObjectiveZoneInitParams {
  float v1[4];
  float v2[4];
};

struct Vector {
  float data[4];
};

struct Matrix {
  float data[16];
};

struct SpeedrunPracticeObjective {
  s32 index;
  u8 pad1[4];
  u64 flags;
  u8 completed_task;
  u8 pad2[7];
  u64 features;
  u64 secrets;
  u64 vehicles;
  u32 starting_position;         // Vector
  u32 starting_rotation;         // Vector
  u32 starting_camera_position;  // Vector
  u32 starting_camera_rotation;  // Matrix
  u8 end_task;
  u32 start_zone_init_params;  // ObjectiveZoneInitParams
  u32 start_zone;              // irrelevant for cpp
  u32 end_zone_init_params;    // ObjectiveZoneInitParams
  u32 end_zone;                // irrelevant for cpp
};

struct SpeedrunCustomCategoryEntry {
  std::string name;
  u64 secrets;
  u64 features;
  u64 vehicles;
  u64 forbidden_features;
  u64 cheats;
  std::string continue_point_name;
  u64 completed_task;
};
void to_json(json& j, const SpeedrunCustomCategoryEntry& obj);
void from_json(const json& j, SpeedrunCustomCategoryEntry& obj);

struct SpeedrunCustomCategory {
  s32 index;
  u64 secrets;
  u64 features;
  u64 vehicles;
  u64 forbidden_features;
  u64 cheats;
  u8 completed_task;
};

}  // namespace kmachine_extras
}  // namespace jak3
