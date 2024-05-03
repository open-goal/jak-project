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
// TODO - move to common
void encode_utf8_string(u32 src_str_ptr, u32 str_dest_ptr);

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

}  // namespace kmachine_extras
}  // namespace jak3
