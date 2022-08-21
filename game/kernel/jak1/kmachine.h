#pragma once

#include "common/common_types.h"
// Discord RPC
struct DiscordRichPresence;
extern int gDiscordRpcEnabled;
extern int64_t gStartTime;
namespace jak1 {
/*!
 * Initialize global variables based on command line parameters
 */
void InitParms(int argc, const char* const* argv);
/*!
 * Initialize the I/O Processor
 */
void InitIOP();
/*!
 * Initialze GOAL Runtime
 */
int InitMachine();
/*!
 * Shutdown GOAL runtime.
 */
int ShutdownMachine();

void InitMachineScheme();

struct DiscordInfo {
  u32 fuel;
  u32 money_total;
  u32 buzzer_total;
  u32 deaths;
  u32 status;
  u32 level;
  u32 cutscene;   // check if cutscene is playing
  u32 ogreboss;   // are we fighting ogreboss?
  u32 plantboss;  // are we fighting plant-boss?
  u32 racer;      // are we driving the zoomer?
  u32 flutflut;   // are we riding on flut flut?
  u32 time_of_day;
};

struct AutoSplitInfoJak1 {
  u32 num_power_cells;
  u32 num_orbs;
  u32 num_scout_flies;
  u32 in_cutscene;
  u32 res_jungle_eggtop;
  u32 res_jungle_lurkerm;
  u32 res_jungle_tower;
  u32 res_jungle_fishgame;
  u32 res_jungle_plant;
  u32 res_jungle_buzzer;
  u32 res_jungle_canyon_end;
  u32 res_jungle_temple_door;
  u32 res_village1_yakow;
  u32 res_village1_mayor_money;
  u32 res_village1_uncle_money;
  u32 res_village1_oracle_money1;
  u32 res_village1_oracle_money2;
  u32 res_beach_ecorocks;
  u32 res_beach_pelican;
  u32 res_beach_flutflut;
  u32 res_beach_seagull;
  u32 res_beach_cannon;
  u32 res_beach_buzzer;
  u32 res_beach_gimmie;
  u32 res_beach_sentinel;
  u32 res_misty_muse;
  u32 res_misty_boat;
  u32 res_misty_warehouse;
  u32 res_misty_cannon;
  u32 res_misty_bike;
  u32 res_misty_buzzer;
  u32 res_misty_bike_jump;
  u32 res_misty_eco_challenge;
  u32 res_village2_gambler_money;
  u32 res_village2_geologist_money;
  u32 res_village2_warrior_money;
  u32 res_village2_oracle_money1;
  u32 res_village2_oracle_money2;
  u32 res_swamp_billy;
  u32 res_swamp_flutflut;
  u32 res_swamp_battle;
  u32 res_swamp_tether_1;
  u32 res_swamp_tether_2;
  u32 res_swamp_tether_3;
  u32 res_swamp_tether_4;
  u32 res_swamp_buzzer;
  u32 res_sunken_platforms;
  u32 res_sunken_pipe;
  u32 res_sunken_slide;
  u32 res_sunken_room;
  u32 res_sunken_sharks;
  u32 res_sunken_buzzer;
  u32 res_sunken_top_of_helix;
  u32 res_sunken_spinning_room;
  u32 res_rolling_race;
  u32 res_rolling_robbers;
  u32 res_rolling_moles;
  u32 res_rolling_plants;
  u32 res_rolling_lake;
  u32 res_rolling_buzzer;
  u32 res_rolling_ring_chase_1;
  u32 res_rolling_ring_chase_2;
  u32 res_snow_eggtop;
  u32 res_snow_ram;
  u32 res_snow_fort;
  u32 res_snow_ball;
  u32 res_snow_bunnies;
  u32 res_snow_buzzer;
  u32 res_snow_bumpers;
  u32 res_snow_cage;
  u32 res_firecanyon_buzzer;
  u32 res_firecanyon_end;
  u32 res_citadel_sage_green;
  u32 res_citadel_sage_blue;
  u32 res_citadel_sage_red;
  u32 res_citadel_sage_yellow;
  u32 res_village3_extra1;
  u32 res_village1_buzzer;
  u32 res_village2_buzzer;
  u32 res_village3_buzzer;
  u32 res_cave_gnawers;
  u32 res_cave_dark_crystals;
  u32 res_cave_dark_climb;
  u32 res_cave_robot_climb;
  u32 res_cave_swing_poles;
  u32 res_cave_spider_tunnel;
  u32 res_cave_platforms;
  u32 res_cave_buzzer;
  u32 res_ogre_boss;
  u32 res_ogre_end;
  u32 res_ogre_buzzer;
  u32 res_lavatube_end;
  u32 res_lavatube_buzzer;
  u32 res_citadel_buzzer;
  u32 res_training_gimmie;
  u32 res_training_door;
  u32 res_training_climb;
  u32 res_training_buzzer;
  u32 res_village3_miner_money1;
  u32 res_village3_miner_money2;
  u32 res_village3_miner_money3;
  u32 res_village3_miner_money4;
  u32 res_village3_oracle_money1;
  u32 res_village3_oracle_money2;
  u32 res_swamp_arm;
  u32 res_red_eggtop;
  u32 res_lavatube_balls;
  u32 res_intro;
  u32 res_ogre_secret;
  u32 int_finalboss_movies;
};
}  // namespace jak1
