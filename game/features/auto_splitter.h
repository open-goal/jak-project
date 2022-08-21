#pragma once

#include <common/common_types.h>

// Contains everything relevant to an autosplitting script
struct AutoSplitterBlockJak1 {
  const char marker[20] = "UnLiStEdStRaTs_JaK1";  // DONT CHANGE THIS EVER
  // general stats
  u32 num_power_cells = 0;
  u32 num_orbs = 0;
  u32 num_scout_flies = 0;
  const char padding_stats[200] = "padding!";  // padding for future growth

  // timing, loading, and cutscene control info
  u32 game_hash = 0;
  u8 in_cutscene = 0;
  u8 is_loading = 0;
  const char padding_controls[200] = "padding!";  // padding for future growth

  // need-resolution flags
  u8 res_training_gimmie = 0;
  u8 res_training_door = 0;
  u8 res_training_climb = 0;
  u8 res_training_buzzer = 0;
  u8 res_jungle_eggtop = 0;
  u8 res_jungle_lurkerm = 0;
  u8 res_jungle_tower = 0;
  u8 res_jungle_fishgame = 0;
  u8 res_jungle_plant = 0;
  u8 res_jungle_buzzer = 0;
  u8 res_jungle_canyon_end = 0;
  u8 res_jungle_temple_door = 0;
  u8 res_village1_yakow = 0;
  u8 res_village1_mayor_money = 0;
  u8 res_village1_uncle_money = 0;
  u8 res_village1_oracle_money1 = 0;
  u8 res_village1_oracle_money2 = 0;
  u8 res_village1_buzzer = 0;
  u8 res_beach_ecorocks = 0;
  u8 res_beach_pelican = 0;
  u8 res_beach_flutflut = 0;
  u8 res_beach_seagull = 0;
  u8 res_beach_cannon = 0;
  u8 res_beach_buzzer = 0;
  u8 res_beach_gimmie = 0;
  u8 res_beach_sentinel = 0;
  u8 res_misty_muse = 0;
  u8 res_misty_boat = 0;
  u8 res_misty_warehouse = 0;
  u8 res_misty_cannon = 0;
  u8 res_misty_bike = 0;
  u8 res_misty_buzzer = 0;
  u8 res_misty_bike_jump = 0;
  u8 res_misty_eco_challenge = 0;
  u8 res_village2_gambler_money = 0;
  u8 res_village2_geologist_money = 0;
  u8 res_village2_warrior_money = 0;
  u8 res_village2_oracle_money1 = 0;
  u8 res_village2_oracle_money2 = 0;
  u8 res_village2_buzzer = 0;
  u8 res_swamp_billy = 0;
  u8 res_swamp_flutflut = 0;
  u8 res_swamp_battle = 0;
  u8 res_swamp_tether_1 = 0;
  u8 res_swamp_tether_2 = 0;
  u8 res_swamp_tether_3 = 0;
  u8 res_swamp_tether_4 = 0;
  u8 res_swamp_buzzer = 0;
  u8 res_swamp_arm = 0;
  u8 res_sunken_platforms = 0;
  u8 res_sunken_pipe = 0;
  u8 res_sunken_slide = 0;
  u8 res_sunken_room = 0;
  u8 res_sunken_sharks = 0;
  u8 res_sunken_buzzer = 0;
  u8 res_sunken_top_of_helix = 0;
  u8 res_sunken_spinning_room = 0;
  u8 res_rolling_race = 0;
  u8 res_rolling_robbers = 0;
  u8 res_rolling_moles = 0;
  u8 res_rolling_plants = 0;
  u8 res_rolling_lake = 0;
  u8 res_rolling_buzzer = 0;
  u8 res_rolling_ring_chase_1 = 0;
  u8 res_rolling_ring_chase_2 = 0;
  u8 res_snow_eggtop = 0;
  u8 res_snow_ram = 0;
  u8 res_snow_fort = 0;
  u8 res_snow_ball = 0;
  u8 res_snow_bunnies = 0;
  u8 res_snow_buzzer = 0;
  u8 res_snow_bumpers = 0;
  u8 res_snow_cage = 0;
  u8 res_red_eggtop = 0;
  u8 res_firecanyon_buzzer = 0;
  u8 res_firecanyon_end = 0;
  u8 res_citadel_sage_green = 0;
  u8 res_citadel_sage_blue = 0;
  u8 res_citadel_sage_red = 0;
  u8 res_citadel_sage_yellow = 0;
  u8 res_citadel_buzzer = 0;
  u8 res_village3_extra1 = 0;
  u8 res_village3_buzzer = 0;
  u8 res_village3_miner_money1 = 0;
  u8 res_village3_miner_money2 = 0;
  u8 res_village3_miner_money3 = 0;
  u8 res_village3_miner_money4 = 0;
  u8 res_village3_oracle_money1 = 0;
  u8 res_village3_oracle_money2 = 0;
  u8 res_cave_gnawers = 0;
  u8 res_cave_dark_crystals = 0;
  u8 res_cave_dark_climb = 0;
  u8 res_cave_robot_climb = 0;
  u8 res_cave_swing_poles = 0;
  u8 res_cave_spider_tunnel = 0;
  u8 res_cave_platforms = 0;
  u8 res_cave_buzzer = 0;
  u8 res_ogre_boss = 0;
  u8 res_ogre_end = 0;
  u8 res_ogre_buzzer = 0;
  u8 res_ogre_secret = 0;
  u8 res_lavatube_end = 0;
  u8 res_lavatube_buzzer = 0;
  u8 res_lavatube_balls = 0;
  u8 res_intro = 0;

  // need introduction tasks
  u8 int_finalboss_movies = 0;

  // Just to make things a bit nicer in a memory view
  const char end[4] = "end";
};
extern AutoSplitterBlockJak1 gJak1AutoSplitterBlock;

void update_autosplitter_block_jak1(u32 jak1_autosplit_info);
void update_autosplitter_jak1_new_game();
