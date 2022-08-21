#include "auto_splitter.h"

#include "game/kernel/jak1/kmachine.h"
#include <game/kernel/common/Ptr.h>
#include <game/kernel/common/kmachine.h>
#include <game/kernel/jak1/kscheme.h>

#include "third-party/fmt/core.h"

AutoSplitterBlockJak1 gJak1AutoSplitterBlock;

void update_autosplitter_block_jak1(u32 jak1_autosplit_info) {
  auto info = jak1_autosplit_info ? Ptr<jak1::AutoSplitInfoJak1>(jak1_autosplit_info).c() : NULL;
  if (!info) {
    return;
  }
  // Update all fields
  gJak1AutoSplitterBlock.num_power_cells = (int)*Ptr<float>(info->num_power_cells).c();
  gJak1AutoSplitterBlock.num_orbs = (int)*Ptr<float>(info->num_orbs).c();
  gJak1AutoSplitterBlock.num_scout_flies = (int)*Ptr<float>(info->num_scout_flies).c();

  gJak1AutoSplitterBlock.in_cutscene =
      Ptr<jak1::Symbol>(info->in_cutscene)->value != offset_of_s7();

  // need-resolution flags
  gJak1AutoSplitterBlock.res_jungle_eggtop =
      Ptr<jak1::Symbol>(info->res_jungle_eggtop)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_jungle_lurkerm =
      Ptr<jak1::Symbol>(info->res_jungle_lurkerm)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_jungle_tower =
      Ptr<jak1::Symbol>(info->res_jungle_tower)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_jungle_fishgame =
      Ptr<jak1::Symbol>(info->res_jungle_fishgame)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_jungle_plant =
      Ptr<jak1::Symbol>(info->res_jungle_plant)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_jungle_buzzer =
      Ptr<jak1::Symbol>(info->res_jungle_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_jungle_canyon_end =
      Ptr<jak1::Symbol>(info->res_jungle_canyon_end)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_jungle_temple_door =
      Ptr<jak1::Symbol>(info->res_jungle_temple_door)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village1_yakow =
      Ptr<jak1::Symbol>(info->res_village1_yakow)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village1_mayor_money =
      Ptr<jak1::Symbol>(info->res_village1_mayor_money)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village1_uncle_money =
      Ptr<jak1::Symbol>(info->res_village1_uncle_money)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village1_oracle_money1 =
      Ptr<jak1::Symbol>(info->res_village1_oracle_money1)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village1_oracle_money2 =
      Ptr<jak1::Symbol>(info->res_village1_oracle_money2)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_ecorocks =
      Ptr<jak1::Symbol>(info->res_beach_ecorocks)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_pelican =
      Ptr<jak1::Symbol>(info->res_beach_pelican)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_flutflut =
      Ptr<jak1::Symbol>(info->res_beach_flutflut)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_seagull =
      Ptr<jak1::Symbol>(info->res_beach_seagull)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_cannon =
      Ptr<jak1::Symbol>(info->res_beach_cannon)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_buzzer =
      Ptr<jak1::Symbol>(info->res_beach_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_gimmie =
      Ptr<jak1::Symbol>(info->res_beach_gimmie)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_beach_sentinel =
      Ptr<jak1::Symbol>(info->res_beach_sentinel)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_muse =
      Ptr<jak1::Symbol>(info->res_misty_muse)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_boat =
      Ptr<jak1::Symbol>(info->res_misty_boat)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_warehouse =
      Ptr<jak1::Symbol>(info->res_misty_warehouse)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_cannon =
      Ptr<jak1::Symbol>(info->res_misty_cannon)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_bike =
      Ptr<jak1::Symbol>(info->res_misty_bike)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_buzzer =
      Ptr<jak1::Symbol>(info->res_misty_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_bike_jump =
      Ptr<jak1::Symbol>(info->res_misty_bike_jump)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_misty_eco_challenge =
      Ptr<jak1::Symbol>(info->res_misty_eco_challenge)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village2_gambler_money =
      Ptr<jak1::Symbol>(info->res_village2_gambler_money)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village2_geologist_money =
      Ptr<jak1::Symbol>(info->res_village2_geologist_money)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village2_warrior_money =
      Ptr<jak1::Symbol>(info->res_village2_warrior_money)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village2_oracle_money1 =
      Ptr<jak1::Symbol>(info->res_village2_oracle_money1)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village2_oracle_money2 =
      Ptr<jak1::Symbol>(info->res_village2_oracle_money2)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_billy =
      Ptr<jak1::Symbol>(info->res_swamp_billy)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_flutflut =
      Ptr<jak1::Symbol>(info->res_swamp_flutflut)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_battle =
      Ptr<jak1::Symbol>(info->res_swamp_battle)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_tether_1 =
      Ptr<jak1::Symbol>(info->res_swamp_tether_1)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_tether_2 =
      Ptr<jak1::Symbol>(info->res_swamp_tether_2)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_tether_3 =
      Ptr<jak1::Symbol>(info->res_swamp_tether_3)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_tether_4 =
      Ptr<jak1::Symbol>(info->res_swamp_tether_4)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_buzzer =
      Ptr<jak1::Symbol>(info->res_swamp_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_platforms =
      Ptr<jak1::Symbol>(info->res_sunken_platforms)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_pipe =
      Ptr<jak1::Symbol>(info->res_sunken_pipe)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_slide =
      Ptr<jak1::Symbol>(info->res_sunken_slide)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_room =
      Ptr<jak1::Symbol>(info->res_sunken_room)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_sharks =
      Ptr<jak1::Symbol>(info->res_sunken_sharks)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_buzzer =
      Ptr<jak1::Symbol>(info->res_sunken_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_top_of_helix =
      Ptr<jak1::Symbol>(info->res_sunken_top_of_helix)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_sunken_spinning_room =
      Ptr<jak1::Symbol>(info->res_sunken_spinning_room)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_race =
      Ptr<jak1::Symbol>(info->res_rolling_race)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_robbers =
      Ptr<jak1::Symbol>(info->res_rolling_robbers)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_moles =
      Ptr<jak1::Symbol>(info->res_rolling_moles)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_plants =
      Ptr<jak1::Symbol>(info->res_rolling_plants)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_lake =
      Ptr<jak1::Symbol>(info->res_rolling_lake)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_buzzer =
      Ptr<jak1::Symbol>(info->res_rolling_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_ring_chase_1 =
      Ptr<jak1::Symbol>(info->res_rolling_ring_chase_1)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_rolling_ring_chase_2 =
      Ptr<jak1::Symbol>(info->res_rolling_ring_chase_2)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_eggtop =
      Ptr<jak1::Symbol>(info->res_snow_eggtop)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_ram =
      Ptr<jak1::Symbol>(info->res_snow_ram)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_fort =
      Ptr<jak1::Symbol>(info->res_snow_fort)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_ball =
      Ptr<jak1::Symbol>(info->res_snow_ball)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_bunnies =
      Ptr<jak1::Symbol>(info->res_snow_bunnies)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_buzzer =
      Ptr<jak1::Symbol>(info->res_snow_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_bumpers =
      Ptr<jak1::Symbol>(info->res_snow_bumpers)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_snow_cage =
      Ptr<jak1::Symbol>(info->res_snow_cage)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_firecanyon_buzzer =
      Ptr<jak1::Symbol>(info->res_firecanyon_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_firecanyon_end =
      Ptr<jak1::Symbol>(info->res_firecanyon_end)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_citadel_sage_green =
      Ptr<jak1::Symbol>(info->res_citadel_sage_green)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_citadel_sage_blue =
      Ptr<jak1::Symbol>(info->res_citadel_sage_blue)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_citadel_sage_red =
      Ptr<jak1::Symbol>(info->res_citadel_sage_red)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_citadel_sage_yellow =
      Ptr<jak1::Symbol>(info->res_citadel_sage_yellow)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_extra1 =
      Ptr<jak1::Symbol>(info->res_village3_extra1)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village1_buzzer =
      Ptr<jak1::Symbol>(info->res_village1_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village2_buzzer =
      Ptr<jak1::Symbol>(info->res_village2_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_buzzer =
      Ptr<jak1::Symbol>(info->res_village3_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_gnawers =
      Ptr<jak1::Symbol>(info->res_cave_gnawers)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_dark_crystals =
      Ptr<jak1::Symbol>(info->res_cave_dark_crystals)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_dark_climb =
      Ptr<jak1::Symbol>(info->res_cave_dark_climb)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_robot_climb =
      Ptr<jak1::Symbol>(info->res_cave_robot_climb)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_swing_poles =
      Ptr<jak1::Symbol>(info->res_cave_swing_poles)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_spider_tunnel =
      Ptr<jak1::Symbol>(info->res_cave_spider_tunnel)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_platforms =
      Ptr<jak1::Symbol>(info->res_cave_platforms)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_cave_buzzer =
      Ptr<jak1::Symbol>(info->res_cave_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_ogre_boss =
      Ptr<jak1::Symbol>(info->res_ogre_boss)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_ogre_end =
      Ptr<jak1::Symbol>(info->res_ogre_end)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_ogre_buzzer =
      Ptr<jak1::Symbol>(info->res_ogre_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_lavatube_end =
      Ptr<jak1::Symbol>(info->res_lavatube_end)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_lavatube_buzzer =
      Ptr<jak1::Symbol>(info->res_lavatube_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_citadel_buzzer =
      Ptr<jak1::Symbol>(info->res_citadel_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_training_gimmie =
      Ptr<jak1::Symbol>(info->res_training_gimmie)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_training_door =
      Ptr<jak1::Symbol>(info->res_training_door)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_training_climb =
      Ptr<jak1::Symbol>(info->res_training_climb)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_training_buzzer =
      Ptr<jak1::Symbol>(info->res_training_buzzer)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_miner_money1 =
      Ptr<jak1::Symbol>(info->res_village3_miner_money1)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_miner_money2 =
      Ptr<jak1::Symbol>(info->res_village3_miner_money2)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_miner_money3 =
      Ptr<jak1::Symbol>(info->res_village3_miner_money3)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_miner_money4 =
      Ptr<jak1::Symbol>(info->res_village3_miner_money4)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_oracle_money1 =
      Ptr<jak1::Symbol>(info->res_village3_oracle_money1)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_village3_oracle_money2 =
      Ptr<jak1::Symbol>(info->res_village3_oracle_money2)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_swamp_arm =
      Ptr<jak1::Symbol>(info->res_swamp_arm)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_red_eggtop =
      Ptr<jak1::Symbol>(info->res_red_eggtop)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_lavatube_balls =
      Ptr<jak1::Symbol>(info->res_lavatube_balls)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_intro = Ptr<jak1::Symbol>(info->res_intro)->value != offset_of_s7();
  gJak1AutoSplitterBlock.res_ogre_secret =
      Ptr<jak1::Symbol>(info->res_ogre_secret)->value != offset_of_s7();

  // Misc Tasks
  gJak1AutoSplitterBlock.int_finalboss_movies =
      Ptr<jak1::Symbol>(info->int_finalboss_movies)->value != offset_of_s7();
  gJak1AutoSplitterBlock.unk_finalboss_movies =
      Ptr<jak1::Symbol>(info->unk_finalboss_movies)->value != offset_of_s7();
  gJak1AutoSplitterBlock.int_jungle_fishgame =
      Ptr<jak1::Symbol>(info->int_jungle_fishgame)->value != offset_of_s7();
}

void update_autosplitter_jak1_new_game() {
  gJak1AutoSplitterBlock.game_hash = std::time(nullptr);
}
