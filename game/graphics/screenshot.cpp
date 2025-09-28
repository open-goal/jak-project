#include "game/graphics/screenshot.h"

/*!
 * @file screenshot.cpp
 * This file contains a basic interface for the screen shot system to make it easier to share with
 * the GOAL kernel.
 */

ScreenShotSettings s_default_screen_shot_settings = {1920, 1080, 8, "screenshot"};

bool g_want_screenshot = false;
ScreenShotSettings* g_screen_shot_settings = &s_default_screen_shot_settings;

void register_screen_shot_settings(ScreenShotSettings* settings) {
  g_screen_shot_settings = settings;
}

const char* get_screen_shot_name() {
  return g_screen_shot_settings->name;
}
