#pragma once

#include <memory>

#include "common/common_types.h"

#include "game/kernel/common/kscheme.h"

/*!
 * @file screenshot.h
 * This file contains a basic interface for the screen shot system to make it easier to share with
 * the GOAL kernel.
 */

// this must match the structure in capture-pc.gc (if present)
struct ScreenShotSettings {
  s32 width;
  s32 height;
  s32 msaa;
  char name[244];
};

extern ScreenShotSettings* g_screen_shot_settings;
extern bool g_want_screenshot;

void register_screen_shot_settings(ScreenShotSettings* settings);
const char* get_screen_shot_name();
