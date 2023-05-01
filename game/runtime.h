#pragma once

/*!
 * @file runtime.h
 * Setup and launcher for the runtime.
 */

#include <thread>

#include "common/common_types.h"
#include "common/versions/versions.h"

#include "game/common/game_common_types.h"
#include "game/kernel/common/kboot.h"

extern u8* g_ee_main_mem;
extern GameVersion g_game_version;
extern int g_server_port;

RuntimeExitStatus exec_runtime(GameLaunchOptions game_options, int argc, const char** argv);

extern std::thread::id g_main_thread_id;
