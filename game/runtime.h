#pragma once

/*!
 * @file runtime.h
 * Setup and launcher for the runtime.
 */

#include <thread>

#include "common/common_types.h"
#include "common/versions.h"

#include "game/kernel/common/kboot.h"

extern u8* g_ee_main_mem;
extern GameVersion g_game_version;

RuntimeExitStatus exec_runtime(int argc, char** argv);

extern std::thread::id g_main_thread_id;
