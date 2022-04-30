#pragma once

/*!
 * @file runtime.h
 * Setup and launcher for the runtime.
 */

#include <thread>

#include "common/common_types.h"
#include "game/kernel/kboot.h"

extern u8* g_ee_main_mem;

RuntimeExitStatus exec_runtime(int argc, char** argv);

extern std::thread::id g_main_thread_id;
