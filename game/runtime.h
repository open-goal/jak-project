/*!
 * @file runtime.h
 * Setup and launcher for the runtime.
 */

#ifndef JAK1_RUNTIME_H
#define JAK1_RUNTIME_H

#include "common/common_types.h"

extern u8* g_ee_main_mem;
u32 exec_runtime(int argc, char** argv);

#endif  // JAK1_RUNTIME_H
