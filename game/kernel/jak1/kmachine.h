#pragma once

#include "common/common_types.h"
// Discord RPC
struct DiscordRichPresence;
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

// To speedup finding the auto-splitter block in GOAL memory
// all this has is a marker for LiveSplit to find, and then the pointer
// to the symbol
struct AutoSplitterBlock {
  const char marker[20] = "UnLiStEdStRaTs_JaK1";
  u64 pointer_to_symbol = 0;
};
extern AutoSplitterBlock g_auto_splitter_block_jak1;
}  // namespace jak1
