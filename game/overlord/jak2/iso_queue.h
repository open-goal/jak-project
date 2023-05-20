#pragma once
#include <string>

#include "common/common_types.h"

#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/pages.h"
namespace jak2 {
extern uint8_t* ScratchPadMemory;
void iso_queue_init_globals();
extern PageList* SpMemoryBuffers;
void ReleaseMessage(CmdHeader* param_1, int param_2);
int QueueMessage(CmdHeader* param_1, int param_2, const char* param_3, int param_4);
void DisplayQueue();
uint8_t* CheckForIsoPageBoundaryCrossing(Buffer* param_1);
void InitBuffers();
void FreeBuffer(Buffer* param_1, int param_2);
Buffer* AllocateBuffer(int param_1, VagCmd* param_2, int param_3);
void UnqueueMessage(CmdHeader* param_1, int param_2);
void ReturnMessage(CmdHeader* param_1);
CmdHeader* GetMessage();
VagCmd* GetVAGCommand();

constexpr int N_PRIORITIES = 4;      // number of queued commands per priority
constexpr int PRI_STACK_LENGTH = 8;  // number of queued commands per priority

struct PriStackEntry {
  CmdHeader* entries[PRI_STACK_LENGTH];
  int count;
};

extern std::string gPriEntryNames[N_PRIORITIES][PRI_STACK_LENGTH];  // my addition for debug
extern PriStackEntry gPriStack[N_PRIORITIES];

}  // namespace jak2
