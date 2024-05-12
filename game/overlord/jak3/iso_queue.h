#ifndef ISO_QUEUE_H_
#define ISO_QUEUE_H_

#include "game/overlord/jak3/iso.h"

namespace jak3 {
constexpr int N_PRIORITIES = 2;
constexpr int PRI_STACK_LENGTH = 8;

struct PriStackEntry {
  ISO_Msg* entries[PRI_STACK_LENGTH];
  int count;
};

void InitBuffers();
bool QueueMessage(ISO_Msg* msg, int priority);
void ReturnMessage(ISO_Msg* msg);
void UnqueueMessage(ISO_Msg* msg);
}  // namespace jak3

#endif  // ISO_QUEUE_H_
