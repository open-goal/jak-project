#include "iso_queue.h"

namespace jak2 {
void* ScratchPadMemory = nullptr;
void iso_queue_init_globals() {
  ScratchPadMemory = nullptr;
}
}  // namespace jak2
