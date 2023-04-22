#include "spustreams.h"

#include "common/common_types.h"

#include "game/sce/iop.h"

using namespace iop;

namespace jak2 {

s32 StreamsThread = 0;
void spusstreams_init_globals() {
  StreamsThread = 0;
}

void WakeSpuStreamsUp() {
  iWakeupThread(StreamsThread);
}

}  // namespace jak2
