#pragma once
#include "game/overlord/jak2/iso.h"

namespace jak2 {
void spusstreams_init_globals();
void WakeSpuStreamsUp();
void InitSpuStreamsThread();
int ProcessVAGData(CmdHeader* param_1_in, Buffer* param_2);
void StopVagStream(VagCmd* param_1, int param_2);
}  // namespace jak2
