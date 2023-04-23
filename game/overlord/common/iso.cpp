#include "iso.h"

#include <cstring>

#include "common/util/Assert.h"

#include "game/common/dgo_rpc_types.h"
#include "game/overlord/common/fake_iso.h"
#include "game/overlord/common/iso_api.h"
#include "game/overlord/jak1/dma.h"
#include "game/overlord/jak1/iso.h"
#include "game/overlord/jak1/ssound.h"
#include "game/overlord/jak1/stream.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/stream.h"
#include "game/runtime.h"
#include "game/sce/iop.h"

using namespace iop;

VagDir gVagDir;

static constexpr s32 LOOP_END = 1;
static constexpr s32 LOOP_REPEAT = 2;
static constexpr s32 LOOP_START = 4;

// Empty ADPCM block with loop flags

// clang-format off
u8 VAG_SilentLoop[0x60] = {
    0x0, LOOP_START | LOOP_REPEAT, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_REPEAT,              0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_END | LOOP_REPEAT,   0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
};
// clang-format on

void iso_init_globals() {
  memset(&gVagDir, 0, sizeof(gVagDir));
}

/*!
 * Does the messagebox have a message in it?
 */
u32 LookMbx(s32 mbx) {
  MsgPacket* msg_packet;
  return PollMbx((&msg_packet), mbx) != KE_MBOX_NOMSG;
}

/*!
 * Wait for a messagebox to have a message. This is inefficient and polls with a 100 us wait.
 * This is stupid because the IOP does have much better syncronization primitives so you don't have
 * to do this.
 */
void WaitMbx(s32 mbx) {
  while (!LookMbx(mbx)) {
    DelayThread(100);
  }
}

/*!
 * Find VAG file by "name", where name is 8 bytes (chars with spaces at the end, treated as two
 * s32's). Returns pointer to name in the VAGDIR file data.
 */
VagDirEntry* FindVAGFile(const char* name) {
  VagDirEntry* entry = gVagDir.vag;
  for (u32 idx = 0; idx < gVagDir.count; idx++) {
    // check if matching name
    if (memcmp(entry->name, name, 8) == 0) {
      return entry;
    }
    entry++;
  }
  return nullptr;
}