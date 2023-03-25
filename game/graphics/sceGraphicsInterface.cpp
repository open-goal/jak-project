#include "game/graphics/sceGraphicsInterface.h"

#include <cstdio>

#include "common/util/Assert.h"

#include "game/graphics/gfx.h"

/*!
 * Wait for rendering to complete.
 * In the PC Port, this currently does nothing.
 *
 * From my current understanding, we can get away with this and just sync everything on vsync.
 * However, there are two calls to this per frame.
 *
 * But I don't fully understand why they call sceGsSyncPath where they do (right before depth cue)
 * so maybe the depth cue looks at the z-buffer of the last rendered frame when setting up the dma
 * for the next frame?  The debug drawing also happens after this.
 *
 * The second call is right before swapping buffers/vsync, so that makes sense.
 *
 *
 */
u32 sceGsSyncPath(u32 mode, u32 timeout) {
  ASSERT(mode == 0 && timeout == 0);
  return Gfx::sync_path();
}

/*!
 * Actual vsync.
 */
u32 sceGsSyncV(u32 mode) {
  ASSERT(mode == 0);
  return Gfx::vsync();
}
