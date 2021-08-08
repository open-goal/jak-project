#pragma once

/*!
 * @file gfx.h
 * Graphics component for the runtime. Handles some low-level routines.
 */

#ifndef RUNTIME_GFX_H
#define RUNTIME_GFX_H

#include <functional>
#include "common/common_types.h"
#include "display.h"
#include "game/kernel/kboot.h"

namespace Gfx {

u32 Init();
void Loop(std::function<bool()> f);
u32 Exit();

u32 vsync();
void send_chain(const void* data, u32 offset);
void texture_upload_now(const u8* tpage, int mode, u32 s7_ptr);

}  // namespace Gfx

#endif  // RUNTIME_GFX_H
