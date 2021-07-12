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

}  // namespace Gfx

#endif  // RUNTIME_GFX_H
