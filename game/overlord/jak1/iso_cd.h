#pragma once

/*!
 * @file iso_cd.cpp
 * IsoFs API for accessing the CD/DVD drive.
 */

#include "iso.h"

#include "common/common_types.h"

namespace jak1 {
void iso_cd_init_globals();
extern IsoFs iso_cd_;
}  // namespace jak1
