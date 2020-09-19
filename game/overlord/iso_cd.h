#pragma once

/*!
 * @file iso_cd.cpp
 * IsoFs API for accessing the CD/DVD drive.
 */

#ifndef JAK_ISO_CD_H
#define JAK_ISO_CD_H

#include "common/common_types.h"
#include "iso.h"

void iso_cd_init_globals();
extern IsoFs iso_cd_;

#endif  // JAK_ISO_CD_H
