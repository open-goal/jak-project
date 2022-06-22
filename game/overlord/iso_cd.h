#pragma once

/*!
 * @file iso_cd.cpp
 * IsoFs API for accessing the CD/DVD drive.
 */

#ifndef JAK_ISO_CD_H
#define JAK_ISO_CD_H

#include "iso.h"

#include "common/common_types.h"

void iso_cd_init_globals();
extern IsoFs iso_cd_;

#endif  // JAK_ISO_CD_H
