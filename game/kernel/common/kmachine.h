#pragma once

#include "common/common_types.h"

/*!
 * Where does OVERLORD load its data from?
 */
enum OverlordDataSource : u32 {
  fakeiso = 0,  //! some sort of development way of getting data
  deviso = 1,   //! some sort of development way of getting data
  iso_cd = 2,   //! use the actual DVD drive
};

extern OverlordDataSource isodrv;

// Get IOP modules from DVD or from dsefilesv
extern u32 modsrc;

// Reboot IOP on start?
extern u32 reboot;

void kmachine_init_globals_common();

/*!
 * Initialize the CD Drive
 */
void InitCD();