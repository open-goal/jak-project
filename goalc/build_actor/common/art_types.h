#pragma once

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/level_extractor/MercData.h"
#include "decompiler/level_extractor/common_formats.h"
#include "goalc/build_level/common/Entity.h"
#include "goalc/build_level/common/FileInfo.h"
#include "goalc/data_compiler/DataObjectGenerator.h"

struct Art {
  std::string name;
  s32 length;
  ResLump lump;
};

struct ArtElement : Art {
  u8 pad[12];
};

struct MercEyeAnimFrame {
  s8 pupil_trans_x;
  s8 pupil_trans_y;
  s8 blink;
  s8 iris_scale;
  s8 pupil_scale;
  s8 lid_scale;
};

struct MercEyeAnimBlock {
  s16 max_frame;
  std::vector<MercEyeAnimFrame> frames;
};