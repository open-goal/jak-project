#pragma once

#include "DataObjectGenerator.h"

#include "decompiler/level_extractor/common_formats.h"
#include "goalc/build_level/common/Entity.h"
#include "goalc/build_level/common/FileInfo.h"

struct Joint {
  std::string name;
  int parent = -1;
  math::Matrix4f bind_pose;
};

struct JointAnim {
  std::string name;
  s16 number;
  s16 length;
};

struct JointAnimCompressed : JointAnim {
  std::vector<u32> data;
};

struct Art {
  std::string name;
  s32 length;
  ResLump* lump;
};

struct ArtElement : Art {
  u8 pad[12];
};

struct ArtJointGeo : ArtElement {};

struct ArtJointAnim : ArtElement {};

struct ArtGroup {
  FileInfo info;

  ArtGroup() {
    info.tool_debug = "Created by OpenGOAL buildactor";
    info.file_type = "art-group";
  }
  size_t generate(DataObjectGenerator& gen);
};
