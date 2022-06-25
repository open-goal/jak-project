#pragma once

#include "common/link_types.h"

constexpr int LINK_FLAG_OUTPUT_LOAD = 0x1;
constexpr int LINK_FLAG_OUTPUT_TRUE = 0x2;
constexpr int LINK_FLAG_EXECUTE = 0x4;
constexpr int LINK_FLAG_PRINT_LOGIN = 0x8;  //! Note, doesn't actually do anything.
constexpr int LINK_FLAG_FORCE_DEBUG = 0x10;
constexpr int LINK_FLAG_FORCE_FAST_LINK = 0x20;

// only used in OpenGOAL
struct SegmentInfo {
  uint32_t offset;
  uint32_t size;
};

// only used in OpenGOAL
struct ObjectFileHeader {
  uint16_t goal_version_major;
  uint16_t goal_version_minor;
  uint32_t object_file_version;
  uint32_t segment_count;
  SegmentInfo link_infos[N_SEG];
  SegmentInfo code_infos[N_SEG];
  uint32_t link_block_length;
};