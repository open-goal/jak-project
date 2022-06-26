#pragma once
#include <cstring>

#include "common/common_types.h"
#include "common/link_types.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kscheme.h"

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

void klink_init_globals();
/*!
 * Stores the state of the linker. Used for multi-threaded linking, so it can be suspended.
 */
struct link_control {
  Ptr<uint8_t> m_object_data;  //! points to the start of the object file
  Ptr<uint8_t> m_entry;        //! points to first code to execute
  char m_object_name[64];      //! object file name
  int32_t m_object_size;       //! object file size
  Ptr<kheapinfo> m_heap;       //! heap we are putting the object file on
  uint32_t m_flags;            //! linker configuration
  Ptr<uint8_t> m_heap_top;     //! where to reset the heap top for clearing temp allocations
  bool m_keep_debug;           //! keep the debug segment, even if DebugSegment is off?
  Ptr<uint8_t> m_link_block_ptr;
  uint32_t m_code_size;
  Ptr<uint8_t> m_code_start;
  uint32_t m_state;
  uint32_t m_segment_process;
  uint32_t m_version;
  int m_heap_gap;
  Ptr<uint8_t> m_original_object_location;
  Ptr<u8> m_reloc_ptr;
  Ptr<u8> m_base_ptr;
  Ptr<u8> m_loc_ptr;
  int m_table_toggle;

  bool m_opengoal;
  bool m_busy;  // only in jak2, but doesn't hurt to set it in jak 1.
  void begin(Ptr<uint8_t> object_file,
             const char* name,
             int32_t size,
             Ptr<kheapinfo> heap,
             uint32_t flags);

  // was originally "work"
  uint32_t jak1_work();
  uint32_t jak2_work();

  uint32_t jak1_work_v3();
  uint32_t jak1_work_v2();
  uint32_t jak2_work_v3();
  uint32_t jak2_work_v2();
  void jak1_finish(bool jump_from_c_to_goal);
  void jak2_finish(bool jump_from_c_to_goal);

  void reset() {
    m_object_data.offset = 0;
    m_entry.offset = 0;
    memset(m_object_name, 0, sizeof(m_object_name));
    m_object_size = 0;
    m_heap.offset = 0;
    m_flags = 0;
    m_heap_top.offset = 0;
    m_keep_debug = false;
    m_link_block_ptr.offset = 0;
    m_code_size = 0;
    m_code_start.offset = 0;
    m_state = 0;
    m_segment_process = 0;
    m_version = 0;
    m_busy = false;
  }
};

void klink_init_globals();
Ptr<u8> c_symlink2(Ptr<u8> objData, Ptr<u8> linkObj, Ptr<u8> relocTable);

extern link_control saved_link_control;
extern Ptr<Function> gfunc_774;  // actually 807 in jak2.
