/*!
 * @file klink.cpp
 * GOAL Linker for x86-64
 * DONE!
 */

#ifndef JAK_KLINK_H
#define JAK_KLINK_H

#include "Ptr.h"
#include "kmalloc.h"
#include "common/link_types.h"
#include "common/common_types.h"

constexpr int LINK_FLAG_OUTPUT_LOAD = 0x1;
constexpr int LINK_FLAG_OUTPUT_TRUE = 0x2;
constexpr int LINK_FLAG_EXECUTE = 0x4;
constexpr int LINK_FLAG_PRINT_LOGIN = 0x8;  //! Note, doesn't actually do anything.
constexpr int LINK_FLAG_FORCE_DEBUG = 0x10;
constexpr int LINK_FLAG_FORCE_FAST_LINK = 0x20;

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
  void begin(Ptr<uint8_t> object_file,
             const char* name,
             int32_t size,
             Ptr<kheapinfo> heap,
             uint32_t flags);
  uint32_t work();
  uint32_t work_v3();
  void finish();

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
  }
};

struct SegmentInfo {
  uint32_t offset;
  uint32_t size;
};

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

u64 link_and_exec_wrapper(u64 data, u64 name, s64 size, u64 heap, u64 flags);

Ptr<uint8_t> link_and_exec(Ptr<uint8_t> data,
                           const char* name,
                           int32_t size,
                           Ptr<kheapinfo> heap,
                           uint32_t flags);

uint64_t link_begin(uint64_t object_data,
                    uint64_t name,
                    int32_t size,
                    uint64_t heap,
                    uint32_t flags);

uint64_t link_resume();
void* ultimate_memcpy(void* dst, void* src, uint32_t size);

extern link_control saved_link_control;

#endif  // JAK_KLINK_H
