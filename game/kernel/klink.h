#pragma once

/*!
 * @file klink.cpp
 * GOAL Linker for x86-64
 * DONE!
 */

#include <cstring>

#include "common/common_types.h"
#include "common/link_types.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kmalloc.h"

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
  void begin(Ptr<uint8_t> object_file,
             const char* name,
             int32_t size,
             Ptr<kheapinfo> heap,
             uint32_t flags);
  uint32_t work();
  uint32_t work_v3();
  uint32_t work_v2();
  void finish(bool jump_from_c_to_goal);

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

void klink_init_globals();

u64 link_and_exec_wrapper(u64* args);

Ptr<uint8_t> link_and_exec(Ptr<uint8_t> data,
                           const char* name,
                           int32_t size,
                           Ptr<kheapinfo> heap,
                           uint32_t flags,
                           bool jump_from_c_to_goal);

uint64_t link_begin(u64* args);

uint64_t link_resume();
void ultimate_memcpy(void* dst, void* src, uint32_t size);

extern link_control saved_link_control;
