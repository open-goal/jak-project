/*!
 * @file klink.cpp
 * GOAL Linker for x86-64
 * Note - this is significantly different from the MIPS linker because the object file format is
 * different.
 * DONE!
 */

#include <cstring>
#include "common/util/assert.h"
#include <cstdio>
#include <common/versions.h>
#include "klink.h"
#include "fileio.h"
#include "kscheme.h"
#include "kboot.h"
#include "kprint.h"
#include "common/symbols.h"
#include "common/goal_constants.h"

namespace {
// turn on printf's for debugging linking issues.
constexpr bool link_debug_printfs = false;

bool is_opengoal_object(const void* data) {
  auto* header = (const LinkHeaderV2*)data;
  return !(header->type_tag == 0xffffffff && (header->version == 2 || header->version == 4));
}
}  // namespace

// space to store a single in-progress linking state.
link_control saved_link_control;

// pointer to GOAL *ultimate-memcpy*, if its loaded.
Ptr<Function> gfunc_774;

void klink_init_globals() {
  saved_link_control.reset();
  gfunc_774.offset = 0;
}

/*!
 * Initialize the link control.
 */
void link_control::begin(Ptr<uint8_t> object_file,
                         const char* name,
                         int32_t size,
                         Ptr<kheapinfo> heap,
                         uint32_t flags) {
  if (is_opengoal_object(object_file.c())) {
    // save data from call to begin
    m_object_data = object_file;
    kstrcpy(m_object_name, name);
    m_object_size = size;
    m_heap = heap;
    m_flags = flags;

    // initialize link control
    m_entry.offset = 0;
    m_heap_top = m_heap->top;
    m_keep_debug = false;
    m_opengoal = true;

    if (link_debug_printfs) {
      char* goal_name = object_file.cast<char>().c();
      printf("link %s\n", goal_name);
      printf("link_control::begin %c%c%c%c\n", goal_name[0], goal_name[1], goal_name[2],
             goal_name[3]);
    }

    // points to the beginning of the linking data
    m_link_block_ptr = object_file + BASIC_OFFSET;
    m_code_size = 0;
    m_code_start = object_file;
    m_state = 0;
    m_segment_process = 0;

    ObjectFileHeader* ofh = m_link_block_ptr.cast<ObjectFileHeader>().c();
    if (ofh->goal_version_major != versions::GOAL_VERSION_MAJOR) {
      fprintf(
          stderr,
          "VERSION ERROR: C Kernel built from GOAL %d.%d, but object file %s is from GOAL %d.%d\n",
          versions::GOAL_VERSION_MAJOR, versions::GOAL_VERSION_MINOR, name, ofh->goal_version_major,
          ofh->goal_version_minor);
      assert(false);
    }
    if (link_debug_printfs) {
      printf("Object file header:\n");
      printf(" GOAL ver %d.%d obj %d len %d\n", ofh->goal_version_major, ofh->goal_version_minor,
             ofh->object_file_version, ofh->link_block_length);
      printf(" segment count %d\n", ofh->segment_count);
      for (int i = 0; i < N_SEG; i++) {
        printf(" seg %d link 0x%04x, 0x%04x data 0x%04x, 0x%04x\n", i, ofh->link_infos[i].offset,
               ofh->link_infos[i].size, ofh->code_infos[i].offset, ofh->code_infos[i].size);
      }
    }

    m_version = ofh->object_file_version;
    if (ofh->object_file_version < 4) {
      // three segment file

      // seek past the header
      m_object_data.offset += ofh->link_block_length;
      // todo, set m_code_size

      if (m_link_block_ptr.offset < m_heap->base.offset ||
          m_link_block_ptr.offset >= m_heap->top.offset) {
        // the link block is outside our heap, or in the top of our heap.  It's somebody else's
        // problem.
        if (link_debug_printfs) {
          printf("Link block somebody else's problem\n");
        }

        if (m_heap->base.offset <= m_object_data.offset &&    // above heap base
            m_object_data.offset < m_heap->top.offset &&      // less than heap top (not needed?)
            m_object_data.offset < m_heap->current.offset) {  // less than heap current
          if (link_debug_printfs) {
            printf("Code block in the heap, kicking it out for copy into heap\n");
          }
          m_heap->current = m_object_data;
        }
      } else {
        // in our heap, we need to move it so we can free up its space later on
        if (link_debug_printfs) {
          printf("Link block needs to be moved!\n");
        }

        // allocate space for a new one
        auto new_link_block = kmalloc(m_heap, ofh->link_block_length, KMALLOC_TOP, "link-block");
        auto old_link_block = m_link_block_ptr - BASIC_OFFSET;

        // copy it
        ultimate_memcpy(new_link_block.c(), old_link_block.c(), ofh->link_block_length);
        m_link_block_ptr = new_link_block + BASIC_OFFSET;

        // if we can save some memory here
        if (old_link_block.offset < m_heap->current.offset) {
          if (link_debug_printfs) {
            printf("Kick out old link block\n");
          }
          m_heap->current = old_link_block;
        }
      }
    } else {
      printf("UNHANDLED OBJECT FILE VERSION\n");
      assert(false);
    }

    if ((m_flags & LINK_FLAG_FORCE_DEBUG) && MasterDebug && !DiskBoot) {
      m_keep_debug = true;
    }
  } else {
    m_opengoal = false;
    // not an open goal object.
    if (link_debug_printfs) {
      printf("Linking GOAL style object\n");
    }

    // initialize
    m_object_data = object_file;
    kstrcpy(m_object_name, name);
    m_object_size = size;
    m_heap = heap;
    m_flags = flags;
    m_entry.offset = 0;
    m_heap_top = m_heap->top;
    m_keep_debug = false;
    m_link_block_ptr = object_file + BASIC_OFFSET;
    m_code_size = 0;
    m_code_start = object_file;
    m_state = 0;
    m_segment_process = 0;

    const auto* header = (LinkHeaderV2*)(m_link_block_ptr.c() - 4);

    m_version = header->version;
    if (header->version < 4) {
      // seek past header
      m_object_data.offset += header->length;
      m_code_size = m_object_size - header->length;
      if (m_link_block_ptr.offset < m_heap->base.offset ||
          m_link_block_ptr.offset >= m_heap->top.offset) {
        // the link block is outside our heap, or in the top of our heap.  It's somebody else's
        // problem.
        if (link_debug_printfs) {
          printf("Link block somebody else's problem\n");
        }

        if (m_heap->base.offset <= m_object_data.offset &&    // above heap base
            m_object_data.offset < m_heap->top.offset &&      // less than heap top (not needed?)
            m_object_data.offset < m_heap->current.offset) {  // less than heap current
          if (link_debug_printfs) {
            printf("Code block in the heap, kicking it out for copy into heap\n");
          }
          m_heap->current = m_object_data;
        }
      } else {
        // in our heap, we need to move it so we can free up its space later on
        if (link_debug_printfs) {
          printf("Link block needs to be moved!\n");
        }

        // allocate space for a new one
        auto new_link_block = kmalloc(m_heap, header->length, KMALLOC_TOP, "link-block");
        auto old_link_block = m_link_block_ptr - BASIC_OFFSET;

        // copy it
        ultimate_memcpy(new_link_block.c(), old_link_block.c(), header->length);
        m_link_block_ptr = new_link_block + BASIC_OFFSET;

        // if we can save some memory here
        if (old_link_block.offset < m_heap->current.offset) {
          if (link_debug_printfs) {
            printf("Kick out old link block\n");
          }
          m_heap->current = old_link_block;
        }
      }

    } else {
      auto header_v4 = (const LinkHeaderV4*)header;
      auto old_object_data = m_object_data;
      m_link_block_ptr =
          old_object_data + header_v4->code_size + sizeof(LinkHeaderV4) + BASIC_OFFSET;
      m_object_data = old_object_data + sizeof(LinkHeaderV4);
      m_code_size = header_v4->code_size;
    }

    if ((m_flags & LINK_FLAG_FORCE_DEBUG) && MasterDebug && !DiskBoot) {
      m_keep_debug = true;
    }
  }
}

/*!
 * Make progress on linking.
 */
uint32_t link_control::work() {
  auto old_debug_segment = DebugSegment;
  if (m_keep_debug) {
    DebugSegment = s7.offset + FIX_SYM_TRUE;
  }

  // set type tag of link block
  *((m_link_block_ptr - 4).cast<u32>()) = *((s7 + FIX_SYM_LINK_BLOCK).cast<u32>());

  uint32_t rv;

  if (m_version == 3) {
    assert(m_opengoal);
    rv = work_v3();
  } else if (m_version == 2 || m_version == 4) {
    assert(!m_opengoal);
    rv = work_v2();
  } else {
    printf("UNHANDLED OBJECT FILE VERSION %d IN WORK!\n", m_version);
    assert(false);
    return 0;
  }

  DebugSegment = old_debug_segment;
  return rv;
}

/*!
 * Link type pointers for a single type in "v3 equivalent" link data
 * Returns a pointer to the link table data after the typelinking data.
 */
uint32_t typelink_v3(Ptr<uint8_t> link, Ptr<uint8_t> data) {
  // get the name of the type
  uint32_t seek = 0;
  char sym_name[256];
  while (link.c()[seek]) {
    sym_name[seek] = link.c()[seek];
    seek++;
    assert(seek < 256);
  }
  sym_name[seek] = 0;
  seek++;

  // determine the number of methods
  uint8_t method_count = link.c()[seek++];

  // intern the GOAL type, creating the vtable if it doesn't exist.
  auto type_ptr = intern_type_from_c(sym_name, method_count);

  // prepare to read the locations of the type pointers
  Ptr<uint32_t> offsets = link.cast<uint32_t>() + seek;
  uint32_t offset_count = *offsets;
  offsets = offsets + 4;
  seek += 4;

  // write the type pointers into memory
  for (uint32_t i = 0; i < offset_count; i++) {
    *(data + offsets.c()[i]).cast<int32_t>() = type_ptr.offset;
    seek += 4;
  }

  return seek;
}

/*!
 * Link symbols (both offsets and pointers) in "v3 equivalent" link data.
 * Returns a pointer to the link table data after the linking data for this symbol.
 */
uint32_t symlink_v3(Ptr<uint8_t> link, Ptr<uint8_t> data) {
  // get the symbol name
  uint32_t seek = 0;
  char sym_name[256];
  while (link.c()[seek]) {
    sym_name[seek] = link.c()[seek];
    seek++;
    assert(seek < 256);
  }
  sym_name[seek] = 0;
  seek++;

  // intern
  auto sym = intern_from_c(sym_name);
  int32_t sym_offset = sym.cast<u32>() - s7;
  uint32_t sym_addr = sym.cast<u32>().offset;

  // prepare to read locations of symbol links
  Ptr<uint32_t> offsets = link.cast<uint32_t>() + seek;
  uint32_t offset_count = *offsets;
  offsets = offsets + 4;
  seek += 4;

  for (uint32_t i = 0; i < offset_count; i++) {
    uint32_t offset = offsets.c()[i];
    seek += 4;
    auto data_ptr = (data + offset).cast<int32_t>();

    if (*data_ptr == -1) {
      // a "-1" indicates that we should store the address.
      *(data + offset).cast<int32_t>() = sym_addr;
    } else {
      // otherwise store the offset to st.  Eventually this should become an s16 instead.
      *(data + offset).cast<int32_t>() = sym_offset;
    }
  }

  return seek;
}

/*!
 * Link a single relative offset (used for RIP)
 */
uint32_t cross_seg_dist_link_v3(Ptr<uint8_t> link,
                                ObjectFileHeader* ofh,
                                int current_seg,
                                int size) {
  // target seg, dist into mine, dist into target, patch loc in mine
  uint8_t target_seg = *link;
  assert(target_seg < ofh->segment_count);

  uint32_t* link_data = (link + 1).cast<uint32_t>().c();
  int32_t mine = link_data[0] + ofh->code_infos[current_seg].offset;
  int32_t tgt = link_data[1] + ofh->code_infos[target_seg].offset;
  int32_t diff = tgt - mine;
  uint32_t offset_of_patch = link_data[2] + ofh->code_infos[current_seg].offset;

  if (!ofh->code_infos[target_seg].offset) {
    // we want to address GOAL 0. In the case where this is a rip-relative load or store, this
    // will crash, which is what we want. If it's an lea and just getting an address, this will get
    // us a nullptr. If you do a method-set! with a null pointer it does nothing, so it's safe to
    // method-set! to things that are in unloaded segments and it'll just keep the old method.
    diff = -mine;
  }
  // printf("link object in seg %d diff %d at %d (%d + %d)\n", target_seg, diff, offset_of_patch,
  // link_data[2], ofh->code_infos[current_seg].offset);

  // both 32-bit and 64-bit pointer links are supported, though 64-bit ones are no longer in use.
  // we still support it just in case we want to run ancient code.
  if (size == 4) {
    *Ptr<int32_t>(offset_of_patch).c() = diff;
  } else if (size == 8) {
    *Ptr<int64_t>(offset_of_patch).c() = diff;
  } else {
    assert(false);
  }

  return 1 + 3 * 4;
}

uint32_t ptr_link_v3(Ptr<u8> link, ObjectFileHeader* ofh, int current_seg) {
  auto* link_data = link.cast<u32>().c();
  u32 patch_loc = link_data[0] + ofh->code_infos[current_seg].offset;
  u32 patch_value = link_data[1] + ofh->code_infos[current_seg].offset;
  *Ptr<u32>(patch_loc).c() = patch_value;
  return 8;
}

/*!
 * Run the linker. For now, all linking is done in two runs.  If this turns out to be too slow,
 * this should be modified to do incremental linking over multiple runs.
 */
uint32_t link_control::work_v3() {
  ObjectFileHeader* ofh = m_link_block_ptr.cast<ObjectFileHeader>().c();
  if (m_state == 0) {
    // state 0 <- copying data.
    // the actual game does all copying in one shot. I assume this is ok because v3 files are just
    // code and always small.  Large data which takes too long to copy should use v2.

    // loop over segments
    for (s32 seg_id = ofh->segment_count - 1; seg_id >= 0; seg_id--) {
      // link the infos
      ofh->link_infos[seg_id].offset += m_link_block_ptr.offset;
      ofh->code_infos[seg_id].offset += m_object_data.offset;

      if (seg_id == DEBUG_SEGMENT) {
        if (!DebugSegment) {
          // clear code info if we aren't going to copy the debug segment.
          ofh->code_infos[seg_id].offset = 0;
          ofh->code_infos[seg_id].size = 0;
        } else {
          if (ofh->code_infos[seg_id].size == 0) {
            // not actually present
            ofh->code_infos[seg_id].offset = 0;
          } else {
            Ptr<u8> src(ofh->code_infos[seg_id].offset);
            ofh->code_infos[seg_id].offset =
                kmalloc(kdebugheap, ofh->code_infos[seg_id].size, 0, "debug-segment").offset;
            if (ofh->code_infos[seg_id].offset == 0) {
              MsgErr("dkernel: unable to malloc %d bytes for debug-segment\n",
                     ofh->code_infos[seg_id].size);
              return 1;
            }
            ultimate_memcpy(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
                            ofh->code_infos[seg_id].size);
          }
        }
      } else if (seg_id == MAIN_SEGMENT) {
        if (ofh->code_infos[seg_id].size == 0) {
          ofh->code_infos[seg_id].offset = 0;
        } else {
          Ptr<u8> src(ofh->code_infos[seg_id].offset);
          ofh->code_infos[seg_id].offset =
              kmalloc(m_heap, ofh->code_infos[seg_id].size, 0, "main-segment").offset;
          if (ofh->code_infos[seg_id].offset == 0) {
            MsgErr("dkernel: unable to malloc %d bytes for main-segment\n",
                   ofh->code_infos[seg_id].size);
            return 1;
          }
          ultimate_memcpy(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
                          ofh->code_infos[seg_id].size);
        }
      } else if (seg_id == TOP_LEVEL_SEGMENT) {
        if (ofh->code_infos[seg_id].size == 0) {
          ofh->code_infos[seg_id].offset = 0;
        } else {
          Ptr<u8> src(ofh->code_infos[seg_id].offset);
          ofh->code_infos[seg_id].offset =
              kmalloc(m_heap, ofh->code_infos[seg_id].size, KMALLOC_TOP, "top-level-segment")
                  .offset;
          if (ofh->code_infos[seg_id].offset == 0) {
            MsgErr("dkernel: unable to malloc %d bytes for top-level-segment\n",
                   ofh->code_infos[seg_id].size);
            return 1;
          }
          ultimate_memcpy(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
                          ofh->code_infos[seg_id].size);
        }
      } else {
        printf("UNHANDLED SEG ID IN WORK V3 STATE 1\n");
      }
    }

    m_state = 1;
    m_segment_process = 0;
    return 0;
  } else if (m_state == 1) {
    // state 1: linking. For now all links are done at once. This is probably going to be fine on a
    // modern computer.  But the game broke this into multiple steps.
    if (m_segment_process < ofh->segment_count) {
      if (ofh->code_infos[m_segment_process].offset) {
        Ptr<u8> lp(ofh->link_infos[m_segment_process].offset);

        while (*lp) {
          switch (*lp) {
            case LINK_TABLE_END:
              break;
            case LINK_SYMBOL_OFFSET:
              lp = lp + 1;
              lp = lp + symlink_v3(lp, Ptr<u8>(ofh->code_infos[m_segment_process].offset));
              break;
            case LINK_TYPE_PTR:
              lp = lp + 1;  // seek past id
              lp = lp + typelink_v3(lp, Ptr<u8>(ofh->code_infos[m_segment_process].offset));
              break;
            case LINK_DISTANCE_TO_OTHER_SEG_64:
              lp = lp + 1;
              lp = lp + cross_seg_dist_link_v3(lp, ofh, m_segment_process, 8);
              break;
            case LINK_DISTANCE_TO_OTHER_SEG_32:
              lp = lp + 1;
              lp = lp + cross_seg_dist_link_v3(lp, ofh, m_segment_process, 4);
              break;
            case LINK_PTR:
              lp = lp + 1;
              lp = lp + ptr_link_v3(lp, ofh, m_segment_process);
              break;
            default:
              printf("unknown link table thing %d\n", *lp);
              assert(false);
              break;
          }
        }
      }

      m_segment_process++;
    } else {
      // all done, can set the entry point to the top-level.
      m_entry = Ptr<u8>(ofh->code_infos[TOP_LEVEL_SEGMENT].offset) + 4;
      return 1;
    }

    return 0;
  }

  else {
    printf("WORK v3 INVALID STATE\n");
    return 1;
  }
}

Ptr<u8> c_symlink2(Ptr<u8> objData, Ptr<u8> linkObj, Ptr<u8> relocTable) {
  u8* relocPtr = relocTable.c();
  Ptr<u8> objPtr = objData;

  do {
    u8 table_value = *relocPtr;
    u32 result = table_value;
    u8* next_reloc = relocPtr + 1;

    if (result & 3) {
      result = (relocPtr[1] << 8) | table_value;
      next_reloc = relocPtr + 2;
      if (result & 2) {
        result = (relocPtr[2] << 16) | result;
        next_reloc = relocPtr + 3;
        if (result & 1) {
          result = (relocPtr[3] << 24) | result;
          next_reloc = relocPtr + 4;
        }
      }
    }

    relocPtr = next_reloc;
    objPtr = objPtr + (result & 0xfffffffc);
    u32 objValue = *(objPtr.cast<u32>());
    if (objValue == 0xffffffff) {
      *(objPtr.cast<u32>()) = linkObj.offset;
    } else {
      // I don't think we should hit this ever.
      assert(false);
    }
  } while (*relocPtr);

  return make_ptr(relocPtr + 1);
}

#define LINK_V2_STATE_INIT_COPY 0
#define LINK_V2_STATE_OFFSETS 1
#define LINK_V2_STATE_SYMBOL_TABLE 2
#define OBJ_V2_CLOSE_ENOUGH 0x90
#define OBJ_V2_MAX_TRANSFER 0x80000

uint32_t link_control::work_v2() {
  //  u32 startCycle = kernel.read_clock(); todo

  if (m_state == LINK_V2_STATE_INIT_COPY) {  // initialization and copying to heap
    // we move the data segment to eliminate gaps
    // very small gaps can be tolerated, as it is not worth the time penalty to move large objects
    // many bytes. if this requires copying a large amount of data, we will do it in smaller chunks,
    // allowing the copy to be spread over multiple game frames

    // state initialization
    if (m_segment_process == 0) {
      m_heap_gap =
          m_object_data - m_heap->current;  // distance between end of heap and start of object
      if (m_object_data.offset < m_heap->current.offset) {
        assert(false);
      }
    }

    if (m_heap_gap <
        OBJ_V2_CLOSE_ENOUGH) {  // close enough, don't relocate the object, just expand the heap
      if (link_debug_printfs) {
        printf("[work_v2] close enough, not moving\n");
      }
      m_heap->current = m_object_data + m_code_size;
      if (m_heap->top.offset <= m_heap->current.offset) {
        MsgErr("dkernel: heap overflow\n");  // game has ~% instead of \n :P
        return 1;
      }
    } else {  // not close enough, need to move the object

      // on the first run of this state...
      if (m_segment_process == 0) {
        m_original_object_location = m_object_data;
        // allocate on heap, will have no gap
        m_object_data = kmalloc(m_heap, m_code_size, 0, "data-segment");
        if (link_debug_printfs) {
          printf("[work_v2] moving from 0x%x to 0x%x\n", m_original_object_location.offset,
                 m_object_data.offset);
        }
        if (!m_object_data.offset) {
          MsgErr("dkernel: unable to malloc %d bytes for data-segment\n", m_code_size);
          return 1;
        }
      }

      // the actual copy
      Ptr<u8> source = m_original_object_location + m_segment_process;
      u32 size = m_code_size - m_segment_process;

      if (size > OBJ_V2_MAX_TRANSFER) {  // around .5 MB
        ultimate_memcpy((m_object_data + m_segment_process).c(), source.c(), OBJ_V2_MAX_TRANSFER);
        m_segment_process += OBJ_V2_MAX_TRANSFER;
        return 0;  // return, don't want to take too long.
      }

      // if we have bytes to copy, but they are less than the max transfer, do it in one shot!
      if (size) {
        ultimate_memcpy((m_object_data + m_segment_process).c(), source.c(), size);
        if (m_segment_process > 0) {  // if we did a previous copy, we return now....
          m_state = LINK_V2_STATE_OFFSETS;
          m_segment_process = 0;
          return 0;
        }
      }
    }

    // otherwise go straight into the next state.
    m_state = LINK_V2_STATE_OFFSETS;
    m_segment_process = 0;
  }

  // init offset phase
  if (m_state == LINK_V2_STATE_OFFSETS && m_segment_process == 0) {
    m_reloc_ptr = m_link_block_ptr + 8;  // seek to link table
    if (*m_reloc_ptr == 0) {             // do we have pointer links to do?
      m_reloc_ptr.offset++;              // if not, seek past the \0, and go to next state
      m_state = LINK_V2_STATE_SYMBOL_TABLE;
      m_segment_process = 0;
    } else {
      m_base_ptr = m_object_data;  // base address for offsetting.
      m_loc_ptr = m_object_data;   // pointer which seeks thru the code
      m_table_toggle = 0;          // are we seeking or fixing?
      m_segment_process = 1;       // we've done first time setup
    }
  }

  if (m_state == LINK_V2_STATE_OFFSETS) {  // pointer fixup
    // this state reads through a table. Values alternate between "seek amount" and "number of
    // consecutive 4-byte
    //  words to fix up".  The counts are encoded using a variable length encoding scheme.  They use
    //  a very stupid
    // method of encoding values which requires O(n) bytes to store the value n.

    // to avoid dropping a frame, we check every 0x400 relocations to see if 0.5 milliseconds have
    // elapsed.
    u32 relocCounter = 0x400;
    while (true) {    // loop over entire table
      while (true) {  // loop over current mode

        // read and seek table
        u8 count = *m_reloc_ptr;
        m_reloc_ptr.offset++;

        if (!m_table_toggle) {  // seek mode
          m_loc_ptr.offset +=
              4 *
              count;  // perform seek (MIPS instructions are 4 bytes, so we >> 2 the seek amount)
        } else {      // offset mode
          for (u32 i = 0; i < count; i++) {
            if (m_loc_ptr.offset % 4) {
              assert(false);
            }
            u32 code = *(m_loc_ptr.cast<u32>());
            code += m_base_ptr.offset;
            *(m_loc_ptr.cast<u32>()) = code;
            m_loc_ptr.offset += 4;
          }
        }

        if (count != 0xff) {
          break;
        }

        if (*m_reloc_ptr == 0) {
          m_reloc_ptr.offset++;
          m_table_toggle = m_table_toggle ^ 1;
        }
      }

      // reached the end of the tableToggle mode
      m_table_toggle = m_table_toggle ^ 1;
      if (*m_reloc_ptr == 0) {
        break;  // end of the state
      }
      relocCounter--;
      if (relocCounter == 0) {
        //        u32 clock_value = kernel.read_clock();
        //        if(clock_value - startCycle > 150000) { // 0.5 milliseconds
        //          return 0;
        //        }
        relocCounter = 0x400;
      }
    }
    m_reloc_ptr.offset++;
    m_state = 2;
    m_segment_process = 0;
  }

  if (m_state == 2) {  // GOAL object fixup
    if (*m_reloc_ptr == 0) {
      m_state = 3;
      m_segment_process = 0;
    } else {
      while (true) {
        u32 relocation = *m_reloc_ptr;
        m_reloc_ptr.offset++;
        Ptr<u8> goalObj;
        char* name;
        if ((relocation & 0x80) == 0) {
          // symbol!
          if (relocation > 9) {
            m_reloc_ptr.offset--;  // no idea what this is.
          }
          name = m_reloc_ptr.cast<char>().c();
          if (link_debug_printfs) {
            printf("[work_v2] symlink: %s\n", name);
          }
          goalObj = intern_from_c(name).cast<u8>();
        } else {
          // type!
          u8 nMethods = relocation & 0x7f;
          if (nMethods == 0) {
            nMethods = 1;
          }
          name = m_reloc_ptr.cast<char>().c();
          if (link_debug_printfs) {
            printf("[work_v2] symlink -type: %s\n", name);
          }
          goalObj = intern_type_from_c(name, nMethods).cast<u8>();
        }
        m_reloc_ptr.offset += strlen(name) + 1;
        // DECOMPILER->hookStartSymlinkV3(_state - 1, _objectData, std::string(name));
        m_reloc_ptr = c_symlink2(m_object_data, goalObj, m_reloc_ptr);
        // DECOMPILER->hookFinishSymlinkV3();
        if (*m_reloc_ptr == 0) {
          break;  // done
        }
        //        u32 currentCycle = kernel.read_clock();
        //        if(currentCycle - startCycle > 150000) {
        //          return 0;
        //        }
      }
      m_state = 3;
      m_segment_process = 0;
    }
  }
  m_entry = m_object_data + 4;
  return 1;
}

/*!
 * Complete linking. This will execute the top-level code for v3 object files, if requested.
 */
void link_control::finish() {
  CacheFlush(m_code_start.c(), m_code_size);
  auto old_debug_segment = DebugSegment;
  if (m_keep_debug) {
    // note - this probably doesn't work because DebugSegment isn't *debug-segment*.
    DebugSegment = s7.offset + FIX_SYM_TRUE;
  }
  if (m_flags & LINK_FLAG_FORCE_FAST_LINK) {
    FastLink = 1;
  }
  *EnableMethodSet = *EnableMethodSet + m_keep_debug;

  ObjectFileHeader* ofh = m_link_block_ptr.cast<ObjectFileHeader>().c();
  if (ofh->object_file_version == 3) {
    // todo check function type of entry

    // execute top level!
    if (m_entry.offset && (m_flags & LINK_FLAG_EXECUTE)) {
      call_goal(m_entry.cast<Function>(), 0, 0, 0, s7.offset, g_ee_main_mem);
    }

    // inform compiler that we loaded.
    if (m_flags & LINK_FLAG_OUTPUT_LOAD) {
      output_segment_load(m_object_name, m_link_block_ptr, m_flags);
    }
  } else {
    if (m_flags & LINK_FLAG_EXECUTE) {
      auto entry = m_entry;
      auto name = basename_goal(m_object_name);
      strcpy(Ptr<char>(LINK_CONTROL_NAME_ADDR).c(), name);
      call_method_of_type_arg2(entry.offset, Ptr<Type>(*((entry - 4).cast<u32>())),
                               GOAL_RELOC_METHOD, m_heap.offset,
                               Ptr<char>(LINK_CONTROL_NAME_ADDR).offset);
    }
  }

  *EnableMethodSet = *EnableMethodSet - m_keep_debug;
  FastLink = 0;  // nested fast links won't work right.
  m_heap->top = m_heap_top;
  DebugSegment = old_debug_segment;
}

/*!
 * Immediately link and execute an object file.
 * DONE, EXACT
 */
Ptr<uint8_t> link_and_exec(Ptr<uint8_t> data,
                           const char* name,
                           int32_t size,
                           Ptr<kheapinfo> heap,
                           uint32_t flags) {
  link_control lc;
  lc.begin(data, name, size, heap, flags);
  uint32_t done;
  do {
    done = lc.work();
  } while (!done);
  lc.finish();
  return lc.m_entry;
}

/*!
 * Wrapper so this can be called from GOAL. Not in original game.
 */
u64 link_and_exec_wrapper(u64 data, u64 name, s64 size, u64 heap, u64 flags) {
  return link_and_exec(Ptr<u8>(data), Ptr<char>(name).c(), size, Ptr<kheapinfo>(heap), flags)
      .offset;
}

/*!
 * GOAL exported function for beginning a link with the saved_link_control
 * 47 -> output_load, output_true, execute, 8, force fast
 * 39 -> no 8 (s7)
 */
uint64_t link_begin(uint64_t object_data,
                    uint64_t name,
                    int32_t size,
                    uint64_t heap,
                    uint32_t flags) {
  saved_link_control.begin(Ptr<u8>(object_data), Ptr<char>(name).c(), size, Ptr<kheapinfo>(heap),
                           flags);
  auto work_result = saved_link_control.work();
  // if we managed to finish in one shot, take care of calling finish
  if (work_result) {
    saved_link_control.finish();
  }

  return work_result != 0;
}

/*!
 * GOAL exported function for doing a small amount of linking work on the saved_link_control
 */
uint64_t link_resume() {
  auto work_result = saved_link_control.work();
  if (work_result) {
    saved_link_control.finish();
  }
  return work_result != 0;
}

/*!
 * The ULTIMATE MEMORY COPY
 * IT IS VERY FAST
 * but it may use the scratchpad.  It is implemented in GOAL, and falls back to normal C memcpy
 * if GOAL isn't loaded, or if the alignment isn't good enough.
 */
void* ultimate_memcpy(void* dst, void* src, uint32_t size) {
  // only possible if alignment is good.
  if (!(u64(dst) & 0xf) && !(u64(src) & 0xf) && !(u64(size) & 0xf) && size > 0xfff) {
    if (!gfunc_774.offset) {
      // GOAL function is unknown, lets see if its loaded:
      auto sym = find_symbol_from_c("ultimate-memcpy");
      if (sym->value == 0) {
        return memmove(dst, src, size);
      }
      gfunc_774.offset = sym->value;
    }
    printf("Replacing goal ultimate-memcpy! with memmove\n");
    return memmove(dst, src, size);
    //    return Ptr<u8>(call_goal(gfunc_774, make_u8_ptr(dst).offset, make_u8_ptr(src).offset,
    //    size,
    //                             s7.offset, g_ee_main_mem))
    //        .c();
  } else {
    return memmove(dst, src, size);
  }
}

// The functions below are not ported because they are specific to the MIPS implementation.
// In the MIPS implementation, the c_ functions are used until GOAL loads its GOAL-implemented
// versions of the same functions.  The update_goal_fns detects this and causes the linker to use
// the GOAL versions once possible. The GOAL version is much faster, but functionally equivalent to
// the C version. The C version is compiled without optimization, so this isn't too surprising.
// the rellink function is unused.
/*
c_rellink3__FPvP12link_segmentPUc
c_symlink2__FPvUiPUc
c_symlink3__FPvUiPUc
update_goal_fns__Fv
 */