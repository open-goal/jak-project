#include "klink.h"

#include "common/goal_constants.h"
#include "common/log/log.h"
#include "common/symbols.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/common/memory_layout.h"
#include "game/kernel/jak2/kscheme.h"
#include "game/mips2c/mips2c_table.h"

#include "fmt/core.h"

static constexpr bool link_debug_printfs = false;

/*!
 * Make progress on linking.
 */
uint32_t link_control::jak2_work() {
  auto old_debug_segment = DebugSegment;
  if (m_keep_debug) {
    DebugSegment = s7.offset + true_symbol_offset(g_game_version);
  }

  // set type tag of link block
  *((m_link_block_ptr - 4).cast<u32>()) =
      *((s7 + jak2_symbols::FIX_SYM_LINK_BLOCK - 1).cast<u32>());

  uint32_t rv;

  if (m_version == 3) {
    ASSERT(m_opengoal);
    rv = jak2_work_v3();
  } else if (m_version == 2 || m_version == 4) {
    ASSERT(!m_opengoal);
    rv = jak2_work_v2();
  } else {
    ASSERT_MSG(false, fmt::format("UNHANDLED OBJECT FILE VERSION {} IN WORK!", m_version));
    return 0;
  }

  DebugSegment = old_debug_segment;
  return rv;
}

namespace {
/*!
 * Link a single relative offset (used for RIP)
 */
uint32_t cross_seg_dist_link_v3(Ptr<uint8_t> link,
                                ObjectFileHeader* ofh,
                                int current_seg,
                                int size) {
  // target seg, dist into mine, dist into target, patch loc in mine
  uint8_t target_seg = *link;
  ASSERT(target_seg < ofh->segment_count);

  uint32_t* link_data = (link + 1).cast<uint32_t>().c();
  int32_t mine = link_data[0] + ofh->code_infos[current_seg].offset;
  int32_t tgt = link_data[1] + ofh->code_infos[target_seg].offset;
  int32_t diff = tgt - mine;
  uint32_t offset_of_patch = link_data[2] + ofh->code_infos[current_seg].offset;

  // second debug segment case added for jak 2.
  if (!ofh->code_infos[target_seg].offset || (!DebugSegment && target_seg == DEBUG_SEGMENT)) {
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
    ASSERT(false);
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
    ASSERT(seek < 256);
  }
  sym_name[seek] = 0;
  seek++;

  // determine the number of methods
  uint32_t method_count = link.c()[seek++];
  // jak2 special
  method_count *= 4;
  if (method_count) {
    method_count += 3;
  }

  // intern the GOAL type, creating the vtable if it doesn't exist.
  auto type_ptr = jak2::intern_type_from_c(sym_name, method_count);

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
    ASSERT(seek < 256);
  }
  sym_name[seek] = 0;
  seek++;

  // intern
  auto sym = jak2::intern_from_c(sym_name);
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
    } else if (*(data_ptr.cast<u32>()) == LINK_SYM_NO_OFFSET_FLAG) {
      *(data + offset).cast<int32_t>() = sym_offset - 1;
    } else {
      // otherwise store the offset to st.
      *(data + offset).cast<int32_t>() = sym_offset;
    }
  }

  return seek;
}
}  // namespace

/*!
 * Run the linker. For now, all linking is done in two runs.  If this turns out to be too slow,
 * this should be modified to do incremental linking over multiple runs.
 */
uint32_t link_control::jak2_work_v3() {
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
            jak2::ultimate_memcpy(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
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
          jak2::ultimate_memcpy(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
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
          jak2::ultimate_memcpy(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
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
              ASSERT_MSG(false, fmt::format("unknown link table thing {}", *lp));
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

#define LINK_V2_STATE_INIT_COPY 0
#define LINK_V2_STATE_OFFSETS 1
#define LINK_V2_STATE_SYMBOL_TABLE 2
#define OBJ_V2_CLOSE_ENOUGH 0x90
#define OBJ_V2_MAX_TRANSFER 0x80000

uint32_t link_control::jak2_work_v2() {
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

      // added in jak 2, move the link block to the top of the heap so we can allocate on
      // the level heap during linking without overwriting link data. this is used for level types
      u32 link_block_size = *m_link_block_ptr.cast<u32>();
      auto new_link_block = kmalloc(m_heap, link_block_size, KMALLOC_TOP, "link-block");
      memmove(new_link_block.c(), m_link_block_ptr.c() - 4, link_block_size);
      m_link_block_ptr = Ptr<uint8_t>(new_link_block.offset + 4);  // basic offset

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
        jak2::ultimate_memcpy((m_object_data + m_segment_process).c(), source.c(),
                              OBJ_V2_MAX_TRANSFER);
        m_segment_process += OBJ_V2_MAX_TRANSFER;
        return 0;  // return, don't want to take too long.
      }

      // if we have bytes to copy, but they are less than the max transfer, do it in one shot!
      if (size) {
        jak2::ultimate_memcpy((m_object_data + m_segment_process).c(), source.c(), size);
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
              ASSERT(false);
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
          goalObj = jak2::intern_from_c(name).cast<u8>();
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
          goalObj = jak2::intern_type_from_c(name, nMethods).cast<u8>();
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
void link_control::jak2_finish(bool jump_from_c_to_goal) {
  CacheFlush(m_code_start.c(), m_code_size);
  auto old_debug_segment = DebugSegment;
  if (m_keep_debug) {
    // note - this probably doesn't work because DebugSegment isn't *debug-segment*.
    DebugSegment = s7.offset + jak2_symbols::FIX_SYM_TRUE;
  }
  if (m_flags & LINK_FLAG_FORCE_FAST_LINK) {
    FastLink = 1;
  }
  *EnableMethodSet = *EnableMethodSet + m_keep_debug;

  ObjectFileHeader* ofh = m_link_block_ptr.cast<ObjectFileHeader>().c();
  lg::debug("link finish: {}", m_object_name);
  if (ofh->object_file_version == 3) {
    // todo check function type of entry

    // setup mips2c functions
    const auto& it = Mips2C::gMips2CLinkCallbacks[GameVersion::Jak2].find(m_object_name);
    if (it != Mips2C::gMips2CLinkCallbacks[GameVersion::Jak2].end()) {
      for (auto& x : it->second) {
        x();
      }
    }

    // execute top level!
    if (m_entry.offset && (m_flags & LINK_FLAG_EXECUTE)) {
      if (jump_from_c_to_goal) {
        u64 goal_stack = u64(g_ee_main_mem) + EE_MAIN_MEM_SIZE - 8;
        call_goal_on_stack(m_entry.cast<Function>(), goal_stack, s7.offset, g_ee_main_mem);
      } else {
        call_goal(m_entry.cast<Function>(), 0, 0, 0, s7.offset, g_ee_main_mem);
      }
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
      jak2::call_method_of_type_arg2(entry.offset, Ptr<jak2::Type>(*((entry - 4).cast<u32>())),
                                     GOAL_RELOC_METHOD, m_heap.offset,
                                     Ptr<char>(LINK_CONTROL_NAME_ADDR).offset);
    }
  }

  *EnableMethodSet = *EnableMethodSet - m_keep_debug;
  FastLink = 0;  // nested fast links won't work right.
  m_heap->top = m_heap_top;
  DebugSegment = old_debug_segment;
  m_busy = false;
}

namespace jak2 {
u32 link_busy() {
  return saved_link_control.m_busy;
}

void link_reset() {
  // seems like a bad idea to do this - you'll probably leak memory.
  saved_link_control.m_busy = 0;
}

Ptr<uint8_t> link_and_exec(Ptr<uint8_t> data,
                           const char* name,
                           int32_t size,
                           Ptr<kheapinfo> heap,
                           uint32_t flags,
                           bool jump_from_c_to_goal) {
  if (link_busy()) {
    printf("-------------> saved link is busy\n");
    // probably won't end well...
  }
  link_control lc;
  lc.jak1_jak2_begin(data, name, size, heap, flags);
  uint32_t done;
  do {
    done = lc.jak2_work();
  } while (!done);
  lc.jak2_finish(jump_from_c_to_goal);
  return lc.m_entry;
}

/*!
 * Wrapper so this can be called from GOAL. Not in original game.
 */
u64 link_and_exec_wrapper(u64* args) {
  // data, name, size, heap, flags
  return link_and_exec(Ptr<u8>(args[0]), Ptr<char>(args[1]).c(), args[2], Ptr<kheapinfo>(args[3]),
                       args[4], false)
      .offset;
}

/*!
 * GOAL exported function for beginning a link with the saved_link_control
 * 47 -> output_load, output_true, execute, 8, force fast
 * 39 -> no 8 (s7)
 */
uint64_t link_begin(u64* args) {
  // object data, name size, heap flags
  saved_link_control.jak1_jak2_begin(Ptr<u8>(args[0]), Ptr<char>(args[1]).c(), args[2],
                                     Ptr<kheapinfo>(args[3]), args[4]);
  auto work_result = saved_link_control.jak2_work();
  // if we managed to finish in one shot, take care of calling finish
  if (work_result) {
    // called from goal
    saved_link_control.jak2_finish(false);
  }

  return work_result != 0;
}

/*!
 * GOAL exported function for doing a small amount of linking work on the saved_link_control
 */
uint64_t link_resume() {
  auto work_result = saved_link_control.jak2_work();
  if (work_result) {
    // called from goal
    saved_link_control.jak2_finish(false);
  }
  return work_result != 0;
}

/*!
 * The ULTIMATE MEMORY COPY
 * IT IS VERY FAST
 * but it may use the scratchpad.  It is implemented in GOAL, and falls back to normal C memcpy
 * if GOAL isn't loaded, or if the alignment isn't good enough.
 */
void ultimate_memcpy(void* dst, void* src, uint32_t size) {
  // only possible if alignment is good.
  if (!(u64(dst) & 0xf) && !(u64(src) & 0xf) && !(u64(size) & 0xf) && size > 0xfff) {
    if (!gfunc_774.offset) {
      // GOAL function is unknown, lets see if its loaded:
      auto sym = jak2::find_symbol_from_c("ultimate-memcpy");
      if (sym->value() == 0) {
        memmove(dst, src, size);
        return;
      }
      gfunc_774.offset = sym->value();
    }

    Ptr<u8>(call_goal(gfunc_774, make_u8_ptr(dst).offset, make_u8_ptr(src).offset, size, s7.offset,
                      g_ee_main_mem))
        .c();
  } else {
    memmove(dst, src, size);
  }
}
}  // namespace jak2
