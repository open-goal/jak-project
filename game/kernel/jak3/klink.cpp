#include "klink.h"

#include "common/common_types.h"
#include "common/goal_constants.h"
#include "common/symbols.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/memory_layout.h"
#include "game/kernel/jak3/kmalloc.h"
#include "game/kernel/jak3/kscheme.h"
#include "game/mips2c/mips2c_table.h"

#include "fmt/core.h"

namespace {
bool is_opengoal_object(void* data) {
  u32 first_word;
  memcpy(&first_word, data, 4);
  return first_word != 0 && first_word != UINT32_MAX;
}
constexpr bool link_debug_printfs = false;
}  // namespace

void link_control::jak3_begin(Ptr<uint8_t> object_file,
                              const char* name,
                              int32_t size,
                              Ptr<kheapinfo> heap,
                              uint32_t flags) {
  if (is_opengoal_object(object_file.c())) {
    m_opengoal = true;
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
    m_busy = true;

    if (link_debug_printfs) {
      char* goal_name = object_file.cast<char>().c();
      printf("link %s\n", m_object_name);
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
      ASSERT(false);
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

        // copy it (was ultimate memcpy, but just use normal one to make it easier)
        memmove(new_link_block.c(), old_link_block.c(), ofh->link_block_length);
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
      ASSERT_MSG(false, "UNHANDLED OBJECT FILE VERSION");
    }

    if ((m_flags & LINK_FLAG_FORCE_DEBUG) && MasterDebug && !DiskBoot) {
      m_keep_debug = true;
    }
  } else {
    m_opengoal = false;
    if (heap == kglobalheap) {
      jak3::kmemopen_from_c(heap, name);
      m_on_global_heap = true;
    } else {
      m_on_global_heap = false;
    }
    m_object_data = object_file;
    kstrcpy(this->m_object_name, name);
    m_object_size = size;
    // l_hdr = (LinkHdrWithType*)this->m_object_data;
    LinkHeaderV5* l_hdr = (LinkHeaderV5*)m_object_data.c();
    m_flags = flags;
    u16 version = l_hdr->core.version;

    if (version == 4 || version == 2) {
      // it's a v4 produced by opengoal... lets just try using jak2's linker
      m_version = version;
      printf("got version 4, falling back to jak1/jak2\n");
      jak1_jak2_begin(object_file, name, size, heap, flags);
      return;
    }
    ASSERT(version == 5);
    m_heap_top = heap->top;
    // this->unk_init1 = 1; TODO
    m_busy = true;
    m_heap = heap;
    m_entry.offset = 0;
    m_keep_debug = false;
    m_link_hdr = &l_hdr->core;  // m_hdr_ptr
    m_code_size = 0;
    // this->m_ptr_2 = l_hdr; just used for cache flush, so skip it! not really the right thing??
    m_state = 0;
    m_segment_process = 0;
    m_moved_link_block = 0;
    if (version == 4) {
      ASSERT_NOT_REACHED();
    } else {
      m_object_data.offset = object_file.offset + l_hdr->core.length_to_get_to_code;
      if (version == 5) {
        static_assert(0x50 == sizeof(LinkHeaderV5));
        size = (size - l_hdr->core.link_length) - sizeof(LinkHeaderV5);
      } else {
        ASSERT_NOT_REACHED();
      }
      m_code_size = size;
      if ((u8*)m_link_hdr < m_heap->base.c() || (u8*)m_link_hdr >= m_heap->top.c()) {
        // the link block is outside our heap, or in the allocated top part.
        // so we ignore it, and leave it as somebody else's problem.

        // let's try to move the code part:
        if (m_heap->base.offset <= m_object_data.offset &&    // above heap base
            m_object_data.offset < m_heap->top.offset &&      // less than heap top (not needed?)
            m_object_data.offset < m_heap->current.offset) {  // less than heap current
          if (link_debug_printfs) {
            printf("Code block in the heap, kicking it out for copy into heap\n");
          }
          m_heap->current = m_object_data;
        }
      } else {
        // the link block is in the heap. This is problematic because we don't want to hang
        // on to this long term, but executing the top-level may allocate on this heap, causing
        // stuff to get added after the hole left by the link data.
        // So, we make a temporary allocation on the top and move it there.

        m_moved_link_block = true;
        Ptr<u8> new_link_block_mem;
        u8* link_block_move_dst;
        u8* old_link_block;
        u32 link_block_move_size;

        if (m_link_hdr->version == 5) {
          // the link block is inside our heap, but we'd like to avoid this.
          // we'll copy the link block, and the header to the temporary part of our heap:

          // where we loaded the link data:
          auto offset_to_link_data = m_link_hdr->length_to_get_to_link;

          // allocate memory for link data, and header (pvVar5)
          new_link_block_mem = kmalloc(m_heap, m_link_hdr->link_length + sizeof(LinkHeaderV5),
                                       KMALLOC_TOP, "link-block");

          // we'll place the header and link block back to back in the newly alloated block,
          // so patch up the offset for this new layout before copying
          m_link_hdr->length_to_get_to_link = sizeof(LinkHeaderV5);

          // move header!
          memmove(new_link_block_mem.c(), object_file.c(), sizeof(LinkHeaderV5));

          // dst: pvVar6
          link_block_move_dst = new_link_block_mem.c() + sizeof(LinkHeaderV5);

          // move link data! (pcVar8)
          old_link_block = object_file.c() + offset_to_link_data;

          link_block_move_size = m_link_hdr->link_length;
        } else {
          // hm, maybe only possible with version 2 or 3??
          ASSERT_NOT_REACHED();
        }

        memmove(link_block_move_dst, old_link_block, link_block_move_size);

        // update our pointer to the link header core.
        m_link_hdr = &((LinkHeaderV5*)new_link_block_mem.c())->core;

        // scary: update the heap to kick out all the link data (and likely the actual data too).
        // we'll be relying on the linking process to copy the data as needed.l
        if (old_link_block < m_heap->current.c()) {
          if (link_debug_printfs) {
            printf("Kick out old link block\n");
          }
          m_heap->current.offset = old_link_block - g_ee_main_mem;
        }
      }
    }
    if ((m_flags & LINK_FLAG_FORCE_DEBUG) && MasterDebug && !DiskBoot) {
      m_keep_debug = true;
    }
    // hack:
    m_version = m_link_hdr->version;
  }
}

uint32_t link_control::jak3_work() {
  auto old_debug_segment = DebugSegment;
  if (m_keep_debug) {
    DebugSegment = s7.offset + true_symbol_offset(g_game_version);
  }

  // set type tag of link block

  uint32_t rv;

  if (m_version == 3) {
    ASSERT(m_opengoal);
    *((m_link_block_ptr - 4).cast<u32>()) =
        *((s7 + jak3_symbols::FIX_SYM_LINK_BLOCK - 1).cast<u32>());
    rv = jak3_work_opengoal();
  } else if (m_version == 5) {
    ASSERT(!m_opengoal);
    *(u32*)(((u8*)m_link_hdr) - 4) = *((s7 + jak3_symbols::FIX_SYM_LINK_BLOCK - 1).cast<u32>());
    rv = jak3_work_v5();
  } else if (m_version == 4 || m_version == 2) {
    // Note: this is a bit of a hack. Jak 3 doesn't support v2/v4. But, OpenGOAL generates data
    // objects in this format. We will just try reusing the jak 2 v2/v4 linker here and see if it
    // works. See corresponding call to jak1_jak2_begin in begin.
    rv = jak3_work_v2_v4();
  } else {
    ASSERT_MSG(false, fmt::format("UNHANDLED OBJECT FILE VERSION {} IN WORK!", m_version));
    return 0;
  }

  DebugSegment = old_debug_segment;
  return rv;
}

namespace jak3 {
void ultimate_memcpy(void* dst, void* src, uint32_t size);
}

uint32_t link_control::jak3_work_v5() {
  if (m_state == 0) {
    // here, we change length_to_get_to_link to an actual pointer to the link table.
    // since we need 32-bits, we'll store offset from g_ee_mem.
    u8* link_data = ((u8*)m_link_hdr) - 4 + m_link_hdr->length_to_get_to_link;
    m_link_hdr->length_to_get_to_link = link_data - g_ee_main_mem;

    m_n_segments = m_link_hdr->n_segments;

    // the link segments table is just at the start of the link data:
    m_link_segments_table = (SegmentInfoV5*)link_data;
    /*
    for (int i = 0; i < m_n_segments; i++) {
      printf(" %d: reloc %d, data %d, size %d, magic %d\n", i, m_link_segments_table[i].relocs,
             m_link_segments_table[i].data, m_link_segments_table[i].size,
             m_link_segments_table[i].magic);
    }
    */
    // for now, only supporting 1 segment
    ASSERT(m_n_segments == 1);

    // fixup the relocs/data offsets into addresses (again, offsets from g_ee_main_mem)
    // relocs is relative to this link data
    m_link_segments_table[0].relocs += (link_data - g_ee_main_mem);
    // data is relative to usual object_data
    m_link_segments_table[0].data += m_object_data.offset;
    ASSERT(m_link_segments_table[0].magic == 1);

    // see if there's even data
    if (m_link_segments_table[0].size == 0) {
      // no data.
      m_link_segments_table[0].data = 0;
    } else {
      // check if we need to move the main segment.
      if (!m_moved_link_block ||
          ((m_link_hdr->link_length + 0x50) <= m_link_hdr->length_to_get_to_code)) {
        // printf(" v5 linker allocating for main segment... (%d)\n", m_moved_link_block);
        auto old_data_offset = m_link_segments_table[0].data;  // 25
        auto new_data = kmalloc(m_heap, m_link_segments_table[0].size, 0, "main-segment");
        m_link_segments_table[0].data = new_data.offset;
        if (!new_data.offset) {
          MsgErr("dkernel: unable to malloc %d bytes for main-segment\n",
                 m_link_segments_table[0].size);
          return 1;
        }
        jak3::ultimate_memcpy(new_data.c(), old_data_offset + g_ee_main_mem,
                              m_link_segments_table[0].size);
      } else {
        m_heap->current = m_object_data + m_code_size;
        if (m_heap->top.offset <= m_heap->current.offset) {
          MsgErr("dkernel: heap overflow\n");
          return 1;
        }
      }
    }

    m_segment_process = 0;
    m_state = 1;
    m_object_data.offset = m_link_segments_table[0].data;

    Ptr<u8> base_ptr(m_link_segments_table[0].data);
    Ptr<u8> data_ptr = base_ptr - 4;
    Ptr<u8> link_ptr(m_link_segments_table[0].relocs);

    bool fixing = false;
    if (*link_ptr) {
      // we have pointers
      while (true) {
        while (true) {
          if (!fixing) {
            // seeking
            data_ptr.offset += 4 * (*link_ptr);
          } else {
            // fixing.
            for (uint32_t i = 0; i < *link_ptr; i++) {
              // uint32_t old_code = *(const uint32_t*)(&data.at(data_ptr));
              u32 old_code = *data_ptr.cast<u32>();
              if ((old_code >> 24) == 0) {
                // printf("modifying pointer at 0x%x (old 0x%x) : now ", data_ptr.offset,
                //      *data_ptr.cast<u32>());
                *data_ptr.cast<u32>() += base_ptr.offset;
                // printf("0x%x\n", *data_ptr.cast<u32>());
              } else {
                ASSERT_NOT_REACHED();
                /*
                f.stats.v3_split_pointers++;
                auto dest_seg = (old_code >> 8) & 0xf;
                auto lo_hi_offset = (old_code >> 12) & 0xf;
                ASSERT(lo_hi_offset);
                ASSERT(dest_seg < 3);
                auto offset_upper = old_code & 0xff;
                uint32_t low_code = *(const uint32_t*)(&data.at(data_ptr + 4 * lo_hi_offset));
                uint32_t offset = low_code & 0xffff;
                if (offset_upper) {
                  offset += (offset_upper << 16);
                }
                f.pointer_link_split_word(seg_id, data_ptr - base_ptr,
                                          data_ptr + 4 * lo_hi_offset - base_ptr, dest_seg, offset);
                */
              }
              data_ptr.offset += 4;
            }
          }

          if (*link_ptr != 0xff)
            break;
          link_ptr.offset++;
          if (*link_ptr == 0) {
            link_ptr.offset++;
            fixing = !fixing;
          }
        }

        link_ptr.offset++;
        fixing = !fixing;
        if (*link_ptr == 0)
          break;
      }
    }
    link_ptr.offset++;

    // symbol linking.
    if (*link_ptr) {
      auto sub_link_ptr = link_ptr;

      while (true) {
        auto reloc = *sub_link_ptr;
        auto next_link_ptr = sub_link_ptr + 1;
        link_ptr = next_link_ptr;

        if ((reloc & 0x80) == 0) {
          link_ptr = sub_link_ptr + 3;  //
          const char* sname = link_ptr.cast<char>().c();
          link_ptr.offset += strlen(sname) + 1;
          // printf("linking symbol %s\n", sname);
          auto goalObj = jak3::intern_from_c(-1, 0, sname);
          link_ptr = c_symlink2(m_object_data, goalObj.cast<u8>(), link_ptr);

        } else if ((reloc & 0x3f) == 0x3f) {
          ASSERT(false);  // todo, does this ever get hit?
        } else {
          int n_methods_base = reloc & 0x3f;
          int n_methods = n_methods_base * 4;
          if (n_methods_base) {
            n_methods += 3;
          }
          link_ptr.offset +=
              2;  // ghidra misses some aliasing here and would have you think this is +1!
          const char* sname = link_ptr.cast<char>().c();
          // printf("linking type %s\n", sname);
          link_ptr.offset += strlen(sname) + 1;
          auto goalObj = jak3::intern_type_from_c(-1, 0, sname, n_methods);
          link_ptr = c_symlink2(m_object_data, goalObj.cast<u8>(), link_ptr);
        }

        sub_link_ptr = link_ptr;
        if (!*sub_link_ptr)
          break;
      }
    }
    m_entry = m_object_data + 4;
    return 1;
  } else {
    ASSERT_NOT_REACHED();
  }
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
  auto type_ptr = jak3::intern_type_from_c(-1, 0, sym_name, method_count);

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
  auto sym = jak3::intern_from_c(-1, 0, sym_name);
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

uint32_t link_control::jak3_work_opengoal() {
  // note: I'm assuming that the allocation we used in jak2/jak1 will still work here. Once work_v5
  // is done, we could revisit this.
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
            memmove(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
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
          memmove(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
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
          memmove(Ptr<u8>(ofh->code_infos[seg_id].offset).c(), src.c(),
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

void link_control::jak3_finish(bool jump_from_c_to_goal) {
  // CacheFlush(this->m_ptr_2, this->m_code_size);
  auto old_debug_segment = DebugSegment;
  if (m_keep_debug) {
    // note - this probably doesn't work because DebugSegment isn't *debug-segment*.
    DebugSegment = s7.offset + jak3_symbols::FIX_SYM_TRUE;
  }
  if (m_flags & LINK_FLAG_FORCE_FAST_LINK) {
    FastLink = 1;
  }

  *EnableMethodSet = *EnableMethodSet + m_keep_debug;

  // printf("finish %s\n", m_object_name);
  if (m_opengoal) {
    // setup mips2c functions
    const auto& it = Mips2C::gMips2CLinkCallbacks[GameVersion::Jak3].find(m_object_name);
    if (it != Mips2C::gMips2CLinkCallbacks[GameVersion::Jak3].end()) {
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
      // printf(" about to call... (0x%x)\n", entry.offset);
      Ptr<jak3::Type> type(*((entry - 4).cast<u32>()));
      // printf(" type is %s\n", jak3::sym_to_cstring(type->symbol));
      jak3::call_method_of_type_arg2(entry.offset, type, GOAL_RELOC_METHOD, m_heap.offset,
                                     Ptr<char>(LINK_CONTROL_NAME_ADDR).offset);
      // printf("  done with call!\n");
    }
  }

  *EnableMethodSet = *EnableMethodSet - this->m_keep_debug;
  FastLink = 0;
  m_heap->top = m_heap_top;
  DebugSegment = old_debug_segment;

  m_busy = false;
  if (m_on_global_heap) {
    jak3::kmemclose();
  }
  return;
}

namespace jak3 {

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
  lc.jak3_begin(data, name, size, heap, flags);
  uint32_t done;
  do {
    done = lc.jak3_work();
  } while (!done);
  lc.jak3_finish(jump_from_c_to_goal);
  return lc.m_entry;
}

u64 link_and_exec_wrapper(u64* args) {
  return link_and_exec(Ptr<u8>(args[0]), Ptr<char>(args[1]).c(), args[2], Ptr<kheapinfo>(args[3]),
                       args[4], false)
      .offset;
}

u32 link_busy() {
  return saved_link_control.m_busy;
}
void link_reset() {
  saved_link_control.m_busy = 0;
}
uint64_t link_begin(u64* args) {
  saved_link_control.jak3_begin(Ptr<u8>(args[0]), Ptr<char>(args[1]).c(), args[2],
                                Ptr<kheapinfo>(args[3]), args[4]);
  auto work_result = saved_link_control.jak3_work();
  // if we managed to finish in one shot, take care of calling finish
  if (work_result) {
    // called from goal
    saved_link_control.jak3_finish(false);
  }
  return work_result != 0;
}
uint64_t link_resume() {
  auto work_result = saved_link_control.jak3_work();
  if (work_result) {
    // called from goal
    saved_link_control.jak3_finish(false);
  }
  return work_result != 0;
}

// Note: update_goal_fns changed to skip the hashtable lookup since symlink2/symlink3 are now fixed
// symbols.

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
      auto sym_val = *((s7 + jak3_symbols::FIX_SYM_ULTIMATE_MEMCPY - 1).cast<u32>());
      if (sym_val == 0) {
        memmove(dst, src, size);
        return;
      }
      gfunc_774.offset = sym_val;
    }

    Ptr<u8>(call_goal(gfunc_774, make_u8_ptr(dst).offset, make_u8_ptr(src).offset, size, s7.offset,
                      g_ee_main_mem))
        .c();
  } else {
    memmove(dst, src, size);
  }
}

}  // namespace jak3

#define LINK_V2_STATE_INIT_COPY 0
#define LINK_V2_STATE_OFFSETS 1
#define LINK_V2_STATE_SYMBOL_TABLE 2
#define OBJ_V2_CLOSE_ENOUGH 0x90
#define OBJ_V2_MAX_TRANSFER 0x80000

uint32_t link_control::jak3_work_v2_v4() {
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
        jak3::ultimate_memcpy((m_object_data + m_segment_process).c(), source.c(),
                              OBJ_V2_MAX_TRANSFER);
        m_segment_process += OBJ_V2_MAX_TRANSFER;
        return 0;  // return, don't want to take too long.
      }

      // if we have bytes to copy, but they are less than the max transfer, do it in one shot!
      if (size) {
        jak3::ultimate_memcpy((m_object_data + m_segment_process).c(), source.c(), size);
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
          goalObj = jak3::intern_from_c(-1, 0, name).cast<u8>();
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
          goalObj = jak3::intern_type_from_c(-1, 0, name, nMethods).cast<u8>();
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
