#include "klink.h"

#include "common/goal_constants.h"
#include "common/symbols.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kscheme.h"

#include "fmt/core.h"

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
 * TODO: this hasn't been carefully checked for jak 2 differences.
 */
void link_control::jak1_jak2_begin(Ptr<uint8_t> object_file,
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
    // not an open goal object.
    if (link_debug_printfs) {
      printf("Linking GOAL style object %s\n", name);
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

        // copy it (was ultimate memcpy)
        memmove(new_link_block.c(), old_link_block.c(), header->length);
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
      // if this is hit - there's a good chance something has overwritten the object file data
      // after linking has started.
      printf("val is 0x%x ptr %p\n", objValue, relocPtr - 1);
      ASSERT(false);
    }
  } while (*relocPtr);

  return make_ptr(relocPtr + 1);
}
