#include "pages.h"

#include <cstdio>
#include <cstring>

#include "common/util/Assert.h"

#include "game/overlord/jak2/iso_queue.h"
#include "game/sce/iop.h"

using namespace iop;
namespace jak2 {

void InitPagedMemory(PageList* pool, int page_count, int page_size) {
  // this is such a hack...
  // we assume that we're allocated on the scratchpad, and can jump bump the pointer again.
  pool->pages = (Page*)(pool + 1);
  pool->page_count = page_count;
  pool->page_size = page_size;
  ScratchPadMemory = ScratchPadMemory + page_count * sizeof(Page);
  int fixed_page_size = page_size;
  if (page_size < 0) {
    fixed_page_size = page_size + 0x7ff;
  }
  pool->sector_per_page = fixed_page_size >> 0xb;
  pool->free_pages = page_count;
  pool->page_memory = nullptr;
  uintptr_t addr = (uintptr_t)AllocSysMemory(0, page_count * page_size + 0x100, nullptr);
  addr += 0x3f;
  addr &= ~uintptr_t(63);
  u8* mem = (u8*)addr;
  if (mem == 0) {
    printf("======================================================================\n");
    printf("IOP: pages InitPagedMemory: no memory for pages\n");
    printf("======================================================================\n");
    ASSERT_NOT_REACHED();
  }
  Page* page = pool->pages;
  pool->page_memory = mem;
  if (0 < page_count) {
    for (int i = 0; i < page_count; i++) {
      page->state = PageState::FREE;
      page->buffer = mem;
      page->maybe_page_id = i;
      page->free_pages = 0;
      page->ptr = mem + page_size - 1;
      page->next = nullptr;
      page->prev = nullptr;
      page->end_page_first_only = nullptr;
      page++;
      mem += page_size;
    }
  }
}

/*!
 * Allocate a list of pages that contain at least size_bytes in total.
 */
Page* AllocPagesBytes(PageList* page_list, u32 size_bytes) {
  u32 num_pages = (size_bytes + page_list->page_size - 1) / page_list->page_size;
  return AllocPages(page_list, num_pages);
}

/*!
 * Allocate a list of pages.
 */
Page* AllocPages(PageList* page_list, u32 num_pages) {
  if (page_list->page_count < num_pages) {
    printf("======================================================================\n");
    printf("IOP: pages AllocPages: %d pages requested %d maximum pages\n", num_pages,
           page_list->page_count);
    printf("======================================================================\n");
    ASSERT_NOT_REACHED();
  }
  if (page_list->free_pages < num_pages) {
    printf("======================================================================\n");
    printf("IOP: pages AllocPages: need %d only %d free pages\n", num_pages, page_list->free_pages);
    printf("======================================================================\n");
    num_pages = page_list->free_pages;
    ASSERT_NOT_REACHED();  // added.
  }

  Page* first_page = nullptr;
  if (num_pages == 0) {
    first_page = nullptr;
  } else {
    Page* prev_page = nullptr;
    int page_idx_in_page_list = 0;
    u32 added_pages = 0;
    Page* iter = page_list->pages;
    do {
      if (iter->state == PageState::FREE) {
        added_pages++;
        page_list->free_pages = page_list->free_pages + -1;
        iter->state = PageState::ALLOCATED_EMPTY;
        iter->pages_after_this = num_pages - added_pages;
        if (!first_page) {
          iter->prev = nullptr;
          first_page = iter;
        } else {
          prev_page->next = iter;
          iter->prev = prev_page;
        }
        iter->end_page_first_only = nullptr;
        prev_page = iter;
      }
      page_idx_in_page_list++;
      iter++;
    } while ((page_idx_in_page_list < MAX_PAGES_IN_POOL) && (added_pages < num_pages));
    prev_page->next = nullptr;
    first_page->end_page_first_only = prev_page;
    first_page->free_pages = added_pages;
  }
  return first_page;
}

/*!
 * Return the linked list of pages to the page list.
 */
Page* FreePagesList(PageList* page_list, Page* pages) {
  if (pages) {
    if (pages->prev) {
      printf("======================================================================\n");
      printf("IOP: pages FreePages: First page %d is not top of list\n", pages->maybe_page_id);
      printf("======================================================================\n");
      ASSERT_NOT_REACHED();
    }
    Page* next;
    do {
      next = pages->next;
      pages->prev = nullptr;
      pages->next = nullptr;
      pages->end_page_first_only = nullptr;
      pages->state = PageState::FREE;
      page_list->free_pages = page_list->free_pages + 1;
      pages = next;
    } while (next);
  }
  return nullptr;
}

/*!
 * Free the "top" page (the first one)
 */
Page* StepTopPage(PageList* param_1, Page* top_page) {
  Page* result = nullptr;

  if (top_page) {
    // if we're first, shouldn't have a prev.
    if (top_page->prev) {
      printf("======================================================================\n");
      printf("IOP: pages StepTopPage: Page %d is not top of list\n", top_page->maybe_page_id);
      printf("======================================================================\n");
      ASSERT_NOT_REACHED();
    }

    // new top page
    result = top_page->next;
    if (result) {
      // set up new top page.
      result->free_pages = top_page->free_pages + -1;
      result->prev = nullptr;
      result->end_page_first_only = top_page->end_page_first_only;
    }

    // return to pool
    top_page->next = nullptr;
    top_page->state = PageState::FREE;
    param_1->free_pages = param_1->free_pages + 1;
  }
  return result;
}

/*!
 * Copy bytes from pages
 */
void FromPagesCopy(const Page* page, const u8* page_ptr, u8* dest, int bytes_to_copy) {
  Page* next_page;

  const auto* page_data_end = page->ptr;
  do {
    auto n_this_time = (page_data_end - page_ptr) + 1;
    do {
      while (true) {
        if (!bytes_to_copy) {
          return;
        }
        if (n_this_time <= bytes_to_copy)
          break;
        memcpy(dest, page_ptr, bytes_to_copy);
        bytes_to_copy = 0;
      }
      memcpy(dest, page_ptr, n_this_time);
      dest = dest + n_this_time;
      next_page = page->next;
      bytes_to_copy = bytes_to_copy - n_this_time;
    } while (!next_page);
    page_ptr = (uint8_t*)next_page->buffer;
    page_data_end = next_page->ptr;
    page = next_page;
  } while (true);
}

}  // namespace jak2
