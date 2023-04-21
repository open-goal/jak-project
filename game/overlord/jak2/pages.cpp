#include "pages.h"

#include <cstdio>
#include <cstring>

#include "common/util/Assert.h"

namespace jak2 {

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
