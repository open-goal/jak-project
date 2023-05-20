#pragma once

#include "common/common_types.h"

namespace jak2 {
enum class PageState { FREE = 0, ALLOCATED_EMPTY = 3, ALLOCATED_FILLED = 4, SIX = 6 };

/*!
 * A linked list of pages associated with a single read.
 * Each "page" points to a buffer of memory.
 */
struct Page {
  // FREE = belongs to the pool, ALLOCATED_EMPTY = in a chain, but no data
  PageState state;

  int maybe_page_id;

  // how many pages in the chain after this one
  int pages_after_this;

  // how many pages are ALLOCATED_EMPTY in the chain?
  int free_pages;

  // the memory for the page
  u8* buffer;

  u8* ptr;

  Page* prev;
  Page* next;
  Page* end_page_first_only;
};

struct PageList {
  u32 page_count;
  u32 page_size;
  u32 sector_per_page;  // round down
  u32 free_pages;
  u8* page_memory;
  Page* pages;
};

constexpr int MAX_PAGES_IN_POOL = 0x12;
void InitPagedMemory(PageList* pool, int page_count, int page_size);
Page* AllocPagesBytes(PageList* page_list, u32 size_bytes);
Page* AllocPages(PageList* page_list, u32 num_pages);
Page* FreePagesList(PageList* page_list, Page* pages);
void FromPagesCopy(const Page* page, const u8* page_ptr, u8* dest, int bytes_to_copy);
Page* StepTopPage(PageList* param_1, Page* top_page);
}  // namespace jak2
