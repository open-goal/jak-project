#include "BucketRenderer.h"

#include "third-party/fmt/core.h"

std::string BucketRenderer::name_and_id() const {
  return fmt::format("[{:2d}] {}", (int)m_my_id, m_name);
}

EmptyBucketRenderer::EmptyBucketRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {}

void EmptyBucketRenderer::render(DmaFollower& dma,
                                 SharedRenderState* render_state,
                                 ScopedProfilerNode& /*prof*/) {
  // an empty bucket should have 4 things:
  // a NEXT in the bucket buffer
  // a CALL that calls the default register buffer chain
  // a CNT then RET to get out of the default register buffer chain
  // a NEXT to get to the next bucket.

  // NEXT
  auto first_tag = dma.current_tag();
  dma.read_and_advance();
  assert(first_tag.kind == DmaTag::Kind::NEXT && first_tag.qwc == 0);

  // CALL
  auto call_tag = dma.current_tag();
  dma.read_and_advance();
  if (!(call_tag.kind == DmaTag::Kind::CALL && call_tag.qwc == 0)) {
    fmt::print("Bucket renderer {} ({}) was supposed to be empty, but wasn't\n", m_my_id, m_name);
  }
  assert(call_tag.kind == DmaTag::Kind::CALL && call_tag.qwc == 0);

  // in the default reg buffer:
  assert(dma.current_tag_offset() == render_state->default_regs_buffer);
  dma.read_and_advance();
  assert(dma.current_tag().kind == DmaTag::Kind::RET);
  dma.read_and_advance();

  // NEXT to next buffer
  auto to_next_buffer = dma.current_tag();
  assert(to_next_buffer.kind == DmaTag::Kind::NEXT);
  assert(to_next_buffer.qwc == 0);
  dma.read_and_advance();

  // and we should now be in the next bucket!
  assert(dma.current_tag_offset() == render_state->next_bucket);
}