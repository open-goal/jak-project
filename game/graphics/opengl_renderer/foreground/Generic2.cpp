#include "Generic2.h"

#include "game/graphics/opengl_renderer/AdgifHandler.h"

Generic2::Generic2(const std::string& name,
                   BucketId my_id,
                   u32 num_verts,
                   u32 num_frags,
                   u32 num_adgif,
                   u32 num_buckets)
    : BucketRenderer(name, my_id) {
  m_verts.resize(num_verts);
  m_fragments.resize(num_frags);
  m_adgifs.resize(num_adgif);
  m_buckets.resize(num_buckets);
  m_indices.resize(num_verts * 3);
}

void Generic2::draw_debug_window() {}

/*!
 * Main render function for Generic2. This will be passed a DMA "follower" from the main
 * OpenGLRenderer that can read a DMA chain, starting at the DMA "bucket" that was filled by the
 * generic renderer. This renderer is expected to follow the chain until it reaches "next_bucket"
 * and then return.
 */
void Generic2::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // completely clear out state. These will get populated by the rendering functions, then displayed
  // by draw_debug_window() if the user opens that window
  m_debug.clear();
  m_stats = Stats();

  // if the user has asked to disable the renderer, just advance the dma follower to the next
  // bucket and return immediately.
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  // Generic2 has 3 passes.
  {
    // our first pass is to go over the DMA chain from the game and extract the data into buffers
    Timer proc_dma_timer;
    auto p = prof.make_scoped_child("dma");
    process_dma(dma, render_state->next_bucket);
    if (m_next_free_vert > 10000) {
      fmt::print("dma: {} in {:.3f} ms\n", m_next_free_vert, proc_dma_timer.getMs());
    }
  }

  {
    // the next pass is to look at all of that data, and figure out the best order to draw it
    // using OpenGL
    Timer setup_timer;
    auto p = prof.make_scoped_child("setup");
    setup_draws();
    if (m_next_free_vert > 10000) {
      fmt::print("setup: {} buckets, {} adgifs {} indices in {:.3f} ms\n", m_next_free_bucket,
                 m_next_free_adgif, m_next_free_idx, setup_timer.getMs());
    }
  }

  {
    // the final pass is the actual drawing.
    auto p = prof.make_scoped_child("drawing");
    do_draws();
  }
}

void Generic2::do_draws() {}