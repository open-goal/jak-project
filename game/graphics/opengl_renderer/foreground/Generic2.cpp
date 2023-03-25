#include "Generic2.h"

#include "game/graphics/opengl_renderer/AdgifHandler.h"

#include "third-party/imgui/imgui.h"

Generic2::Generic2(const std::string& name,
                   int my_id,
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

  opengl_setup();
}

Generic2::~Generic2() {
  opengl_cleanup();
}

void Generic2::draw_debug_window() {
  ImGui::Checkbox("Alpha 1", &m_alpha_draw_enable[0]);
  ImGui::Checkbox("Alpha 2", &m_alpha_draw_enable[1]);
  ImGui::Checkbox("Alpha 3", &m_alpha_draw_enable[2]);
  ImGui::Checkbox("Alpha 4", &m_alpha_draw_enable[3]);
  ImGui::Checkbox("Alpha 5", &m_alpha_draw_enable[4]);
  ImGui::Checkbox("Alpha 6", &m_alpha_draw_enable[5]);
  ImGui::Checkbox("Alpha 7", &m_alpha_draw_enable[6]);

  ImGui::Text("Max Seen:");
  ImGui::Text(" frag: %d/%d %.1f%%", m_max_frags_seen, (int)m_fragments.size(),
              100.f * m_max_frags_seen / (float)m_fragments.size());
  ImGui::Text(" vert: %d/%d %.1f%%", m_max_verts_seen, (int)m_verts.size(),
              100.f * m_max_verts_seen / (float)m_verts.size());
  ImGui::Text(" adgif: %d/%d %.1f%%", m_max_frags_seen, (int)m_adgifs.size(),
              100.f * m_max_adgifs_seen / (float)m_adgifs.size());
  ImGui::Text(" idx: %d/%d %.1f%%", m_max_frags_seen, (int)m_indices.size(),
              100.f * m_max_indices_seen / (float)m_indices.size());
  ImGui::Text(" bucket: %d/%d %.1f%%", m_max_frags_seen, (int)m_fragments.size(),
              100.f * m_max_frags_seen / (float)m_fragments.size());
}

/*!
 * Main render function for Generic2. This will be passed a DMA "follower" from the main
 * OpenGLRenderer that can read a DMA chain, starting at the DMA "bucket" that was filled by the
 * generic renderer. This renderer is expected to follow the chain until it reaches "next_bucket"
 * and then return.
 */
void Generic2::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  render_in_mode(dma, render_state, prof, Mode::NORMAL);
}

void Generic2::render_in_mode(DmaFollower& dma,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof,
                              Mode mode) {
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
    auto p = prof.make_scoped_child("dma");
    switch (mode) {
      case Mode::NORMAL:
        process_dma(dma, render_state->next_bucket);
        break;
      case Mode::LIGHTNING:
        process_dma_lightning(dma, render_state->next_bucket);
        break;
      default:
        ASSERT_NOT_REACHED();
    }
  }

  {
    // the next pass is to look at all of that data, and figure out the best order to draw it
    // using OpenGL
    auto p = prof.make_scoped_child("setup");
    switch (mode) {
      case Mode::NORMAL:
        setup_draws(true);
        break;
      case Mode::LIGHTNING:
        setup_draws(false);
        break;
      default:
        ASSERT_NOT_REACHED();
    }
  }

  {
    // the final pass is the actual drawing.
    auto p = prof.make_scoped_child("drawing");
    do_draws(render_state, p);
  }
}