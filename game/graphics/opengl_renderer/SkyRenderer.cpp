#include "SkyRenderer.h"

#include "game/graphics/opengl_renderer/AdgifHandler.h"
#include "game/graphics/pipelines/opengl.h"

#include "third-party/imgui/imgui.h"

// The sky texture system blends together sky textures from different levels and times of day
// to create the final sky texture.

// The sequence is:
//  set-display-gs-state 8qw
//  copy-sky-textures (between 0 and 8, usually 2.)
//  copy-cloud-texture
//  set alpha state
//  reset display gs state
// and this happens twice: one for each level.  Note that the first call to either of the copy
// functions will use "draw" mode instead of "blend"
// The results are stored in special sky textures.

// size of the sky texture is 64x96, but it's actually a 64x64 (clouds) and a 32x32 (sky)

SkyBlendHandler::SkyBlendHandler(const std::string& name,
                                 int my_id,
                                 int level_id,
                                 std::shared_ptr<SkyBlendGPU> shared_blender,
                                 std::shared_ptr<SkyBlendCPU> shared_blender_cpu)
    : BucketRenderer(name, my_id),
      m_shared_gpu_blender(shared_blender),
      m_shared_cpu_blender(shared_blender_cpu),
      m_tfrag_renderer(fmt::format("tfrag-{}", name),
                       my_id,
                       {tfrag3::TFragmentTreeKind::TRANS, tfrag3::TFragmentTreeKind::LOWRES_TRANS},
                       true,
                       level_id) {}

void SkyBlendHandler::init_shaders(ShaderLibrary& shaders) {
  m_tfrag_renderer.init_shaders(shaders);
}

void SkyBlendHandler::handle_sky_copies(DmaFollower& dma,
                                        SharedRenderState* render_state,
                                        ScopedProfilerNode& prof) {
  if (!m_enabled) {
    while (dma.current_tag().qwc == 6) {
      dma.read_and_advance();
      dma.read_and_advance();
    }
    return;
  } else {
    if (render_state->use_sky_cpu) {
      m_gpu_stats = m_shared_cpu_blender->do_sky_blends(dma, render_state, prof);

    } else {
      m_gpu_stats = m_shared_gpu_blender->do_sky_blends(dma, render_state, prof);
    }
  }
}

void SkyBlendHandler::render(DmaFollower& dma,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof) {
  m_gpu_stats = {};
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sky renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  if (dma.current_tag().qwc != 8) {
    auto tfrag_prof = prof.make_scoped_child("tfrag-trans");
    m_tfrag_renderer.render(dma, render_state, tfrag_prof);
    return;
  }

  // first is the set-display-gs-state
  auto set_display = dma.read_and_advance();
  ASSERT(set_display.size_bytes == 8 * 16);

  handle_sky_copies(dma, render_state, prof);

  auto reset_alpha = dma.read_and_advance();
  ASSERT(reset_alpha.size_bytes == 16 * 2);

  auto reset_gs = dma.read_and_advance();
  ASSERT(reset_gs.size_bytes == 16 * 8);

  auto empty = dma.read_and_advance();
  ASSERT(empty.size_bytes == 0);
  ASSERT(empty.vif0() == 0);
  ASSERT(empty.vif1() == 0);

  if (dma.current_tag().kind != DmaTag::Kind::CALL) {
    auto tfrag_prof = prof.make_scoped_child("tfrag-trans");
    m_tfrag_renderer.render(dma, render_state, tfrag_prof);
  } else {
    ASSERT(dma.current_tag().kind == DmaTag::Kind::CALL);
    dma.read_and_advance();
    dma.read_and_advance();  // cnt
    ASSERT(dma.current_tag().kind == DmaTag::Kind::RET);
    dma.read_and_advance();  // ret
    dma.read_and_advance();  // ret
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
  }
}

void SkyBlendHandler::draw_debug_window() {
  ImGui::Separator();
  ImGui::Text("Draw/Blend ( sky ): %d/%d", m_gpu_stats.sky_draws, m_gpu_stats.sky_blends);
  ImGui::Text("Draw/Blend (cloud): %d/%d", m_gpu_stats.cloud_draws, m_gpu_stats.cloud_blends);

  if (ImGui::TreeNode("tfrag")) {
    m_tfrag_renderer.draw_debug_window();
    ImGui::TreePop();
  }
}

SkyRenderer::SkyRenderer(const std::string& name, int my_id)
    : BucketRenderer(name, my_id), m_direct_renderer("sky-direct", my_id, 100) {}

void SkyRenderer::render(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  m_direct_renderer.reset_state();
  m_frame_stats = {};
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sky renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  auto setup_packet = dma.read_and_advance();
  ASSERT(setup_packet.size_bytes == 16 * 4);
  m_direct_renderer.render_gif(setup_packet.data, setup_packet.size_bytes, render_state, prof);

  if (dma.current_tag().qwc == 5) {
    auto draw_setup_packet = dma.read_and_advance();
    m_direct_renderer.render_gif(draw_setup_packet.data, draw_setup_packet.size_bytes, render_state,
                                 prof);
    // tex0: tbw = 1, th = 5, hw = 5, sky-base-block
    // mmag/mmin = 1
    // clamp
    // drawing.
    int dma_idx = 0;
    while (dma.current_tag().kind == DmaTag::Kind::CNT) {
      m_frame_stats.gif_packets++;
      auto data = dma.read_and_advance();
      ASSERT(data.vifcode0().kind == VifCode::Kind::NOP);
      ASSERT(data.vifcode1().kind == VifCode::Kind::DIRECT);
      ASSERT(data.vifcode1().immediate == data.size_bytes / 16);
      if (m_enabled) {
        m_direct_renderer.render_gif(data.data, data.size_bytes, render_state, prof);
      }
      dma_idx++;
    }

    auto empty = dma.read_and_advance();
    ASSERT(empty.size_bytes == 0);
    ASSERT(empty.vif0() == 0);
    ASSERT(empty.vif1() == 0);

    ASSERT(dma.current_tag().kind == DmaTag::Kind::CALL);
    dma.read_and_advance();
    dma.read_and_advance();  // cnt
    ASSERT(dma.current_tag().kind == DmaTag::Kind::RET);
    dma.read_and_advance();  // ret
    dma.read_and_advance();  // ret
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
  } else {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      auto data = dma.read_and_advance();
      if (data.size_bytes && m_enabled) {
        m_direct_renderer.render_vif(data.vif0(), data.vif1(), data.data, data.size_bytes,
                                     render_state, prof);
      }

      if (dma.current_tag_offset() == render_state->default_regs_buffer) {
        dma.read_and_advance();  // cnt
        ASSERT(dma.current_tag().kind == DmaTag::Kind::RET);
        dma.read_and_advance();  // ret
      }
    }
  }

  m_direct_renderer.flush_pending(render_state, prof);
}

void SkyRenderer::draw_debug_window() {
  ImGui::Separator();
  ImGui::Text("GIF packets: %d", m_frame_stats.gif_packets);

  if (ImGui::TreeNode("direct")) {
    m_direct_renderer.draw_debug_window();
    ImGui::TreePop();
  }
}
