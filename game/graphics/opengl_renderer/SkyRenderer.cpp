#include "SkyRenderer.h"
#include "third-party/imgui/imgui.h"
#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/opengl_renderer/AdgifHandler.h"
#include "common/log/log.h"

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

SkyTextureHandler::SkyTextureHandler(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {
  // generate textures for sky blending
  glGenFramebuffers(2, m_framebuffers);
  glGenTextures(2, m_textures);

  GLint old_framebuffer;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &old_framebuffer);

  // setup the framebuffers
  for (int i = 0; i < 2; i++) {
    glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffers[i]);
    glBindTexture(GL_TEXTURE_2D, m_textures[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_sizes[i], m_sizes[i], 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8_REV, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_textures[i], 0);
    GLenum draw_buffers[1] = {GL_COLOR_ATTACHMENT0};
    glDrawBuffers(1, draw_buffers);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
      lg::error("SkyTextureHandler setup failed.");
    }
  }
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  glGenBuffers(1, &m_gl_vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_gl_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * 6, nullptr, GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, old_framebuffer);

  // we only draw squares
  m_vertex_data[0].x = 0;
  m_vertex_data[0].y = 0;

  m_vertex_data[1].x = 1;
  m_vertex_data[1].y = 0;

  m_vertex_data[2].x = 0;
  m_vertex_data[2].y = 1;

  m_vertex_data[3].x = 1;
  m_vertex_data[3].y = 0;

  m_vertex_data[4].x = 0;
  m_vertex_data[4].y = 1;

  m_vertex_data[5].x = 1;
  m_vertex_data[5].y = 1;
}

SkyTextureHandler::~SkyTextureHandler() {
  glDeleteFramebuffers(2, m_framebuffers);
  glDeleteBuffers(1, &m_gl_vertex_buffer);
  glDeleteTextures(2, m_textures);
}

void SkyTextureHandler::handle_sky_copies(DmaFollower& dma,
                                          SharedRenderState* render_state,
                                          ScopedProfilerNode& prof) {
  if (!m_enabled) {
    while (dma.current_tag().qwc == 6) {
      dma.read_and_advance();
      dma.read_and_advance();
    }
    return;
  }
  GLuint vao;
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);

  GLint old_viewport[4];
  glGetIntegerv(GL_VIEWPORT, old_viewport);

  GLint old_framebuffer;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &old_framebuffer);

  while (dma.current_tag().qwc == 6) {
    // assuming that the vif and gif-tag is correct
    auto setup_data = dma.read_and_advance();
    if (render_state->dump_playback) {
      // continue;
    }

    // first is an adgif
    AdgifHelper adgif(setup_data.data + 16);
    assert(adgif.is_normal_adgif());
    assert(adgif.alpha().data == 0x8000000068);  // Cs + Cd

    // next is the actual draw
    auto draw_data = dma.read_and_advance();
    assert(draw_data.size_bytes == 6 * 16);

    GifTag draw_or_blend_tag(draw_data.data);

    // the first draw overwrites the previous frame's draw by disabling alpha blend (ABE = 0)
    bool is_first_draw = !GsPrim(draw_or_blend_tag.prim()).abe();

    // here's we're relying on the format of the drawing to get the alpha/offset.
    u32 coord;
    u32 intensity;
    memcpy(&coord, draw_data.data + (5 * 16), 4);
    memcpy(&intensity, draw_data.data + 16, 4);

    // we didn't parse the render-to-texture setup earlier, so we need a way to tell sky from
    // clouds. we can look at the drawing coordinates to tell - the sky is smaller than the clouds.
    int buffer_idx = 0;
    if (coord == 0x200) {
      // sky
      buffer_idx = 0;
    } else if (coord == 0x400) {
      buffer_idx = 1;
    } else {
      assert(false);  // bad data
    }

    // look up the source texture
    auto tex = render_state->texture_pool->lookup(adgif.tex0().tbp0());
    assert(tex);

    if (!tex->on_gpu) {
      render_state->texture_pool->upload_to_gpu(tex);
    }

    // setup for rendering!
    glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffers[buffer_idx]);
    glViewport(0, 0, m_sizes[buffer_idx], m_sizes[buffer_idx]);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_textures[buffer_idx], 0);
    render_state->shaders[ShaderId::SKY_BLEND].activate();

    // if the first is set, it disables alpha. we can just clear here, so it's easier to find
    // in renderdoc.
    if (is_first_draw) {
      float clear[4] = {0, 0, 0, 0};
      glClearBufferfv(GL_COLOR, 0, clear);
    }

    // intensities should be 0-128 (maybe higher is okay, but I don't see how this could be
    // generated with the GOAL code.)
    assert(intensity <= 128);

    // todo - could do this on the GPU, but probably not worth it for <20 triangles...
    float intensity_float = intensity / 128.f;
    for (auto& vert : m_vertex_data) {
      vert.intensity = intensity_float;
    }

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);

    // will add.
    glBlendFunc(GL_ONE, GL_ONE);

    // setup draw data
    glBindBuffer(GL_ARRAY_BUFFER, m_gl_vertex_buffer);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(Vertex) * 6, m_vertex_data);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0,         // location 0 in the shader
                          3,         // 3 floats per vert
                          GL_FLOAT,  // floats
                          GL_TRUE,   // normalized, ignored,
                          0,         // tightly packed
                          0

    );
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::SKY_BLEND].id(), "T0"), 0);

    // Draw a sqaure
    glDrawArrays(GL_TRIANGLES, 0, 6);

    // 1 draw, 2 triangles
    prof.add_draw_call(1);
    prof.add_tri(2);

    if (buffer_idx == 0) {
      if (is_first_draw) {
        m_stats.sky_draws++;
      } else {
        m_stats.sky_blends++;
      }
    } else {
      if (is_first_draw) {
        m_stats.cloud_draws++;
      } else {
        m_stats.cloud_blends++;
      }
    }
  }

  // put in pool.
  for (int i = 0; i < 2; i++) {
    // todo - these are hardcoded and rely on the vram layout.
    u32 tbp = i == 0 ? 8064 : 8096;

    // lookup existing, or create a new entry
    TextureRecord* tex = render_state->texture_pool->lookup(tbp);
    if (!tex) {
      auto tsp = std::make_shared<TextureRecord>();
      render_state->texture_pool->set_texture(tbp, tsp);
      tex = tsp.get();
    }

    // update it
    tex->gpu_texture = m_textures[i];
    tex->on_gpu = true;
    tex->only_on_gpu = true;
    tex->do_gc = false;
    tex->w = m_sizes[i];
    tex->h = m_sizes[i];
    tex->name = fmt::format("PC-SKY-{}", i);
  }

  glViewport(old_viewport[0], old_viewport[1], old_viewport[2], old_viewport[3]);
  glBindFramebuffer(GL_FRAMEBUFFER, old_framebuffer);
  glBindVertexArray(0);
  glDeleteVertexArrays(1, &vao);
}

void SkyTextureHandler::render(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  m_stats = {};
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sky renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  // first is the set-display-gs-state
  auto set_display = dma.read_and_advance();
  assert(set_display.size_bytes == 8 * 16);

  handle_sky_copies(dma, render_state, prof);

  auto reset_alpha = dma.read_and_advance();
  assert(reset_alpha.size_bytes == 16 * 2);

  auto reset_gs = dma.read_and_advance();
  assert(reset_gs.size_bytes == 16 * 8);

  auto empty = dma.read_and_advance();
  assert(empty.size_bytes == 0);
  assert(empty.vif0() == 0);
  assert(empty.vif1() == 0);

  assert(dma.current_tag().kind == DmaTag::Kind::CALL);
  dma.read_and_advance();
  dma.read_and_advance();  // cnt
  assert(dma.current_tag().kind == DmaTag::Kind::RET);
  dma.read_and_advance();  // ret
  dma.read_and_advance();  // ret
  assert(dma.current_tag_offset() == render_state->next_bucket);
}

void SkyTextureHandler::draw_debug_window() {
  ImGui::Separator();
  ImGui::Text("Draw/Blend ( sky ): %d/%d", m_stats.sky_draws, m_stats.sky_blends);
  ImGui::Text("Draw/Blend (cloud): %d/%d", m_stats.cloud_draws, m_stats.cloud_blends);
}

SkyRenderer::SkyRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id),
      m_direct_renderer("sky-direct", my_id, 100, DirectRenderer::Mode::NORMAL) {}

void SkyRenderer::render(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  m_direct_renderer.reset_state();
  m_frame_stats = {};
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sky renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  auto setup_packet = dma.read_and_advance();
  assert(setup_packet.size_bytes == 16 * 4);
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
      assert(data.vifcode0().kind == VifCode::Kind::NOP);
      assert(data.vifcode1().kind == VifCode::Kind::DIRECT);
      assert(data.vifcode1().immediate == data.size_bytes / 16);
      if (m_enabled) {
        m_direct_renderer.render_gif(data.data, data.size_bytes, render_state, prof);
      }
      dma_idx++;
    }

    auto empty = dma.read_and_advance();
    assert(empty.size_bytes == 0);
    assert(empty.vif0() == 0);
    assert(empty.vif1() == 0);

    assert(dma.current_tag().kind == DmaTag::Kind::CALL);
    dma.read_and_advance();
    dma.read_and_advance();  // cnt
    assert(dma.current_tag().kind == DmaTag::Kind::RET);
    dma.read_and_advance();  // ret
    dma.read_and_advance();  // ret
    assert(dma.current_tag_offset() == render_state->next_bucket);
  } else {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      auto data = dma.read_and_advance();
      if (data.size_bytes && m_enabled) {
        m_direct_renderer.render_vif(data.vif0(), data.vif1(), data.data, data.size_bytes,
                                     render_state, prof);
      }

      if (dma.current_tag_offset() == render_state->default_regs_buffer) {
        dma.read_and_advance();  // cnt
        assert(dma.current_tag().kind == DmaTag::Kind::RET);
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
