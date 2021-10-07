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
  for (int i = 0; i < 2; i++) {
    glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffers[i]);
    glBindTexture(GL_TEXTURE_2D, m_textures[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, m_sizes[i], m_sizes[i], 0, GL_RGBA, GL_FLOAT, 0);
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
  glBindBuffer(GL_ARRAY_BUFFER, 0);

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

void SkyTextureHandler::serialize(Serializer& ser) {
  if (ser.is_saving()) {
  }
}

void SkyTextureHandler::handle_sky_copies(DmaFollower& dma,
                                          SharedRenderState* render_state,
                                          ScopedProfilerNode& prof) {
  GLuint vao;
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);

  GLint old_viewport[4];
  glGetIntegerv(GL_VIEWPORT, old_viewport);

  while (dma.current_tag().qwc == 6) {
    // assuming that the vif and gif-tag is correct
    auto setup_data = dma.read_and_advance();
    AdgifHelper adgif(setup_data.data + 16);
    assert(adgif.is_normal_adgif());
    assert(adgif.alpha().data == 0x8000000068);  // Cs + Cd

    //    fmt::print("Source: {}\n", adgif.tex0().tbp0());

    auto draw_data = dma.read_and_advance();
    assert(draw_data.size_bytes == 6 * 16);

    GifTag draw_or_blend_tag(draw_data.data);

    bool is_first_draw = !GsPrim(draw_or_blend_tag.prim()).abe();
    u32 coord;
    u32 intensity;
    memcpy(&coord, draw_data.data + (5 * 16), 4);
    memcpy(&intensity, draw_data.data + 16, 4);
    //    fmt::print("draw: {} {} 0x{:x}\n", is_first_draw, intensity, coord);

    int buffer_idx = 0;
    if (coord == 0x200) {
      // sky
      buffer_idx = 0;
    } else if (coord == 0x400) {
      buffer_idx = 1;
    } else {
      assert(false);  // bad data
    }

    auto tex = render_state->texture_pool->lookup(adgif.tex0().tbp0());
    assert(tex);

    if (!tex->on_gpu) {
      render_state->texture_pool->upload_to_gpu(tex);
    }

    glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffers[buffer_idx]);
    glViewport(0, 0, m_sizes[buffer_idx], m_sizes[buffer_idx]);
    render_state->shaders[ShaderId::SKY_BLEND].activate();
    if (is_first_draw) {
      float clear[4] = {0, 0, 0, 0};
      glClearBufferfv(GL_COLOR, 0, clear);
    }

    assert(intensity <= 128);
    float intensity_float = intensity / 128.f;
    for (auto& vert : m_vertex_data) {
      vert.intensity = intensity_float;
    }

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE);

    glBindBuffer(GL_ARRAY_BUFFER, m_gl_vertex_buffer);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(Vertex) * 6, m_vertex_data);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0,         // location 0 in the shader
                          3,         // 3 floats per vert
                          GL_FLOAT,  // floats
                          GL_TRUE,   // normalized, ignored,
                          0,         // tightly packed
                          0          // offset in array (why is is this a pointer...)

    );
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::SKY_BLEND].id(), "T0"), 0);

    glDrawArrays(GL_TRIANGLES, 0, 6);
    prof.add_draw_call(1);
    prof.add_tri(2);
  }

  // TODO: don't add on very frame
  for (int i = 0; i < 2; i++) {
    auto tex = std::make_shared<TextureRecord>();
    tex->gpu_texture = m_textures[i];
    tex->on_gpu = true;
    tex->do_gc = false;
    tex->name = fmt::format("PC-SKY-{}", i);
    render_state->texture_pool->set_texture(i == 0 ? 8064 : 8096, tex);
  }

  glViewport(old_viewport[0], old_viewport[1], old_viewport[2], old_viewport[3]);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glBindVertexArray(0);
  glDeleteVertexArrays(1, &vao);
}

void SkyTextureHandler::render(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  m_debug_dma_str.clear();

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

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto tag = dma.current_tag();
    m_debug_dma_str += fmt::format("@ 0x{:x} tag: {}\n", dma.current_tag_offset(), tag.print());
    auto data = dma.read_and_advance();
    VifCode code(data.vif0());
    m_debug_dma_str += fmt::format(" vif: {}\n", code.print());
    if (code.kind == VifCode::Kind::NOP || code.kind == VifCode::Kind::FLUSHA) {
      m_debug_dma_str += fmt::format(" vif: {}\n", VifCode(data.vif1()).print());
    }
  }
}

void SkyTextureHandler::draw_debug_window() {
  ImGui::Separator();
  ImGui::Checkbox("DMA print", &m_print_debug_dma);
  if (m_print_debug_dma) {
    ImGui::Text("%s", m_debug_dma_str.c_str());
  }
}

SkyRenderer::SkyRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id),
      m_direct_renderer("sky-direct", my_id, 100, DirectRenderer::Mode::NORMAL) {}

void SkyRenderer::render(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  m_debug_dma_str.clear();
  m_direct_renderer.reset_state();
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

  auto draw_setup_packet = dma.read_and_advance();
  assert(draw_setup_packet.size_bytes == 16 * 5);
  m_direct_renderer.render_gif(draw_setup_packet.data, draw_setup_packet.size_bytes, render_state,
                               prof);
  // tex0: tbw = 1, th = 5, hw = 5, sky-base-block
  // mmag/mmin = 1
  // clamp
  // drawing.
  int dma_idx = 0;
  while (dma.current_tag().kind == DmaTag::Kind::CNT) {
    auto data = dma.read_and_advance();
    assert(data.vifcode0().kind == VifCode::Kind::NOP);
    assert(data.vifcode1().kind == VifCode::Kind::DIRECT);
    assert(data.vifcode1().immediate == data.size_bytes / 16);
    m_direct_renderer.render_gif(data.data, data.size_bytes, render_state, prof);
    m_direct_renderer.flush_pending(render_state, prof);
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
}

void SkyRenderer::draw_debug_window() {
  ImGui::Separator();
  ImGui::Checkbox("DMA print", &m_print_debug_dma);
  if (m_print_debug_dma) {
    ImGui::Text("%s", m_debug_dma_str.c_str());
  }

  if (ImGui::TreeNode("direct")) {
    m_direct_renderer.draw_debug_window();
    ImGui::TreePop();
  }
}
