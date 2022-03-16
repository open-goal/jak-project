#include "OceanTexture.h"
#include "game/graphics/opengl_renderer/AdgifHandler.h"
#include "third-party/imgui/imgui.h"

constexpr int OCEAN_TEX_TBP = 8160;  // todo
OceanTexture::OceanTexture(bool generate_mipmaps)
    : m_generate_mipmaps(generate_mipmaps),
      m_result_texture(TEX0_SIZE,
                       TEX0_SIZE,
                       GL_UNSIGNED_INT_8_8_8_8_REV,
                       m_generate_mipmaps ? NUM_MIPS : 1),
      m_temp_texture(TEX0_SIZE, TEX0_SIZE, GL_UNSIGNED_INT_8_8_8_8_REV),
      m_hack_renderer("ocean-tex-unoptimized", BucketId::BUCKET0, 0x8000) {
  m_dbuf_x = m_dbuf_a;
  m_dbuf_y = m_dbuf_b;

  m_tbuf_x = m_tbuf_a;
  m_tbuf_y = m_tbuf_b;

  init_pc();

  // initialize the mipmap drawing
  glGenVertexArrays(1, &m_mipmap.vao);
  glBindVertexArray(m_mipmap.vao);
  glGenBuffers(1, &m_mipmap.vtx_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_mipmap.vtx_buffer);
  std::vector<MipMap::Vertex> vertices = {
      {-1, -1, 0, 0}, {-1, 1, 0, 1}, {1, -1, 1, 0}, {1, 1, 1, 1}};
  glBufferData(GL_ARRAY_BUFFER, sizeof(MipMap::Vertex) * 4, vertices.data(), GL_STATIC_DRAW);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(
      0,                                  // location 0 in the shader
      2,                                  // 4 color components
      GL_FLOAT,                           // floats
      GL_FALSE,                           // normalized, ignored,
      sizeof(MipMap::Vertex),             //
      (void*)offsetof(MipMap::Vertex, x)  // offset in array (why is this a pointer...)
  );
  glVertexAttribPointer(
      1,                                  // location 0 in the shader
      2,                                  // 4 color components
      GL_FLOAT,                           // floats
      GL_FALSE,                           // normalized, ignored,
      sizeof(MipMap::Vertex),             //
      (void*)offsetof(MipMap::Vertex, s)  // offset in array (why is this a pointer...)
  );
  glBindVertexArray(0);
}

OceanTexture::~OceanTexture() {
  destroy_pc();
}

void OceanTexture::init_textures(TexturePool& pool) {
  TextureInput in;
  in.gpu_texture = m_result_texture.texture();
  in.w = TEX0_SIZE;
  in.h = TEX0_SIZE;
  in.page_name = "PC-OCEAN";
  in.name = fmt::format("pc-ocean-mip-{}", m_generate_mipmaps);
  m_tex0_gpu = pool.give_texture_and_load_to_vram(in, OCEAN_TEX_TBP);
}

void OceanTexture::draw_debug_window() {
  if (m_tex0_gpu) {
    ImGui::Image((void*)m_tex0_gpu->gpu_textures.at(0).gl, ImVec2(m_tex0_gpu->w, m_tex0_gpu->h));
  }
  ImGui::Checkbox("Optimized Version", &m_use_ocean_specific);
}

void OceanTexture::flush(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_use_ocean_specific) {
    flush_pc(render_state, prof);
  } else {
    m_hack_renderer.flush_pending(render_state, prof);
  }
}

void OceanTexture::setup_renderer() {
  if (m_use_ocean_specific) {
    setup_pc();
  } else {
    m_hack_renderer.reset_state();
  }
}

void OceanTexture::handle_tex_call_start(SharedRenderState* render_state,
                                         ScopedProfilerNode& prof) {
  if (m_use_ocean_specific) {
    run_L1_PC();
  } else {
    run_L1(render_state, prof);
  }
}

void OceanTexture::handle_tex_call_rest(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_use_ocean_specific) {
    run_L2_PC();
  } else {
    run_L2(render_state, prof);
  }
}

void OceanTexture::handle_ocean_texture(DmaFollower& dma,
                                        SharedRenderState* render_state,
                                        ScopedProfilerNode& prof) {
  // if we're doing mipmaps, render to temp.
  // otherwise, render directly to target.
  FramebufferTexturePairContext ctxt(m_generate_mipmaps ? m_temp_texture : m_result_texture);
  // render to the first texture
  {
    // (set-display-gs-state arg0 ocean-tex-page-0 128 128 0 0)
    auto data = dma.read_and_advance();
    (void)data;
  }

  // set up VIF
  {
    // (new 'static 'vif-tag :cmd (vif-cmd base))
    // (new 'static 'vif-tag :imm #xc0 :cmd (vif-cmd offset))
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == 0);
    ASSERT(data.vifcode0().kind == VifCode::Kind::BASE);
    ASSERT(data.vifcode1().kind == VifCode::Kind::OFFSET);
    ASSERT(data.vifcode0().immediate == 0);
    ASSERT(data.vifcode1().immediate == 0xc0);
  }

  // load texture constants
  {
    // (ocean-texture-add-constants arg0)
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == sizeof(OceanTextureConstants));
    ASSERT(data.vifcode0().kind == VifCode::Kind::STCYCL);
    ASSERT(data.vifcode0().immediate == 0x404);
    ASSERT(data.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(data.vifcode1().num == data.size_bytes / 16);
    ASSERT(data.vifcode1().immediate == TexVu1Data::CONSTANTS);
    memcpy(&m_texture_constants, data.data, sizeof(OceanTextureConstants));
  }

  // set up GS for envmap texture drawing
  {
    // (ocean-texture-add-envmap arg0)
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == sizeof(AdGifData) + 16);  // 16 for the giftag.
    ASSERT(data.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(data.vifcode1().kind == VifCode::Kind::DIRECT);
    memcpy(&m_envmap_adgif, data.data + 16, sizeof(AdGifData));

    // fmt::print("adgif is:\n{}\n\n", AdgifHelper(m_envmap_adgif).print());
    /*
     * adgif is:
[0] TEX0_1
  tbp0: 10010 tbw: 2 psm: 19 tw: 6 th: 6 tcc: 1 tfx: 0 cbp: 10046 cpsm: 0 csm: 0
[1] TEX1_1
  lcm: false mxl: 0 mmag: true mmin: 4 mtba: false l: 0 k: 0
[2] MIPTBP1_1
[3] CLAMP_1
  0x5
[4] ALPHA_1
  (Cs - Cd) * As / 128.0 + Cd

     */

    // HACK
    setup_renderer();
    if (!m_use_ocean_specific) {
      m_hack_renderer.render_gif(data.data, UINT32_MAX, render_state, prof);
    }
  }

  // vertices are uploaded double buffered
  m_texture_vertices_loading = m_texture_vertices_a;
  m_texture_vertices_drawing = m_texture_vertices_b;

  // add first group of vertices
  {
    // (ocean-texture-add-verts arg0 sv-16)
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == sizeof(m_texture_vertices_a));
    ASSERT(data.vifcode0().kind == VifCode::Kind::STCYCL);
    ASSERT(data.vifcode0().immediate == 0x404);
    ASSERT(data.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(data.vifcode1().num == data.size_bytes / 16);
    VifCodeUnpack up(data.vifcode1());
    ASSERT(up.addr_qw == 0);
    ASSERT(up.use_tops_flag == true);
    memcpy(m_texture_vertices_loading, data.data, sizeof(m_texture_vertices_a));
  }

  // first call
  {
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == 0);
    ASSERT(data.vifcode0().kind == VifCode::Kind::MSCALF);
    ASSERT(data.vifcode0().immediate == TexVu1Prog::START);
    ASSERT(data.vifcode1().kind == VifCode::Kind::STMOD);  // not sure why...
    handle_tex_call_start(render_state, prof);
  }

  // loop over vertex groups
  for (int i = 0; i < NUM_FRAG_LOOPS; i++) {
    auto verts = dma.read_and_advance();
    ASSERT(verts.size_bytes == sizeof(m_texture_vertices_a));
    ASSERT(verts.vifcode0().kind == VifCode::Kind::STCYCL);
    ASSERT(verts.vifcode0().immediate == 0x404);
    ASSERT(verts.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(verts.vifcode1().num == verts.size_bytes / 16);
    VifCodeUnpack up(verts.vifcode1());
    ASSERT(up.addr_qw == 0);
    ASSERT(up.use_tops_flag == true);
    memcpy(m_texture_vertices_loading, verts.data, sizeof(m_texture_vertices_a));

    auto call = dma.read_and_advance();
    ASSERT(call.size_bytes == 0);
    ASSERT(call.vifcode0().kind == VifCode::Kind::MSCALF);
    ASSERT(call.vifcode0().immediate == TexVu1Prog::REST);
    ASSERT(call.vifcode1().kind == VifCode::Kind::STMOD);  // not sure why...
    handle_tex_call_rest(render_state, prof);
  }

  // last upload does something weird...
  {
    // (ocean-texture-add-verts-last arg0 (the-as (inline-array vector) sv-48) sv-64)
    auto data0 = dma.read_and_advance();
    ASSERT(data0.size_bytes == 128 * 16);
    ASSERT(data0.vifcode0().kind == VifCode::Kind::STCYCL);
    ASSERT(data0.vifcode0().immediate == 0x404);
    ASSERT(data0.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(data0.vifcode1().num == data0.size_bytes / 16);
    VifCodeUnpack up0(data0.vifcode1());
    ASSERT(up0.addr_qw == 0);
    ASSERT(up0.use_tops_flag == true);
    memcpy(m_texture_vertices_loading, data0.data, 128 * 16);

    auto data1 = dma.read_and_advance();
    ASSERT(data1.size_bytes == 64 * 16);
    ASSERT(data1.vifcode0().kind == VifCode::Kind::STCYCL);
    ASSERT(data1.vifcode0().immediate == 0x404);
    ASSERT(data1.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(data1.vifcode1().num == data1.size_bytes / 16);
    VifCodeUnpack up1(data1.vifcode1());
    ASSERT(up1.addr_qw == 128);
    ASSERT(up1.use_tops_flag == true);
    memcpy(m_texture_vertices_loading + 128, data1.data, 64 * 16);
  }

  // last rest call
  {
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == 0);
    ASSERT(data.vifcode0().kind == VifCode::Kind::MSCALF);
    ASSERT(data.vifcode0().immediate == TexVu1Prog::REST);
    ASSERT(data.vifcode1().kind == VifCode::Kind::STMOD);  // not sure why...
    handle_tex_call_rest(render_state, prof);
  }

  // last call
  {
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == 0);
    ASSERT(data.vifcode0().kind == VifCode::Kind::MSCALF);
    ASSERT(data.vifcode0().immediate == TexVu1Prog::DONE);
    ASSERT(data.vifcode1().kind == VifCode::Kind::STMOD);  // not sure why...
    // this program does nothing.
  }

  flush(render_state, prof);
  if (m_generate_mipmaps) {
    // if we did mipmaps, the above code rendered to temp, and now we need to generate mipmaps
    // in the real output
    make_texture_with_mipmaps(render_state, prof);
  }

  // give to gpu!
  render_state->texture_pool->move_existing_to_vram(m_tex0_gpu, OCEAN_TEX_TBP);
}

/*!
 * Generate mipmaps for the ocean texture.
 * There's a trick here - we reduce the intensity of alpha on the lower lods. This lets texture
 * filtering slowly fade the alpha value out to 0 with distance.
 */
void OceanTexture::make_texture_with_mipmaps(SharedRenderState* render_state,
                                             ScopedProfilerNode& prof) {
  glBindVertexArray(m_mipmap.vao);
  render_state->shaders[ShaderId::OCEAN_TEXTURE_MIPMAP].activate();
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_TEXTURE_MIPMAP].id(),
                                   "alpha_intensity"),
              1.0);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, m_temp_texture.texture());
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glUniform1i(
      glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_TEXTURE_MIPMAP].id(), "tex_T0"),
      0);
  glBindBuffer(GL_ARRAY_BUFFER, m_mipmap.vtx_buffer);

  for (int i = 0; i < NUM_MIPS; i++) {
    FramebufferTexturePairContext ctxt(m_result_texture, i);
    glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_TEXTURE_MIPMAP].id(),
                                     "alpha_intensity"),
                std::max(0.f, 1.f - 0.4f * i));
    glUniform1f(
        glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_TEXTURE_MIPMAP].id(), "scale"),
        1.f / (1 << i));
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    prof.add_draw_call();
    prof.add_tri(2);
  }
  glBindVertexArray(0);
}
