#include "CommonOceanRenderer.h"

#include "common/log/log.h"

constexpr int OCEAN_TEX_TBP_JAK1 = 8160;  // todo
constexpr int OCEAN_TEX_TBP_JAK2 = 672;

CommonOceanRenderer::CommonOceanRenderer() {
  m_vertices.resize(4096 * 10);  // todo decrease
  for (auto& buf : m_indices) {
    buf.resize(4096 * 10);
  }

  // create OpenGL objects
  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenBuffers(NUM_BUCKETS, m_ogl.index_buffer);
  glGenVertexArrays(1, &m_ogl.vao);

  // set up the vertex array
  glBindVertexArray(m_ogl.vao);
  for (int i = 0; i < NUM_BUCKETS; i++) {
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[i]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_indices[i].size() * sizeof(u32), nullptr,
                 GL_STREAM_DRAW);
  }
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_vertices.size() * sizeof(Vertex), nullptr, GL_STREAM_DRAW);

  // xyz
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                            // location 0 in the shader
                        3,                            // 3 floats per vert
                        GL_FLOAT,                     // floats
                        GL_TRUE,                      // normalized, ignored,
                        sizeof(Vertex),               //
                        (void*)offsetof(Vertex, xyz)  // offset in array
  );

  // rgba
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1,                             // location 1 in the shader
                        4,                             // 4 color components
                        GL_UNSIGNED_BYTE,              // u8
                        GL_TRUE,                       // normalized (255 becomes 1)
                        sizeof(Vertex),                //
                        (void*)offsetof(Vertex, rgba)  //
  );

  // stq
  glEnableVertexAttribArray(2);
  glVertexAttribPointer(2,                            // location 2 in the shader
                        3,                            // 2 floats per vert
                        GL_FLOAT,                     // floats
                        GL_FALSE,                     // normalized, ignored
                        sizeof(Vertex),               //
                        (void*)offsetof(Vertex, stq)  // offset in array
  );

  // byte data
  glEnableVertexAttribArray(3);
  glVertexAttribIPointer(3,                            // location 3 in the shader
                         4,                            //
                         GL_UNSIGNED_BYTE,             // u8's
                         sizeof(Vertex),               //
                         (void*)offsetof(Vertex, fog)  // offset in array
  );

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

CommonOceanRenderer::~CommonOceanRenderer() {
  glDeleteBuffers(1, &m_ogl.vertex_buffer);
  glDeleteBuffers(3, m_ogl.index_buffer);
  glDeleteVertexArrays(1, &m_ogl.vao);
}

void CommonOceanRenderer::init_for_near() {
  m_next_free_vertex = 0;
  for (auto& x : m_next_free_index) {
    x = 0;
  }
}

void CommonOceanRenderer::kick_from_near(const u8* data) {
  bool eop = false;

  u32 offset = 0;
  while (!eop) {
    GifTag tag(data + offset);
    offset += 16;

    if (tag.nreg() == 3) {
      ASSERT(tag.pre());
      if (GsPrim(tag.prim()).kind() == GsPrim::Kind::TRI_STRIP) {
        handle_near_vertex_gif_data_strip(data, offset, tag.nloop());
      } else {
        handle_near_vertex_gif_data_fan(data, offset, tag.nloop());
      }
      offset += 16 * 3 * tag.nloop();
    } else if (tag.nreg() == 1) {
      handle_near_adgif(data, offset, tag.nloop());
      offset += 16 * 1 * tag.nloop();
    } else {
      ASSERT(false);
    }

    eop = tag.eop();
  }
}

void CommonOceanRenderer::handle_near_vertex_gif_data_strip(const u8* data, u32 offset, u32 loop) {
  m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = UINT32_MAX;
  bool reset_last = false;
  for (u32 i = 0; i < loop; i++) {
    auto& dest_vert = m_vertices[m_next_free_vertex++];

    // stq
    memcpy(dest_vert.stq.data(), data + offset, 12);
    offset += 16;

    // rgba
    dest_vert.rgba[0] = data[offset];
    dest_vert.rgba[1] = data[offset + 4];
    dest_vert.rgba[2] = data[offset + 8];
    dest_vert.rgba[3] = data[offset + 12];
    offset += 16;

    // xyz
    u32 x = 0, y = 0;
    memcpy(&x, data + offset, 4);
    memcpy(&y, data + offset + 4, 4);

    u64 upper;
    memcpy(&upper, data + offset + 8, 8);
    u32 z = (upper >> 4) & 0xffffff;
    offset += 16;

    dest_vert.xyz[0] = (float)(x << 16) / (float)UINT32_MAX;
    dest_vert.xyz[1] = (float)(y << 16) / (float)UINT32_MAX;
    dest_vert.xyz[2] = (float)(z << 8) / (float)UINT32_MAX;

    u8 f = (upper >> 36);
    dest_vert.fog = f;

    auto vidx = m_next_free_vertex - 1;
    bool adc = upper & (1ull << 47);
    if (!adc) {
      m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = vidx;
      reset_last = false;
    } else {
      if (reset_last) {
        m_next_free_index[m_current_bucket] -= 3;
      }
      m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = UINT32_MAX;
      m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = vidx - 1;
      m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = vidx;
      reset_last = true;
    }
  }
}

void CommonOceanRenderer::handle_near_vertex_gif_data_fan(const u8* data, u32 offset, u32 loop) {
  u32 ind_of_fan_start = UINT32_MAX;
  bool fan_running = false;
  // :regs0 (gif-reg-id st) :regs1 (gif-reg-id rgbaq) :regs2 (gif-reg-id xyzf2)
  for (u32 i = 0; i < loop; i++) {
    auto& dest_vert = m_vertices[m_next_free_vertex++];

    // stq
    memcpy(dest_vert.stq.data(), data + offset, 12);
    offset += 16;

    // rgba
    dest_vert.rgba[0] = data[offset];
    dest_vert.rgba[1] = data[offset + 4];
    dest_vert.rgba[2] = data[offset + 8];
    dest_vert.rgba[3] = data[offset + 12];
    offset += 16;

    // xyz
    u32 x = 0, y = 0;
    memcpy(&x, data + offset, 4);
    memcpy(&y, data + offset + 4, 4);

    u64 upper;
    memcpy(&upper, data + offset + 8, 8);
    u32 z = (upper >> 4) & 0xffffff;
    offset += 16;

    dest_vert.xyz[0] = (float)(x << 16) / (float)UINT32_MAX;
    dest_vert.xyz[1] = (float)(y << 16) / (float)UINT32_MAX;
    dest_vert.xyz[2] = (float)(z << 8) / (float)UINT32_MAX;

    u8 f = (upper >> 36);
    dest_vert.fog = f;

    auto vidx = m_next_free_vertex - 1;

    if (ind_of_fan_start == UINT32_MAX) {
      ind_of_fan_start = vidx;
    } else {
      if (fan_running) {
        // hack to draw fans with strips. this isn't efficient, but fans happen extremely rarely
        // (you basically have to put the camera intersecting the ocean and looking fwd)
        m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = UINT32_MAX;
        m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = vidx;
        m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = vidx - 1;
        m_indices[m_current_bucket][m_next_free_index[m_current_bucket]++] = ind_of_fan_start;
      } else {
        fan_running = true;
      }
    }
  }
}

void CommonOceanRenderer::handle_near_adgif(const u8* data, u32 offset, u32 count) {
  u32 most_recent_tbp = 0;

  for (u32 i = 0; i < count; i++) {
    u64 value;
    GsRegisterAddress addr;
    memcpy(&value, data + offset + 16 * i, sizeof(u64));
    memcpy(&addr, data + offset + 16 * i + 8, sizeof(GsRegisterAddress));
    switch (addr) {
      case GsRegisterAddress::MIPTBP1_1:
        // ignore this, it's just mipmapping settings
        break;
      case GsRegisterAddress::TEX1_1: {
        GsTex1 reg(value);
        ASSERT(reg.mmag());
      } break;
      case GsRegisterAddress::CLAMP_1: {
        bool s = value & 0b001;
        bool t = value & 0b100;
        ASSERT(s == t);
        if (s) {
          m_current_bucket = VertexBucket::ENV_MAP;
        }
      } break;
      case GsRegisterAddress::TEX0_1: {
        GsTex0 reg(value);
        ASSERT(reg.tfx() == GsTex0::TextureFunction::MODULATE);
        if (!reg.tcc()) {
          m_current_bucket = VertexBucket::RGB_TEXTURE;
        }
        most_recent_tbp = reg.tbp0();
      } break;
      case GsRegisterAddress::ALPHA_1: {
        GsAlpha reg(value);
        if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
            reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
            reg.c_mode() == GsAlpha::BlendMode::DEST && reg.d_mode() == GsAlpha::BlendMode::DEST) {
          m_current_bucket = VertexBucket::ENV_MAP;
        }
      } break;

      case GsRegisterAddress::FRAME_1: {
        u32 mask = value >> 32;
        if (mask) {
          m_current_bucket = VertexBucket::ALPHA;
        }
      } break;

      default:
        lg::debug("reg: {}", register_address_name(addr));
        break;
    }
  }

  if (m_current_bucket == VertexBucket::ENV_MAP) {
    m_envmap_tex = most_recent_tbp;
  }

  if (m_vertices.size() - 128 < m_next_free_vertex) {
    ASSERT(false);  // add more vertices.
  }
}

void CommonOceanRenderer::flush_near(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  glBindVertexArray(m_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  glBufferData(GL_ARRAY_BUFFER, m_next_free_vertex * sizeof(Vertex), m_vertices.data(),
               GL_STREAM_DRAW);
  render_state->shaders[ShaderId::OCEAN_COMMON].activate();
  glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "fog_color"),
              render_state->fog_color[0] / 255.f, render_state->fog_color[1] / 255.f,
              render_state->fog_color[2] / 255.f, render_state->fog_intensity / 255);

  glDepthMask(GL_FALSE);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glDepthFunc(GL_GEQUAL);

  for (int bucket = 0; bucket < 3; bucket++) {
    switch (bucket) {
      case 0: {
        glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ZERO);
        glBlendEquation(GL_FUNC_ADD);
        auto tbp =
            render_state->version == GameVersion::Jak1 ? OCEAN_TEX_TBP_JAK1 : OCEAN_TEX_TBP_JAK2;
        auto tex = render_state->texture_pool->lookup(tbp);
        if (!tex) {
          tex = render_state->texture_pool->get_placeholder_texture();
        }
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, *tex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        glUniform1i(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "tex_T0"), 0);
        glUniform1i(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "bucket"), 0);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      }

      break;
      case 1:
        glBlendFuncSeparate(GL_ZERO, GL_ONE, GL_ONE, GL_ZERO);
        glUniform1f(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "alpha_mult"),
            1.f);
        glUniform1i(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "bucket"), 1);
        break;
      case 2:
        auto tex = render_state->texture_pool->lookup(m_envmap_tex);
        if (!tex) {
          tex = render_state->texture_pool->get_placeholder_texture();
        }
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, *tex);

        glBlendFuncSeparate(GL_DST_ALPHA, GL_ONE, GL_ONE, GL_ZERO);
        glBlendEquation(GL_FUNC_ADD);
        glUniform1i(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "bucket"), 2);
        break;
    }
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[bucket]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_next_free_index[bucket] * sizeof(u32),
                 m_indices[bucket].data(), GL_STREAM_DRAW);
    glDrawElements(GL_TRIANGLE_STRIP, m_next_free_index[bucket], GL_UNSIGNED_INT, nullptr);
    prof.add_draw_call();
    prof.add_tri(m_next_free_index[bucket]);
  }
}

void CommonOceanRenderer::kick_from_mid(const u8* data) {
  bool eop = false;

  u32 offset = 0;
  while (!eop) {
    GifTag tag(data + offset);
    offset += 16;

    // unpack registers.
    // faster to do it once outside of the nloop loop.
    GifTag::RegisterDescriptor reg_desc[16];
    u32 nreg = tag.nreg();
    for (u32 i = 0; i < nreg; i++) {
      reg_desc[i] = tag.reg(i);
    }

    auto format = tag.flg();
    if (format == GifTag::Format::PACKED) {
      if (tag.nreg() == 1) {
        ASSERT(!tag.pre());
        ASSERT(tag.nloop() == 5);
        handle_mid_adgif(data, offset);
        offset += 5 * 16;
      } else {
        ASSERT(tag.nreg() == 3);
        ASSERT(tag.pre());
        m_current_bucket = GsPrim(tag.prim()).abe() ? 1 : 0;

        int count = tag.nloop();
        if (GsPrim(tag.prim()).kind() == GsPrim::Kind::TRI_STRIP) {
          handle_near_vertex_gif_data_strip(data, offset, tag.nloop());
        } else {
          handle_near_vertex_gif_data_fan(data, offset, tag.nloop());
        }
        offset += 3 * 16 * count;
        // todo handle.
      }
    } else {
      ASSERT(false);  // format not packed or reglist.
    }

    eop = tag.eop();
  }
}

void CommonOceanRenderer::handle_mid_adgif(const u8* data, u32 offset) {
  u32 most_recent_tbp = 0;

  for (u32 i = 0; i < 5; i++) {
    u64 value;
    GsRegisterAddress addr;
    memcpy(&value, data + offset + 16 * i, sizeof(u64));
    memcpy(&addr, data + offset + 16 * i + 8, sizeof(GsRegisterAddress));
    switch (addr) {
      case GsRegisterAddress::MIPTBP1_1:
      case GsRegisterAddress::MIPTBP2_1:
        // ignore this, it's just mipmapping settings
        break;
      case GsRegisterAddress::TEX1_1: {
        GsTex1 reg(value);
        ASSERT(reg.mmag());
      } break;
      case GsRegisterAddress::CLAMP_1: {
        bool s = value & 0b001;
        bool t = value & 0b100;
        ASSERT(s == t);
      } break;
      case GsRegisterAddress::TEX0_1: {
        GsTex0 reg(value);
        ASSERT(reg.tfx() == GsTex0::TextureFunction::MODULATE);
        most_recent_tbp = reg.tbp0();
      } break;
      case GsRegisterAddress::ALPHA_1: {
      } break;

      default:
        lg::debug("reg: {}", register_address_name(addr));
        break;
    }
  }

  if (most_recent_tbp != OCEAN_TEX_TBP_JAK2) {
    m_envmap_tex = most_recent_tbp;
  }

  if (m_vertices.size() - 128 < m_next_free_vertex) {
    ASSERT(false);  // add more vertices.
  }
}

void CommonOceanRenderer::init_for_mid() {
  m_next_free_vertex = 0;
  for (auto& x : m_next_free_index) {
    x = 0;
  }
}

void reverse_indices(u32* indices, u32 count) {
  if (count) {
    for (u32 a = 0, b = count - 1; a < b; a++, b--) {
      std::swap(indices[a], indices[b]);
    }
  }
}

void CommonOceanRenderer::flush_mid(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  glBindVertexArray(m_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  glBufferData(GL_ARRAY_BUFFER, m_next_free_vertex * sizeof(Vertex), m_vertices.data(),
               GL_STREAM_DRAW);
  render_state->shaders[ShaderId::OCEAN_COMMON].activate();
  glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "fog_color"),
              render_state->fog_color[0] / 255.f, render_state->fog_color[1] / 255.f,
              render_state->fog_color[2] / 255.f, render_state->fog_intensity / 255);

  glDepthMask(GL_TRUE);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_ALWAYS);
  glDisable(GL_BLEND);

  // note:
  // there are some places where the game draws the same section of ocean twice, in this order:
  // - low poly mesh with ocean texture
  // - low poly mesh with envmap texture
  // - high poly mesh with ocean texture (overwrites previous draw)
  // - high poly mesh with envmap texture (overwrites previous draw)

  // we draw all ocean textures together and all envmap textures togther. luckily, there's a trick
  // we can use to get the same result.
  // first, we'll draw all ocean textures. The high poly mesh is drawn second, so it wins.
  // then, we'll draw all envmaps, but with two changes:
  // - first, we draw it in reverse, so the high poly versions are drawn first
  // - second, we'll modify the shader to set alpha = 0 of the destination. when the low poly
  //    version is drawn on top, it won't draw at all because of the blending mode
  //    (s_factor = DST_ALPHA, d_factor = 1)

  // draw it in reverse
  reverse_indices(m_indices[1].data(), m_next_free_index[1]);

  for (int bucket = 0; bucket < 2; bucket++) {
    switch (bucket) {
      case 0: {
        auto tbp =
            render_state->version == GameVersion::Jak1 ? OCEAN_TEX_TBP_JAK1 : OCEAN_TEX_TBP_JAK2;
        auto tex = render_state->texture_pool->lookup(tbp);
        if (!tex) {
          tex = render_state->texture_pool->get_placeholder_texture();
        }
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, *tex);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        glUniform1i(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "tex_T0"), 0);
        glUniform1i(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "bucket"), 3);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      }

      break;
      case 1:
        glEnable(GL_BLEND);
        auto tex = render_state->texture_pool->lookup(m_envmap_tex);
        if (!tex) {
          tex = render_state->texture_pool->get_placeholder_texture();
        }
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, *tex);

        glBlendFuncSeparate(GL_DST_ALPHA, GL_ONE, GL_ONE, GL_ZERO);
        glBlendEquation(GL_FUNC_ADD);
        glUniform1i(
            glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_COMMON].id(), "bucket"), 4);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        break;
    }
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[bucket]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_next_free_index[bucket] * sizeof(u32),
                 m_indices[bucket].data(), GL_STREAM_DRAW);
    glDrawElements(GL_TRIANGLE_STRIP, m_next_free_index[bucket], GL_UNSIGNED_INT, nullptr);
    prof.add_draw_call();
    prof.add_tri(m_next_free_index[bucket]);
  }
}
