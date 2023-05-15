#include "ShadowRenderer.h"

#include <cfloat>

#include "third-party/imgui/imgui.h"

ShadowRenderer::ShadowRenderer(const std::string& name, int my_id) : BucketRenderer(name, my_id) {
  // create OpenGL objects
  glGenBuffers(1, &m_ogl.vertex_buffer);

  glGenBuffers(2, m_ogl.index_buffer);
  glGenVertexArrays(1, &m_ogl.vao);

  // set up the vertex array
  glBindVertexArray(m_ogl.vao);
  for (int i = 0; i < 2; i++) {
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[i]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, MAX_INDICES * sizeof(u32), nullptr, GL_STREAM_DRAW);
  }
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, MAX_VERTICES * sizeof(Vertex), nullptr, GL_STREAM_DRAW);

  // xyz
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                            // location 0 in the shader
                        3,                            // 3 floats per vert
                        GL_FLOAT,                     // floats
                        GL_TRUE,                      // normalized, ignored,
                        sizeof(Vertex),               //
                        (void*)offsetof(Vertex, xyz)  // offset in array
  );

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

void ShadowRenderer::draw_debug_window() {
  ImGui::Checkbox("Volume", &m_debug_draw_volume);
  ImGui::Text("Vert: %d, Front: %d, Back: %d\n", m_next_vertex, m_next_front_index,
              m_next_back_index);
}

ShadowRenderer::~ShadowRenderer() {
  glDeleteBuffers(1, &m_ogl.vertex_buffer);
  glDeleteBuffers(2, m_ogl.index_buffer);
  glDeleteVertexArrays(1, &m_ogl.vao);
}

void ShadowRenderer::xgkick(u16 imm) {
  u32 ind_of_fan_start = UINT32_MAX;
  bool fan_running = false;
  const u8* data = (const u8*)(m_vu_data + imm);

  u8 rgba[4] = {1, 2, 3, 4};

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
      if (tag.pre()) {
        GsPrim prim(tag.prim());
        ASSERT(prim.kind() == GsPrim::Kind::TRI_FAN);
      }
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          switch (reg_desc[reg]) {
            case GifTag::RegisterDescriptor::AD: {
              u64 value;
              GsRegisterAddress addr;
              memcpy(&value, data + offset, sizeof(u64));
              memcpy(&addr, data + offset + 8, sizeof(GsRegisterAddress));

              switch (addr) {
                case GsRegisterAddress::TEXFLUSH:
                  break;
                case GsRegisterAddress::RGBAQ: {
                  rgba[0] = data[0 + offset];
                  rgba[1] = data[1 + offset];
                  rgba[2] = data[2 + offset];
                  rgba[3] = data[3 + offset];
                  float Q;
                  memcpy(&Q, data + offset + 4, 4);
                  // fmt::print("rgba: {} {} {} {}: {}\n", rgba[0], rgba[1], rgba[2], rgba[3], Q);
                } break;
                default:
                  ASSERT_MSG(false, fmt::format("Address {} is not supported",
                                                register_address_name(addr)));
              }
            } break;
            case GifTag::RegisterDescriptor::ST: {
              float s, t;
              memcpy(&s, data + offset, 4);
              memcpy(&t, data + offset + 4, 4);
              // fmt::print("st: {} {}\n", s, t);
            } break;
            case GifTag::RegisterDescriptor::RGBAQ:
              for (int i = 0; i < 4; i++) {
                rgba[i] = data[offset + i * 4];
              }
              // fmt::print("rgbaq: {} {} {} {}\n", rgba[0], rgba[1], rgba[2], rgba[3]);
              break;
            case GifTag::RegisterDescriptor::XYZF2:
              // handle_xyzf2_packed(data + offset, render_state, prof);
              {
                u32 x, y;
                memcpy(&x, data + offset, 4);
                memcpy(&y, data + offset + 4, 4);

                u64 upper;
                memcpy(&upper, data + offset + 8, 8);
                u32 z = (upper >> 4) & 0xffffff;

                x <<= 16;
                y <<= 16;
                z <<= 8;
                u32 vidx = m_next_vertex++;
                auto& v = m_vertices[vidx];
                ASSERT(m_next_vertex < MAX_VERTICES);
                v.xyz[0] = (float)x / (float)UINT32_MAX;
                v.xyz[1] = (float)y / (float)UINT32_MAX;
                v.xyz[2] = (float)z / (float)UINT32_MAX;

                if (ind_of_fan_start == UINT32_MAX) {
                  ind_of_fan_start = vidx;
                } else {
                  if (fan_running) {
                    // todo, actually use triangle fans in opengl...
                    if (rgba[0] > 0) {
                      // back
                      m_back_indices[m_next_back_index++] = vidx;
                      m_back_indices[m_next_back_index++] = vidx - 1;
                      m_back_indices[m_next_back_index++] = ind_of_fan_start;
                    } else {
                      m_front_indices[m_next_front_index++] = vidx;
                      m_front_indices[m_next_front_index++] = vidx - 1;
                      m_front_indices[m_next_front_index++] = ind_of_fan_start;
                    }
                  } else {
                    fan_running = true;
                  }
                }

                // fmt::print("xyzfadc: {} {} {} {} {}\n", x, y, z, f, adc);
              }
              break;
            default:
              ASSERT_MSG(false, fmt::format("Register {} is not supported in packed mode yet\n",
                                            reg_descriptor_name(reg_desc[reg])));
          }
          offset += 16;  // PACKED = quadwords
        }
      }
    } else {
      ASSERT(false);  // format not packed or reglist.
    }

    eop = tag.eop();
  }
}

void ShadowRenderer::render(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  m_next_vertex = 0;
  m_next_back_index = 0;
  m_next_front_index = 0;

  // jump to bucket
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);

  // see if bucket is empty or not
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  {
    // constants
    auto constants = dma.read_and_advance();
    auto v0 = constants.vifcode0();
    auto v1 = constants.vifcode1();
    ASSERT(v0.kind == VifCode::Kind::STCYCL);
    ASSERT(v0.immediate == 0x404);
    ASSERT(v1.kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(v1.immediate == Vu1Data::CONSTANTS);
    ASSERT(v1.num == 13);
    memcpy(m_vu_data + v1.immediate, constants.data, v1.num * 16);
  }

  {
    // gif constants
    auto constants = dma.read_and_advance();
    auto v0 = constants.vifcode0();
    auto v1 = constants.vifcode1();
    ASSERT(v0.kind == VifCode::Kind::STCYCL);
    ASSERT(v0.immediate == 0x404);
    ASSERT(v1.kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(v1.immediate == Vu1Data::GIF_CONSTANTS);
    ASSERT(v1.num == 4);
    memcpy(m_vu_data + v1.immediate, constants.data, v1.num * 16);
  }

  {
    // matrix constants
    auto constants = dma.read_and_advance();
    auto v0 = constants.vifcode0();
    auto v1 = constants.vifcode1();
    ASSERT(v0.kind == VifCode::Kind::STCYCL);
    ASSERT(v0.immediate == 0x404);
    ASSERT(v1.kind == VifCode::Kind::UNPACK_V4_32);
    ASSERT(v1.immediate == Vu1Data::MATRIX);
    ASSERT(v1.num == 4);
    memcpy(m_vu_data + v1.immediate, constants.data, v1.num * 16);
  }

  {
    // exec 10
    auto mscal = dma.read_and_advance();
    ASSERT(mscal.vifcode1().kind == VifCode::Kind::FLUSHE);
    ASSERT(mscal.vifcode0().kind == VifCode::Kind::MSCALF);
    ASSERT(mscal.vifcode0().immediate == Vu1Code::INIT);
    run_mscal10_vu2c();
  }

  {
    // init gs direct
    dma.read_and_advance();
  }

  while (dma.current_tag().kind != DmaTag::Kind::CALL) {
    auto next = dma.read_and_advance();
    auto v1 = next.vifcode1();
    if (next.vifcode0().kind == VifCode::Kind::FLUSHA &&
        next.vifcode1().kind == VifCode::Kind::UNPACK_V4_32) {
      auto up = next.vifcode1();
      VifCodeUnpack unpack(up);
      ASSERT(!unpack.use_tops_flag);
      ASSERT((u32)unpack.addr_qw + up.num < 1024);
      memcpy(m_vu_data + unpack.addr_qw, next.data, up.num * 16);
      ASSERT(up.num * 16 == next.size_bytes);
    } else if (next.vifcode0().kind == VifCode::Kind::NOP &&
               next.vifcode1().kind == VifCode::Kind::UNPACK_V4_32) {
      auto up = next.vifcode1();
      VifCodeUnpack unpack(up);
      ASSERT(!unpack.use_tops_flag);
      ASSERT((u32)unpack.addr_qw + up.num < 1024);
      memcpy(m_vu_data + unpack.addr_qw, next.data, up.num * 16);
      ASSERT(up.num * 16 == next.size_bytes);
    } else if (next.vifcode0().kind == VifCode::Kind::NOP &&
               next.vifcode1().kind == VifCode::Kind::UNPACK_V4_8) {
      auto up = VifCodeUnpack(v1);
      ASSERT(!up.use_tops_flag);
      ASSERT(up.is_unsigned);
      u16 addr = up.addr_qw;
      ASSERT(addr + v1.num <= 1024);

      u32 temp[4];
      for (u32 i = 0; i < v1.num; i++) {
        for (u32 j = 0; j < 4; j++) {
          temp[j] = next.data[4 * i + j];
        }
        memcpy(m_vu_data + addr + i, temp, 16);
      }

      u32 offset = 4 * v1.num;
      ASSERT(offset + 16 == next.size_bytes);

      u32 after[4];
      memcpy(&after, next.data + offset, 16);
      ASSERT(after[0] == 0);
      ASSERT(after[1] == 0);
      ASSERT(after[2] == 0);
      VifCode mscal(after[3]);
      ASSERT(mscal.kind == VifCode::Kind::MSCALF);
      run_mscal_vu2c(mscal.immediate);
    } else if (next.vifcode0().kind == VifCode::Kind::FLUSHA &&
               next.vifcode1().kind == VifCode::Kind::DIRECT) {
      // there's 4 direct transfers to set up various registers.
      // we only care about the one that has the color value.
      auto xfer1 = dma.read_and_advance();
      dma.read_and_advance();
      dma.read_and_advance();
      auto r = *(xfer1.data + 24);
      auto g = *(xfer1.data + 25);
      auto b = *(xfer1.data + 26);
      auto a = *(xfer1.data + 27);
      m_color.x() = r / 255.0f;
      m_color.y() = g / 255.0f;
      m_color.z() = b / 255.0f;
      m_color.w() = a / 128.0f;
      // fmt::print("rgba: {} {} {} {}\n", r, g, b, a);
    } else {
      ASSERT_MSG(false, fmt::format("{} {}", next.vifcode0().print(), next.vifcode1().print()));
    }
  }

  for (int i = 0; i < 4; i++) {
    dma.read_and_advance();
  }
  ASSERT(dma.current_tag_offset() == render_state->next_bucket);

  draw(render_state, prof);
}

void ShadowRenderer::draw(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // enable stencil!
  glEnable(GL_STENCIL_TEST);
  glStencilMask(0xFF);

  u32 clear_vertices = m_next_vertex;
  m_vertices[m_next_vertex++] = Vertex{math::Vector3f(0.3, 0.3, 0), 0};
  m_vertices[m_next_vertex++] = Vertex{math::Vector3f(0.3, 0.7, 0), 0};
  m_vertices[m_next_vertex++] = Vertex{math::Vector3f(0.7, 0.3, 0), 0};
  m_vertices[m_next_vertex++] = Vertex{math::Vector3f(0.7, 0.7, 0), 0};
  m_front_indices[m_next_front_index++] = clear_vertices;
  m_front_indices[m_next_front_index++] = clear_vertices + 1;
  m_front_indices[m_next_front_index++] = clear_vertices + 2;
  m_front_indices[m_next_front_index++] = clear_vertices + 3;
  m_front_indices[m_next_front_index++] = clear_vertices + 2;
  m_front_indices[m_next_front_index++] = clear_vertices + 1;

  glBindVertexArray(m_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_next_vertex * sizeof(Vertex), m_vertices, GL_STREAM_DRAW);

  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDepthFunc(GL_GEQUAL);

  render_state->shaders.at(ShaderId::SHADOW).activate();

  glDepthMask(GL_FALSE);  // no depth writes.
  if (m_debug_draw_volume) {
    glEnable(GL_BLEND);
    glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ZERO, GL_ONE);
  } else {
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);  // no color writes.
  }

  // First pass.
  // here, we don't write depth or color.
  // but we increment stencil on depth fail.

  {
    glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::SHADOW].id(), "color_uniform"),
                0.0f, 128.0f / 256, 0.0f, 127.0f / 256);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[0]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_next_front_index * sizeof(u32), m_front_indices,
                 GL_STREAM_DRAW);
    glStencilFunc(GL_ALWAYS, 0, 0);          // always pass stencil
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);  // increment on depth pass.
    glDrawElements(GL_TRIANGLES, (m_next_front_index - 6), GL_UNSIGNED_INT, nullptr);

    if (m_debug_draw_volume) {
      glDisable(GL_BLEND);
      glUniform4f(
          glGetUniformLocation(render_state->shaders[ShaderId::SHADOW].id(), "color_uniform"), 0.0f,
          0.0f, 0.0f, 0.5f);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawElements(GL_TRIANGLES, (m_next_front_index - 6), GL_UNSIGNED_INT, nullptr);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      glEnable(GL_BLEND);
    }
    prof.add_draw_call();
    prof.add_tri(m_next_back_index / 3);
  }

  {
    glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::SHADOW].id(), "color_uniform"),
                128.0f / 256, 0.0f, 0.0f, 130.0f / 256);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[1]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_next_back_index * sizeof(u32), m_back_indices,
                 GL_STREAM_DRAW);
    // Second pass.
    // same settings, but decrement.
    glStencilFunc(GL_ALWAYS, 0, 0);
    glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);  // decrement on depth pass.
    glDrawElements(GL_TRIANGLES, m_next_back_index, GL_UNSIGNED_INT, nullptr);
    if (m_debug_draw_volume) {
      glDisable(GL_BLEND);
      glUniform4f(
          glGetUniformLocation(render_state->shaders[ShaderId::SHADOW].id(), "color_uniform"), 0.0f,
          0.0f, 0.0f, 0.5f);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawElements(GL_TRIANGLES, (m_next_back_index - 0), GL_UNSIGNED_INT, nullptr);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      glEnable(GL_BLEND);
    }

    prof.add_draw_call();
    prof.add_tri(m_next_front_index / 3);
  }

  // finally, draw shadow.
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[0]);
  glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::SHADOW].id(), "color_uniform"),
              m_color.x(), m_color.y(), m_color.z(), m_color.w());
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_FALSE);
  // glStencilFunc(GL_GREATER, 0, 0);
  glStencilFunc(GL_NOTEQUAL, 0, 0xFF);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glDepthFunc(GL_ALWAYS);

  glEnable(GL_BLEND);
  glBlendEquation(GL_FUNC_ADD);
  glBlendFuncSeparate(GL_DST_COLOR, GL_ZERO, GL_ONE, GL_ZERO);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, (void*)(sizeof(u32) * (m_next_front_index - 6)));
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  prof.add_draw_call();
  prof.add_tri(2);
  glDepthMask(GL_TRUE);

  glDisable(GL_STENCIL_TEST);
}
