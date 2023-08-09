#include "Shadow2.h"

#include "third-party/imgui/imgui.h"

Shadow2::Shadow2(const std::string& name, int my_id) : BucketRenderer(name, my_id) {
  m_vertex_buffer.resize(kMaxVerts);
  m_front_index_buffer.resize(kMaxInds);
  m_back_index_buffer.resize(kMaxInds);
  // a

  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenBuffers(2, m_ogl.index_buffer);
  glGenVertexArrays(1, &m_ogl.vao);
  glBindVertexArray(m_ogl.vao);
  for (int i = 0; i < 2; i++) {
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[i]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, kMaxInds * sizeof(u32), nullptr, GL_STREAM_DRAW);
  }
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, kMaxVerts * sizeof(ShadowVertex), nullptr, GL_STREAM_DRAW);

  // xyz
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                                  // location 0 in the shader
                        3,                                  // 3 floats per vert
                        GL_FLOAT,                           // floats
                        GL_TRUE,                            // normalized, ignored,
                        sizeof(ShadowVertex),               //
                        (void*)offsetof(ShadowVertex, pos)  // offset in array
  );

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

Shadow2::~Shadow2() {
  glDeleteBuffers(1, &m_ogl.vertex_buffer);
  glDeleteBuffers(2, m_ogl.index_buffer);
  glDeleteVertexArrays(1, &m_ogl.vao);
}

void Shadow2::init_shaders(ShaderLibrary& shaders) {
  const auto id = shaders[ShaderId::SHADOW2].id();
  m_ogl.uniforms.perspective[0] = glGetUniformLocation(id, "perspective_x");
  m_ogl.uniforms.perspective[1] = glGetUniformLocation(id, "perspective_y");
  m_ogl.uniforms.perspective[2] = glGetUniformLocation(id, "perspective_z");
  m_ogl.uniforms.perspective[3] = glGetUniformLocation(id, "perspective_w");

  m_ogl.uniforms.color = glGetUniformLocation(id, "color_uniform");
  m_ogl.uniforms.fog = glGetUniformLocation(id, "fog");
  m_ogl.uniforms.hvdf_offset = glGetUniformLocation(id, "hvdf_offset");
  m_ogl.uniforms.clear_mode = glGetUniformLocation(id, "clear_mode");
}

void Shadow2::draw_debug_window() {
  ImGui::Checkbox("volume", &m_debug_draw_volume);
}

void Shadow2::reset_buffers() {
  m_front_index_buffer_used = 0;
  m_back_index_buffer_used = 0;
  m_vertex_buffer_used = 0;
}

void Shadow2::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // jump to bucket
  dma.read_and_advance();

  if (dma.current_tag_offset() == render_state->next_bucket) {
    // nothing
    return;
  }

  auto maybe_constants = dma.read_and_advance();
  if (maybe_constants.size_bytes == 0 && maybe_constants.vif0() == 0 &&
      maybe_constants.vif1() == 0) {
    // nothing
    return;
  }

  reset_buffers();

  // shadow-vu1-constants
  ASSERT(maybe_constants.size_bytes >= sizeof(ShadowVu1Constants));
  FrameConstants frame_constants;
  memcpy(&frame_constants.constants, maybe_constants.data, sizeof(ShadowVu1Constants));

  // ?? no idea what this is.
  auto mystery_data_dma = dma.read_and_advance();
  ASSERT(mystery_data_dma.size_bytes == 4 * 16);
  memcpy(frame_constants.mystery.data, mystery_data_dma.data, 4 * 16);

  // the perspective matrix
  auto perspective_matrix_dma = dma.read_and_advance();
  ASSERT(perspective_matrix_dma.size_bytes == sizeof(CameraMatrix));
  memcpy(&frame_constants.camera, perspective_matrix_dma.data, sizeof(CameraMatrix));

  // mscal 0xa
  {
    auto init_mscal = dma.read_and_advance();
    auto vif0 = init_mscal.vifcode0();

    ASSERT(vif0.kind == VifCode::Kind::MSCALF);
    ASSERT(vif0.immediate == 10);
  }

  // nothing
  {
    auto nothing = dma.read_and_advance();
    ASSERT(nothing.size_bytes == 0);
    ASSERT(nothing.vif0() == 0);
    ASSERT(nothing.vif1() == 0);
  }

  // maybe the invert z thing
  DmaFollower f = dma;
  auto maybe_invert_z = f.read_and_advance();
  [[maybe_unused]] bool invert_z = false;
  if (maybe_invert_z.vif0() == 0 && maybe_invert_z.vifcode1().kind == VifCode::Kind::DIRECT &&
      maybe_invert_z.vifcode1().immediate == 35) {
    invert_z = true;
    dma.read_and_advance();
  }

  // loop over uploads
  InputData current_input;
  bool have_color = false;
  while (true) {
    if (dma.current_tag_offset() == render_state->next_bucket) {
      break;
    }
    auto transfer = dma.read_and_advance();
    auto vif0 = transfer.vifcode0();
    auto vif1 = transfer.vifcode1();

    if (vif1.kind == VifCode::Kind::UNPACK_V4_32) {
      ASSERT(vif0.kind == VifCode::Kind::FLUSH || vif0.kind == VifCode::Kind::NOP);
      ASSERT(vif1.num * 16 == transfer.size_bytes);  // no extra data
      int addr = vif1.immediate;
      switch (addr) {
        case kTopVertexDataAddr:
          current_input.top_vertex_data = transfer.data;
          break;
        case kBottomVertexDataAddr:
          current_input.bottom_vertex_data = transfer.data;
          break;
        default:
          ASSERT_NOT_REACHED_MSG(fmt::format("Unknown address for transfer: {}\n", addr));
      }
    } else if (vif1.kind == VifCode::Kind::UNPACK_V4_8) {
      ASSERT(vif0.kind == VifCode::Kind::FLUSH || vif0.kind == VifCode::Kind::NOP);
      auto up = VifCodeUnpack(vif1);
      ASSERT(!up.use_tops_flag);
      ASSERT(up.is_unsigned);
      u16 addr = up.addr_qw;

      u32 offset = 4 * vif1.num;
      ASSERT(offset + 16 == transfer.size_bytes);

      u32 after[4];
      memcpy(&after, transfer.data + offset, 16);
      ASSERT(after[0] == 0);
      ASSERT(after[1] == 0);
      ASSERT(after[2] == 0);
      VifCode mscal(after[3]);
      ASSERT(mscal.kind == VifCode::Kind::MSCALF);

      switch (addr) {
        case kCapIndexDataAddr:
          current_input.cap_index_data_size = 4 * vif1.num;
          current_input.cap_index_data = transfer.data;
          break;
        case kWallIndexDataAddr:
          current_input.wall_index_data_size = 4 * vif1.num;
          current_input.wall_index_data = transfer.data;
          break;
        default:
          ASSERT_NOT_REACHED_MSG(fmt::format("Unknown address for transfer: {}\n", addr));
      }

      switch (mscal.immediate) {
        case 2:
          buffer_from_mscal2(current_input);
          break;
        case 4:
          buffer_from_mscal4(current_input);
          break;
        case 6:
          buffer_from_mscal6(current_input);
          break;
        default:
          printf("mscal %d\n", mscal.immediate);
          ASSERT_NOT_REACHED();
      }
    } else if (vif0.kind == VifCode::Kind::FLUSHA && vif1.kind == VifCode::Kind::DIRECT) {
      if (transfer.size_bytes == 560) {
        have_color = true;
        memcpy(m_color, transfer.data + 8 * 3, 4);
      }
      // ignore
    } else if (vif0.kind == VifCode::Kind::NOP && vif1.kind == VifCode::Kind::NOP) {
      // ignore
    }

    else {
      fmt::print("unhandled transfer in shadow2\n{} bytes\n{}\n{}\n", transfer.size_bytes,
                 vif0.print(), vif1.print());
    }
  }

  ASSERT(have_color);
  draw_buffers(render_state, prof, frame_constants);
  auto transfers = 0;
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();
    if (data.size_bytes == 560) {
      memcpy(m_color, data.data + 8 * 3, 4);
    }
    transfers++;
  }
  ASSERT(transfers < 7);
}

void Shadow2::buffer_from_mscal2(const InputData& in) {
  // draw top caps.
  add_cap_tris(in.cap_index_data, in.top_vertex_data, false);

  // draw bottom caps.
  add_cap_tris(in.cap_index_data, in.bottom_vertex_data, true);
}

void Shadow2::buffer_from_mscal4(const InputData& in) {
  add_wall_quads(in.wall_index_data, in.top_vertex_data, in.bottom_vertex_data);
}

void Shadow2::buffer_from_mscal6(const InputData& in) {
  // draw top caps.
  add_flippable_tris(in.cap_index_data, in.top_vertex_data, false);
  add_flippable_tris(in.cap_index_data, in.bottom_vertex_data, true);
}

Shadow2::ShadowVertex* Shadow2::alloc_verts(int n) {
  auto* result = &m_vertex_buffer[m_vertex_buffer_used];
  m_vertex_buffer_used += n;
  ASSERT(m_vertex_buffer_used <= m_vertex_buffer.size());
  return result;
}

u32* Shadow2::alloc_inds(int n, bool front) {
  if (!front) {
    auto* result = &m_front_index_buffer[m_front_index_buffer_used];
    m_front_index_buffer_used += n;
    ASSERT(m_front_index_buffer_used <= m_front_index_buffer.size());
    return result;
  } else {
    auto* result = &m_back_index_buffer[m_back_index_buffer_used];
    m_back_index_buffer_used += n;
    ASSERT(m_back_index_buffer_used <= m_back_index_buffer.size());
    return result;
  }
}

const u8* Shadow2::add_cap_tris(const u8* byte_data, const u8* vertex_data, bool flip) {
  const int num_single_tris = *byte_data++;
  for (int i = 0; i < 3; i++) {
    int v = *byte_data++;
    ASSERT(v == 0);
  }
  for (int i = 0; i < num_single_tris; i++) {
    int vertex_addrs[3];
    vertex_addrs[0] = *byte_data++;  // vi04
    vertex_addrs[1] = *byte_data++;  // vi05
    vertex_addrs[2] = *byte_data++;  // vi06
    const int bonus = *byte_data++;  // unused, but not zero?
    ASSERT(bonus == 1);              // idk

    // due to unpackv4-8 alignment, they inserted up to 3 dummy tris at the end. let's just skip.
    if (!vertex_addrs[0] && !vertex_addrs[1] && !vertex_addrs[2]) {
      ASSERT(i + 4 >= num_single_tris);
      continue;
    }

    // load vertices (to vf17, vf18, vf19)
    const int opengl_vertex_idx = m_vertex_buffer_used;
    auto* vertex_rt_camera = alloc_verts(3);
    if (flip) {
      memcpy(vertex_rt_camera[0].pos.data(), vertex_data + 16 * vertex_addrs[0], 12);
      memcpy(vertex_rt_camera[1].pos.data(), vertex_data + 16 * vertex_addrs[2], 12);
      memcpy(vertex_rt_camera[2].pos.data(), vertex_data + 16 * vertex_addrs[1], 12);
    } else {
      for (int j = 0; j < 3; j++) {
        memcpy(vertex_rt_camera[j].pos.data(), vertex_data + 16 * vertex_addrs[j], 12);
      }
    }

    const math::Vector3f v1_v0_rt_camera = vertex_rt_camera[1].pos - vertex_rt_camera[0].pos;
    const math::Vector3f v2_v0_rt_camera = vertex_rt_camera[2].pos - vertex_rt_camera[0].pos;
    const math::Vector3f tri_normal = v1_v0_rt_camera.cross(v2_v0_rt_camera);
    const float normal_dot_eye = tri_normal.dot(vertex_rt_camera[0].pos);
    auto* idx_buffer = alloc_inds(4, normal_dot_eye > 0);
    for (int j = 0; j < 3; j++) {
      idx_buffer[j] = opengl_vertex_idx + j;
    }
    idx_buffer[3] = UINT32_MAX;
  }
  return byte_data;
}

const u8* Shadow2::add_flippable_tris(const u8* byte_data, const u8* vertex_data, bool flip) {
  const int num_single_tris = *byte_data++;
  for (int i = 0; i < 3; i++) {
    int v = *byte_data++;
    ASSERT(v == 0);
  }
  for (int i = 0; i < num_single_tris; i++) {
    int vertex_addrs[3];
    vertex_addrs[0] = *byte_data++;  // vi04
    vertex_addrs[1] = *byte_data++;  // vi05
    vertex_addrs[2] = *byte_data++;  // vi06
    const int flip_flag = *byte_data++;

    // due to unpackv4-8 alignment, they inserted up to 3 dummy tris at the end. let's just skip.
    if (!vertex_addrs[0] && !vertex_addrs[1] && !vertex_addrs[2]) {
      ASSERT(i + 4 >= num_single_tris);
      continue;
    }

    // load vertices (to vf17, vf18, vf19)
    const int opengl_vertex_idx = m_vertex_buffer_used;
    auto* vertex_rt_camera = alloc_verts(3);
    if ((flip ^ flip_flag) == 0) {
      memcpy(vertex_rt_camera[0].pos.data(), vertex_data + 16 * vertex_addrs[0], 12);
      memcpy(vertex_rt_camera[1].pos.data(), vertex_data + 16 * vertex_addrs[2], 12);
      memcpy(vertex_rt_camera[2].pos.data(), vertex_data + 16 * vertex_addrs[1], 12);

    } else {
      for (int j = 0; j < 3; j++) {
        memcpy(vertex_rt_camera[j].pos.data(), vertex_data + 16 * vertex_addrs[j], 12);
      }
    }

    const math::Vector3f v1_v0_rt_camera = vertex_rt_camera[1].pos - vertex_rt_camera[0].pos;
    const math::Vector3f v2_v0_rt_camera = vertex_rt_camera[2].pos - vertex_rt_camera[0].pos;
    const math::Vector3f tri_normal = v1_v0_rt_camera.cross(v2_v0_rt_camera);
    const float normal_dot_eye = tri_normal.dot(vertex_rt_camera[0].pos);
    auto* idx_buffer = alloc_inds(4, normal_dot_eye > 0);
    for (int j = 0; j < 3; j++) {
      idx_buffer[j] = opengl_vertex_idx + j;
    }
    idx_buffer[3] = UINT32_MAX;
  }
  return byte_data;
}

const u8* Shadow2::add_wall_quads(const u8* byte_data,
                                  const u8* vertex_data_0,
                                  const u8* vertex_data_1) {
  const int num_quads = *byte_data++;
  for (int i = 0; i < 3; i++) {
    int v = *byte_data++;
    ASSERT(v == 0);
  }

  for (int i = 0; i < num_quads; i++) {
    int vertex_addrs[2];
    vertex_addrs[0] = *byte_data++;  // vi04
    vertex_addrs[1] = *byte_data++;  // vi05
    int side_control = *byte_data++;

    const int bonus = *byte_data++;  // unused, but not zero?
    (void)bonus;

    // due to unpackv4-8 alignment, they inserted up to 3 dummy tris at the end. let's just skip.
    if (!vertex_addrs[0] && !vertex_addrs[1]) {
      ASSERT(i + 4 >= num_quads);
      continue;
    }

    const int opengl_vertex_idx = m_vertex_buffer_used;
    auto* vertex_rt_camera = alloc_verts(4);

    if (side_control == 0) {
      memcpy(vertex_rt_camera[0].pos.data(), vertex_data_0 + 16 * vertex_addrs[1], 12);
      memcpy(vertex_rt_camera[1].pos.data(), vertex_data_0 + 16 * vertex_addrs[0], 12);
      memcpy(vertex_rt_camera[2].pos.data(), vertex_data_1 + 16 * vertex_addrs[0], 12);
      memcpy(vertex_rt_camera[3].pos.data(), vertex_data_1 + 16 * vertex_addrs[1], 12);
    } else {
      memcpy(vertex_rt_camera[0].pos.data(), vertex_data_0 + 16 * vertex_addrs[0], 12);
      memcpy(vertex_rt_camera[1].pos.data(), vertex_data_0 + 16 * vertex_addrs[1], 12);
      memcpy(vertex_rt_camera[2].pos.data(), vertex_data_1 + 16 * vertex_addrs[1], 12);
      memcpy(vertex_rt_camera[3].pos.data(), vertex_data_1 + 16 * vertex_addrs[0], 12);
    }

    const math::Vector3f v1_v0_rt_camera = vertex_rt_camera[1].pos - vertex_rt_camera[0].pos;
    const math::Vector3f v2_v0_rt_camera = vertex_rt_camera[2].pos - vertex_rt_camera[0].pos;
    const math::Vector3f tri_normal = v1_v0_rt_camera.cross(v2_v0_rt_camera);
    const float normal_dot_eye = tri_normal.dot(vertex_rt_camera[0].pos);
    auto* idx_buffer = alloc_inds(5, normal_dot_eye > 0);
    idx_buffer[0] = opengl_vertex_idx + 1;
    idx_buffer[1] = opengl_vertex_idx + 0;
    idx_buffer[2] = opengl_vertex_idx + 2;
    idx_buffer[3] = opengl_vertex_idx + 3;
    idx_buffer[4] = UINT32_MAX;
  }
  return byte_data;
}

namespace {
void set_uniform(GLint id, const math::Vector4f& value) {
  glUniform4f(id, value[0], value[1], value[2], value[3]);
}
}  // namespace

void Shadow2::draw_buffers(SharedRenderState* render_state,
                           ScopedProfilerNode& prof,
                           const FrameConstants& constants) {
  if (!m_front_index_buffer_used && !m_back_index_buffer_used) {
    return;
  }

  if (render_state->stencil_dirty) {
    glClearStencil(0);
    glClear(GL_STENCIL_BUFFER_BIT);
  }
  render_state->stencil_dirty = true;

  render_state->shaders.at(ShaderId::SHADOW2).activate();

  // uniforms:
  set_uniform(m_ogl.uniforms.hvdf_offset, constants.constants.hvdfoff);
  glUniform1f(m_ogl.uniforms.fog, constants.constants.fog[0]);
  for (int i = 0; i < 4; i++) {
    set_uniform(m_ogl.uniforms.perspective[i], constants.camera.v[i]);
  }
  glUniform1i(m_ogl.uniforms.clear_mode, 0);

  // enable stencil!
  glEnable(GL_STENCIL_TEST);
  glStencilMask(0xFF);

  u32 clear_vertices = m_vertex_buffer_used;
  m_vertex_buffer[m_vertex_buffer_used++] = ShadowVertex{math::Vector3f(0.3, 0.3, 0), 0};
  m_vertex_buffer[m_vertex_buffer_used++] = ShadowVertex{math::Vector3f(0.3, 0.7, 0), 0};
  m_vertex_buffer[m_vertex_buffer_used++] = ShadowVertex{math::Vector3f(0.7, 0.3, 0), 0};
  m_vertex_buffer[m_vertex_buffer_used++] = ShadowVertex{math::Vector3f(0.7, 0.7, 0), 0};
  m_front_index_buffer[m_front_index_buffer_used++] = clear_vertices;
  m_front_index_buffer[m_front_index_buffer_used++] = clear_vertices + 1;
  m_front_index_buffer[m_front_index_buffer_used++] = clear_vertices + 2;
  m_front_index_buffer[m_front_index_buffer_used++] = clear_vertices + 3;
  m_front_index_buffer[m_front_index_buffer_used++] = UINT32_MAX;
  m_front_index_buffer[m_front_index_buffer_used++] = UINT32_MAX;

  glBindVertexArray(m_ogl.vao);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_vertex_buffer_used * sizeof(ShadowVertex), m_vertex_buffer.data(),
               GL_STREAM_DRAW);

  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDepthFunc(GL_GEQUAL);

  glDepthMask(GL_FALSE);  // no depth writes.
  if (m_debug_draw_volume) {
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ZERO, GL_ONE);
  } else {
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);  // no color writes.
  }

  // First pass.
  // here, we don't write depth or color.
  // but we increment stencil on depth fail.

  {
    glUniform4f(m_ogl.uniforms.color, 0., 0.4, 0., 0.5);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[0]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_front_index_buffer_used * sizeof(u32),
                 m_front_index_buffer.data(), GL_STREAM_DRAW);
    glStencilFunc(GL_ALWAYS, 0, 0);          // always pass stencil
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);  // increment on depth pass.
    glEnable(GL_PRIMITIVE_RESTART);
    glPrimitiveRestartIndex(UINT32_MAX);
    glDrawElements(GL_TRIANGLE_STRIP, (m_front_index_buffer_used - 6), GL_UNSIGNED_INT, nullptr);

    if (m_debug_draw_volume) {
      glDisable(GL_BLEND);
      glUniform4f(m_ogl.uniforms.color, 0., 0.0, 0., 0.5);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawElements(GL_TRIANGLE_STRIP, (m_front_index_buffer_used - 6), GL_UNSIGNED_INT, nullptr);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      glEnable(GL_BLEND);
      prof.add_draw_call();
      prof.add_tri(m_front_index_buffer_used / 3);
    }
    prof.add_draw_call();
    prof.add_tri(m_front_index_buffer_used / 3);
  }

  {
    glUniform4f(m_ogl.uniforms.color, 0.4, 0.0, 0., 0.5);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[1]);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_back_index_buffer_used * sizeof(u32),
                 m_back_index_buffer.data(), GL_STREAM_DRAW);

    // Second pass.
    // same settings, but decrement.
    glStencilFunc(GL_ALWAYS, 0, 0);
    glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);  // decrement on depth pass.
    glDrawElements(GL_TRIANGLE_STRIP, m_back_index_buffer_used, GL_UNSIGNED_INT, nullptr);
    if (m_debug_draw_volume) {
      glDisable(GL_BLEND);
      glUniform4f(m_ogl.uniforms.color, 0., 0.0, 0., 0.5);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawElements(GL_TRIANGLE_STRIP, (m_back_index_buffer_used - 0), GL_UNSIGNED_INT, nullptr);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      glEnable(GL_BLEND);
      prof.add_draw_call();
      prof.add_tri(m_back_index_buffer_used / 3);
    }

    prof.add_draw_call();
    prof.add_tri(m_back_index_buffer_used / 3);
  }

  // finally, draw shadow.
  glUniform1i(m_ogl.uniforms.clear_mode, 1);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer[0]);
  glStencilFunc(GL_NOTEQUAL, 0, 0xFF);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glDepthFunc(GL_ALWAYS);

  glEnable(GL_BLEND);
  glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ONE, GL_ZERO);

  bool have_darken = false;
  bool have_lighten = false;
  bool lighten_channel[3] = {false, false, false};
  bool darken_channel[3] = {false, false, false};
  for (int i = 0; i < 3; i++) {
    if (m_color[i] > 128) {
      have_lighten = true;
      lighten_channel[i] = true;
    } else if (m_color[i] < 128) {
      have_darken = true;
      darken_channel[i] = true;
    }
  }

  if (have_darken) {
    glColorMask(darken_channel[0], darken_channel[1], darken_channel[2], false);
    glUniform4f(m_ogl.uniforms.color, (128 - m_color[0]) / 256.f, (128 - m_color[1]) / 256.f,
                (128 - m_color[2]) / 256.f, 0);
    glBlendEquation(GL_FUNC_REVERSE_SUBTRACT);
    glDrawElements(GL_TRIANGLE_STRIP, 6, GL_UNSIGNED_INT,
                   (void*)(sizeof(u32) * (m_front_index_buffer_used - 6)));
  }

  if (have_lighten) {
    glColorMask(lighten_channel[0], lighten_channel[1], lighten_channel[2], false);
    glUniform4f(m_ogl.uniforms.color, (m_color[0] - 128) / 256.f, (m_color[1] - 128) / 256.f,
                (m_color[2] - 128) / 256.f, 0);
    glBlendEquation(GL_FUNC_ADD);
    glDrawElements(GL_TRIANGLE_STRIP, 6, GL_UNSIGNED_INT,
                   (void*)(sizeof(u32) * (m_front_index_buffer_used - 6)));
  }

  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

  prof.add_draw_call();
  prof.add_tri(2);
  glBlendEquation(GL_FUNC_ADD);
  glDepthMask(GL_TRUE);

  glDisable(GL_STENCIL_TEST);
}
