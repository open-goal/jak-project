#include "Sprite3.h"

#include "common/log/log.h"

#include "game/graphics/opengl_renderer/background/background_common.h"
#include "game/graphics/opengl_renderer/dma_helpers.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

namespace {

/*!
 * Does the next DMA transfer look like it could be the start of a 2D group?
 */
bool looks_like_2d_chunk_start(const DmaFollower& dma) {
  return dma.current_tag().qwc == 1 && dma.current_tag().kind == DmaTag::Kind::CNT;
}

/*!
 * Read the header. Asserts if it's bad.
 * Returns the number of sprites.
 * Advances 1 dma transfer
 */
u32 process_sprite_chunk_header(DmaFollower& dma) {
  auto transfer = dma.read_and_advance();
  // note that flg = true, this should use double buffering
  bool ok = verify_unpack_with_stcycl(transfer, VifCode::Kind::UNPACK_V4_32, 4, 4, 1,
                                      SpriteDataMem::Header, false, true);
  ASSERT(ok);
  u32 header[4];
  memcpy(header, transfer.data, 16);
  ASSERT(header[0] <= Sprite3::SPRITES_PER_CHUNK);
  return header[0];
}

constexpr int SPRITE_RENDERER_MAX_SPRITES = 1920 * 10;
}  // namespace

Sprite3::Sprite3(const std::string& name, int my_id)
    : BucketRenderer(name, my_id), m_direct(name, my_id, 1024) {
  opengl_setup();
}

void Sprite3::opengl_setup() {
  // Set up OpenGL for 'normal' sprites
  opengl_setup_normal();

  // Set up OpenGL for distort sprites
  opengl_setup_distort();
}

void Sprite3::opengl_setup_normal() {
  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenVertexArrays(1, &m_ogl.vao);
  glBindVertexArray(m_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  auto verts = SPRITE_RENDERER_MAX_SPRITES * 4;
  auto bytes = verts * sizeof(SpriteVertex3D);
  glBufferData(GL_ARRAY_BUFFER, bytes, nullptr, GL_STREAM_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(
      0,                                       // location 0 in the shader
      4,                                       // 4 floats per vert (w unused)
      GL_FLOAT,                                // floats
      GL_TRUE,                                 // normalized, ignored,
      sizeof(SpriteVertex3D),                  //
      (void*)offsetof(SpriteVertex3D, xyz_sx)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(1);
  glVertexAttribPointer(
      1,                                        // location 1 in the shader
      4,                                        // 4 color components
      GL_FLOAT,                                 // floats
      GL_TRUE,                                  // normalized, ignored,
      sizeof(SpriteVertex3D),                   //
      (void*)offsetof(SpriteVertex3D, quat_sy)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(2);
  glVertexAttribPointer(
      2,                                     // location 2 in the shader
      4,                                     // 4 color components
      GL_FLOAT,                              // floats
      GL_TRUE,                               // normalized, ignored,
      sizeof(SpriteVertex3D),                //
      (void*)offsetof(SpriteVertex3D, rgba)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(3);
  glVertexAttribIPointer(
      3,                                             // location 3 in the shader
      2,                                             // 4 color components
      GL_UNSIGNED_SHORT,                             // floats
      sizeof(SpriteVertex3D),                        //
      (void*)offsetof(SpriteVertex3D, flags_matrix)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(4);
  glVertexAttribIPointer(
      4,                                     // location 4 in the shader
      4,                                     // 3 floats per vert
      GL_UNSIGNED_SHORT,                     // floats
      sizeof(SpriteVertex3D),                //
      (void*)offsetof(SpriteVertex3D, info)  // offset in array (why is this a pointer...)
  );
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  u32 idx_buffer_len = SPRITE_RENDERER_MAX_SPRITES * 5;
  glGenBuffers(1, &m_ogl.index_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_len * sizeof(u32), nullptr, GL_STREAM_DRAW);

  glBindVertexArray(0);

  m_vertices_3d.resize(verts);
  m_index_buffer_data.resize(idx_buffer_len);

  m_default_mode.disable_depth_write();
  m_default_mode.set_depth_test(GsTest::ZTest::GEQUAL);
  m_default_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
  m_default_mode.set_aref(38);
  m_default_mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  m_default_mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
  m_default_mode.set_at(true);
  m_default_mode.set_zt(true);
  m_default_mode.set_ab(true);

  m_current_mode = m_default_mode;
}

/*!
 * Handle DMA data that does the per-frame setup.
 * This should get the dma chain immediately after the call to sprite-draw-distorters.
 * It ends right before the sprite-add-matrix-data for the 3d's
 */
void Sprite3::handle_sprite_frame_setup(DmaFollower& dma, GameVersion version) {
  // first is some direct data
  auto direct_data = dma.read_and_advance();
  ASSERT(direct_data.size_bytes == 3 * 16);
  memcpy(m_sprite_direct_setup, direct_data.data, 3 * 16);

  // next would be the program, but it's 0 size on the PC and isn't sent.

  // next is the "frame data"
  switch (version) {
    case GameVersion::Jak1: {
      auto frame_data = dma.read_and_advance();
      ASSERT(frame_data.size_bytes == (int)sizeof(SpriteFrameDataJak1));  // very cool
      ASSERT(frame_data.vifcode0().kind == VifCode::Kind::STCYCL);
      VifCodeStcycl frame_data_stcycl(frame_data.vifcode0());
      ASSERT(frame_data_stcycl.cl == 4);
      ASSERT(frame_data_stcycl.wl == 4);
      ASSERT(frame_data.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
      VifCodeUnpack frame_data_unpack(frame_data.vifcode1());
      ASSERT(frame_data_unpack.addr_qw == SpriteDataMem::FrameData);
      ASSERT(frame_data_unpack.use_tops_flag == false);
      SpriteFrameDataJak1 jak1_data;
      memcpy(&jak1_data, frame_data.data, sizeof(SpriteFrameDataJak1));
      m_frame_data.from_jak1(jak1_data);
    } break;
    case GameVersion::Jak2: {
      auto frame_data = dma.read_and_advance();
      ASSERT(frame_data.size_bytes == (int)sizeof(SpriteFrameData));  // very cool
      ASSERT(frame_data.vifcode0().kind == VifCode::Kind::STCYCL);
      VifCodeStcycl frame_data_stcycl(frame_data.vifcode0());
      ASSERT(frame_data_stcycl.cl == 4);
      ASSERT(frame_data_stcycl.wl == 4);
      ASSERT(frame_data.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
      VifCodeUnpack frame_data_unpack(frame_data.vifcode1());
      ASSERT(frame_data_unpack.addr_qw == SpriteDataMem::FrameData);
      ASSERT(frame_data_unpack.use_tops_flag == false);
      memcpy(&m_frame_data, frame_data.data, sizeof(SpriteFrameData));
    } break;
    default:
      ASSERT_NOT_REACHED();
  }

  // next, a MSCALF.
  auto mscalf = dma.read_and_advance();
  ASSERT(mscalf.size_bytes == 0);
  ASSERT(mscalf.vifcode0().kind == VifCode::Kind::MSCALF);
  ASSERT(mscalf.vifcode0().immediate == SpriteProgMem::Init);
  ASSERT(mscalf.vifcode1().kind == VifCode::Kind::FLUSHE);

  // next base and offset
  auto base_offset = dma.read_and_advance();
  ASSERT(base_offset.size_bytes == 0);
  ASSERT(base_offset.vifcode0().kind == VifCode::Kind::BASE);
  ASSERT(base_offset.vifcode0().immediate == SpriteDataMem::Buffer0);
  ASSERT(base_offset.vifcode1().kind == VifCode::Kind::OFFSET);
  ASSERT(base_offset.vifcode1().immediate == SpriteDataMem::Buffer1);
}

void Sprite3::render_3d(DmaFollower& dma) {
  // one time matrix data
  auto matrix_data = dma.read_and_advance();
  ASSERT(matrix_data.size_bytes == sizeof(Sprite3DMatrixData));

  bool unpack_ok = verify_unpack_with_stcycl(matrix_data, VifCode::Kind::UNPACK_V4_32, 4, 4, 5,
                                             SpriteDataMem::Matrix, false, false);
  ASSERT(unpack_ok);
  static_assert(sizeof(m_3d_matrix_data) == 5 * 16);
  memcpy(&m_3d_matrix_data, matrix_data.data, sizeof(m_3d_matrix_data));
  // TODO
}

void Sprite3::render_2d_group0(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  // opengl sprite frame setup
  auto shid = render_state->shaders[ShaderId::SPRITE3].id();
  glUniform4fv(glGetUniformLocation(shid, "hvdf_offset"), 1, m_3d_matrix_data.hvdf_offset.data());
  glUniform1f(glGetUniformLocation(shid, "pfog0"), m_frame_data.pfog0);
  glUniform1f(glGetUniformLocation(shid, "min_scale"), m_frame_data.min_scale);
  glUniform1f(glGetUniformLocation(shid, "max_scale"), m_frame_data.max_scale);
  glUniform1f(glGetUniformLocation(shid, "fog_min"), m_frame_data.fog_min);
  glUniform1f(glGetUniformLocation(shid, "fog_max"), m_frame_data.fog_max);
  // glUniform1f(glGetUniformLocation(shid, "bonus"), m_frame_data.bonus);
  // glUniform4fv(glGetUniformLocation(shid, "hmge_scale"), 1, m_frame_data.hmge_scale.data());
  glUniform1f(glGetUniformLocation(shid, "deg_to_rad"), m_frame_data.deg_to_rad);
  glUniform1f(glGetUniformLocation(shid, "inv_area"), m_frame_data.inv_area);
  glUniformMatrix4fv(glGetUniformLocation(shid, "camera"), 1, GL_FALSE,
                     m_3d_matrix_data.camera.data());
  glUniform4fv(glGetUniformLocation(shid, "xy_array"), 8, m_frame_data.xy_array[0].data());
  glUniform4fv(glGetUniformLocation(shid, "xyz_array"), 4, m_frame_data.xyz_array[0].data());
  glUniform4fv(glGetUniformLocation(shid, "st_array"), 4, m_frame_data.st_array[0].data());
  glUniform4fv(glGetUniformLocation(shid, "basis_x"), 1, m_frame_data.basis_x.data());
  glUniform4fv(glGetUniformLocation(shid, "basis_y"), 1, m_frame_data.basis_y.data());

  u16 last_prog = -1;

  while (looks_like_2d_chunk_start(dma)) {
    m_debug_stats.blocks_2d_grp0++;
    // 4 packets per chunk

    // first is the header
    u32 sprite_count = process_sprite_chunk_header(dma);
    m_debug_stats.count_2d_grp0 += sprite_count;

    // second is the vector data
    u32 expected_vec_size = sizeof(SpriteVecData2d) * sprite_count;
    auto vec_data = dma.read_and_advance();
    ASSERT(expected_vec_size <= sizeof(m_vec_data_2d));
    unpack_to_no_stcycl(&m_vec_data_2d, vec_data, VifCode::Kind::UNPACK_V4_32, expected_vec_size,
                        SpriteDataMem::Vector, false, true);

    // third is the adgif data
    u32 expected_adgif_size = sizeof(AdGifData) * sprite_count;
    auto adgif_data = dma.read_and_advance();
    ASSERT(expected_adgif_size <= sizeof(m_adgif));
    unpack_to_no_stcycl(&m_adgif, adgif_data, VifCode::Kind::UNPACK_V4_32, expected_adgif_size,
                        SpriteDataMem::Adgif, false, true);

    // fourth is the actual run!!!!!
    auto run = dma.read_and_advance();
    ASSERT(run.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(run.vifcode1().kind == VifCode::Kind::MSCAL);

    if (m_enabled) {
      if (run.vifcode1().immediate != last_prog) {
        // one-time setups and flushing
        flush_sprites(render_state, prof, false);
      }

      if (run.vifcode1().immediate == SpriteProgMem::Sprites2dGrp0) {
        if (m_2d_enable) {
          do_block_common(SpriteMode::Mode2D, sprite_count, render_state, prof);
        }
      } else {
        if (m_3d_enable) {
          do_block_common(SpriteMode::Mode3D, sprite_count, render_state, prof);
        }
      }
      last_prog = run.vifcode1().immediate;
    }
  }
}

void Sprite3::render_fake_shadow(DmaFollower& dma) {
  // TODO
  // nop + flushe
  auto nop_flushe = dma.read_and_advance();
  ASSERT(nop_flushe.vifcode0().kind == VifCode::Kind::NOP);
  ASSERT(nop_flushe.vifcode1().kind == VifCode::Kind::FLUSHE);
}

/*!
 * Handle DMA data for group1 2d's (HUD)
 */
void Sprite3::render_2d_group1(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  // one time matrix data upload
  auto mat_upload = dma.read_and_advance();
  bool mat_ok = verify_unpack_with_stcycl(mat_upload, VifCode::Kind::UNPACK_V4_32, 4, 4, 80,
                                          SpriteDataMem::Matrix, false, false);
  ASSERT(mat_ok);
  ASSERT(mat_upload.size_bytes == sizeof(m_hud_matrix_data));
  memcpy(&m_hud_matrix_data, mat_upload.data, sizeof(m_hud_matrix_data));

  // opengl sprite frame setup
  glUniform4fv(
      glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "hud_hvdf_offset"), 1,
      m_hud_matrix_data.hvdf_offset.data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "hud_hvdf_user"),
               75, m_hud_matrix_data.user_hvdf[0].data());
  glUniformMatrix4fv(
      glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "hud_matrix"), 1,
      GL_FALSE, m_hud_matrix_data.matrix.data());

  // loop through chunks.
  while (looks_like_2d_chunk_start(dma)) {
    m_debug_stats.blocks_2d_grp1++;
    // 4 packets per chunk

    // first is the header
    u32 sprite_count = process_sprite_chunk_header(dma);
    m_debug_stats.count_2d_grp1 += sprite_count;

    // second is the vector data
    u32 expected_vec_size = sizeof(SpriteVecData2d) * sprite_count;
    auto vec_data = dma.read_and_advance();
    ASSERT(expected_vec_size <= sizeof(m_vec_data_2d));
    unpack_to_no_stcycl(&m_vec_data_2d, vec_data, VifCode::Kind::UNPACK_V4_32, expected_vec_size,
                        SpriteDataMem::Vector, false, true);

    // third is the adgif data
    u32 expected_adgif_size = sizeof(AdGifData) * sprite_count;
    auto adgif_data = dma.read_and_advance();
    ASSERT(expected_adgif_size <= sizeof(m_adgif));
    unpack_to_no_stcycl(&m_adgif, adgif_data, VifCode::Kind::UNPACK_V4_32, expected_adgif_size,
                        SpriteDataMem::Adgif, false, true);

    // fourth is the actual run!!!!!
    auto run = dma.read_and_advance();
    ASSERT(run.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(run.vifcode1().kind == VifCode::Kind::MSCAL);
    ASSERT(run.vifcode1().immediate == SpriteProgMem::Sprites2dHud);
    if (m_enabled && m_2d_enable) {
      do_block_common(SpriteMode::ModeHUD, sprite_count, render_state, prof);
    }
  }
}

void Sprite3::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  switch (render_state->version) {
    case GameVersion::Jak1:
      render_jak1(dma, render_state, prof);
      break;
    case GameVersion::Jak2:
      render_jak2(dma, render_state, prof);
      break;
    default:
      ASSERT_NOT_REACHED();
  }
}

void Sprite3::render_jak2(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  m_debug_stats = {};
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0 || data0.vifcode1().kind == VifCode::Kind::NOP);
  ASSERT(data0.vif0() == 0 || data0.vifcode0().kind == VifCode::Kind::MARK);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag_offset() == render_state->next_bucket) {
    return;
  }

  // First is the distorter (temporarily disabled for jak 2)
  {
    // auto child = prof.make_scoped_child("distorter");
    // render_distorter(dma, render_state, child);
  }

  // next, the normal sprite stuff
  render_state->shaders[ShaderId::SPRITE3].activate();
  handle_sprite_frame_setup(dma, render_state->version);

  // 3d sprites
  render_3d(dma);

  // 2d draw
  // m_sprite_renderer.reset_state();
  {
    auto child = prof.make_scoped_child("2d-group0");
    render_2d_group0(dma, render_state, child);
    flush_sprites(render_state, prof, false);
  }

  // shadow draw
  render_fake_shadow(dma);

  // 2d draw (HUD)
  {
    auto child = prof.make_scoped_child("2d-group1");
    render_2d_group1(dma, render_state, child);
    flush_sprites(render_state, prof, true);
    auto nop_flushe = dma.read_and_advance();
    ASSERT(nop_flushe.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(nop_flushe.vifcode1().kind == VifCode::Kind::FLUSHE);
  }

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBlendEquation(GL_FUNC_ADD);

  {
    auto p = prof.make_scoped_child("glow");
    glow_dma_and_draw(dma, render_state, p);
  }

  // fmt::print("next bucket is 0x{}\n", render_state->next_bucket);
  while (dma.current_tag_offset() != render_state->next_bucket) {
    // auto tag = dma.current_tag();
    auto data = dma.read_and_advance();
    (void)data;
    // VifCode code(data.vif0());
    // fmt::print("@ 0x{:x} tag: {}", dma.current_tag_offset(), tag.print());
    // fmt::print(" vif0: {}\n", code.print());
    // fmt::print(" vif1: {}\n", VifCode(data.vif1()).print());
  }
}

void Sprite3::render_jak1(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  m_debug_stats = {};
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sprite renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  // First is the distorter
  {
    auto child = prof.make_scoped_child("distorter");
    render_distorter(dma, render_state, child);
  }

  render_state->shaders[ShaderId::SPRITE3].activate();

  // next, sprite frame setup.
  handle_sprite_frame_setup(dma, render_state->version);

  // 3d sprites
  render_3d(dma);

  // 2d draw
  // m_sprite_renderer.reset_state();
  {
    auto child = prof.make_scoped_child("2d-group0");
    render_2d_group0(dma, render_state, child);
    flush_sprites(render_state, prof, false);
  }

  // shadow draw
  render_fake_shadow(dma);

  // 2d draw (HUD)
  {
    auto child = prof.make_scoped_child("2d-group1");
    render_2d_group1(dma, render_state, child);
    flush_sprites(render_state, prof, true);
  }

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBlendEquation(GL_FUNC_ADD);

  // TODO finish this up.
  // fmt::print("next bucket is 0x{}\n", render_state->next_bucket);
  while (dma.current_tag_offset() != render_state->next_bucket) {
    //    auto tag = dma.current_tag();
    // fmt::print("@ 0x{:x} tag: {}", dma.current_tag_offset(), tag.print());
    auto data = dma.read_and_advance();
    VifCode code(data.vif0());
    // fmt::print(" vif0: {}\n", code.print());
    if (code.kind == VifCode::Kind::NOP) {
      // fmt::print(" vif1: {}\n", VifCode(data.vif1()).print());
    }
  }
}

void Sprite3::draw_debug_window() {
  ImGui::Separator();
  ImGui::Text("Distort sprites: %d", m_distort_stats.total_sprites);
  ImGui::Text("2D Group 0 (World) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp0,
              m_debug_stats.count_2d_grp0);
  ImGui::Text("2D Group 1 (HUD) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp1,
              m_debug_stats.count_2d_grp1);
  ImGui::Checkbox("Culling", &m_enable_culling);
  ImGui::Checkbox("2d", &m_2d_enable);
  ImGui::SameLine();
  ImGui::Checkbox("3d", &m_3d_enable);
  ImGui::Checkbox("Distort", &m_distort_enable);
  ImGui::Checkbox("Distort instancing", &m_enable_distort_instancing);
  ImGui::Separator();
  m_glow_renderer.draw_debug_window();
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Render (for real)

void Sprite3::flush_sprites(SharedRenderState* render_state,
                            ScopedProfilerNode& prof,
                            bool double_draw) {
  glBindVertexArray(m_ogl.vao);

  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  // upload vertex buffer
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_sprite_idx * sizeof(SpriteVertex3D) * 4, m_vertices_3d.data(),
               GL_STREAM_DRAW);

  // two passes through the buckets. first to build the index buffer
  u32 idx_offset = 0;
  for (const auto bucket : m_bucket_list) {
    memcpy(&m_index_buffer_data[idx_offset], bucket->ids.data(), bucket->ids.size() * sizeof(u32));
    bucket->offset_in_idx_buffer = idx_offset;
    idx_offset += bucket->ids.size();
  }

  // now upload it
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_offset * sizeof(u32), m_index_buffer_data.data(),
               GL_STREAM_DRAW);

  // now do draws!
  for (const auto bucket : m_bucket_list) {
    u32 tbp = bucket->key >> 32;
    DrawMode mode;
    mode.as_int() = bucket->key & 0xffffffff;

    std::optional<u64> tex;
    tex = render_state->texture_pool->lookup(tbp);

    if (!tex) {
      lg::warn("Failed to find texture at {}, using random", tbp);
      tex = render_state->texture_pool->get_placeholder_texture();
    }
    ASSERT(tex);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, *tex);

    auto settings = setup_opengl_from_draw_mode(mode, GL_TEXTURE0, false);

    glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_min"),
                double_draw ? settings.aref_first : 0.016);
    glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_max"),
                10.f);
    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "tex_T0"), 0);

    prof.add_draw_call();
    prof.add_tri(2 * (bucket->ids.size() / 5));

    glDrawElements(GL_TRIANGLE_STRIP, bucket->ids.size(), GL_UNSIGNED_INT,
                   (void*)(bucket->offset_in_idx_buffer * sizeof(u32)));

    if (double_draw) {
      switch (settings.kind) {
        case DoubleDrawKind::NONE:
          break;
        case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
          prof.add_draw_call();
          prof.add_tri(2 * (bucket->ids.size() / 5));
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_min"),
              -10.f);
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_max"),
              settings.aref_second);
          glDepthMask(GL_FALSE);
          glDrawElements(GL_TRIANGLE_STRIP, bucket->ids.size(), GL_UNSIGNED_INT,
                         (void*)(bucket->offset_in_idx_buffer * sizeof(u32)));
          break;
        default:
          ASSERT(false);
      }
    }
  }

  m_sprite_buckets.clear();
  m_bucket_list.clear();
  m_last_bucket_key = UINT64_MAX;
  m_last_bucket = nullptr;
  m_sprite_idx = 0;
  glBindVertexArray(0);
}

void Sprite3::handle_tex0(u64 val,
                          SharedRenderState* /*render_state*/,
                          ScopedProfilerNode& /*prof*/) {
  GsTex0 reg(val);

  // update tbp
  m_current_tbp = reg.tbp0();
  m_current_mode.set_tcc(reg.tcc());

  // tbw: assume they got it right
  // psm: assume they got it right
  // tw: assume they got it right
  // th: assume they got it right

  ASSERT(reg.tfx() == GsTex0::TextureFunction::MODULATE);
  ASSERT(reg.psm() != GsTex0::PSM::PSMT4HH);

  // cbp: assume they got it right
  // cpsm: assume they got it right
  // csm: assume they got it right
}

void Sprite3::handle_tex1(u64 val,
                          SharedRenderState* /*render_state*/,
                          ScopedProfilerNode& /*prof*/) {
  GsTex1 reg(val);
  m_current_mode.set_filt_enable(reg.mmag());
}

void Sprite3::handle_zbuf(u64 val,
                          SharedRenderState* /*render_state*/,
                          ScopedProfilerNode& /*prof*/) {
  // note: we can basically ignore this. There's a single z buffer that's always configured the same
  // way - 24-bit, at offset 448.
  GsZbuf x(val);
  ASSERT(x.psm() == TextureFormat::PSMZ24);
  ASSERT(x.zbp() == 448 || x.zbp() == 304);  // 304 for jak 2.

  m_current_mode.set_depth_write_enable(!x.zmsk());
}

void Sprite3::handle_clamp(u64 val,
                           SharedRenderState* /*render_state*/,
                           ScopedProfilerNode& /*prof*/) {
  if (!(val == 0b101 || val == 0 || val == 1 || val == 0b100)) {
    ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", val));
  }

  m_current_mode.set_clamp_s_enable(val & 0b001);
  m_current_mode.set_clamp_t_enable(val & 0b100);
}

void Sprite3::update_mode_from_alpha1(u64 val, DrawMode& mode) {
  GsAlpha reg(val);
  if (reg.a_mode() == GsAlpha::BlendMode::SOURCE && reg.b_mode() == GsAlpha::BlendMode::DEST &&
      reg.c_mode() == GsAlpha::BlendMode::SOURCE && reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - Cd) * As + Cd
    // Cs * As  + (1 - As) * Cd
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);

  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - 0) * As + Cd
    // Cs * As + (1) * CD
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_SRC_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    ASSERT(reg.fix() == 128);
    // Cv = (Cs - 0) * FIX + Cd
    // if fix = 128, it works out to 1.0
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_FIX_DST);
    // src plus dest
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::DEST &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // Cv = (Cs - Cd) * FIX + Cd
    ASSERT(reg.fix() == 64);
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_FIX_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.b_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (0 - Cs) * As + Cd
    // Cd - Cs * As
    // s, d
    mode.set_alpha_blend(DrawMode::AlphaBlend::ZERO_SRC_SRC_DST);
  }

  else {
    lg::error("unsupported blend: a {} b {} c {} d {}", (int)reg.a_mode(), (int)reg.b_mode(),
              (int)reg.c_mode(), (int)reg.d_mode());
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    ASSERT(false);
  }
}

void Sprite3::handle_alpha(u64 val,
                           SharedRenderState* /*render_state*/,
                           ScopedProfilerNode& /*prof*/) {
  update_mode_from_alpha1(val, m_current_mode);
}

void Sprite3::do_block_common(SpriteMode mode,
                              u32 count,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof) {
  m_current_mode = m_default_mode;
  for (u32 sprite_idx = 0; sprite_idx < count; sprite_idx++) {
    if (m_sprite_idx == SPRITE_RENDERER_MAX_SPRITES) {
      flush_sprites(render_state, prof, mode == ModeHUD);
    }

    if (mode == Mode2D && render_state->has_pc_data && m_enable_culling) {
      // we can skip sprites that are out of view
      // it's probably possible to do this for 3D as well.
      auto bsphere = m_vec_data_2d[sprite_idx].xyz_sx;
      bsphere.w() = std::max(bsphere.w(), m_vec_data_2d[sprite_idx].sy());
      if (bsphere.w() == 0 || !sphere_in_view_ref(bsphere, render_state->camera_planes)) {
        continue;
      }
    }

    auto& adgif = m_adgif[sprite_idx];
    handle_tex0(adgif.tex0_data, render_state, prof);
    handle_tex1(adgif.tex1_data, render_state, prof);
    if (GsRegisterAddress(adgif.clamp_addr) == GsRegisterAddress::ZBUF_1) {
      handle_zbuf(adgif.clamp_data, render_state, prof);
    } else {
      handle_clamp(adgif.clamp_data, render_state, prof);
    }
    handle_alpha(adgif.alpha_data, render_state, prof);

    u64 key = (((u64)m_current_tbp) << 32) | m_current_mode.as_int();
    Bucket* bucket;
    if (key == m_last_bucket_key) {
      bucket = m_last_bucket;
    } else {
      auto it = m_sprite_buckets.find(key);
      if (it == m_sprite_buckets.end()) {
        bucket = &m_sprite_buckets[key];
        bucket->key = key;
        m_bucket_list.push_back(bucket);
      } else {
        bucket = &it->second;
      }
    }
    u32 start_vtx_id = m_sprite_idx * 4;
    bucket->ids.push_back(start_vtx_id);
    bucket->ids.push_back(start_vtx_id + 1);
    bucket->ids.push_back(start_vtx_id + 2);
    bucket->ids.push_back(start_vtx_id + 3);
    bucket->ids.push_back(UINT32_MAX);

    auto& vert1 = m_vertices_3d.at(start_vtx_id + 0);

    vert1.xyz_sx = m_vec_data_2d[sprite_idx].xyz_sx;
    vert1.quat_sy = m_vec_data_2d[sprite_idx].flag_rot_sy;
    vert1.rgba = m_vec_data_2d[sprite_idx].rgba / 255;
    vert1.flags_matrix[0] = m_vec_data_2d[sprite_idx].flag();
    vert1.flags_matrix[1] = m_vec_data_2d[sprite_idx].matrix();
    vert1.info[0] = 0;  // hack
    vert1.info[1] = m_current_mode.get_tcc_enable();
    vert1.info[2] = 0;
    vert1.info[3] = mode;

    m_vertices_3d.at(start_vtx_id + 1) = vert1;
    m_vertices_3d.at(start_vtx_id + 2) = vert1;
    m_vertices_3d.at(start_vtx_id + 3) = vert1;

    m_vertices_3d.at(start_vtx_id + 1).info[2] = 1;
    m_vertices_3d.at(start_vtx_id + 2).info[2] = 3;
    m_vertices_3d.at(start_vtx_id + 3).info[2] = 2;

    ++m_sprite_idx;
  }
}
