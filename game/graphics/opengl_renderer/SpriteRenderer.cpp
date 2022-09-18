#include "SpriteRenderer.h"

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
  ASSERT(header[0] <= SpriteRenderer::SPRITES_PER_CHUNK);
  return header[0];
}
}  // namespace

constexpr int SPRITE_RENDERER_MAX_SPRITES = 8000;

SpriteRenderer::SpriteRenderer(const std::string& name, int my_id) : BucketRenderer(name, my_id) {
  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenVertexArrays(1, &m_ogl.vao);
  glBindVertexArray(m_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  auto verts = SPRITE_RENDERER_MAX_SPRITES * 3 * 2;
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
      1,                                        // location 0 in the shader
      4,                                        // 4 color components
      GL_FLOAT,                                 // floats
      GL_TRUE,                                  // normalized, ignored,
      sizeof(SpriteVertex3D),                   //
      (void*)offsetof(SpriteVertex3D, quat_sy)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(2);
  glVertexAttribPointer(
      2,                                     // location 0 in the shader
      4,                                     // 4 color components
      GL_FLOAT,                              // floats
      GL_TRUE,                               // normalized, ignored,
      sizeof(SpriteVertex3D),                //
      (void*)offsetof(SpriteVertex3D, rgba)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(3);
  glVertexAttribIPointer(
      3,                                             // location 0 in the shader
      2,                                             // 4 color components
      GL_UNSIGNED_SHORT,                             // floats
      sizeof(SpriteVertex3D),                        //
      (void*)offsetof(SpriteVertex3D, flags_matrix)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(4);
  glVertexAttribIPointer(
      4,                                     // location 0 in the shader
      4,                                     // 3 floats per vert
      GL_UNSIGNED_SHORT,                     // floats
      sizeof(SpriteVertex3D),                //
      (void*)offsetof(SpriteVertex3D, info)  // offset in array (why is this a pointer...)
  );
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  m_vertices_3d.resize(verts);
}

/*!
 * Run the sprite distorter.  Currently nothing uses sprite-distorter so this just skips through
 * the table upload stuff that runs every frame, even if there are no sprites.
 */
void SpriteRenderer::render_distorter(DmaFollower& dma,
                                      SharedRenderState* /*render_state*/,
                                      ScopedProfilerNode& /*prof*/) {
  // Next thing should be the sprite-distorter setup
  // m_direct_renderer.reset_state();
  while (dma.current_tag().qwc != 7) {
    dma.read_and_advance();
    // m_direct_renderer.render_vif(direct_data.vif0(), direct_data.vif1(), direct_data.data,
    // direct_data.size_bytes, render_state, prof);
  }
  // m_direct_renderer.flush_pending(render_state, prof);
  auto sprite_distorter_direct_setup = dma.read_and_advance();
  ASSERT(sprite_distorter_direct_setup.vifcode0().kind == VifCode::Kind::NOP);
  ASSERT(sprite_distorter_direct_setup.vifcode1().kind == VifCode::Kind::DIRECT);
  ASSERT(sprite_distorter_direct_setup.vifcode1().immediate == 7);
  memcpy(m_sprite_distorter_setup, sprite_distorter_direct_setup.data, 7 * 16);

  // Next thing should be the sprite-distorter tables
  auto sprite_distorter_tables = dma.read_and_advance();
  ASSERT(sprite_distorter_tables.size_bytes == 0x8b * 16);
  ASSERT(sprite_distorter_tables.vifcode0().kind == VifCode::Kind::STCYCL);
  VifCodeStcycl distorter_table_transfer(sprite_distorter_tables.vifcode0());
  ASSERT(distorter_table_transfer.cl == 4);
  ASSERT(distorter_table_transfer.wl == 4);
  // TODO: check unpack cmd (vif1)

  // TODO: do something with the table

  // next would be the program, but we don't have it.

  // TODO: next is the sprite-distorter (currently not used)
}

/*!
 * Handle DMA data that does the per-frame setup.
 * This should get the dma chain immediately after the call to sprite-draw-distorters.
 * It ends right before the sprite-add-matrix-data for the 3d's
 */
void SpriteRenderer::handle_sprite_frame_setup(DmaFollower& dma) {
  // first is some direct data
  auto direct_data = dma.read_and_advance();
  ASSERT(direct_data.size_bytes == 3 * 16);
  memcpy(m_sprite_direct_setup, direct_data.data, 3 * 16);

  // next would be the program, but it's 0 size on the PC and isn't sent.

  // next is the "frame data"
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

void SpriteRenderer::render_3d(DmaFollower& dma) {
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

void SpriteRenderer::render_2d_group0(DmaFollower& dma,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof) {
  // opengl sprite frame setup
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "hvdf_offset"), 1,
               m_3d_matrix_data.hvdf_offset.data());
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "pfog0"),
              m_frame_data.pfog0);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "min_scale"),
              m_frame_data.min_scale);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "max_scale"),
              m_frame_data.max_scale);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "fog_min"),
              m_frame_data.fog_min);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "fog_max"),
              m_frame_data.fog_max);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "bonus"),
              m_frame_data.bonus);
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "hmge_scale"), 1,
               m_frame_data.hmge_scale.data());
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "deg_to_rad"),
              m_frame_data.deg_to_rad);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "inv_area"),
              m_frame_data.inv_area);
  glUniformMatrix4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "camera"),
                     1, GL_FALSE, m_3d_matrix_data.camera.data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "xy_array"), 8,
               m_frame_data.xy_array[0].data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "xyz_array"), 4,
               m_frame_data.xyz_array[0].data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "st_array"), 4,
               m_frame_data.st_array[0].data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "basis_x"), 1,
               m_frame_data.basis_x.data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "basis_y"), 1,
               m_frame_data.basis_y.data());

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
        flush_sprites(render_state, prof);
        if (run.vifcode1().immediate == SpriteProgMem::Sprites2dGrp0 &&
            m_prim_gl_state.current_register != m_frame_data.sprite_2d_giftag.prim()) {
          m_prim_gl_state.from_register(m_frame_data.sprite_2d_giftag.prim());
        } else if (m_prim_gl_state.current_register != m_frame_data.sprite_3d_giftag.prim()) {
          m_prim_gl_state.from_register(m_frame_data.sprite_3d_giftag.prim());
        }
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

void SpriteRenderer::render_fake_shadow(DmaFollower& dma) {
  // TODO
  // nop + flushe
  auto nop_flushe = dma.read_and_advance();
  ASSERT(nop_flushe.vifcode0().kind == VifCode::Kind::NOP);
  ASSERT(nop_flushe.vifcode1().kind == VifCode::Kind::FLUSHE);
}

/*!
 * Handle DMA data for group1 2d's (HUD)
 */
void SpriteRenderer::render_2d_group1(DmaFollower& dma,
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
      glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "hud_hvdf_offset"), 1,
      m_hud_matrix_data.hvdf_offset.data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "hud_hvdf_user"),
               75, m_hud_matrix_data.user_hvdf[0].data());
  glUniformMatrix4fv(
      glGetUniformLocation(render_state->shaders[ShaderId::SPRITE].id(), "hud_matrix"), 1, GL_FALSE,
      m_hud_matrix_data.matrix.data());

  m_prim_gl_state.from_register(m_frame_data.sprite_2d_giftag2.prim());

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

void SpriteRenderer::render(DmaFollower& dma,
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

  render_state->shaders[ShaderId::SPRITE].activate();

  // First is the distorter
  {
    auto child = prof.make_scoped_child("distorter");
    render_distorter(dma, render_state, child);
  }

  // next, sprite frame setup.
  handle_sprite_frame_setup(dma);

  // 3d sprites
  render_3d(dma);

  // 2d draw
  // m_sprite_renderer.reset_state();
  {
    auto child = prof.make_scoped_child("2d-group0");
    render_2d_group0(dma, render_state, child);
    flush_sprites(render_state, prof);
  }

  // shadow draw
  render_fake_shadow(dma);

  // 2d draw (HUD)
  {
    auto child = prof.make_scoped_child("2d-group1");
    render_2d_group1(dma, render_state, child);
    flush_sprites(render_state, prof);
  }

  // TODO finish this up.
  // fmt::print("next bucket is 0x{}\n", render_state->next_bucket);
  while (dma.current_tag_offset() != render_state->next_bucket) {
    //    auto tag = dma.current_tag();
    // fmt::print("@ 0x{:x} tag: {}", dma.current_tag_offset(), tag.print());
    auto data = dma.read_and_advance();
    VifCode code(data.vif0());
    // fmt::print(" vif: {}\n", code.print());
    if (code.kind == VifCode::Kind::NOP) {
      // fmt::print(" vif: {}\n", VifCode(data.vif1()).print());
    }
  }

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBlendEquation(GL_FUNC_ADD);
}

void SpriteRenderer::draw_debug_window() {
  ImGui::Separator();
  ImGui::Text("2D Group 0 (World) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp0,
              m_debug_stats.count_2d_grp0);
  ImGui::Text("2D Group 1 (HUD) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp1,
              m_debug_stats.count_2d_grp1);
  ImGui::Checkbox("Culling", &m_enable_culling);
  ImGui::Checkbox("2d", &m_2d_enable);
  ImGui::SameLine();
  ImGui::Checkbox("3d", &m_3d_enable);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Render (for real)

void SpriteRenderer::flush_sprites(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  for (int i = 0; i <= m_adgif_index; ++i) {
    update_gl_texture(render_state, i);
  }

  if (m_sprite_offset == 0) {
    // nothing to render
    m_adgif_index = 0;
    return;
  }

  update_gl_blend(m_adgif_state_stack[m_adgif_index]);

  if (m_adgif_state_stack[m_adgif_index].z_write) {
    glDepthMask(GL_TRUE);
  } else {
    glDepthMask(GL_FALSE);
  }

  glBindVertexArray(m_ogl.vao);

  // render!
  // fmt::print("drawing {} sprites\n", m_sprite_offset);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_sprite_offset * sizeof(SpriteVertex3D) * 6, m_vertices_3d.data(),
               GL_STREAM_DRAW);

  glDrawArrays(GL_TRIANGLES, 0, m_sprite_offset * 6);

  glBindVertexArray(0);
  int n_tris = m_sprite_offset * 6 / 3;
  prof.add_tri(n_tris);
  prof.add_draw_call(1);

  m_sprite_offset = 0;
  m_adgif_index = 0;
}

void SpriteRenderer::handle_tex0(u64 val,
                                 SharedRenderState* /*render_state*/,
                                 ScopedProfilerNode& /*prof*/) {
  GsTex0 reg(val);

  // update tbp

  m_adgif_state.reg_tex0 = reg;
  m_adgif_state.texture_base_ptr = reg.tbp0();
  m_adgif_state.using_mt4hh = reg.psm() == GsTex0::PSM::PSMT4HH;
  m_adgif_state.tcc = reg.tcc();

  // tbw: assume they got it right
  // psm: assume they got it right
  // tw: assume they got it right
  // th: assume they got it right

  ASSERT(reg.tfx() == GsTex0::TextureFunction::MODULATE);

  // cbp: assume they got it right
  // cpsm: assume they got it right
  // csm: assume they got it right
}

void SpriteRenderer::handle_tex1(u64 val,
                                 SharedRenderState* /*render_state*/,
                                 ScopedProfilerNode& /*prof*/) {
  GsTex1 reg(val);
  // for now, we aren't going to handle mipmapping. I don't think it's used with direct.
  //   ASSERT(reg.mxl() == 0);
  // if that's true, we can ignore LCM, MTBA, L, K

  m_adgif_state.enable_tex_filt = reg.mmag();

  // MMAG/MMIN specify texture filtering. For now, assume always linear
  //  ASSERT(reg.mmag() == true);
  //  if (!(reg.mmin() == 1 || reg.mmin() == 4)) {  // with mipmap off, both of these are linear
  //                                                //    lg::error("unsupported mmin");
  //  }
}

void SpriteRenderer::handle_zbuf(u64 val,
                                 SharedRenderState* /*render_state*/,
                                 ScopedProfilerNode& /*prof*/) {
  // note: we can basically ignore this. There's a single z buffer that's always configured the same
  // way - 24-bit, at offset 448.
  GsZbuf x(val);
  ASSERT(x.psm() == TextureFormat::PSMZ24);
  ASSERT(x.zbp() == 448);

  m_adgif_state.z_write = !x.zmsk();
}

void SpriteRenderer::handle_clamp(u64 val,
                                  SharedRenderState* /*render_state*/,
                                  ScopedProfilerNode& /*prof*/) {
  if (!(val == 0b101 || val == 0 || val == 1 || val == 0b100)) {
    ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", val));
  }

  m_adgif_state.reg_clamp = val;
  m_adgif_state.clamp_s = val & 0b001;
  m_adgif_state.clamp_t = val & 0b100;
}

void SpriteRenderer::update_gl_blend(AdGifState& state) {
  if (!m_prim_gl_state.alpha_blend_enable) {
    glDisable(GL_BLEND);
  } else {
    glEnable(GL_BLEND);
    if (state.a == GsAlpha::BlendMode::SOURCE && state.b == GsAlpha::BlendMode::DEST &&
        state.c == GsAlpha::BlendMode::SOURCE && state.d == GsAlpha::BlendMode::DEST) {
      // (Cs - Cd) * As + Cd
      // Cs * As  + (1 - As) * Cd
      // s, d
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glBlendEquation(GL_FUNC_ADD);
    } else if (state.a == GsAlpha::BlendMode::SOURCE &&
               state.b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
               state.c == GsAlpha::BlendMode::SOURCE && state.d == GsAlpha::BlendMode::DEST) {
      // (Cs - 0) * As + Cd
      // Cs * As + (1) * Cd
      // s, d
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glBlendEquation(GL_FUNC_ADD);
    } else if (state.a == GsAlpha::BlendMode::ZERO_OR_FIXED &&
               state.b == GsAlpha::BlendMode::SOURCE && state.c == GsAlpha::BlendMode::SOURCE &&
               state.d == GsAlpha::BlendMode::DEST) {
      // (0 - Cs) * As + Cd
      // Cd - Cs * As
      // s, d
      glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      glBlendEquation(GL_FUNC_REVERSE_SUBTRACT);
    } else {
      // unsupported blend: a 0 b 2 c 2 d 1
      lg::error("unsupported blend: a {} b {} c {} d {} NOTE THIS DOWN IMMEDIATELY!!", (int)state.a,
                (int)state.b, (int)state.c, (int)state.d);
      ASSERT(false);
    }
  }
}

void SpriteRenderer::handle_alpha(u64 val,
                                  SharedRenderState* /*render_state*/,
                                  ScopedProfilerNode& /*prof*/) {
  GsAlpha reg(val);

  m_adgif_state.from_register(reg);
}

void SpriteRenderer::update_gl_prim(SharedRenderState* /*render_state*/) {
  // currently gouraud is handled in setup.
  const auto& state = m_prim_gl_state;
  if (state.fogging_enable) {
    //    ASSERT(false);
  }
  if (state.aa_enable) {
    ASSERT(false);
  }
  if (state.use_uv) {
    ASSERT(false);
  }
  if (state.ctxt) {
    ASSERT(false);
  }
  if (state.fix) {
    ASSERT(false);
  }
}

void SpriteRenderer::update_gl_texture(SharedRenderState* render_state, int unit) {
  std::optional<u64> tex;
  auto& state = m_adgif_state_stack[unit];
  if (!state.used) {
    // nothing used this state, don't bother binding the texture.
    return;
  }
  if (state.using_mt4hh) {
    tex = render_state->texture_pool->lookup_mt4hh(state.texture_base_ptr);
  } else {
    tex = render_state->texture_pool->lookup(state.texture_base_ptr);
  }

  if (!tex) {
    lg::warn("Failed to find texture at {}, using random", state.texture_base_ptr);
    tex = render_state->texture_pool->get_placeholder_texture();
  }
  ASSERT(tex);

  glActiveTexture(GL_TEXTURE20 + unit);
  glBindTexture(GL_TEXTURE_2D, *tex);
  // Note: CLAMP and CLAMP_TO_EDGE are different...
  if (state.clamp_s) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  }

  if (state.clamp_t) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (state.enable_tex_filt) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }

  state.used = false;
}

void SpriteRenderer::do_block_common(SpriteMode mode,
                                     u32 count,
                                     SharedRenderState* render_state,
                                     ScopedProfilerNode& prof) {
  for (u32 sprite_idx = 0; sprite_idx < count; sprite_idx++) {
    if (m_sprite_offset == SPRITE_RENDERER_MAX_SPRITES) {
      flush_sprites(render_state, prof);
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
    // fmt::print("adgif: {:X} {:X} {:X} {:X}\n", adgif.tex0_data, adgif.tex1_data,
    // adgif.clamp_data, adgif.alpha_data); fmt::print("adgif regs: {} {} {} {} {}\n",
    // register_address_name(adgif.tex0_addr), register_address_name(adgif.tex1_addr),
    // register_address_name(adgif.mip_addr), register_address_name(adgif.clamp_addr),
    // register_address_name(adgif.alpha_addr));
    handle_tex0(adgif.tex0_data, render_state, prof);
    handle_tex1(adgif.tex1_data, render_state, prof);
    // handle_mip(adgif.mip_data, render_state, prof);
    if (GsRegisterAddress(adgif.clamp_addr) == GsRegisterAddress::ZBUF_1) {
      handle_zbuf(adgif.clamp_data, render_state, prof);
    } else {
      handle_clamp(adgif.clamp_data, render_state, prof);
    }
    handle_alpha(adgif.alpha_data, render_state, prof);

    if (!m_adgif_state_stack[m_adgif_index].used) {
      m_adgif_state_stack[m_adgif_index] = m_adgif_state;
      m_adgif_state_stack[m_adgif_index].used = true;
    } else if (m_adgif_state != m_adgif_state_stack[m_adgif_index]) {
      if (m_adgif_index + 1 == ADGIF_STATE_COUNT ||
          !m_adgif_state.nontexture_equal(m_adgif_state_stack[m_adgif_index])) {
        flush_sprites(render_state, prof);
      } else {
        m_adgif_index++;
      }
      m_adgif_state_stack[m_adgif_index] = m_adgif_state;
      m_adgif_state_stack[m_adgif_index].used = true;
    }

    int vert_idx = 6 * m_sprite_offset;

    auto& vert1 = m_vertices_3d.at(vert_idx + 0);

    vert1.xyz_sx = m_vec_data_2d[sprite_idx].xyz_sx;
    vert1.quat_sy = m_vec_data_2d[sprite_idx].flag_rot_sy;
    vert1.rgba = m_vec_data_2d[sprite_idx].rgba / 255;
    vert1.flags_matrix[0] = m_vec_data_2d[sprite_idx].flag();
    vert1.flags_matrix[1] = m_vec_data_2d[sprite_idx].matrix();
    vert1.info[0] = m_adgif_index;
    vert1.info[1] = m_adgif_state_stack[m_adgif_index].tcc;
    vert1.info[2] = 0;
    vert1.info[3] = mode;

    m_vertices_3d.at(vert_idx + 1) = vert1;
    m_vertices_3d.at(vert_idx + 2) = vert1;
    m_vertices_3d.at(vert_idx + 3) = vert1;
    m_vertices_3d.at(vert_idx + 4) = vert1;
    m_vertices_3d.at(vert_idx + 5) = vert1;

    m_vertices_3d.at(vert_idx + 1).info[2] = 1;
    m_vertices_3d.at(vert_idx + 2).info[2] = 2;
    m_vertices_3d.at(vert_idx + 3).info[2] = 2;
    m_vertices_3d.at(vert_idx + 4).info[2] = 3;
    m_vertices_3d.at(vert_idx + 5).info[2] = 0;

    ++m_sprite_offset;
  }
}
