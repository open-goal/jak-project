#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "SpriteRenderer.h"
#include "game/graphics/opengl_renderer/dma_helpers.h"

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
  assert(ok);
  u32 header[4];
  memcpy(header, transfer.data, 16);
  assert(header[0] <= SpriteRenderer::SPRITES_PER_CHUNK);
  return header[0];
}
}  // namespace

SpriteRenderer::SpriteRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id),
      m_sprite_renderer(fmt::format("{}.sprites", name),
                        my_id,
                        16384,
                        DirectRenderer::Mode::SPRITE_CPU),
      m_direct_renderer(fmt::format("{}.direct", name), my_id, 100, DirectRenderer::Mode::NORMAL) {}

/*!
 * Run the sprite distorter.  Currently nothing uses sprite-distorter so this just skips through
 * the table upload stuff that runs every frame, even if there are no sprites.
 */
void SpriteRenderer::render_distorter(DmaFollower& dma,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof) {
  // Next thing should be the sprite-distorter setup
  m_direct_renderer.reset_state();
  while (dma.current_tag().qwc != 7) {
    auto direct_data = dma.read_and_advance();
    m_direct_renderer.render_vif(direct_data.vif0(), direct_data.vif1(), direct_data.data,
                                 direct_data.size_bytes, render_state, prof);
  }
  m_direct_renderer.flush_pending(render_state, prof);
  auto sprite_distorter_direct_setup = dma.read_and_advance();
  assert(sprite_distorter_direct_setup.vifcode0().kind == VifCode::Kind::NOP);
  assert(sprite_distorter_direct_setup.vifcode1().kind == VifCode::Kind::DIRECT);
  assert(sprite_distorter_direct_setup.vifcode1().immediate == 7);
  memcpy(m_sprite_distorter_setup, sprite_distorter_direct_setup.data, 7 * 16);

  // Next thing should be the sprite-distorter tables
  auto sprite_distorter_tables = dma.read_and_advance();
  assert(sprite_distorter_tables.size_bytes == 0x8b * 16);
  assert(sprite_distorter_tables.vifcode0().kind == VifCode::Kind::STCYCL);
  VifCodeStcycl distorter_table_transfer(sprite_distorter_tables.vifcode0());
  assert(distorter_table_transfer.cl == 4);
  assert(distorter_table_transfer.wl == 4);
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
  assert(direct_data.size_bytes == 3 * 16);
  memcpy(m_sprite_direct_setup, direct_data.data, 3 * 16);

  // next would be the program, but it's 0 size on the PC and isn't sent.

  // next is the "frame data"
  auto frame_data = dma.read_and_advance();
  assert(frame_data.size_bytes == (int)sizeof(SpriteFrameData));  // very cool
  assert(frame_data.vifcode0().kind == VifCode::Kind::STCYCL);
  VifCodeStcycl frame_data_stcycl(frame_data.vifcode0());
  assert(frame_data_stcycl.cl == 4);
  assert(frame_data_stcycl.wl == 4);
  assert(frame_data.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
  VifCodeUnpack frame_data_unpack(frame_data.vifcode1());
  assert(frame_data_unpack.addr_qw == SpriteDataMem::FrameData);
  assert(frame_data_unpack.use_tops_flag == false);
  memcpy(&m_frame_data, frame_data.data, sizeof(SpriteFrameData));

  // next, a MSCALF.
  auto mscalf = dma.read_and_advance();
  assert(mscalf.size_bytes == 0);
  assert(mscalf.vifcode0().kind == VifCode::Kind::MSCALF);
  assert(mscalf.vifcode0().immediate == SpriteProgMem::Init);
  assert(mscalf.vifcode1().kind == VifCode::Kind::FLUSHE);

  // next base and offset
  auto base_offset = dma.read_and_advance();
  assert(base_offset.size_bytes == 0);
  assert(base_offset.vifcode0().kind == VifCode::Kind::BASE);
  assert(base_offset.vifcode0().immediate == SpriteDataMem::Buffer0);
  assert(base_offset.vifcode1().kind == VifCode::Kind::OFFSET);
  assert(base_offset.vifcode1().immediate == SpriteDataMem::Buffer1);
}

void SpriteRenderer::render_3d(DmaFollower& dma) {
  // one time matrix data
  auto matrix_data = dma.read_and_advance();
  assert(matrix_data.size_bytes == sizeof(Sprite3DMatrixData));

  bool unpack_ok = verify_unpack_with_stcycl(matrix_data, VifCode::Kind::UNPACK_V4_32, 4, 4, 5,
                                             SpriteDataMem::Matrix, false, false);
  assert(unpack_ok);
  static_assert(sizeof(m_3d_matrix_data) == 5 * 16);
  memcpy(&m_3d_matrix_data, matrix_data.data, sizeof(m_3d_matrix_data));
  // TODO
}

void SpriteRenderer::render_2d_group0(DmaFollower& dma,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof) {
  while (looks_like_2d_chunk_start(dma)) {
    m_debug_stats.blocks_2d_grp0++;
    // 4 packets per chunk

    // first is the header
    u32 sprite_count = process_sprite_chunk_header(dma);
    m_debug_stats.count_2d_grp0 += sprite_count;

    // second is the vector data
    u32 expected_vec_size = sizeof(SpriteVecData2d) * sprite_count;
    auto vec_data = dma.read_and_advance();
    assert(expected_vec_size <= sizeof(m_vec_data_2d));
    unpack_to_no_stcycl(&m_vec_data_2d, vec_data, VifCode::Kind::UNPACK_V4_32, expected_vec_size,
                        SpriteDataMem::Vector, false, true);

    // third is the adgif data
    u32 expected_adgif_size = sizeof(AdGifData) * sprite_count;
    auto adgif_data = dma.read_and_advance();
    assert(expected_adgif_size <= sizeof(m_adgif));
    unpack_to_no_stcycl(&m_adgif, adgif_data, VifCode::Kind::UNPACK_V4_32, expected_adgif_size,
                        SpriteDataMem::Adgif, false, true);

    // fourth is the actual run!!!!!
    auto run = dma.read_and_advance();
    assert(run.vifcode0().kind == VifCode::Kind::NOP);
    assert(run.vifcode1().kind == VifCode::Kind::MSCAL);

    // HACK: this renderers 3D sprites with the 2D renderer. amazingly, it almost works.
    // assert(run.vifcode1().immediate == SpriteProgMem::Sprites2dGrp0);
    if (m_enabled) {
      if (run.vifcode1().immediate == SpriteProgMem::Sprites2dGrp0) {
        if (m_2d_enable) {
          do_2d_group0_block_cpu(sprite_count, render_state, prof);
        }
      } else {
        if (m_3d_enable) {
          do_3d_block_cpu(sprite_count, render_state, prof);
        }
      }
    }
  }
}

void SpriteRenderer::render_fake_shadow(DmaFollower& dma) {
  // TODO
  // nop + flushe
  auto nop_flushe = dma.read_and_advance();
  assert(nop_flushe.vifcode0().kind == VifCode::Kind::NOP);
  assert(nop_flushe.vifcode1().kind == VifCode::Kind::FLUSHE);
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
  assert(mat_ok);
  assert(mat_upload.size_bytes == sizeof(m_hud_matrix_data));
  memcpy(&m_hud_matrix_data, mat_upload.data, sizeof(m_hud_matrix_data));

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
    assert(expected_vec_size <= sizeof(m_vec_data_2d));
    unpack_to_no_stcycl(&m_vec_data_2d, vec_data, VifCode::Kind::UNPACK_V4_32, expected_vec_size,
                        SpriteDataMem::Vector, false, true);

    // third is the adgif data
    u32 expected_adgif_size = sizeof(AdGifData) * sprite_count;
    auto adgif_data = dma.read_and_advance();
    assert(expected_adgif_size <= sizeof(m_adgif));
    unpack_to_no_stcycl(&m_adgif, adgif_data, VifCode::Kind::UNPACK_V4_32, expected_adgif_size,
                        SpriteDataMem::Adgif, false, true);

    // fourth is the actual run!!!!!
    auto run = dma.read_and_advance();
    assert(run.vifcode0().kind == VifCode::Kind::NOP);
    assert(run.vifcode1().kind == VifCode::Kind::MSCAL);
    assert(run.vifcode1().immediate == SpriteProgMem::Sprites2dHud);
    if (m_enabled && m_2d_enable) {
      do_2d_group1_block_cpu(sprite_count, render_state, prof);
    }
  }
}

void SpriteRenderer::render(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  m_debug_stats = {};
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sprite renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

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
  m_sprite_renderer.reset_state();
  {
    auto child = prof.make_scoped_child("2d-group0");
    render_2d_group0(dma, render_state, child);
  }

  // shadow draw
  render_fake_shadow(dma);

  // 2d draw (HUD)
  {
    auto child = prof.make_scoped_child("2d-group1");
    render_2d_group1(dma, render_state, child);
    m_sprite_renderer.flush_pending(render_state, child);
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
}

void SpriteRenderer::draw_debug_window() {
  ImGui::Separator();
  ImGui::Text("2D Group 0 (World) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp0,
              m_debug_stats.count_2d_grp0);
  ImGui::Text("2D Group 1 (HUD) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp1,
              m_debug_stats.count_2d_grp1);
  ImGui::Checkbox("Extra Debug", &m_extra_debug);
  ImGui::Checkbox("2d", &m_2d_enable);
  ImGui::SameLine();
  ImGui::Checkbox("3d", &m_3d_enable);
  ImGui::SameLine();
  ImGui::Checkbox("3d-debug", &m_3d_debug);
  if (ImGui::TreeNode("direct")) {
    m_sprite_renderer.draw_debug_window();
    ImGui::TreePop();
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Render (for real)

namespace {
Vector4f matrix_transform(const Matrix4f& mat, const Vector4f& pt) {
  // mulaw.xyzw ACC, vf28, vf00
  // maddax.xyzw ACC, vf25, vf01
  // madday.xyzw ACC, vf26, vf01
  // maddz.xyzw vf02, vf27, vf01
  return mat.col(3) + (mat.col(0) * pt[0]) + (mat.col(1) * pt[1]) + (mat.col(2) * pt[2]);
}

bool clip_xyz_plus_minus(const Vector4f& pt) {
  float pw = std::abs(pt.w());
  float mw = -pw;
  for (int i = 0; i < 3; i++) {
    if (pt[i] > pw) {
      return true;
    }
    if (pt[i] < mw) {
      return true;
    }
  }
  return false;
}

void imgui_vec(const Vector4f& vec, const char* name = nullptr, int indent = 0) {
  std::string spacing(indent, ' ');
  if (name) {
    ImGui::Text("%s%s: %f, %f, %f, %f", spacing.c_str(), name, vec.x(), vec.y(), vec.z(), vec.w());
  } else {
    ImGui::Text("%s%f, %f, %f, %f", spacing.c_str(), vec.x(), vec.y(), vec.z(), vec.w());
  }
}
}  // namespace

/*!
 * Render the sprites!
 * This is a somewhat inefficient way to do it:
 * The VU program is (poorly) translated to C, then the gs packet is sent to a DirectRenderer.
 * In the future we should make a sprite-specific renderer which would have some benefits:
 *  - do this math on the GPU
 *  - special case the primitive buffer stuff
 */
void SpriteRenderer::do_2d_group1_block_cpu(u32 count,
                                            SharedRenderState* render_state,
                                            ScopedProfilerNode& prof) {
  if (m_extra_debug) {
    ImGui::Begin("Sprite Extra Debug 2d_1");
  }

  // set up double buffering
  //  xtop vi02                  |  nop
  //  nop                        |  nop
  // load sprite count from header
  // vi04 = count
  //  ilwr.x vi04, vi02          |  nop
  // vi02 = m_vec_data_2d
  //  iaddi vi02, vi02, 0x1      |  nop
  // vi03 = m_adgif
  //  iaddiu vi03, vi02, 0x90    |  nop

  // this VU program uses "software pipelining"
  // it's a little bit tricky to use software pipelining in a case like
  // this where sometimes you want to reject a sprite entirely and jump ahead
  // so sometimes they reset back to L7 on rejection.

  // The approach in this translation is to assume we loop back to L7 every time
  // and not worry about the pipeline stuff that shows up in L8 and on.
  // you can enter from L7 at anytime, they are not assumed to only run on the first go.
  // (though if their implementation has bugs we will not replicate them correctly...)

  Matrix4f camera_matrix = m_hud_matrix_data.matrix;  // vf25, vf26, vf27, vf28

  for (u32 sprite_idx = 0; sprite_idx < count; sprite_idx++) {
    if (m_extra_debug) {
      ImGui::Text("Sprite: %d", sprite_idx);
    }
    SpriteHud2DPacket packet;
    memset(&packet, 0, sizeof(packet));
    //  L7 (prologue, and early abort)
    //  ilw.y vi08, 1(vi02)        |  nop                          vi08 = matrix
    u32 offset_selector = m_vec_data_2d[sprite_idx].matrix();

    // moved this out of the loop.
    //  lq.xyzw vf25, 900(vi00)    |  nop                          vf25 = cam_mat
    //  lq.xyzw vf26, 901(vi00)    |  nop
    //  lq.xyzw vf27, 902(vi00)    |  nop
    //  lq.xyzw vf28, 903(vi00)    |  nop
    //  lq.xyzw vf30, 904(vi08)    |  nop                          vf30 = hvdf_offset
    // vf30
    Vector4f hvdf_offset = offset_selector == 0 ? m_hud_matrix_data.hvdf_offset
                                                : m_hud_matrix_data.user_hvdf[offset_selector - 1];

    //  lqi.xyzw vf01, vi02        |  nop
    Vector4f pos_vf01 = m_vec_data_2d[sprite_idx].xyz_sx;
    if (m_extra_debug) {
      imgui_vec(pos_vf01, "POS", 2);
    }
    //  lqi.xyzw vf05, vi02        |  nop
    Vector4f flags_vf05 = m_vec_data_2d[sprite_idx].flag_rot_sy;
    //  lqi.xyzw vf11, vi02        |  nop
    Vector4f color_vf11 = m_vec_data_2d[sprite_idx].rgba;

    // multiplications from the right column
    Vector4f transformed_pos_vf02 = matrix_transform(camera_matrix, pos_vf01);

    Vector4f scales_vf01 = pos_vf01;  // now used for something else.
    //  lq.xyzw vf12, 1020(vi00)   |  mulaw.xyzw ACC, vf28, vf00
    // vf12 is fog consts
    Vector4f fog_consts_vf12(m_frame_data.fog_min, m_frame_data.fog_max, m_frame_data.max_scale,
                             m_frame_data.bonus);
    //  ilw.y vi08, 1(vi02)        |  maddax.xyzw ACC, vf25, vf01
    // load offset selector for the next round.
    //  nop                        |  madday.xyzw ACC, vf26, vf01
    //  nop                        |  maddz.xyzw vf02, vf27, vf01

    //  move.w vf05, vf00          |  addw.z vf01, vf00, vf05
    // scales_vf01.z = sy
    scales_vf01.z() = flags_vf05.w();  // start building the scale vector
    flags_vf05.w() = 1.f;              // what are we building in flags right now??

    //  nop                        |  nop
    //  div Q, vf31.x, vf02.w      |  muly.z vf05, vf05, vf31
    float Q = m_frame_data.pfog0 / transformed_pos_vf02.w();
    flags_vf05.z() *= m_frame_data.deg_to_rad;
    //  nop                        |  mul.xyzw vf03, vf02, vf29
    Vector4f scaled_pos_vf03 = transformed_pos_vf02.elementwise_multiply(m_frame_data.hmge_scale);
    //  nop                        |  nop
    //  nop                        |  nop
    //  nop                        |  mulz.z vf04, vf05, vf05 (ts)
    // fmt::print("rot is {} degrees\n", flags_vf05.z() * 360.0 / (2.0 * M_PI));

    //  the load is for rotation stuff,
    //  lq.xyzw vf14, 1001(vi00)   |  clipw.xyz vf03, vf03      (used for fcand)
    //  iaddi vi06, vi00, 0x1      |  adda.xyzw ACC, vf11, vf11 (used for fmand)

    // upcoming fcand with 0x3f, that checks all of them.
    bool fcand_result = clip_xyz_plus_minus(scaled_pos_vf03);
    bool fmand_result = color_vf11.w() == 0;  // (really w+w, but I don't think it matters?)

    //  L8:
    //  xgkick double buffer setup
    //  ior vi05, vi15, vi00       |  mul.zw vf01, vf01, Q
    scales_vf01.z() *= Q;  // sy
    scales_vf01.w() *= Q;  // sx

    //  lq.xyzw vf06, 998(vi00)    |  mulz.xyzw vf15, vf05, vf04 (ts)
    auto adgif_vf06 = m_frame_data.adgif_giftag;

    //  lq.xyzw vf14, 1002(vi00) ts|  mula.xyzw ACC, vf05, vf14 (ts)

    //  fmand vi01, vi06           |  mul.xyz vf02, vf02, Q
    transformed_pos_vf02.x() *= Q;
    transformed_pos_vf02.y() *= Q;
    transformed_pos_vf02.z() *= Q;

    //    if (m_extra_debug) {
    //      imgui_vec(transformed_pos_vf02, "scaled xf");
    //    }

    //  ibne vi00, vi01, L10       |  addz.x vf01, vf00, vf01
    scales_vf01.x() = scales_vf01.z();  // = sy
    if (fmand_result) {
      if (m_extra_debug) {
        ImGui::TextColored(ImVec4(0.8, 0.2, 0.2, 1.0), "fmand reject");
        ImGui::Separator();
      }
      continue;  // reject!
    }

    //  lqi.xyzw vf07, vi03        |  mulz.xyzw vf16, vf15, vf04 (ts)
    // vf07 is first use adgif

    //  lq.xyzw vf14, 1003(vi00)   |  madda.xyzw ACC, vf15, vf14 (ts both)

    //  lqi.xyzw vf08, vi03        |  add.xyzw vf10, vf02, vf30
    // vf08 is second user adgif
    Vector4f offset_pos_vf10 = transformed_pos_vf02 + hvdf_offset;
    //    if (m_extra_debug) {
    //      ImGui::Text("sel %d", offset_selector);
    //      //ImGui::Text("hvdf off z: %f tf/w z: %f", hvdf_offset.z(), transformed_pos_vf02.z());
    //      imgui_vec(hvdf_offset, "hvdf");
    //      imgui_vec(transformed_pos_vf02, "tf'd");
    //    }

    //  lqi.xyzw vf09, vi03        |  mulw.x vf01, vf01, vf01
    // vf09 is third user adgif
    scales_vf01.x() *= scales_vf01.w();  // x = sx * sy

    //  sqi.xyzw vf06, vi05        |  mulz.xyzw vf15, vf16, vf04 (ts)
    // FIRST ADGIF IS adgif_vf06
    packet.adgif_giftag = adgif_vf06;

    //  lq.xyzw vf14, 1004(vi00)   |  madda.xyzw ACC, vf16, vf14 (ts both)

    //  sqi.xyzw vf07, vi05        |  maxx.w vf10, vf10, vf12
    // SECOND ADGIF is first user
    // just do all 5 now.
    packet.user_adgif = m_adgif[sprite_idx];

    offset_pos_vf10.w() = std::max(offset_pos_vf10.w(), m_frame_data.fog_max);

    //  sqi.xyzw vf08, vi05        |  maxz.zw vf01, vf01, vf31
    // THIRD ADGIF is second user
    scales_vf01.z() = std::max(scales_vf01.z(), m_frame_data.min_scale);
    scales_vf01.w() = std::max(scales_vf01.w(), m_frame_data.min_scale);

    //  sqi.xyzw vf09, vi05        |  mulz.xyzw vf16, vf15, vf04 (ts)
    // FOURTH ADGIF is third user

    //  lq.xyzw vf14, 1005(vi00)   |  madda.xyzw ACC, vf15, vf14 (ts both)

    //  lqi.xyzw vf06, vi03        |  mulw.x vf01, vf01, vf31
    // vf06 is fourth user adgif
    scales_vf01.x() *= m_frame_data.inv_area;  // x = sx * sy * inv_area (area ratio)

    //  lqi.xyzw vf07, vi03        |  miniy.w vf10, vf10, vf12
    // vf07 is fifth user adgif
    offset_pos_vf10.w() = std::min(offset_pos_vf10.w(), m_frame_data.fog_min);

    //  lq.xyzw vf08, 1000(vi00)   |  nop
    // vf08 is 2d giftag 2

    //  ilw.x vi07, -2(vi02)       |  madd.xyzw vf05, vf16, vf14
    auto flag_vi07 = m_vec_data_2d[sprite_idx].flag();
    Vector4f vf05_sincos(0, 0, std::sin(flags_vf05.z()), std::cos(flags_vf05.z()));

    //  lq.xyzw vf30, 904(vi08)    |  nop
    // pipline

    //  lqi.xyzw vf23, vi02        |  miniw.x vf01, vf01, vf00
    // pipeline
    scales_vf01.x() = std::min(scales_vf01.x(), 1.f);

    //  lqi.xyzw vf24, vi02        |  mulx.w vf11, vf11, vf01
    // pipeline
    color_vf11.w() *= scales_vf01.x();  // is this right? doesn't this stall??

    //  fcand vi01, 0x3f           |  mulaw.xyzw ACC, vf28, vf00
    // already computed                 pipeline

    //  lq.xyzw vf17, 1006(vi00)   |  maddax.xyzw ACC, vf25, vf23 (pipeline)
    Vector4f basis_x_vf17 = m_frame_data.basis_x;

    //  lq.xyzw vf18, 1007(vi00)   |  madday.xyzw ACC, vf26, vf23 (pipeline)
    Vector4f basis_y_vf18 = m_frame_data.basis_y;

    assert(flag_vi07 == 0);
    Vector4f* xy_array = m_frame_data.xy_array + flag_vi07;
    //  lq.xyzw vf19, 980(vi07)    |  ftoi0.xyzw vf11, vf11
    Vector4f xy0_vf19 = xy_array[0];
    math::Vector<s32, 4> color_integer_vf11 = color_vf11.cast<s32>();

    //  lq.xyzw vf20, 981(vi07)    |  maddz.xyzw vf02, vf27, vf23 (pipeline)
    Vector4f xy1_vf20 = xy_array[1];

    //  lq.xyzw vf21, 982(vi07)    |  mulaw.xyzw ACC, vf17, vf05
    Vector4f xy2_vf21 = xy_array[2];
    Vector4f acc = basis_x_vf17 * vf05_sincos.w();

    //  lq.xyzw vf22, 983(vi07)    |  msubz.xyzw vf12, vf18, vf05
    Vector4f xy3_vf22 = xy_array[3];
    Vector4f vf12_rotated = acc - (basis_y_vf18 * vf05_sincos.z());
    //  sq.xyzw vf11, 3(vi05)      |  mulaz.xyzw ACC, vf17, vf05
    // EIGHTH is color integer
    packet.color = color_integer_vf11;

    acc = basis_x_vf17 * vf05_sincos.z();

    //  lqi.xyzw vf11, vi02        |  maddw.xyzw vf13, vf18, vf05
    //  (pipeline)
    Vector4f vf13_rotated_trans = acc + basis_y_vf18 * vf05_sincos.w();

    //  move.w vf24, vf00          |  addw.z vf23, vf00, vf24 (pipeline both)

    //  div Q, vf31.x, vf02.w      |  mulw.xyzw vf12, vf12, vf01
    //  (pipeline)
    vf12_rotated *= scales_vf01.w();

    //  ibne vi00, vi01, L9        |  muly.z vf24, vf24, vf31 (pipeline)
    if (fcand_result) {
      if (m_extra_debug) {
        ImGui::TextColored(ImVec4(0.8, 0.2, 0.2, 1.0), "fcand reject");
        ImGui::Separator();
      }
      continue;  // reject (could move earlier)
    }

    //  ilw.y vi08, 1(vi02)        |  mulz.xyzw vf13, vf13, vf01
    //  (pipeline)
    vf13_rotated_trans *= scales_vf01.z();

    // LEFT OFF HERE!

    //  sqi.xyzw vf06, vi05        |  mul.xyzw vf03, vf02, vf29
    // FIFTH is fourth user

    //  sqi.xyzw vf07, vi05        |  mulaw.xyzw ACC, vf10, vf00
    // SIXTH is fifth user
    acc = offset_pos_vf10;

    //  sqi.xyzw vf08, vi05        |  maddax.xyzw ACC, vf12, vf19
    // SEVENTH is giftag2
    packet.sprite_giftag = m_frame_data.sprite_2d_giftag2;
    acc += vf12_rotated * xy0_vf19.x();

    //  lq.xyzw vf06, 988(vi00)    |  maddy.xyzw vf19, vf13, vf19
    Vector4f st0_vf06 = m_frame_data.st_array[0];
    xy0_vf19 = acc + vf13_rotated_trans * xy0_vf19.y();

    //  lq.xyzw vf07, 989(vi00)    |  mulaw.xyzw ACC, vf10, vf00
    Vector4f st1_vf07 = m_frame_data.st_array[1];
    acc = offset_pos_vf10;

    //  lq.xyzw vf08, 990(vi00)    |  maddax.xyzw ACC, vf12, vf20
    Vector4f st2_vf08 = m_frame_data.st_array[2];
    acc += vf12_rotated * xy1_vf20.x();

    //  lq.xyzw vf09, 991(vi00)    |  maddy.xyzw vf20, vf13, vf20
    Vector4f st3_vf09 = m_frame_data.st_array[3];
    xy1_vf20 = acc + vf13_rotated_trans * xy1_vf20.y();

    //  sq.xyzw vf06, 1(vi05)      |  mulaw.xyzw ACC, vf10, vf00
    // NINTH is st0
    packet.st0 = st0_vf06;
    acc = offset_pos_vf10;

    //  sq.xyzw vf07, 3(vi05)      |  maddax.xyzw ACC, vf12, vf21
    // ELEVEN is st1
    packet.st1 = st1_vf07;
    acc += vf12_rotated * xy2_vf21.x();

    //  sq.xyzw vf08, 5(vi05)      |  maddy.xyzw vf21, vf13, vf21
    // THIRTEEN is st2
    packet.st2 = st2_vf08;
    xy2_vf21 = acc + vf13_rotated_trans * xy2_vf21.y();

    //  sq.xyzw vf09, 7(vi05)      |  mulaw.xyzw ACC, vf10, vf00
    // FIFTEEN is st3
    packet.st3 = st3_vf09;
    acc = offset_pos_vf10;

    //  nop                        |  maddax.xyzw ACC, vf12, vf22
    acc += vf12_rotated * xy3_vf22.x();

    //  nop                        |  maddy.xyzw vf22, vf13, vf22
    xy3_vf22 = acc + vf13_rotated_trans * xy3_vf22.y();

    //  lq.xyzw vf12, 1020(vi00)   |  ftoi4.xyzw vf19, vf19
    //  (pipeline)
    auto xy0_vf19_int = (xy0_vf19 * 16.f).cast<s32>();

    //  lq.xyzw vf14, 1001(vi00)   |  ftoi4.xyzw vf20, vf20
    //  (pipeline)
    auto xy1_vf20_int = (xy1_vf20 * 16.f).cast<s32>();

    //  move.xyzw vf05, vf24       |  ftoi4.xyzw vf21, vf21
    // (pipeline)
    auto xy2_vf21_int = (xy2_vf21 * 16.f).cast<s32>();

    //  move.xyzw vf01, vf23       |  ftoi4.xyzw vf22, vf22
    //  (pipeline)
    auto xy3_vf22_int = (xy3_vf22 * 16.f).cast<s32>();

    if (m_extra_debug) {
      u32 zi = xy3_vf22_int.z() >> 4;
      ImGui::Text("z (int): 0x%08x %s", zi, zi >= (1 << 24) ? "bad" : "");
      ImGui::Text("z (flt): %f", (double)(((u32)zi) << 8) / UINT32_MAX);
    }

    //  sq.xyzw vf19, 2(vi05)      |  mulz.z vf04, vf24, vf24 (pipeline)
    // TENTH is xy0int
    packet.xy0 = xy0_vf19_int;
    //  sq.xyzw vf20, 4(vi05)      |  clipw.xyz vf03, vf03    (pipeline)
    // TWELVE is xy1int
    packet.xy1 = xy1_vf20_int;
    //  sq.xyzw vf21, 6(vi05)      |  nop
    // FOURTEEN is xy2int
    packet.xy2 = xy2_vf21_int;
    //  sq.xyzw vf22, 8(vi05)      |  nop
    // SIXTEEN is xy3int
    packet.xy3 = xy3_vf22_int;

    m_sprite_renderer.render_gif((const u8*)&packet, sizeof(packet), render_state, prof);
    if (m_extra_debug) {
      imgui_vec(vf12_rotated, "vf12", 2);
      imgui_vec(vf13_rotated_trans, "vf13", 2);
      ImGui::Separator();
    }

    //  xgkick vi15                |  nop
    //  iaddi vi04, vi04, -0x1     |  nop
    //  iaddiu vi01, vi00, 0x672   |  nop
    //  ibne vi00, vi04, L8        |  nop
    //  isub vi15, vi01, vi15      |  adda.xyzw ACC, vf11, vf11
    //  nop                        |  nop :e
    //  nop                        |  nop
    //  L9:
    //  iaddi vi04, vi04, -0x1     |  nop
    //  iaddi vi02, vi02, -0x3     |  nop
    //  ibne vi00, vi04, L7        |  nop
    //  nop                        |  nop
    //  nop                        |  nop :e
    //  nop                        |  nop
    //  L10:
    //  iaddi vi04, vi04, -0x1     |  nop
    //  iaddi vi03, vi03, 0x4      |  nop
    //  ibne vi00, vi04, L7        |  nop
    //  nop                        |  nop
    //  nop                        |  nop :e
    //  nop                        |  nop
  }

  if (m_extra_debug) {
    ImGui::End();
  }
}

std::array<math::Vector3f, 3> sprite_quat_to_rot(float qi, float qj, float qk) {
  std::array<math::Vector3f, 3> result;
  float qr = std::sqrt(std::abs(1.f - (qi * qi + qj * qj + qk * qk)));
  // fmt::print("q: {} {} {} {}\n", qi, qj, qk, qr);
  result[0][0] = 1.f - 2.f * (qj * qj + qk * qk);
  result[1][0] = 2.f * (qi * qj - qk * qr);
  result[2][0] = 2.f * (qi * qk + qj * qr);
  result[0][1] = 2.f * (qi * qj + qk * qr);
  result[1][1] = 1.f - 2.f * (qi * qi + qk * qk);
  result[2][1] = 2.f * (qj * qk - qi * qr);
  result[0][2] = 2.f * (qi * qk - qj * qr);
  result[1][2] = 2.f * (qj * qk + qi * qr);
  result[2][2] = 1.f - 2.f * (qi * qi + qj * qj);
  return result;
}

Vector4f sprite_transform2(const Vector4f& root,
                           const Vector4f& off,
                           const Matrix4f& cam,
                           const std::array<math::Vector3f, 3>& sprite_rot,
                           float sx,
                           float sy,
                           const Vector4f& hvdf_off,
                           float pfog0,
                           float fog_min,
                           float fog_max) {
  Vector4f pos = root;
  // fmt::print("root   : {}\n", root.to_string_aligned());
  // fmt::print("off    : {} s {} {}\n", off.to_string_aligned(), sx, sy);

  math::Vector3f offset =
      sprite_rot[0] * off.x() * sx + sprite_rot[1] * off.y() + sprite_rot[2] * off.z() * sy;
  // fmt::print("off (r): {}\n", offset.to_string_aligned());

  pos.x() += offset.x();
  pos.y() += offset.y();
  pos.z() += offset.z();
  Vector4f transformed_pos = matrix_transform(cam, pos);
  float Q = pfog0 / transformed_pos.w();
  transformed_pos.x() *= Q;
  transformed_pos.y() *= Q;
  transformed_pos.z() *= Q;
  Vector4f offset_pos = transformed_pos + hvdf_off;
  offset_pos.w() = std::max(offset_pos.w(), fog_max);
  offset_pos.w() = std::min(offset_pos.w(), fog_min);

  return offset_pos;
}

void SpriteRenderer::do_3d_block_cpu(u32 count,
                                     SharedRenderState* render_state,
                                     ScopedProfilerNode& prof) {
  Matrix4f camera_matrix = m_3d_matrix_data.camera;  // vf25, vf26, vf27, vf28
  for (u32 sprite_idx = 0; sprite_idx < count; sprite_idx++) {
    SpriteHud2DPacket packet;
    memset(&packet, 0, sizeof(packet));
    //  ilw.y vi08, 1(vi02)        |  nop                          vi08 = matrix
    u32 offset_selector = m_vec_data_2d[sprite_idx].matrix();
    // assert(offset_selector == 0 || offset_selector == 1);
    // moved this out of the loop.
    //  lq.xyzw vf25, 900(vi00)    |  nop                          vf25 = cam_mat
    //  lq.xyzw vf26, 901(vi00)    |  nop
    //  lq.xyzw vf27, 902(vi00)    |  nop
    //  lq.xyzw vf28, 903(vi00)    |  nop
    //  lq.xyzw vf30, 904(vi00)    |  nop                          vf30 = hvdf_offset
    // vf30
    Vector4f hvdf_offset = m_3d_matrix_data.hvdf_offset;

    //  lqi.xyzw vf01, vi02        |  nop
    Vector4f pos_vf01 = m_vec_data_2d[sprite_idx].xyz_sx;
    //  lqi.xyzw vf05, vi02        |  nop
    Vector4f flags_vf05 = m_vec_data_2d[sprite_idx].flag_rot_sy;
    //  lqi.xyzw vf11, vi02        |  nop
    Vector4f color_vf11 = m_vec_data_2d[sprite_idx].rgba;

    // multiplications from the right column
    Vector4f transformed_pos_vf02 = matrix_transform(camera_matrix, pos_vf01);

    Vector4f scales_vf01 = pos_vf01;  // now used for something else.
    //  lq.xyzw vf12, 1020(vi00)   |  mulaw.xyzw ACC, vf28, vf00
    // vf12 is fog consts
    Vector4f fog_consts_vf12(m_frame_data.fog_min, m_frame_data.fog_max, m_frame_data.max_scale,
                             m_frame_data.bonus);
    //  ilw.y vi08, 1(vi02)        |  maddax.xyzw ACC, vf25, vf01
    // load offset selector for the next round.
    //  nop                        |  madday.xyzw ACC, vf26, vf01
    //  nop                        |  maddz.xyzw vf02, vf27, vf01

    //  move.w vf05, vf00          |  addw.z vf01, vf00, vf05
    // scales_vf01.z = sy
    scales_vf01.z() = flags_vf05.w();  // start building the scale vector
    flags_vf05.w() = 1.f;              // what are we building in flags right now??

    //  nop                        |  nop
    //  div Q, vf31.x, vf02.w      |  muly.z vf05, vf05, vf31
    float Q = m_frame_data.pfog0 / transformed_pos_vf02.w();
    flags_vf05.z() *= m_frame_data.deg_to_rad;
    //  nop                        |  mul.xyzw vf03, vf02, vf29
    Vector4f scaled_pos_vf03 = transformed_pos_vf02.elementwise_multiply(m_frame_data.hmge_scale);
    //  nop                        |  nop
    //  nop                        |  nop
    //  nop                        |  mulz.z vf04, vf05, vf05 (ts)
    // fmt::print("rot is {} degrees\n", flags_vf05.z() * 360.0 / (2.0 * M_PI));

    //  the load is for rotation stuff,
    //  lq.xyzw vf14, 1001(vi00)   |  clipw.xyz vf03, vf03      (used for fcand)
    //  iaddi vi06, vi00, 0x1      |  adda.xyzw ACC, vf11, vf11 (used for fmand)

    // upcoming fcand with 0x3f, that checks all of them.
    bool fcand_result = clip_xyz_plus_minus(scaled_pos_vf03);
    bool fmand_result = color_vf11.w() == 0;  // (really w+w, but I don't think it matters?)

    //  L8:
    //  xgkick double buffer setup
    //  ior vi05, vi15, vi00       |  mul.zw vf01, vf01, Q
    scales_vf01.z() *= Q;  // sy
    scales_vf01.w() *= Q;  // sx

    //  lq.xyzw vf06, 998(vi00)    |  mulz.xyzw vf15, vf05, vf04 (ts)
    auto adgif_vf06 = m_frame_data.adgif_giftag;

    //  lq.xyzw vf14, 1002(vi00) ts|  mula.xyzw ACC, vf05, vf14 (ts)

    //  fmand vi01, vi06           |  mul.xyz vf02, vf02, Q
    transformed_pos_vf02.x() *= Q;
    transformed_pos_vf02.y() *= Q;
    transformed_pos_vf02.z() *= Q;

    //  ibne vi00, vi01, L10       |  addz.x vf01, vf00, vf01
    scales_vf01.x() = scales_vf01.z();  // = sy
    if (fmand_result) {
      continue;  // reject!
    }

    //  lqi.xyzw vf07, vi03        |  mulz.xyzw vf16, vf15, vf04 (ts)
    // vf07 is first use adgif

    //  lq.xyzw vf14, 1003(vi00)   |  madda.xyzw ACC, vf15, vf14 (ts both)

    //  lqi.xyzw vf08, vi03        |  add.xyzw vf10, vf02, vf30
    // vf08 is second user adgif
    Vector4f offset_pos_vf10 = transformed_pos_vf02 + hvdf_offset;
    //    if (m_extra_debug) {
    //      ImGui::Text("sel %d", offset_selector);
    //      //ImGui::Text("hvdf off z: %f tf/w z: %f", hvdf_offset.z(), transformed_pos_vf02.z());
    //      imgui_vec(hvdf_offset, "hvdf");
    //      imgui_vec(transformed_pos_vf02, "tf'd");
    //    }

    //  lqi.xyzw vf09, vi03        |  mulw.x vf01, vf01, vf01
    // vf09 is third user adgif
    scales_vf01.x() *= scales_vf01.w();  // x = sx * sy

    //  sqi.xyzw vf06, vi05        |  mulz.xyzw vf15, vf16, vf04 (ts)
    // FIRST ADGIF IS adgif_vf06
    packet.adgif_giftag = adgif_vf06;

    // just do all 5 now.
    packet.user_adgif = m_adgif[sprite_idx];

    offset_pos_vf10.w() = std::max(offset_pos_vf10.w(), m_frame_data.fog_max);

    scales_vf01.z() = std::max(scales_vf01.z(), m_frame_data.min_scale);
    scales_vf01.w() = std::max(scales_vf01.w(), m_frame_data.min_scale);

    scales_vf01.x() *= m_frame_data.inv_area;  // x = sx * sy * inv_area (area ratio)

    offset_pos_vf10.w() = std::min(offset_pos_vf10.w(), m_frame_data.fog_min);

    scales_vf01.z() = std::min(scales_vf01.z(), fog_consts_vf12.z());
    scales_vf01.w() = std::min(scales_vf01.w(), fog_consts_vf12.z());
    bool use_first_giftag = offset_selector == 0;

    auto flag_vi07 = m_vec_data_2d[sprite_idx].flag();

    scales_vf01.x() = std::min(scales_vf01.x(), 1.f);

    transformed_pos_vf02.w() = offset_pos_vf10.w() - fog_consts_vf12.y();

    color_vf11.w() *= scales_vf01.x();  // is this right? doesn't this stall??

    //    ibne vi00, vi09, L6        |  nop
    if (transformed_pos_vf02.w() != 0) {
      use_first_giftag = false;
    }

    flag_vi07 = 0;  // todo hack
    Vector4f* xy_array = m_frame_data.xyz_array + flag_vi07;
    math::Vector<s32, 4> color_integer_vf11 = color_vf11.cast<s32>();

    packet.color = color_integer_vf11;

    if (fcand_result) {
      continue;  // reject (could move earlier)
    }

    Vector4f transformed[4];

    flags_vf05 = m_vec_data_2d[sprite_idx].flag_rot_sy;
    // do rot
    auto rot = sprite_quat_to_rot(flags_vf05.x(), flags_vf05.y(), flags_vf05.z());
    //    fmt::print("root: {}\n", offset_pos_vf10.to_string_aligned());

    // for (int i = 0; i < 3; i++) {
    //  fmt::print("M{}: {}\n", i, rot[i].to_string_aligned());
    // }
    for (int i = 0; i < 4; i++) {
      transformed[i] =
          sprite_transform2(m_vec_data_2d[sprite_idx].xyz_sx, xy_array[i], camera_matrix, rot,
                            m_vec_data_2d[sprite_idx].sx(), m_vec_data_2d[sprite_idx].sy(),
                            m_3d_matrix_data.hvdf_offset, m_frame_data.pfog0, m_frame_data.fog_min,
                            m_frame_data.fog_max);
    }
    Vector4f xy0_vf19 = transformed[0];
    Vector4f xy1_vf20 = transformed[1];
    Vector4f xy2_vf21 = transformed[2];
    Vector4f xy3_vf22 = transformed[3];

    packet.sprite_giftag =
        use_first_giftag ? m_frame_data.sprite_2d_giftag : m_frame_data.sprite_2d_giftag2;

    Vector4f st0_vf06 = m_frame_data.st_array[0];
    Vector4f st1_vf07 = m_frame_data.st_array[1];
    Vector4f st2_vf08 = m_frame_data.st_array[2];
    Vector4f st3_vf09 = m_frame_data.st_array[3];

    packet.st0 = st0_vf06;
    packet.st1 = st1_vf07;
    packet.st2 = st2_vf08;
    packet.st3 = st3_vf09;

    auto xy0_vf19_int = (xy0_vf19 * 16.f).cast<s32>();
    auto xy1_vf20_int = (xy1_vf20 * 16.f).cast<s32>();
    auto xy2_vf21_int = (xy2_vf21 * 16.f).cast<s32>();
    auto xy3_vf22_int = (xy3_vf22 * 16.f).cast<s32>();

    packet.xy0 = xy0_vf19_int;
    packet.xy1 = xy1_vf20_int;
    packet.xy2 = xy2_vf21_int;
    packet.xy3 = xy3_vf22_int;

    m_sprite_renderer.render_gif((const u8*)&packet, sizeof(packet), render_state, prof);
  }
}

void SpriteRenderer::do_2d_group0_block_cpu(u32 count,
                                            SharedRenderState* render_state,
                                            ScopedProfilerNode& prof) {
  if (m_extra_debug) {
    ImGui::Begin("Sprite Extra Debug 2d_0");
  }

  Matrix4f camera_matrix = m_3d_matrix_data.camera;  // vf25, vf26, vf27, vf28
  for (u32 sprite_idx = 0; sprite_idx < count; sprite_idx++) {
    if (m_extra_debug) {
      ImGui::Text("Sprite: %d", sprite_idx);
    }
    SpriteHud2DPacket packet;
    memset(&packet, 0, sizeof(packet));
    //  ilw.y vi08, 1(vi02)        |  nop                          vi08 = matrix
    u32 offset_selector = m_vec_data_2d[sprite_idx].matrix();
    // assert(offset_selector == 0 || offset_selector == 1);
    // moved this out of the loop.
    //  lq.xyzw vf25, 900(vi00)    |  nop                          vf25 = cam_mat
    //  lq.xyzw vf26, 901(vi00)    |  nop
    //  lq.xyzw vf27, 902(vi00)    |  nop
    //  lq.xyzw vf28, 903(vi00)    |  nop
    //  lq.xyzw vf30, 904(vi00)    |  nop                          vf30 = hvdf_offset
    // vf30
    Vector4f hvdf_offset = m_3d_matrix_data.hvdf_offset;

    //  lqi.xyzw vf01, vi02        |  nop
    Vector4f pos_vf01 = m_vec_data_2d[sprite_idx].xyz_sx;
    if (m_extra_debug) {
      imgui_vec(pos_vf01, "POS", 2);
    }
    //  lqi.xyzw vf05, vi02        |  nop
    Vector4f flags_vf05 = m_vec_data_2d[sprite_idx].flag_rot_sy;
    //  lqi.xyzw vf11, vi02        |  nop
    Vector4f color_vf11 = m_vec_data_2d[sprite_idx].rgba;

    // multiplications from the right column
    Vector4f transformed_pos_vf02 = matrix_transform(camera_matrix, pos_vf01);

    Vector4f scales_vf01 = pos_vf01;  // now used for something else.
    //  lq.xyzw vf12, 1020(vi00)   |  mulaw.xyzw ACC, vf28, vf00
    // vf12 is fog consts
    Vector4f fog_consts_vf12(m_frame_data.fog_min, m_frame_data.fog_max, m_frame_data.max_scale,
                             m_frame_data.bonus);
    //  ilw.y vi08, 1(vi02)        |  maddax.xyzw ACC, vf25, vf01
    // load offset selector for the next round.
    //  nop                        |  madday.xyzw ACC, vf26, vf01
    //  nop                        |  maddz.xyzw vf02, vf27, vf01

    //  move.w vf05, vf00          |  addw.z vf01, vf00, vf05
    // scales_vf01.z = sy
    scales_vf01.z() = flags_vf05.w();  // start building the scale vector
    flags_vf05.w() = 1.f;              // what are we building in flags right now??

    //  nop                        |  nop
    //  div Q, vf31.x, vf02.w      |  muly.z vf05, vf05, vf31
    float Q = m_frame_data.pfog0 / transformed_pos_vf02.w();
    flags_vf05.z() *= m_frame_data.deg_to_rad;
    //  nop                        |  mul.xyzw vf03, vf02, vf29
    Vector4f scaled_pos_vf03 = transformed_pos_vf02.elementwise_multiply(m_frame_data.hmge_scale);
    //  nop                        |  nop
    //  nop                        |  nop
    //  nop                        |  mulz.z vf04, vf05, vf05 (ts)
    // fmt::print("rot is {} degrees\n", flags_vf05.z() * 360.0 / (2.0 * M_PI));

    //  the load is for rotation stuff,
    //  lq.xyzw vf14, 1001(vi00)   |  clipw.xyz vf03, vf03      (used for fcand)
    //  iaddi vi06, vi00, 0x1      |  adda.xyzw ACC, vf11, vf11 (used for fmand)

    // upcoming fcand with 0x3f, that checks all of them.
    bool fcand_result = clip_xyz_plus_minus(scaled_pos_vf03);
    bool fmand_result = color_vf11.w() == 0;  // (really w+w, but I don't think it matters?)

    //  L8:
    //  xgkick double buffer setup
    //  ior vi05, vi15, vi00       |  mul.zw vf01, vf01, Q
    scales_vf01.z() *= Q;  // sy
    scales_vf01.w() *= Q;  // sx

    //  lq.xyzw vf06, 998(vi00)    |  mulz.xyzw vf15, vf05, vf04 (ts)
    auto adgif_vf06 = m_frame_data.adgif_giftag;

    //  lq.xyzw vf14, 1002(vi00) ts|  mula.xyzw ACC, vf05, vf14 (ts)

    //  fmand vi01, vi06           |  mul.xyz vf02, vf02, Q
    transformed_pos_vf02.x() *= Q;
    transformed_pos_vf02.y() *= Q;
    transformed_pos_vf02.z() *= Q;

    //    if (m_extra_debug) {
    //      imgui_vec(transformed_pos_vf02, "scaled xf");
    //    }

    //  ibne vi00, vi01, L10       |  addz.x vf01, vf00, vf01
    scales_vf01.x() = scales_vf01.z();  // = sy
    if (fmand_result) {
      if (m_extra_debug) {
        ImGui::TextColored(ImVec4(0.8, 0.2, 0.2, 1.0), "fmand (1) reject");
        ImGui::Separator();
      }
      continue;  // reject!
    }

    //  lqi.xyzw vf07, vi03        |  mulz.xyzw vf16, vf15, vf04 (ts)
    // vf07 is first use adgif

    //  lq.xyzw vf14, 1003(vi00)   |  madda.xyzw ACC, vf15, vf14 (ts both)

    //  lqi.xyzw vf08, vi03        |  add.xyzw vf10, vf02, vf30
    // vf08 is second user adgif
    Vector4f offset_pos_vf10 = transformed_pos_vf02 + hvdf_offset;
    //    if (m_extra_debug) {
    //      ImGui::Text("sel %d", offset_selector);
    //      //ImGui::Text("hvdf off z: %f tf/w z: %f", hvdf_offset.z(), transformed_pos_vf02.z());
    //      imgui_vec(hvdf_offset, "hvdf");
    //      imgui_vec(transformed_pos_vf02, "tf'd");
    //    }

    //  lqi.xyzw vf09, vi03        |  mulw.x vf01, vf01, vf01
    // vf09 is third user adgif
    scales_vf01.x() *= scales_vf01.w();  // x = sx * sy

    //  sqi.xyzw vf06, vi05        |  mulz.xyzw vf15, vf16, vf04 (ts)
    // FIRST ADGIF IS adgif_vf06
    packet.adgif_giftag = adgif_vf06;

    //  lq.xyzw vf14, 1004(vi00)   |  madda.xyzw ACC, vf16, vf14 (ts both)

    //  sqi.xyzw vf07, vi05        |  maxx.w vf10, vf10, vf12
    // SECOND ADGIF is first user
    // just do all 5 now.
    packet.user_adgif = m_adgif[sprite_idx];

    offset_pos_vf10.w() = std::max(offset_pos_vf10.w(), m_frame_data.fog_max);

    //  sqi.xyzw vf08, vi05        |  maxz.zw vf01, vf01, vf31
    // THIRD ADGIF is second user
    scales_vf01.z() = std::max(scales_vf01.z(), m_frame_data.min_scale);
    scales_vf01.w() = std::max(scales_vf01.w(), m_frame_data.min_scale);

    //  sqi.xyzw vf09, vi05        |  mulz.xyzw vf16, vf15, vf04 (ts)
    // FOURTH ADGIF is third user

    //  lq.xyzw vf14, 1005(vi00)   |  madda.xyzw ACC, vf15, vf14 (ts both)

    //  lqi.xyzw vf06, vi03        |  mulw.x vf01, vf01, vf31
    // vf06 is fourth user adgif
    scales_vf01.x() *= m_frame_data.inv_area;  // x = sx * sy * inv_area (area ratio)

    //  lqi.xyzw vf07, vi03        |  miniy.w vf10, vf10, vf12
    // vf07 is fifth user adgif
    offset_pos_vf10.w() = std::min(offset_pos_vf10.w(), m_frame_data.fog_min);

    //  lq.xyzw vf08, 999(vi00)   |  miniz.zw vf01, vf01, vf12
    // vf08 is 2d giftag 1 (NOTE THIS IS DIFFERENT FROM 2d 1)!!!!!
    scales_vf01.z() = std::min(scales_vf01.z(), fog_consts_vf12.z());
    scales_vf01.w() = std::min(scales_vf01.w(), fog_consts_vf12.z());
    bool use_first_giftag = offset_selector == 0;

    //  ilw.x vi07, -2(vi02)       |  madd.xyzw vf05, vf16, vf14
    auto flag_vi07 = m_vec_data_2d[sprite_idx].flag();
    Vector4f vf05_sincos(0, 0, std::sin(flags_vf05.z()), std::cos(flags_vf05.z()));

    //  lqi.xyzw vf23, vi02        |  miniw.x vf01, vf01, vf00
    // pipeline
    scales_vf01.x() = std::min(scales_vf01.x(), 1.f);

    //   nop                        |  suby.w vf02, vf10, vf12 (unique)
    transformed_pos_vf02.w() = offset_pos_vf10.w() - fog_consts_vf12.y();

    //  lqi.xyzw vf24, vi02        |  mulx.w vf11, vf11, vf01
    // pipeline
    color_vf11.w() *= scales_vf01.x();  // is this right? doesn't this stall??

    //  fcand vi01, 0x3f           |  mulaw.xyzw ACC, vf28, vf00
    // already computed                 pipeline

    //  lq.xyzw vf17, 1006(vi00)   |  maddax.xyzw ACC, vf25, vf23 (pipeline)
    Vector4f basis_x_vf17 = m_frame_data.basis_x;

    //    fmand vi09, vi06           |  nop
    //    ibne vi00, vi09, L6        |  nop
    if (transformed_pos_vf02.w() != 0) {
      if (m_extra_debug) {
        ImGui::TextColored(ImVec4(0.8, 0.2, 0.2, 1.0), "fmand (2) trick");
      }
      use_first_giftag = false;
    }

    //  lq.xyzw vf18, 1007(vi00)   |  madday.xyzw ACC, vf26, vf23 (pipeline)
    Vector4f basis_y_vf18 = m_frame_data.basis_y;

    // assert(flag_vi07 == 0);
    Vector4f* xy_array = m_frame_data.xy_array + flag_vi07;
    //  lq.xyzw vf19, 980(vi07)    |  ftoi0.xyzw vf11, vf11
    Vector4f xy0_vf19 = xy_array[0];
    math::Vector<s32, 4> color_integer_vf11 = color_vf11.cast<s32>();

    //  lq.xyzw vf20, 981(vi07)    |  maddz.xyzw vf02, vf27, vf23 (pipeline)
    Vector4f xy1_vf20 = xy_array[1];

    //  lq.xyzw vf21, 982(vi07)    |  mulaw.xyzw ACC, vf17, vf05
    Vector4f xy2_vf21 = xy_array[2];
    Vector4f acc = basis_x_vf17 * vf05_sincos.w();

    //  lq.xyzw vf22, 983(vi07)    |  msubz.xyzw vf12, vf18, vf05
    Vector4f xy3_vf22 = xy_array[3];
    Vector4f vf12_rotated = acc - (basis_y_vf18 * vf05_sincos.z());
    //  sq.xyzw vf11, 3(vi05)      |  mulaz.xyzw ACC, vf17, vf05
    // EIGHTH is color integer
    packet.color = color_integer_vf11;

    acc = basis_x_vf17 * vf05_sincos.z();

    //  lqi.xyzw vf11, vi02        |  maddw.xyzw vf13, vf18, vf05
    //  (pipeline)
    Vector4f vf13_rotated_trans = acc + basis_y_vf18 * vf05_sincos.w();

    //  move.w vf24, vf00          |  addw.z vf23, vf00, vf24 (pipeline both)

    //  div Q, vf31.x, vf02.w      |  mulw.xyzw vf12, vf12, vf01
    //  (pipeline)
    vf12_rotated *= scales_vf01.w();

    //  ibne vi00, vi01, L9        |  muly.z vf24, vf24, vf31 (pipeline)
    if (fcand_result) {
      if (m_extra_debug) {
        ImGui::TextColored(ImVec4(0.8, 0.2, 0.2, 1.0), "fcand reject");
        ImGui::Separator();
      }
      continue;  // reject (could move earlier)
    }

    //  ilw.y vi08, 1(vi02)        |  mulz.xyzw vf13, vf13, vf01
    //  (pipeline)
    vf13_rotated_trans *= scales_vf01.z();

    // LEFT OFF HERE!

    //  sqi.xyzw vf06, vi05        |  mul.xyzw vf03, vf02, vf29
    // FIFTH is fourth user

    //  sqi.xyzw vf07, vi05        |  mulaw.xyzw ACC, vf10, vf00
    // SIXTH is fifth user
    acc = offset_pos_vf10;

    //  sqi.xyzw vf08, vi05        |  maddax.xyzw ACC, vf12, vf19
    // SEVENTH is giftag2
    packet.sprite_giftag =
        use_first_giftag ? m_frame_data.sprite_2d_giftag : m_frame_data.sprite_2d_giftag2;
    acc += vf12_rotated * xy0_vf19.x();

    //  lq.xyzw vf06, 988(vi00)    |  maddy.xyzw vf19, vf13, vf19
    Vector4f st0_vf06 = m_frame_data.st_array[0];
    xy0_vf19 = acc + vf13_rotated_trans * xy0_vf19.y();

    //  lq.xyzw vf07, 989(vi00)    |  mulaw.xyzw ACC, vf10, vf00
    Vector4f st1_vf07 = m_frame_data.st_array[1];
    acc = offset_pos_vf10;

    //  lq.xyzw vf08, 990(vi00)    |  maddax.xyzw ACC, vf12, vf20
    Vector4f st2_vf08 = m_frame_data.st_array[2];
    acc += vf12_rotated * xy1_vf20.x();

    //  lq.xyzw vf09, 991(vi00)    |  maddy.xyzw vf20, vf13, vf20
    Vector4f st3_vf09 = m_frame_data.st_array[3];
    xy1_vf20 = acc + vf13_rotated_trans * xy1_vf20.y();

    //  sq.xyzw vf06, 1(vi05)      |  mulaw.xyzw ACC, vf10, vf00
    // NINTH is st0
    packet.st0 = st0_vf06;
    acc = offset_pos_vf10;

    //  sq.xyzw vf07, 3(vi05)      |  maddax.xyzw ACC, vf12, vf21
    // ELEVEN is st1
    packet.st1 = st1_vf07;
    acc += vf12_rotated * xy2_vf21.x();

    //  sq.xyzw vf08, 5(vi05)      |  maddy.xyzw vf21, vf13, vf21
    // THIRTEEN is st2
    packet.st2 = st2_vf08;
    xy2_vf21 = acc + vf13_rotated_trans * xy2_vf21.y();

    //  sq.xyzw vf09, 7(vi05)      |  mulaw.xyzw ACC, vf10, vf00
    // FIFTEEN is st3
    packet.st3 = st3_vf09;
    acc = offset_pos_vf10;

    //  nop                        |  maddax.xyzw ACC, vf12, vf22
    acc += vf12_rotated * xy3_vf22.x();

    //  nop                        |  maddy.xyzw vf22, vf13, vf22
    xy3_vf22 = acc + vf13_rotated_trans * xy3_vf22.y();

    //  lq.xyzw vf12, 1020(vi00)   |  ftoi4.xyzw vf19, vf19
    //  (pipeline)
    auto xy0_vf19_int = (xy0_vf19 * 16.f).cast<s32>();

    //  lq.xyzw vf14, 1001(vi00)   |  ftoi4.xyzw vf20, vf20
    //  (pipeline)
    auto xy1_vf20_int = (xy1_vf20 * 16.f).cast<s32>();

    //  move.xyzw vf05, vf24       |  ftoi4.xyzw vf21, vf21
    // (pipeline)
    auto xy2_vf21_int = (xy2_vf21 * 16.f).cast<s32>();

    //  move.xyzw vf01, vf23       |  ftoi4.xyzw vf22, vf22
    //  (pipeline)
    auto xy3_vf22_int = (xy3_vf22 * 16.f).cast<s32>();

    if (m_extra_debug) {
      u32 zi = xy3_vf22_int.z() >> 4;
      ImGui::Text("z (int): 0x%08x %s", zi, zi >= (1 << 24) ? "bad" : "");
      ImGui::Text("z (flt): %f", (double)(((u32)zi) << 8) / UINT32_MAX);
    }

    //  sq.xyzw vf19, 2(vi05)      |  mulz.z vf04, vf24, vf24 (pipeline)
    // TENTH is xy0int
    packet.xy0 = xy0_vf19_int;
    //  sq.xyzw vf20, 4(vi05)      |  clipw.xyz vf03, vf03    (pipeline)
    // TWELVE is xy1int
    packet.xy1 = xy1_vf20_int;
    //  sq.xyzw vf21, 6(vi05)      |  nop
    // FOURTEEN is xy2int
    packet.xy2 = xy2_vf21_int;
    //  sq.xyzw vf22, 8(vi05)      |  nop
    // SIXTEEN is xy3int
    packet.xy3 = xy3_vf22_int;

    m_sprite_renderer.render_gif((const u8*)&packet, sizeof(packet), render_state, prof);
    if (m_extra_debug) {
      imgui_vec(vf12_rotated, "vf12", 2);
      imgui_vec(vf13_rotated_trans, "vf13", 2);
      ImGui::Separator();
    }

    //  xgkick vi15                |  nop
    //  iaddi vi04, vi04, -0x1     |  nop
    //  iaddiu vi01, vi00, 0x672   |  nop
    //  ibne vi00, vi04, L8        |  nop
    //  isub vi15, vi01, vi15      |  adda.xyzw ACC, vf11, vf11
    //  nop                        |  nop :e
    //  nop                        |  nop
    //  L9:
    //  iaddi vi04, vi04, -0x1     |  nop
    //  iaddi vi02, vi02, -0x3     |  nop
    //  ibne vi00, vi04, L7        |  nop
    //  nop                        |  nop
    //  nop                        |  nop :e
    //  nop                        |  nop
    //  L10:
    //  iaddi vi04, vi04, -0x1     |  nop
    //  iaddi vi03, vi03, 0x4      |  nop
    //  ibne vi00, vi04, L7        |  nop
    //  nop                        |  nop
    //  nop                        |  nop :e
    //  nop                        |  nop
  }

  if (m_extra_debug) {
    ImGui::End();
  }
}
