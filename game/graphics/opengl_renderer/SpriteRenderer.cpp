#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "SpriteRenderer.h"

bool verify_unpack_with_stcycl(const DmaTransfer& transfer,
                               VifCode::Kind unpack_kind,
                               u16 cl,
                               u16 wl,
                               u32 qwc,
                               u32 addr,
                               bool usn,
                               bool flg) {
  if (transfer.size_bytes != qwc * 16) {
    fmt::print("verify_unpack: bad size {} vs {}\n", transfer.size_bytes, qwc * 16);
    return false;
  }

  if (transfer.vifcode0().kind != VifCode::Kind::STCYCL) {
    fmt::print("verify_unpack: bad vifcode 0\n");
    return false;
  }

  if (transfer.vifcode1().kind != unpack_kind) {
    fmt::print("verify_unpack: bad vifcode 1\n");
    return false;
  }

  VifCodeStcycl stcycl(transfer.vifcode0());
  VifCodeUnpack unpack(transfer.vifcode1());

  if (stcycl.cl != cl || stcycl.wl != wl) {
    fmt::print("verify_unpack: bad cl/wl {}/{} vs {}/{}\n", stcycl.cl, stcycl.wl, cl, wl);
    return false;
  }

  if (unpack.addr_qw != addr || unpack.use_tops_flag != flg || unpack.is_unsigned != usn) {
    fmt::print("verify_unpack: bad unpack {}/{}/{} vs {}/{}/{}", unpack.addr_qw,
               unpack.use_tops_flag, unpack.is_unsigned, addr, flg, usn);
    return false;
  }

  if (transfer.vifcode1().num != qwc) {
    fmt::print("verify_unpack: bad num {} vs {}\n", transfer.vifcode1().num, qwc);
    return false;
  }

  return true;
}

bool verify_unpack_no_stcycl(const DmaTransfer& transfer,
                             VifCode::Kind unpack_kind,
                             u32 qwc,
                             u32 addr,
                             bool usn,
                             bool flg) {
  if (transfer.size_bytes != qwc * 16) {
    fmt::print("verify_unpack: bad size {} vs {}\n", transfer.size_bytes, qwc * 16);
    return false;
  }

  if (transfer.vifcode0().kind != VifCode::Kind::NOP) {
    fmt::print("verify_unpack: bad vifcode 0\n");
    return false;
  }

  if (transfer.vifcode1().kind != unpack_kind) {
    fmt::print("verify_unpack: bad vifcode 1\n");
    return false;
  }

  VifCodeUnpack unpack(transfer.vifcode1());

  if (unpack.addr_qw != addr || unpack.use_tops_flag != flg || unpack.is_unsigned != usn) {
    fmt::print("verify_unpack: bad unpack {}/{}/{} vs {}/{}/{}", unpack.addr_qw,
               unpack.use_tops_flag, unpack.is_unsigned, addr, flg, usn);
    return false;
  }

  if (transfer.vifcode1().num != qwc) {
    fmt::print("verify_unpack: bad num {} vs {}\n", transfer.vifcode1().num, qwc);
    return false;
  }

  return true;
}

void unpack_to_no_stcycl(void* dst,
                         const DmaTransfer& transfer,
                         VifCode::Kind unpack_kind,
                         u32 size_bytes,
                         u32 addr,
                         bool usn,
                         bool flg) {
  bool ok = verify_unpack_no_stcycl(transfer, unpack_kind, size_bytes / 16, addr, usn, flg);
  assert(ok);
  assert((size_bytes & 0xf) == 0);
  memcpy(dst, transfer.data, size_bytes);
}

bool looks_like_2d_chunk_start(const DmaFollower& dma) {
  return dma.current_tag().qwc == 1 && dma.current_tag().kind == DmaTag::Kind::CNT;
}

/*!
 * Read the header. Asserts if it's bad.
 * Returns the number of sprites.
 * Advances 1 dma transfer
 */
u32 process_header(DmaFollower& dma) {
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

SpriteRenderer::SpriteRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {}

/*!
 * Run the sprite distorter.  Currently nothing uses sprite-distorter so this just skips through
 * the table upload stuff that runs every frame, even if there are no sprites.
 */
void SpriteRenderer::render_distorter(DmaFollower& dma) {
  // Next thing should be the sprite-distorter setup
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

void SpriteRenderer::render_2d_group0(DmaFollower& dma) {
  (void)dma;
  // TODO
}

void SpriteRenderer::render_fake_shadow(DmaFollower& dma) {
  // TODO
  // nop + flushe
  auto nop_flushe = dma.read_and_advance();
  assert(nop_flushe.vifcode0().kind == VifCode::Kind::NOP);
  assert(nop_flushe.vifcode1().kind == VifCode::Kind::FLUSHE);
}

void SpriteRenderer::render_2d_group1(DmaFollower& dma) {
  // one time matrix data upload
  auto mat_upload = dma.read_and_advance();
  bool mat_ok = verify_unpack_with_stcycl(mat_upload, VifCode::Kind::UNPACK_V4_32, 4, 4, 80,
                                          SpriteDataMem::Matrix, false, false);
  assert(mat_ok);
  assert(mat_upload.size_bytes == sizeof(m_hud_matrix_data));
  memcpy(&m_hud_matrix_data, mat_upload.data, sizeof(m_hud_matrix_data));

  // might need something smarter here...
  while (looks_like_2d_chunk_start(dma)) {
    m_debug_stats.blocks_2d_grp1++;
    // 4 packets per chunk

    // first is the header
    u32 sprite_count = process_header(dma);
    m_debug_stats.count_2d_grp1 += sprite_count;

    // second is the vector data
    u32 expected_vec_size = sizeof(SpriteVecData2d) * sprite_count;
    auto vec_data = dma.read_and_advance();
    assert(expected_vec_size <= sizeof(m_vec_data_2d));
    unpack_to_no_stcycl(&m_vec_data_2d, vec_data, VifCode::Kind::UNPACK_V4_32, expected_vec_size,
                        SpriteDataMem::Vector, false, true);

    // third is the adgif data
    u32 expected_adgif_size = sizeof(AdGif) * sprite_count;
    auto adgif_data = dma.read_and_advance();
    assert(expected_adgif_size <= sizeof(m_adgif));
    unpack_to_no_stcycl(&m_adgif, adgif_data, VifCode::Kind::UNPACK_V4_32, expected_adgif_size,
                        SpriteDataMem::Adgif, false, true);

    // fourth is the actual run!!!!!
    auto run = dma.read_and_advance();
    assert(run.vifcode0().kind == VifCode::Kind::NOP);
    assert(run.vifcode1().kind == VifCode::Kind::MSCAL);
    assert(run.vifcode1().immediate == SpriteProgMem::Sprites2dHud);
    do_2d_group1_block(sprite_count);
  }
}

void SpriteRenderer::render(DmaFollower& dma, SharedRenderState* render_state) {
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
  render_distorter(dma);

  // next, sprite frame setup.
  handle_sprite_frame_setup(dma);

  // 3d sprites
  render_3d(dma);

  // 2d draw
  render_2d_group0(dma);

  // shadow draw
  render_fake_shadow(dma);

  // 2d draw (HUD)
  render_2d_group1(dma);

  // fmt::print("next bucket is 0x{}\n", render_state->next_bucket);
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto tag = dma.current_tag();
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
  ImGui::Text("2D Group 1 (HUD) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp1,
              m_debug_stats.count_2d_grp1);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Render (for real)

void SpriteRenderer::do_2d_group1_block(u32 count) {
  fmt::print("sprites: {}\n", count);

  // currently,

//  xtop vi02                  |  nop
//  nop                        |  nop
//  ilwr.x vi04, vi02          |  nop
//  iaddi vi02, vi02, 0x1      |  nop
//  iaddiu vi03, vi02, 0x90    |  nop
//  L7:
//  ilw.y vi08, 1(vi02)        |  nop
//  lq.xyzw vf25, 900(vi00)    |  nop
//  lq.xyzw vf26, 901(vi00)    |  nop
//  lq.xyzw vf27, 902(vi00)    |  nop
//  lq.xyzw vf28, 903(vi00)    |  nop
//  lq.xyzw vf30, 904(vi08)    |  nop
//  lqi.xyzw vf01, vi02        |  nop
//  lqi.xyzw vf05, vi02        |  nop
//  lqi.xyzw vf11, vi02        |  nop
//  lq.xyzw vf12, 1020(vi00)   |  mulaw.xyzw ACC, vf28, vf00
//  ilw.y vi08, 1(vi02)        |  maddax.xyzw ACC, vf25, vf01
//  nop                        |  madday.xyzw ACC, vf26, vf01
//  nop                        |  maddz.xyzw vf02, vf27, vf01
//  move.w vf05, vf00          |  addw.z vf01, vf00, vf05
//  nop                        |  nop
//  div Q, vf31.x, vf02.w      |  muly.z vf05, vf05, vf31
//  nop                        |  mul.xyzw vf03, vf02, vf29
//  nop                        |  nop
//  nop                        |  nop
//  nop                        |  mulz.z vf04, vf05, vf05
//  lq.xyzw vf14, 1001(vi00)   |  clipw.xyz vf03, vf03
//  iaddi vi06, vi00, 0x1      |  adda.xyzw ACC, vf11, vf11
//  L8:
//  ior vi05, vi15, vi00       |  mul.zw vf01, vf01, Q
//  lq.xyzw vf06, 998(vi00)    |  mulz.xyzw vf15, vf05, vf04
//  lq.xyzw vf14, 1002(vi00)   |  mula.xyzw ACC, vf05, vf14
//  fmand vi01, vi06           |  mul.xyz vf02, vf02, Q
//  ibne vi00, vi01, L10       |  addz.x vf01, vf00, vf01
//  lqi.xyzw vf07, vi03        |  mulz.xyzw vf16, vf15, vf04
//  lq.xyzw vf14, 1003(vi00)   |  madda.xyzw ACC, vf15, vf14
//  lqi.xyzw vf08, vi03        |  add.xyzw vf10, vf02, vf30
//  lqi.xyzw vf09, vi03        |  mulw.x vf01, vf01, vf01
//  sqi.xyzw vf06, vi05        |  mulz.xyzw vf15, vf16, vf04
//  lq.xyzw vf14, 1004(vi00)   |  madda.xyzw ACC, vf16, vf14
//  sqi.xyzw vf07, vi05        |  maxx.w vf10, vf10, vf12
//  sqi.xyzw vf08, vi05        |  maxz.zw vf01, vf01, vf31
//  sqi.xyzw vf09, vi05        |  mulz.xyzw vf16, vf15, vf04
//  lq.xyzw vf14, 1005(vi00)   |  madda.xyzw ACC, vf15, vf14
//  lqi.xyzw vf06, vi03        |  mulw.x vf01, vf01, vf31
//  lqi.xyzw vf07, vi03        |  miniy.w vf10, vf10, vf12
//  lq.xyzw vf08, 1000(vi00)   |  nop
//  ilw.x vi07, -2(vi02)       |  madd.xyzw vf05, vf16, vf14
//  lq.xyzw vf30, 904(vi08)    |  nop
//  lqi.xyzw vf23, vi02        |  miniw.x vf01, vf01, vf00
//  lqi.xyzw vf24, vi02        |  mulx.w vf11, vf11, vf01
//  fcand vi01, 0x3f           |  mulaw.xyzw ACC, vf28, vf00
//  lq.xyzw vf17, 1006(vi00)   |  maddax.xyzw ACC, vf25, vf23
//  lq.xyzw vf18, 1007(vi00)   |  madday.xyzw ACC, vf26, vf23
//  lq.xyzw vf19, 980(vi07)    |  ftoi0.xyzw vf11, vf11
//  lq.xyzw vf20, 981(vi07)    |  maddz.xyzw vf02, vf27, vf23
//  lq.xyzw vf21, 982(vi07)    |  mulaw.xyzw ACC, vf17, vf05
//  lq.xyzw vf22, 983(vi07)    |  msub.xyzw vf12, vf18, vf05
//  sq.xyzw vf11, 3(vi05)      |  mulaz.xyzw ACC, vf17, vf05
//  lqi.xyzw vf11, vi02        |  maddw.xyzw vf13, vf18, vf05
//  move.w vf24, vf00          |  addw.z vf23, vf00, vf24
//  div Q, vf31.x, vf02.w      |  mulw.xyzw vf12, vf12, vf01
//  ibne vi00, vi01, L9        |  muly.z vf24, vf24, vf31
//  ilw.y vi08, 1(vi02)        |  mulz.xyzw vf13, vf13, vf01
//  sqi.xyzw vf06, vi05        |  mul.xyzw vf03, vf02, vf29
//  sqi.xyzw vf07, vi05        |  mulaw.xyzw ACC, vf10, vf00
//  sqi.xyzw vf08, vi05        |  maddax.xyzw ACC, vf12, vf19
//  lq.xyzw vf06, 988(vi00)    |  maddy.xyzw vf19, vf13, vf19
//  lq.xyzw vf07, 989(vi00)    |  mulaw.xyzw ACC, vf10, vf00
//  lq.xyzw vf08, 990(vi00)    |  maddax.xyzw ACC, vf12, vf20
//  lq.xyzw vf09, 991(vi00)    |  maddy.xyzw vf20, vf13, vf20
//  sq.xyzw vf06, 1(vi05)      |  mulaw.xyzw ACC, vf10, vf00
//  sq.xyzw vf07, 3(vi05)      |  maddax.xyzw ACC, vf12, vf21
//  sq.xyzw vf08, 5(vi05)      |  maddy.xyzw vf21, vf13, vf21
//  sq.xyzw vf09, 7(vi05)      |  mulaw.xyzw ACC, vf10, vf00
//  nop                        |  maddax.xyzw ACC, vf12, vf22
//  nop                        |  maddy.xyzw vf22, vf13, vf22
//  lq.xyzw vf12, 1020(vi00)   |  ftoi4.xyzw vf19, vf19
//  lq.xyzw vf14, 1001(vi00)   |  ftoi4.xyzw vf20, vf20
//  move.xyzw vf05, vf24       |  ftoi4.xyzw vf21, vf21
//  move.xyzw vf01, vf23       |  ftoi4.xyzw vf22, vf22
//  sq.xyzw vf19, 2(vi05)      |  mulz.z vf04, vf24, vf24
//  sq.xyzw vf20, 4(vi05)      |  clipw.xyz vf03, vf03
//  sq.xyzw vf21, 6(vi05)      |  nop
//  sq.xyzw vf22, 8(vi05)      |  nop
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