#include "OceanTexture.h"

OceanTexture::OceanTexture() : m_tex0(TEX0_SIZE, TEX0_SIZE, GL_UNSIGNED_INT_8_8_8_8_REV) {
  m_dbuf_x = m_dbuf_a;
  m_dbuf_y = m_dbuf_b;

  m_tbuf_x = m_tbuf_a;
  m_tbuf_y = m_tbuf_b;
}

void OceanTexture::handle_tex_call_start(DmaFollower& dma,
                                         SharedRenderState* render_state,
                                         ScopedProfilerNode& prof) {
  //  L1:
  //  lq.xyzw vf14_startx, 988(vi00)    |  maxw.xyzw vf01_ones, vf00, vf00
  vu.startx = Vf(m_texture_constants.start);
  //  lq.xyzw vf02_offset, 989(vi00)
  //  lq.xyzw vf03_tbuf, 986(vi00)
  //  lq.xyzw vf04_dbuf, 987(vi00)
  //  lq.xyzw vf05_giftag, 985(vi00)
  //  lq.xyzw vf06_cam_nrm, 991(vi00)
  //  lq.xyzw vf07_constants, 990(vi00)
  //  iaddiu vi11_0x80, vi00, 0x80
  //  mtir vi08_tptr, vf03_tbuf.x
  vu.tptr = get_tbuf();
  //  mtir vi09_tbase, vf03_tbuf.x
  vu.tbase = get_tbuf();
  //  mr32.xyzw vf03_tbuf, vf03_tbuf
  swap_tbuf();
  //  xtop vi05_in_ptr
  vu.in_ptr = swap_vu_upload_buffers();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5();

  //  nop                     :e
  //  nop
}

void OceanTexture::handle_tex_call_rest(DmaFollower& dma,
                                        SharedRenderState* render_state,
                                        ScopedProfilerNode& prof) {
  //  L2:
  //  xtop vi05_in_ptr
  vu.in_ptr = swap_vu_upload_buffers();
  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5();

  //  nop                     :e
  //  nop
}

void OceanTexture::run_L3() {

}

void OceanTexture::run_L5() {
  
}

void OceanTexture::handle_tex_call_done(DmaFollower& dma,
                                        SharedRenderState* render_state,
                                        ScopedProfilerNode& prof) {
  // this program does nothing.
}

void OceanTexture::handle_ocean_texture(DmaFollower& dma,
                                        SharedRenderState* render_state,
                                        ScopedProfilerNode& prof) {
  // render to the first texture
  {
    // (set-display-gs-state arg0 ocean-tex-page-0 128 128 0 0)
    auto data = dma.read_and_advance();
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
    handle_tex_call_start(dma, render_state, prof);
    // TODO handle call and swapping buffer
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
    handle_tex_call_rest(dma, render_state, prof);

    // TODO handle call and swapping buffer
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
    handle_tex_call_rest(dma, render_state, prof);
    // TODO handle call and swapping buffer
  }

  // last call
  {
    auto data = dma.read_and_advance();
    ASSERT(data.size_bytes == 0);
    ASSERT(data.vifcode0().kind == VifCode::Kind::MSCALF);
    ASSERT(data.vifcode0().immediate == TexVu1Prog::DONE);
    ASSERT(data.vifcode1().kind == VifCode::Kind::STMOD);  // not sure why...
    handle_tex_call_done(dma, render_state, prof);
    // TODO handle call and swapping buffer
  }
}