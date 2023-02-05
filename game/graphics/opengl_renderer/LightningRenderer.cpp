#include "LightningRenderer.h"

LightningRenderer::LightningRenderer(const std::string& name, int id)
    : BucketRenderer(name, id), m_generic(name, id) {}

LightningRenderer::~LightningRenderer() {}

void LightningRenderer::draw_debug_window() {
  m_generic.draw_debug_window();
}

bool is_nop_zero(const DmaTransfer& xf) {
  return xf.size_bytes == 0 && xf.vifcode0().kind == VifCode::Kind::NOP &&
         xf.vifcode1().kind == VifCode::Kind::NOP;
}

void unpack_vertex(Generic2::Vertex* out, const u8* in, int count) {
  for (int i = 0; i < count; i++) {
    // st:
    s32 s, t;
    memcpy(&s, in, 4);
    memcpy(&t, in + 4, 4);
    s32 s_masked = s & 0xfffffffe;
    out->st[0] = s_masked;
    out->st[1] = t;
    out->adc = s_masked == s;

    // rgba
    u32 data[4];
    memcpy(data, in + 16, 16);
    for (int j = 0; j < 4; j++) {
      out->rgba[j] = data[j];
    }

    // pos
    float p[4];
    memcpy(p, in + 32, 16);
    for (int j = 0; j < 3; j++) {
      out->xyz[j] = p[j];
    }

    out++;
    in += (16 * 3);
  }
}

void LightningRenderer::render(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  m_generic.reset_buffers();
  auto first_data = dma.read_and_advance();
  // if unused, sends 0 nop nop
  if (is_nop_zero(first_data) && render_state->next_bucket == dma.current_tag_offset()) {
    return;
  }

  // intro:
  //  0: MARK NOP (profiling)
  if (!(first_data.vifcode0().kind == VifCode::Kind::MARK &&
         first_data.vifcode1().kind == VifCode::Kind::NOP)) {
//    fmt::print("bad dma: {} {} {}\n", first_data.size_bytes, first_data.vifcode0().print(), first_data.vifcode1().print());
//    while (dma.current_tag_offset() != render_state->next_bucket) {
//      first_data = dma.read_and_advance();
//      fmt::print("    dma: {} {} {}\n", first_data.size_bytes, first_data.vifcode0().print(), first_data.vifcode1().print());
//
//    }
//    return;
  }

  //  32: NOP DIRECT (set GS registers)
  // for lightning, this is always masking z writes
  auto direct_setup = dma.read_and_advance();
  ASSERT(direct_setup.size_bytes == 32 && direct_setup.vifcode0().kind == VifCode::Kind::NOP &&
         direct_setup.vifcode1().kind == VifCode::Kind::DIRECT);
  m_generic.m_drawing_config.zmsk = true;

  //  128: STCYCL cl: 4 wl: 4 UNPACK-V4-32: 8 addr: 897 us: false tops: false
  // upload VU1 constants

  /*
 (deftype generic-constants (structure)
  ((fog         vector :inline :offset-assert 0)
   (adgif       gs-gif-tag  :inline :offset-assert 16) ;; was qword
   (hvdf-offset vector :inline :offset-assert 32)
   (hmge-scale  vector :inline :offset-assert 48)
   (invh-scale  vector :inline :offset-assert 64)
   (guard       vector :inline :offset-assert 80)
   (flush       qword  :inline :offset-assert 96)
   (stores      qword  :inline :offset-assert 112)
   )
  )
   */
  auto constants = dma.read_and_advance();
  ASSERT(constants.size_bytes == 128);
  ASSERT(constants.vifcode0().kind == VifCode::Kind::STCYCL);
  ASSERT(constants.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
  memcpy(&m_generic.m_drawing_config.pfog0, constants.data + 0, 4);
  memcpy(&m_generic.m_drawing_config.fog_min, constants.data + 4, 4);
  memcpy(&m_generic.m_drawing_config.fog_max, constants.data + 8, 4);
  memcpy(m_generic.m_drawing_config.hvdf_offset.data(), constants.data + 32, 16);

  //  32: MSCALF 0x0 STMOD 0b0
  auto mscalf = dma.read_and_advance();
  ASSERT(mscalf.vifcode0().kind == VifCode::Kind::MSCALF &&
         mscalf.vifcode1().kind == VifCode::Kind::STMOD);
  //  0: NOP NOP
  auto another_nop = dma.read_and_advance();
  ASSERT(is_nop_zero(another_nop));

  auto maybe_first_upload = dma.read_and_advance();
  while (maybe_first_upload.vifcode1().kind == VifCode::Kind::UNPACK_V4_32) {
    auto second_upload = dma.read_and_advance();
    auto mscal = dma.read_and_advance();

    auto* frag = &m_generic.next_frag();
    ASSERT(maybe_first_upload.size_bytes == Generic2::FRAG_HEADER_SIZE + 5 * 16);  // header + adgif
    memcpy(frag->header, maybe_first_upload.data, Generic2::FRAG_HEADER_SIZE);
    frag->adgif_idx = m_generic.m_next_free_adgif;
    frag->adgif_count = 1;
    frag->mscal_addr = 6;
    frag->uses_hud = false;
    auto* adgif = &m_generic.next_adgif();
    memcpy(&adgif->data, maybe_first_upload.data + Generic2::FRAG_HEADER_SIZE, sizeof(AdGifData));
    // (new 'static 'gif-tag-regs-32 :regs0 (gif-reg-id st) :regs1 (gif-reg-id rgbaq) :regs2
    // (gif-reg-id xyzf2))
    int num_vtx = second_upload.size_bytes / (16 * 3);
    frag->vtx_count = num_vtx;
    frag->vtx_idx = m_generic.m_next_free_vert;
    m_generic.alloc_vtx(num_vtx);
    unpack_vertex(&m_generic.m_verts[frag->vtx_idx], second_upload.data, num_vtx);

    // run
    //  192: NOP UNPACK-V4-32: 12 addr: 837 us: false tops: false
    //  1536: NOP UNPACK-V4-32: 96 addr: 9 us: false tops: false
    //  0: NOP MSCAL 0x6

    maybe_first_upload = dma.read_and_advance();
  }

  auto flusha = dma.read_and_advance();
  auto end = dma.read_and_advance();
  ASSERT(render_state->next_bucket == dma.current_tag_offset());

  // ending:
  //  0: NOP NOP
  //  160: FLUSHA DIRECT
  //  0: NOP NOP

  m_generic.setup_draws();
  m_generic.do_draws(render_state, prof);
}
void LightningRenderer::init_shaders(ShaderLibrary& shaders) {
  m_generic.init_shaders(shaders);
}