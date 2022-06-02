#include "Generic2.h"

/*!
 * Main function to set up Generic2 draw lists.
 * This function figures out which vertices belong to which draw settings.
 */
void Generic2::setup_draws() {
  if (m_next_free_frag == 0) {
    return;
  }
  m_gs = GsState();
  link_adgifs_back_to_frags();
  process_matrices();
  determine_draw_modes();
  draws_to_buckets();
  final_vertex_update();
  build_index_buffer();
}

/*!
 * For each adgif, determine the draw mode.
 * There's a bunch of stuff in adgifs that don't really matter, and this filters out all that junk
 * They also do a bunch of tricks where some of the GS state is left over from the previous draw.
 *
 * For each adgif, it determines the "draw mode" which is used as a unique identifier for OpenGL
 * settings, the tbp (texture vram address), and the "vertex flags" that need to be set for each
 * vertex.  This information is used in later steps.
 */
void Generic2::determine_draw_modes() {
  // initialize draw mode
  DrawMode current_mode;
  current_mode.set_at(true);
  current_mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  current_mode.set_aref(0x26);
  current_mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
  current_mode.set_zt(true);
  current_mode.set_depth_test(GsTest::ZTest::GEQUAL);
  current_mode.set_depth_write_enable(!m_drawing_config.zmsk);
  current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_SRC_SRC_SRC);
  m_gs.set_fog_flag(true);

  u32 tbp = -1;

  // these are copies of the state
  GsTex0 tex0;
  tex0.data = UINT64_MAX;

  // iterate over all adgifs
  for (u32 i = 0; i < m_next_free_adgif; i++) {
    auto& ad = m_adgifs[i].data;
    auto& frag = m_fragments[m_adgifs[i].frag];
    m_adgifs[i].uses_hud = frag.uses_hud;

    // the header contains the giftag which might set fogging based on pre.
    GifTag tag(frag.header + (4 * 16));
    if (tag.pre()) {
      GsPrim prim(tag.prim());
      m_gs.set_fog_flag(prim.fge());
    }

    // ADGIF 0
    ASSERT((u8)ad.tex0_addr == (u32)GsRegisterAddress::TEX0_1);
    if (ad.tex0_data != tex0.data) {
      tex0.data = ad.tex0_data;
      GsTex0 reg(ad.tex0_data);
      tbp = reg.tbp0();
      // tbw
      if (reg.psm() == GsTex0::PSM::PSMT4HH) {
        tbp |= 0x8000;
      }
      // tw/th
      current_mode.set_tcc(reg.tcc());
      m_gs.set_tcc_flag(reg.tcc());
      bool decal = reg.tfx() == GsTex0::TextureFunction::DECAL;
      current_mode.set_decal(decal);
      m_gs.set_decal_flag(decal);
      ASSERT(reg.tfx() == GsTex0::TextureFunction::DECAL ||
             reg.tfx() == GsTex0::TextureFunction::MODULATE);
    }

    // ADGIF 1
    ASSERT((u8)ad.tex1_addr == (u32)GsRegisterAddress::TEX1_1);
    {
      GsTex1 reg(ad.tex1_data);
      current_mode.set_filt_enable(reg.mmag());
    }

    // ADGIF 2
    ASSERT((u8)ad.mip_addr == (u32)GsRegisterAddress::MIPTBP1_1);

    // ADGIF 3
    ASSERT((u8)ad.clamp_addr == (u32)GsRegisterAddress::CLAMP_1);
    {
      bool clamp_s = ad.clamp_data & 0b001;
      bool clamp_t = ad.clamp_data & 0b100;
      current_mode.set_clamp_s_enable(clamp_s);
      current_mode.set_clamp_t_enable(clamp_t);
    }

    std::optional<u64> final_alpha;

    // ADGIF 4
    if ((u8)ad.alpha_addr == (u32)GsRegisterAddress::ALPHA_1) {
      final_alpha = ad.alpha_data;
    } else {
      ASSERT((u8)ad.alpha_addr == (u32)GsRegisterAddress::MIPTBP2_1);
    }

    u64 bonus_adgif_data[4];
    memcpy(bonus_adgif_data, frag.header + (5 * 16), 4 * sizeof(u64));

    u64 final_test;
    if ((u8)bonus_adgif_data[1] == (u8)(GsRegisterAddress::ALPHA_1)) {
      ASSERT((u8)bonus_adgif_data[1] == (u8)(GsRegisterAddress::ALPHA_1));
      final_alpha = bonus_adgif_data[0];
      ASSERT((u8)bonus_adgif_data[3] == (u8)(GsRegisterAddress::TEST_1));
      final_test = bonus_adgif_data[2];
    } else {
      // ADGIF 5
      ASSERT((u8)bonus_adgif_data[1] == (u8)(GsRegisterAddress::TEST_1));
      final_test = bonus_adgif_data[0];

      // ADGIF 6
      if ((u8)bonus_adgif_data[3] == (u8)(GsRegisterAddress::ALPHA_1)) {
        final_alpha = bonus_adgif_data[2];
      } else {
        ASSERT((u8)bonus_adgif_data[3] == (u8)(GsRegisterAddress::TEST_1));
        final_test = bonus_adgif_data[2];
      }
    }

    if (final_alpha) {
      GsAlpha reg(*final_alpha);
      if (m_gs.gs_alpha != reg) {
        m_gs.gs_alpha = reg;
        auto a = reg.a_mode();
        auto b = reg.b_mode();
        auto c = reg.c_mode();
        auto d = reg.d_mode();
        if (a == GsAlpha::BlendMode::SOURCE && b == GsAlpha::BlendMode::DEST &&
            c == GsAlpha::BlendMode::SOURCE && d == GsAlpha::BlendMode::DEST) {
          current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
        } else if (a == GsAlpha::BlendMode::SOURCE && b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
                   c == GsAlpha::BlendMode::SOURCE && d == GsAlpha::BlendMode::DEST) {
          current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_SRC_DST);
        } else if (a == GsAlpha::BlendMode::ZERO_OR_FIXED && b == GsAlpha::BlendMode::SOURCE &&
                   c == GsAlpha::BlendMode::SOURCE && d == GsAlpha::BlendMode::DEST) {
          current_mode.set_alpha_blend(DrawMode::AlphaBlend::ZERO_SRC_SRC_DST);
        } else if (a == GsAlpha::BlendMode::SOURCE && b == GsAlpha::BlendMode::DEST &&
                   c == GsAlpha::BlendMode::ZERO_OR_FIXED && d == GsAlpha::BlendMode::DEST) {
          current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_FIX_DST);
        } else if (a == GsAlpha::BlendMode::SOURCE && b == GsAlpha::BlendMode::SOURCE &&
                   c == GsAlpha::BlendMode::SOURCE && d == GsAlpha::BlendMode::SOURCE) {
          current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_SRC_SRC_SRC);
        } else if (a == GsAlpha::BlendMode::SOURCE && b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
                   c == GsAlpha::BlendMode::DEST && d == GsAlpha::BlendMode::DEST) {
          current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_DST_DST);
        } else if (a == GsAlpha::BlendMode::SOURCE && b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
                   c == GsAlpha::BlendMode::ZERO_OR_FIXED && d == GsAlpha::BlendMode::DEST) {
          current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_FIX_DST);
        } else {
          fmt::print("unsupported blend: a {} b {} c {} d {}\n", (int)a, (int)b, (int)c, (int)d);
          // ASSERT(false);
        }
      }
    }

    {
      GsTest reg(final_test);
      current_mode.set_at(reg.alpha_test_enable());
      if (reg.alpha_test_enable()) {
        switch (reg.alpha_test()) {
          case GsTest::AlphaTest::NEVER:
            current_mode.set_alpha_test(DrawMode::AlphaTest::NEVER);
            break;
          case GsTest::AlphaTest::ALWAYS:
            current_mode.set_alpha_test(DrawMode::AlphaTest::ALWAYS);
            break;
          case GsTest::AlphaTest::GEQUAL:
            current_mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
            break;
          default:
            ASSERT(false);
        }
      }

      current_mode.set_aref(reg.aref());
      current_mode.set_alpha_fail(reg.afail());
      current_mode.set_zt(reg.zte());
      current_mode.set_depth_test(reg.ztest());
    }

    m_adgifs[i].mode = current_mode;
    m_adgifs[i].vtx_flags = m_gs.vertex_flags;
    m_adgifs[i].tbp = tbp;
    m_adgifs[i].fix = m_gs.gs_alpha.fix();
  }
}

/*!
 * For each adgif, figure out the vertices that it belongs to, in the giant vertex buffer.
 */
void Generic2::link_adgifs_back_to_frags() {
  for (u32 i = 0; i < m_next_free_frag; i++) {
    auto& frag = m_fragments[i];
    for (u32 j = 0; j < frag.adgif_count; j++) {
      auto& ad = m_adgifs[frag.adgif_idx + j];
      ad.vtx_count = (ad.data.tex1_addr >> 32) & 0xfff;  // drop the eop flag
      ad.vtx_idx = frag.vtx_idx + ((ad.data.tex0_addr >> 32) & 0xffff) / 3;
      ASSERT(ad.vtx_count + ad.vtx_idx <= frag.vtx_count + frag.vtx_idx);
      ad.frag = i;
    }
  }
}

/*!
 * Build linked lists of adgifs that share the same settings.
 * TODO: also determine texture units per bucket here.
 */
void Generic2::draws_to_buckets() {
  std::unordered_map<u64, u32> draw_key_to_bucket;
  for (u32 i = 0; i < m_next_free_adgif; i++) {
    auto& ad = m_adgifs[i];
    if (ad.uses_hud) {
      // put all hud draws in separate buckets.
      // there's some really weird messed up draws for the orbs that fly up to the corner when
      // breaking a crate on a zoomer.
      u32 bucket_idx = m_next_free_bucket++;
      ASSERT(bucket_idx < m_buckets.size());
      draw_key_to_bucket[ad.key()] = bucket_idx;
      auto& bucket = m_buckets[bucket_idx];
      bucket.tbp = ad.tbp;
      bucket.mode = ad.mode;
      bucket.start = i;
      bucket.last = i;
      ad.next = UINT32_MAX;
    } else {
      u64 key = ad.key();
      const auto& bucket_it = draw_key_to_bucket.find(key);
      if (bucket_it == draw_key_to_bucket.end()) {
        // new bucket!
        u32 bucket_idx = m_next_free_bucket++;
        ASSERT(bucket_idx < m_buckets.size());
        draw_key_to_bucket[key] = bucket_idx;
        auto& bucket = m_buckets[bucket_idx];
        bucket.tbp = ad.tbp;
        bucket.mode = ad.mode;
        bucket.start = i;
        bucket.last = i;
        ad.next = UINT32_MAX;
      } else {
        // existing bucket!
        auto& bucket = m_buckets[bucket_it->second];
        m_adgifs[bucket.last].next = i;
        ad.next = UINT32_MAX;
        bucket.last = i;
      }
    }
  }
}

/*!
 * Extract the matrix. They are exactly a perspective projection and they are all the same.
 * I don't think this will hold for TIE...
 */
void Generic2::process_matrices() {
  // first, we need to find the projection matrix.
  // most of the time, it's first. If you have the hud open, there may be a few others.
  bool found_proj_matrix = false;
  std::array<math::Vector4f, 4> projection_matrix, hud_matrix;
  for (u32 i = 0; i < m_next_free_frag; i++) {
    float mat_33;
    memcpy(&mat_33, m_fragments[i].header + 15 * sizeof(float), sizeof(float));
    if (mat_33 == 0) {
      // got it.
      memcpy(&projection_matrix, m_fragments[i].header, 64);
      found_proj_matrix = true;
      break;
    }
  }

  if (!found_proj_matrix) {
    for (auto& row : projection_matrix) {
      row.fill(0);
    }
  }

  // mark as hud/proj
  bool found_hud_matrix = false;
  for (u32 i = 0; i < m_next_free_frag; i++) {
    float mat_33;
    memcpy(&mat_33, m_fragments[i].header + 15 * sizeof(float), sizeof(float));
    if (mat_33 == 0) {
      m_fragments[i].uses_hud = false;
    } else {
      m_fragments[i].uses_hud = true;
      if (!found_hud_matrix) {
        found_hud_matrix = true;
        memcpy(&hud_matrix, m_fragments[i].header, 64);
      }
    }
  }

  m_drawing_config.proj_scale[0] = projection_matrix[0][0];
  m_drawing_config.proj_scale[1] = projection_matrix[1][1];
  m_drawing_config.proj_scale[2] = projection_matrix[2][2];
  m_drawing_config.proj_mat_23 = projection_matrix[2][3];
  m_drawing_config.proj_mat_32 = projection_matrix[3][2];

  if (found_hud_matrix) {
    m_drawing_config.hud_scale[0] = hud_matrix[0][0];
    m_drawing_config.hud_scale[1] = hud_matrix[1][1];
    m_drawing_config.hud_scale[2] = hud_matrix[2][2];
    m_drawing_config.hud_mat_23 = hud_matrix[2][3];
    m_drawing_config.hud_mat_32 = hud_matrix[3][2];
    m_drawing_config.hud_mat_33 = hud_matrix[3][3];
  }

  m_drawing_config.uses_hud = found_hud_matrix;
}

/*!
 * After all bucketing/draw modes have been determined, fill out the flag fields of all vertices.
 * TODO: fill out texture units
 */
void Generic2::final_vertex_update() {
  for (u32 i = 0; i < m_next_free_adgif; i++) {
    auto& ad = m_adgifs[i];
    for (u32 j = 0; j < ad.vtx_count; j++) {
      m_verts[ad.vtx_idx + j].flags = ad.vtx_flags;
    }
  }
}

/*!
 * Build the index buffer.
 */
void Generic2::build_index_buffer() {
  for (u32 bucket_idx = 0; bucket_idx < m_next_free_bucket; bucket_idx++) {
    auto& bucket = m_buckets[bucket_idx];
    bucket.tri_count = 0;
    bucket.idx_idx = m_next_free_idx;

    u32 adgif_idx = bucket.start;
    while (adgif_idx != UINT32_MAX) {
      auto& adgif = m_adgifs[adgif_idx];
      m_indices[m_next_free_idx++] = UINT32_MAX;
      for (u32 vidx = adgif.vtx_idx; vidx < adgif.vtx_idx + adgif.vtx_count; vidx++) {
        auto& vtx = m_verts[vidx];
        if (vtx.adc) {
          m_indices[m_next_free_idx++] = vidx;
          bucket.tri_count++;
        } else {
          m_indices[m_next_free_idx++] = UINT32_MAX;
          m_indices[m_next_free_idx++] = vidx - 1;
          m_indices[m_next_free_idx++] = vidx;
        }
      }
      bucket.tri_count -= 2;
      adgif_idx = adgif.next;
    }

    bucket.idx_count = m_next_free_idx - bucket.idx_idx;
  }
}