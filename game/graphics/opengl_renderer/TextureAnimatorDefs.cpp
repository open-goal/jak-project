#include "game/graphics/opengl_renderer/TextureAnimator.h"

void TextureAnimator::setup_texture_anims_jak3() {
  m_darkjak_clut_blender_idx = create_clut_blender_group(
      {
          "jakc-arm",
          "jakc-eyebrow",
          "jakc-face",
          "jakc-finger",
          "jakc-hair",
      },
      "-norm", "-dark", {});

  m_darkjak_highres_clut_blender_idx = create_clut_blender_group(
      {
          "jakchires-arm",
          "jakchires-eye",
          "jakchires-eyebrow",
          "jakchires-eyelid",
          "jakchires-facelft",
          "jakchires-facert",
          "jakchires-hair",
      },
      "-norm", "-dark", "MHCTYCST.DGO", true);

  // default-water
  {
    FixedAnimDef bomb_gradient;
    bomb_gradient.tex_name = "bomb-gradient";
    bomb_gradient.color = math::Vector4<u8>{0, 0, 0, 0x80};
    bomb_gradient.override_size = math::Vector2<int>(64, 64);

    auto& bomb_rim = bomb_gradient.layers.emplace_back();
    bomb_rim.tex_name = "bomb-gradient-rim";
    bomb_rim.end_time = 300.f;
    bomb_rim.set_blend_b2_d1();
    bomb_rim.set_no_z_write_no_z_test();
    bomb_rim.channel_masks[3] = false;  // no alpha writes.

    auto& bomb_flames = bomb_gradient.layers.emplace_back();
    bomb_flames.tex_name = "bomb-gradient-flames";
    bomb_flames.end_time = 300.f;
    bomb_flames.set_blend_b2_d1();
    bomb_flames.set_no_z_write_no_z_test();
    bomb_flames.channel_masks[3] = false;  // no alpha writes.

    FixedAnimDef blue_beam_dest;
    blue_beam_dest.tex_name = "blue-beam-dest";
    blue_beam_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    blue_beam_dest.override_size = math::Vector2<int>(128, 128);

    auto& beam01 = blue_beam_dest.layers.emplace_back();
    beam01.tex_name = "lightning-beam-01";
    beam01.end_time = 300.f;
    beam01.set_blend_b2_d1();
    beam01.set_no_z_write_no_z_test();
    beam01.channel_masks[3] = false;  // no alpha writes.

    auto& beam02 = blue_beam_dest.layers.emplace_back();
    beam02.tex_name = "lightning-beam-02";
    beam02.end_time = 300.f;
    beam02.set_blend_b2_d1();
    beam02.set_no_z_write_no_z_test();
    beam02.channel_masks[3] = false;  // no alpha writes.

    FixedAnimDef lightjak_wings;
    lightjak_wings.tex_name = "lightjak-wings";
    lightjak_wings.color = math::Vector4<u8>{0, 0, 0, 0x80};
    lightjak_wings.override_size = math::Vector2<int>(64, 16);

    auto& u_src = lightjak_wings.layers.emplace_back();
    u_src.tex_name = "lightjak-wings-u-src";
    u_src.end_time = 300.f;
    u_src.set_blend_b2_d1();
    u_src.set_no_z_write_no_z_test();
    u_src.set_clamp();
    u_src.channel_masks[3] = false;  // no alpha writes.

    auto& v_src = lightjak_wings.layers.emplace_back();
    v_src.tex_name = "lightjak-wings-v-src";
    v_src.end_time = 300.f;
    v_src.set_blend_b2_d1();
    v_src.set_no_z_write_no_z_test();
    v_src.set_clamp();
    v_src.channel_masks[3] = false;  // no alpha writes.

    FixedAnimDef mushroom_dest;
    mushroom_dest.tex_name = "mushroom-dest";
    mushroom_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    mushroom_dest.override_size = math::Vector2<int>(128, 64);

    auto& mushroom_src0 = mushroom_dest.layers.emplace_back();
    mushroom_src0.tex_name = "mushroom-src";
    mushroom_src0.end_time = 1200.f;
    mushroom_src0.set_blend_b1_d1();
    mushroom_src0.set_no_z_write_no_z_test();
    mushroom_src0.set_clamp();

    auto& mushroom_src1 = mushroom_dest.layers.emplace_back();
    mushroom_src1.tex_name = "mushroom-src";
    mushroom_src1.end_time = 1200.f;
    // :alpha (new 'static 'gs-alpha :b #x2 :c #x2 :d #x2 :fix #x80)
    mushroom_src1.set_blend(GsAlpha::BlendMode::SOURCE, GsAlpha::BlendMode::ZERO_OR_FIXED,
                            GsAlpha::BlendMode::ZERO_OR_FIXED, GsAlpha::BlendMode::ZERO_OR_FIXED,
                            0x80);
    mushroom_src1.set_no_z_write_no_z_test();
    mushroom_src1.set_clamp();
    mushroom_src1.channel_masks[3] = false;  // no alpha writes.

    m_default_water_anim_array_idx =
        create_fixed_anim_array({bomb_gradient, blue_beam_dest, lightjak_wings, mushroom_dest});
  }

  // default-warp
  {
    FixedAnimDef shield_env_rim_dest;
    shield_env_rim_dest.tex_name = "shield-env-rim-dest";
    shield_env_rim_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    shield_env_rim_dest.override_size = math::Vector2<int>(64, 64);
    shield_env_rim_dest.move_to_pool = true;

    auto& cwhite = shield_env_rim_dest.layers.emplace_back();
    cwhite.tex_name = "common-white";
    cwhite.end_time = 1200.f;
    cwhite.set_blend_b2_d1();
    cwhite.set_no_z_write_no_z_test();

    for (int i = 0; i < 2; i++) {
      auto& uscroll = shield_env_rim_dest.layers.emplace_back();
      uscroll.tex_name = "shield-env-uscroll";
      uscroll.end_time = 1200.f;
      uscroll.set_blend_b2_d1();
      uscroll.set_no_z_write_no_z_test();
      uscroll.channel_masks[3] = false;  // no alpha writes.
    }

    for (int i = 0; i < 2; i++) {
      auto& rim_src = shield_env_rim_dest.layers.emplace_back();
      rim_src.tex_name = "shield-env-rim-src";
      rim_src.end_time = 1200.f;
      rim_src.set_blend_b2_d1();
      rim_src.set_no_z_write_no_z_test();
      rim_src.channel_masks[3] = false;  // no alpha writes.
    }

    m_default_warp_anim_array_idx = create_fixed_anim_array({shield_env_rim_dest});
  }

  // templea-water
  {
    FixedAnimDef templea_waterfall_dest;
    templea_waterfall_dest.tex_name = "templea-waterfall-dest";
    templea_waterfall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    templea_waterfall_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = templea_waterfall_dest.layers.emplace_back();
      water.tex_name = "templea-waterfall";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef lightjak_wings;
    lightjak_wings.tex_name = "lightjak-wings";
    lightjak_wings.color = math::Vector4<u8>{0, 0, 0, 0x80};
    lightjak_wings.override_size = math::Vector2<int>(64, 16);

    auto& u_src = lightjak_wings.layers.emplace_back();
    u_src.tex_name = "lightjak-wings-u-src";
    u_src.end_time = 300.f;
    u_src.set_blend_b2_d1();
    u_src.set_no_z_write_no_z_test();
    u_src.set_clamp();
    u_src.channel_masks[3] = false;  // no alpha writes.

    auto& v_src = lightjak_wings.layers.emplace_back();
    v_src.tex_name = "lightjak-wings-v-src";
    v_src.end_time = 300.f;
    v_src.set_blend_b2_d1();
    v_src.set_no_z_write_no_z_test();
    v_src.set_clamp();
    v_src.channel_masks[3] = false;  // no alpha writes.

    m_templea_water_anim_array_idx =
        create_fixed_anim_array({templea_waterfall_dest, lightjak_wings});
  }

  // hanga-sprite
  {
    FixedAnimDef dest2;
    dest2.move_to_pool = true;
    dest2.color = math::Vector4<u8>(0, 0, 0, 0x80);
    dest2.tex_name = "glider-ring-dest2";
    dest2.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 17; i++) {
      auto& foam = dest2.layers.emplace_back();
      foam.tex_name = "splash-foam";
      //    :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
      foam.set_no_z_write_no_z_test();
      if (i == 16) {
        // this layer is configured, but most of the settings do nothing because it uses
        // set-alpha-texture-anim-layer-func
        foam.disable = true;
      }
      //    :alpha (new 'static 'gs-alpha :b #x2 :d #x1)
      foam.set_clamp();
      foam.set_blend_b2_d1();
    }
    dest2.set_alpha = true;
    dest2.set_times({
        {0.f, 25.f},    // 0
        {25.f, 150.f},  // 1
        {25.f, 50.f},
        {50.f, 150.f},
        {0.f, 25.f},
        {50.f, 75.f},
        {75.f, 150.f},
        {0.f, 50.f},
        {75.f, 100.f},
        {100.f, 150.f},
        {0.f, 75.f},
        {100.f, 125.f},
        {125.f, 150.f},
        {0.f, 100.f},
        {125.f, 150.f},
        {0.f, 125.f},
        {0.f, 150.f},
    });

    FixedAnimDef dest;
    dest.color = math::Vector4<u8>(0, 0, 0, 0x80);
    dest.tex_name = "glider-ring-dest";
    dest.override_size = math::Vector2<int>(128, 128);
    dest.move_to_pool = true;

    auto& ring_dest2 = dest.layers.emplace_back();
    ring_dest2.set_blend_b2_d1();
    ring_dest2.set_no_z_write_no_z_test();
    ring_dest2.tex_name = "glider-ring-dest2";
    ring_dest2.end_time = 9000.f;

    for (int i = 0; i < 2; i++) {
      auto& dsrc = dest.layers.emplace_back();
      dsrc.set_blend_b2_d1();
      dsrc.set_no_z_write_no_z_test();
      dsrc.end_time = 9000.f;
      dsrc.tex_name = "racegate";
    }

    m_hanga_sprite_anim_array_idx = create_fixed_anim_array({dest2, dest});
  }

  // foresta-water
  {
    FixedAnimDef fora_water_dest;
    fora_water_dest.tex_name = "fora-water-dest";
    fora_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    fora_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = fora_water_dest.layers.emplace_back();
      water.tex_name = "fora-water";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef fora_waterfall_dest;
    fora_waterfall_dest.tex_name = "fora-waterfall-01-dest";
    fora_waterfall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    fora_waterfall_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 3; i++) {
      auto& water = fora_waterfall_dest.layers.emplace_back();
      water.tex_name = "fora-waterfall-01";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef fora_water_wave_dest;
    fora_water_wave_dest.tex_name = "fora-water-wave-01-dest";
    fora_water_wave_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    fora_water_wave_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = fora_water_wave_dest.layers.emplace_back();
      water.tex_name = "fora-water-wave-01";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_foresta_water_anim_array_idx =
        create_fixed_anim_array({fora_water_dest, fora_waterfall_dest, fora_water_wave_dest});
  }

  // forestb-water
  {
    FixedAnimDef forb_water_dest;
    forb_water_dest.tex_name = "forb-water-dest";
    forb_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    forb_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = forb_water_dest.layers.emplace_back();
      water.tex_name = "forb-water";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef forb_waterfall_dest;
    forb_waterfall_dest.tex_name = "forb-waterfall-01-dest";
    forb_waterfall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    forb_waterfall_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 3; i++) {
      auto& water = forb_waterfall_dest.layers.emplace_back();
      water.tex_name = "forb-waterfall-01";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef forb_water_wave_dest;
    forb_water_wave_dest.tex_name = "forb-water-wave-01-dest";
    forb_water_wave_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    forb_water_wave_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = forb_water_wave_dest.layers.emplace_back();
      water.tex_name = "forb-water-wave-01";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_forestb_water_anim_array_idx =
        create_fixed_anim_array({forb_water_dest, forb_waterfall_dest, forb_water_wave_dest});
  }

  // lforplnt-pris
  {
    FixedAnimDef gem_dest;
    gem_dest.tex_name = "mh-gem-dest";
    gem_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    gem_dest.override_size = math::Vector2<int>(32, 32);

    auto& gem0 = gem_dest.layers.emplace_back();
    gem0.tex_name = "mh-gem";
    gem0.end_time = 300.f;
    gem0.set_blend_b2_d1();
    gem0.set_no_z_write_no_z_test();

    auto& gem1 = gem_dest.layers.emplace_back();
    gem1.tex_name = "mh-gem-alpha-01";
    gem1.end_time = 300.f;
    gem1.set_blend_b2_d1();
    gem1.set_no_z_write_no_z_test();

    auto& gem2 = gem_dest.layers.emplace_back();
    gem2.tex_name = "mh-gem-alpha-02";
    gem2.end_time = 300.f;
    gem2.set_blend_b2_d1();
    gem2.set_no_z_write_no_z_test();

    m_lforplnt_pris_anim_array_idx = create_fixed_anim_array({gem_dest});
  }

  // lmhcitya-tfrag
  {
    FixedAnimDef base_goo_dest;
    base_goo_dest.tex_name = "mhcitya-base-goo-01-dest";
    base_goo_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    base_goo_dest.override_size = math::Vector2<int>(128, 128);

    for (size_t i = 0; i < 3; i++) {
      auto& goo = base_goo_dest.layers.emplace_back();
      goo.tex_name = "mhcitya-base-goo-01";
      goo.end_time = 6000.f;
      goo.set_no_z_write_no_z_test();
      // :alpha (new 'static 'gs-alpha :b #x2 :c #x2 :d #x1 :fix #x2b)
      goo.blend_modes[0] = GsAlpha::BlendMode::SOURCE;
      goo.blend_modes[1] = GsAlpha::BlendMode::ZERO_OR_FIXED;
      goo.blend_modes[2] = GsAlpha::BlendMode::ZERO_OR_FIXED;
      goo.blend_modes[3] = GsAlpha::BlendMode::DEST;
      goo.blend_fix = 0x2b;
    }

    m_lmhcitya_tfrag_anim_array_idx = create_fixed_anim_array({base_goo_dest});
  }

  // lmhcityb-tfrag
  {
    FixedAnimDef base_goo_dest;
    base_goo_dest.tex_name = "mhcityb-base-goo-01-dest";
    base_goo_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    base_goo_dest.override_size = math::Vector2<int>(128, 128);

    for (size_t i = 0; i < 3; i++) {
      auto& goo = base_goo_dest.layers.emplace_back();
      goo.tex_name = "mhcityb-base-goo-01";
      goo.end_time = 6000.f;
      goo.set_no_z_write_no_z_test();
      // :alpha (new 'static 'gs-alpha :b #x2 :c #x2 :d #x1 :fix #x2b)
      goo.set_blend(GsAlpha::BlendMode::SOURCE, GsAlpha::BlendMode::ZERO_OR_FIXED,
                    GsAlpha::BlendMode::ZERO_OR_FIXED, GsAlpha::BlendMode::DEST, 0x2b);
    }
    m_lmhcityb_tfrag_anim_array_idx = create_fixed_anim_array({base_goo_dest});
  }

  // mhcitya-pris
  // destination texture is missing
  // {
  //   FixedAnimDef door_skin_dest;
  //   door_skin_dest.tex_name = "mhcity-de-door-skin-01-dest";
  //   door_skin_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
  //   door_skin_dest.override_size = math::Vector2<int>(128, 128);
  //
  //   auto& door0 = door_skin_dest.layers.emplace_back();
  //   door0.tex_name = "mhcity-de-door-skin-01";
  //   door0.end_time = 1.f;
  //   door0.set_no_z_write_no_z_test();
  //   // :alpha (new 'static 'gs-alpha :b #x2 :c #x2 :d #x1 :fix #x80)
  //   door0.set_blend(GsAlpha::BlendMode::SOURCE, GsAlpha::BlendMode::ZERO_OR_FIXED,
  //                   GsAlpha::BlendMode::ZERO_OR_FIXED, GsAlpha::BlendMode::DEST, 0x80);
  //
  //   auto& door1 = door_skin_dest.layers.emplace_back();
  //   door1.tex_name = "mhcity-de-door-skin-02";
  //   door1.end_time = 1.f;
  //   door1.set_no_z_write_no_z_test();
  //   // :alpha (new 'static 'gs-alpha :b #x2 :c #x2 :d #x1 :fix #x80)
  //   door0.set_blend(GsAlpha::BlendMode::SOURCE, GsAlpha::BlendMode::ZERO_OR_FIXED,
  //                   GsAlpha::BlendMode::ZERO_OR_FIXED, GsAlpha::BlendMode::DEST, 0x80);
  //
  //   auto& door2 = door_skin_dest.layers.emplace_back();
  //   door2.tex_name = "mhcity-de-door-skin-01";
  //   door2.end_time = 1.f;
  //   door2.set_no_z_write_no_z_test();
  //   door1.set_blend_b1_d1();
  //
  //   m_mhcitya_pris_anim_array_idx = create_fixed_anim_array({door_skin_dest});
  // }

  // templec-water
  {
    FixedAnimDef tplc_water;
    tplc_water.tex_name = "tplc-water-dest";
    tplc_water.color = math::Vector4<u8>{0, 0, 0, 0x80};
    tplc_water.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = tplc_water.layers.emplace_back();
      water.tex_name = "tplc-water";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_templec_water_anim_array_idx = create_fixed_anim_array({tplc_water});
  }

  // sewc-water
  {
    FixedAnimDef sewer_water_01_dest;
    sewer_water_01_dest.tex_name = "sewer-water-01-c-dest";
    sewer_water_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-01-c";
      water.end_time = 900.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_01_dest;
    sewer_waterfall_01_dest.tex_name = "sewer-waterfall-01-c-dest";
    sewer_waterfall_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_01_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-01-c";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-c-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-c";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_wave_01_dest;
    sewer_water_wave_01_dest.tex_name = "sewer-water-wave-01-c-dest";
    sewer_water_wave_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_wave_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-01-c";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_highlight_01_dest;
    sewer_water_highlight_01_dest.tex_name = "sewer-water-highlight-01-c-dest";
    sewer_water_highlight_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_highlight_01_dest.override_size = math::Vector2<int>(64, 32);

    for (int i = 0; i < 2; i++) {
      auto& water = sewer_water_highlight_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-highlight-01-c";
      water.end_time = 600.f;
      water.set_blend_b1_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewc_water_anim_array_idx = create_fixed_anim_array(
        {sewer_water_01_dest, sewer_waterfall_01_dest, sewer_waterfall_02_dest,
         sewer_water_wave_01_dest, sewer_water_highlight_01_dest});
  }

  // sewd-water
  {
    FixedAnimDef sewer_water_01_dest;
    sewer_water_01_dest.tex_name = "sewer-water-01-d-dest";
    sewer_water_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-01-d";
      water.end_time = 900.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-d-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-d";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_highlight_01_dest;
    sewer_water_highlight_01_dest.tex_name = "sewer-water-highlight-01-d-dest";
    sewer_water_highlight_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_highlight_01_dest.override_size = math::Vector2<int>(64, 32);

    for (int i = 0; i < 2; i++) {
      auto& water = sewer_water_highlight_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-highlight-01-d";
      water.end_time = 600.f;
      water.set_blend_b1_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_wave_01_dest;
    sewer_water_wave_01_dest.tex_name = "sewer-water-wave-01-d-dest";
    sewer_water_wave_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_01_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_wave_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-01-d";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_wave_02_dest;
    sewer_water_wave_02_dest.tex_name = "sewer-water-wave-02-d-dest";
    sewer_water_wave_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_02_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 2; i++) {
      auto& water = sewer_water_wave_02_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-02-d";
      water.end_time = 300.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_still_01_dest;
    sewer_water_still_01_dest.tex_name = "sewer-water-still-01-d-dest";
    sewer_water_still_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_still_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_still_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-still-01-d";
      water.end_time = 4500.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewd_water_anim_array_idx = create_fixed_anim_array(
        {sewer_water_01_dest, sewer_waterfall_02_dest, sewer_water_highlight_01_dest,
         sewer_water_wave_01_dest, sewer_water_wave_02_dest, sewer_water_still_01_dest});
  }

  // sewe-water
  {
    FixedAnimDef sewer_water_01_dest;
    sewer_water_01_dest.tex_name = "sewer-water-01-e-dest";
    sewer_water_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-01-e";
      water.end_time = 900.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_01_dest;
    sewer_waterfall_01_dest.tex_name = "sewer-waterfall-01-e-dest";
    sewer_waterfall_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_01_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-01-e";
      water.end_time = 450.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-e-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-e";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_highlight_01_dest;
    sewer_water_highlight_01_dest.tex_name = "sewer-water-highlight-01-e-dest";
    sewer_water_highlight_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_highlight_01_dest.override_size = math::Vector2<int>(64, 32);

    for (int i = 0; i < 2; i++) {
      auto& water = sewer_water_highlight_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-highlight-01-e";
      water.end_time = 600.f;
      water.set_blend_b1_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewe_water_anim_array_idx =
        create_fixed_anim_array({sewer_water_01_dest, sewer_waterfall_01_dest,
                                 sewer_waterfall_02_dest, sewer_water_highlight_01_dest});
  }

  // sewg-water
  {
    FixedAnimDef sewer_water_01_dest;
    sewer_water_01_dest.tex_name = "sewer-water-01-g-dest";
    sewer_water_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-01-g";
      water.end_time = 900.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-g-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-g";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_wave_01_dest;
    sewer_water_wave_01_dest.tex_name = "sewer-water-wave-01-g-dest";
    sewer_water_wave_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_wave_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-01-g";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewg_water_anim_array_idx = create_fixed_anim_array(
        {sewer_water_01_dest, sewer_waterfall_02_dest, sewer_water_wave_01_dest});
  }

  // sewh-water
  {
    FixedAnimDef sewer_water_01_dest;
    sewer_water_01_dest.tex_name = "sewer-water-01-h-dest";
    sewer_water_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-01-h";
      water.end_time = 900.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-h-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-h";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_wave_02_dest;
    sewer_water_wave_02_dest.tex_name = "sewer-water-wave-02-h-dest";
    sewer_water_wave_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_02_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 2; i++) {
      auto& water = sewer_water_wave_02_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-02-h";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_watefall_froth_01_dest;
    sewer_watefall_froth_01_dest.tex_name = "sewer-watefall-froth-01-h-dest";
    sewer_watefall_froth_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_watefall_froth_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_watefall_froth_01_dest.layers.emplace_back();
      water.tex_name = "sewer-watefall-froth-01-h";
      water.end_time = 525.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewh_water_anim_array_idx =
        create_fixed_anim_array({sewer_water_01_dest, sewer_waterfall_02_dest,
                                 sewer_water_wave_02_dest, sewer_watefall_froth_01_dest});
  }

  // sewi-water
  {
    FixedAnimDef sewer_water_still_01_dest;
    sewer_water_still_01_dest.tex_name = "sewer-water-still-01-i-dest";
    sewer_water_still_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_still_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_still_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-still-01-i";
      water.end_time = 4500.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-i-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-i";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_wave_01_dest;
    sewer_water_wave_01_dest.tex_name = "sewer-water-wave-01-i-dest";
    sewer_water_wave_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_wave_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-01-i";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewi_water_anim_array_idx = create_fixed_anim_array(
        {sewer_water_still_01_dest, sewer_waterfall_02_dest, sewer_water_wave_01_dest});
  }

  // sewj-water
  {
    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-j-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-j";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_watefall_froth_01_dest;
    sewer_watefall_froth_01_dest.tex_name = "sewer-watefall-froth-01-j-dest";
    sewer_watefall_froth_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_watefall_froth_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_watefall_froth_01_dest.layers.emplace_back();
      water.tex_name = "sewer-watefall-froth-01-j";
      water.end_time = 525.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewj_water_anim_array_idx =
        create_fixed_anim_array({sewer_waterfall_02_dest, sewer_watefall_froth_01_dest});
  }

  // sewl-water
  {
    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-l-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-l";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_watefall_froth_01_dest;
    sewer_watefall_froth_01_dest.tex_name = "sewer-watefall-froth-01-l-dest";
    sewer_watefall_froth_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_watefall_froth_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_watefall_froth_01_dest.layers.emplace_back();
      water.tex_name = "sewer-watefall-froth-01-l";
      water.end_time = 525.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewl_water_anim_array_idx =
        create_fixed_anim_array({sewer_waterfall_02_dest, sewer_watefall_froth_01_dest});
  }

  // sewm-water
  {
    FixedAnimDef sewer_water_01_dest;
    sewer_water_01_dest.tex_name = "sewer-water-01-m-dest";
    sewer_water_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-01-m";
      water.end_time = 900.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_01_dest;
    sewer_waterfall_01_dest.tex_name = "sewer-waterfall-01-m-dest";
    sewer_waterfall_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_01_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-01-m";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-m-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-m";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_highlight_01_dest;
    sewer_water_highlight_01_dest.tex_name = "sewer-water-highlight-01-m-dest";
    sewer_water_highlight_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_highlight_01_dest.override_size = math::Vector2<int>(64, 32);

    for (int i = 0; i < 2; i++) {
      auto& water = sewer_water_highlight_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-highlight-01-m";
      water.end_time = 600.f;
      water.set_blend_b1_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_wave_01_dest;
    sewer_water_wave_01_dest.tex_name = "sewer-water-wave-01-m-dest";
    sewer_water_wave_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_01_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_wave_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-01-m";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_still_01_dest;
    sewer_water_still_01_dest.tex_name = "sewer-water-still-01-m-dest";
    sewer_water_still_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_still_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_still_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-still-01-m";
      water.end_time = 4500.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_watefall_froth_01_dest;
    sewer_watefall_froth_01_dest.tex_name = "sewer-watefall-froth-01-m-dest";
    sewer_watefall_froth_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_watefall_froth_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_watefall_froth_01_dest.layers.emplace_back();
      water.tex_name = "sewer-watefall-froth-01-m";
      water.end_time = 525.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewm_water_anim_array_idx = create_fixed_anim_array(
        {sewer_water_01_dest, sewer_waterfall_01_dest, sewer_waterfall_02_dest,
         sewer_water_highlight_01_dest, sewer_water_wave_01_dest, sewer_water_still_01_dest,
         sewer_watefall_froth_01_dest});
  }

  // sewn-water
  {
    FixedAnimDef sewer_waterfall_01_dest;
    sewer_waterfall_01_dest.tex_name = "sewer-waterfall-01-n-dest";
    sewer_waterfall_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_01_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-01-n";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_waterfall_02_dest;
    sewer_waterfall_02_dest.tex_name = "sewer-waterfall-02-n-dest";
    sewer_waterfall_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_waterfall_02_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_waterfall_02_dest.layers.emplace_back();
      water.tex_name = "sewer-waterfall-02-n";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    // destination texture is missing
    // FixedAnimDef sewer_water_highlight_01_dest;
    // sewer_water_highlight_01_dest.tex_name = "sewer-water-highlight-01-m-dest";
    // sewer_water_highlight_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    // sewer_water_highlight_01_dest.override_size = math::Vector2<int>(64, 32);
    //
    // for (int i = 0; i < 2; i++) {
    //   auto& water = sewer_water_highlight_01_dest.layers.emplace_back();
    //   water.tex_name = "sewer-water-highlight-01-n";
    //   water.end_time = 600.f;
    //   water.set_blend_b1_d1();
    //   water.set_no_z_write_no_z_test();
    // }

    FixedAnimDef sewer_water_wave_01_dest;
    sewer_water_wave_01_dest.tex_name = "sewer-water-wave-01-n-dest";
    sewer_water_wave_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_wave_01_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_wave_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-wave-01-n";
      water.end_time = 1800.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef sewer_water_still_01_dest;
    sewer_water_still_01_dest.tex_name = "sewer-water-still-01-n-dest";
    sewer_water_still_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sewer_water_still_01_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = sewer_water_still_01_dest.layers.emplace_back();
      water.tex_name = "sewer-water-still-01-n";
      water.end_time = 4500.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_sewn_water_anim_array_idx = create_fixed_anim_array(
        {sewer_waterfall_01_dest, sewer_waterfall_02_dest,  // sewer_water_highlight_01_dest,
         sewer_water_wave_01_dest, sewer_water_still_01_dest});
  }

  // hanga-water
  // destination texture is missing
  // {
  //   FixedAnimDef des_thermal_01_dest;
  //   des_thermal_01_dest.tex_name = "des-thermal-01-dest";
  //   des_thermal_01_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
  //   des_thermal_01_dest.override_size = math::Vector2<int>(128, 64);
  //
  //   for (int i = 0; i < 3; i++) {
  //     auto& water = des_thermal_01_dest.layers.emplace_back();
  //     water.tex_name = "des-thermal-01";
  //     water.end_time = 1200.f;
  //     water.set_blend_b2_d1();
  //     water.set_no_z_write_no_z_test();
  //   }
  //
  //   m_hanga_water_anim_array_idx = create_fixed_anim_array({des_thermal_01_dest});
  // }

  // desresc-warp
  {
    FixedAnimDef sat_shield_dest;
    sat_shield_dest.tex_name = "sat-shield-dest";
    sat_shield_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    sat_shield_dest.override_size = math::Vector2<int>(64, 64);
    sat_shield_dest.move_to_pool = true;

    for (int i = 0; i < 2; i++) {
      auto& env_uvscroll = sat_shield_dest.layers.emplace_back();
      env_uvscroll.tex_name = "sat-shield-env-uvscroll";
      env_uvscroll.end_time = 300.f;
      env_uvscroll.set_blend_b2_d1();
      env_uvscroll.set_no_z_write_no_z_test();
      env_uvscroll.channel_masks[3] = false;
    }

    auto& shield = sat_shield_dest.layers.emplace_back();
    shield.tex_name = "sat-shield";
    shield.end_time = 300.f;
    shield.set_blend_b2_d1();
    shield.set_no_z_write_no_z_test();
    shield.channel_masks[3] = false;

    m_desresc_warp_anim_array_idx = create_fixed_anim_array({sat_shield_dest});
  }

  // templea-warp
  // templeb-warp
  // volcanox-warp
  // deshover
  // ljkdxvin
  // ltnfxhip
  {
    FixedAnimDef holograph_env_rim_dest;
    holograph_env_rim_dest.tex_name = "holograph-env-rim-dest";
    holograph_env_rim_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    holograph_env_rim_dest.move_to_pool = true;

    auto& env_noise = holograph_env_rim_dest.layers.emplace_back();
    env_noise.tex_name = "holograph-env-noise";
    env_noise.end_time = 1200.f;
    env_noise.set_blend_b2_d1();
    env_noise.set_no_z_write_no_z_test();
    env_noise.channel_masks[3] = false;  // no alpha writes.

    auto& env_scan = holograph_env_rim_dest.layers.emplace_back();
    env_scan.tex_name = "holograph-env-scan";
    env_scan.end_time = 1200.f;
    env_scan.set_blend_b2_d1();
    env_scan.set_no_z_write_no_z_test();
    env_scan.channel_masks[3] = false;  // no alpha writes.

    for (int i = 0; i < 2; i++) {
      auto& rim = holograph_env_rim_dest.layers.emplace_back();
      rim.tex_name = "holograph-env-rim";
      rim.end_time = 1200.f;
      rim.set_blend_b2_d1();
      rim.set_no_z_write_no_z_test();
      rim.channel_masks[3] = false;  // no alpha writes.
    }

    auto& phong_rim = holograph_env_rim_dest.layers.emplace_back();
    phong_rim.tex_name = "environment-phong-rim";
    phong_rim.end_time = 1200.f;
    phong_rim.set_blend_b2_d1();
    phong_rim.set_no_z_write_no_z_test();
    phong_rim.channel_masks[3] = false;  // no alpha writes.

    m_templea_warp_anim_array_idx = create_fixed_anim_array({holograph_env_rim_dest});
    m_templeb_warp_anim_array_idx = create_fixed_anim_array({holograph_env_rim_dest});
    m_volcanox_warp_anim_array_idx = create_fixed_anim_array({holograph_env_rim_dest});
    m_deshover_anim_array_idx = create_fixed_anim_array({holograph_env_rim_dest});
    m_ljkdxvin_anim_array_idx = create_fixed_anim_array({holograph_env_rim_dest});
    m_ltnfxhip_anim_array_idx = create_fixed_anim_array({holograph_env_rim_dest});
  }

  // lgunnorm-water
  {
    FixedAnimDef kg_target_forcefield_dest;
    kg_target_forcefield_dest.tex_name = "kg-target-c-forcefield-01-dest";
    kg_target_forcefield_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    kg_target_forcefield_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 6; i++) {
      auto& forcefield = kg_target_forcefield_dest.layers.emplace_back();
      forcefield.tex_name = "kg-target-c-forcefield-01";
      forcefield.set_blend_b2_d1();
      forcefield.set_no_z_write_no_z_test();
      forcefield.channel_masks[3] = false;
    }

    kg_target_forcefield_dest.set_times({
        {0.f, 75.f},
        {0.f, 75.f},
        {0.f, 75.f},
        {75.f, 150.f},
        {75.f, 150.f},
        {75.f, 150.f},
    });

    m_lgunnorm_water_anim_array_idx = create_fixed_anim_array({kg_target_forcefield_dest});
  }

  // templex-water
  {
    FixedAnimDef temple_waterfall_dest;
    temple_waterfall_dest.tex_name = "temple-waterfall-dest";
    temple_waterfall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    temple_waterfall_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& waterfall = temple_waterfall_dest.layers.emplace_back();
      waterfall.tex_name = "temple-waterfall";
      waterfall.end_time = 600.f;
      waterfall.set_blend_b2_d1();
      waterfall.set_no_z_write_no_z_test();
    }

    m_templex_water_anim_array_idx = create_fixed_anim_array({temple_waterfall_dest});
  }

  // desertd-water
  {
    FixedAnimDef des_waterfall_dest;
    des_waterfall_dest.tex_name = "des-waterfall-dest";
    des_waterfall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    des_waterfall_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& waterfall = des_waterfall_dest.layers.emplace_back();
      waterfall.tex_name = "des-waterfall";
      waterfall.end_time = 600.f;
      waterfall.set_blend_b2_d1();
      waterfall.set_no_z_write_no_z_test();
    }

    m_desertd_water_anim_array_idx = create_fixed_anim_array({des_waterfall_dest});
  }

  // towerb-water
  {
    FixedAnimDef tow_energy_bridge_dest;
    tow_energy_bridge_dest.tex_name = "tow-energy-bridge-dest";
    tow_energy_bridge_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    tow_energy_bridge_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& bridge = tow_energy_bridge_dest.layers.emplace_back();
      bridge.tex_name = "tow-energy-bridge";
      bridge.end_time = 2100.f;
      bridge.set_blend_b2_d1();
      bridge.set_no_z_write_no_z_test();
    }

    m_towerb_water_anim_array_idx = create_fixed_anim_array({tow_energy_bridge_dest});
  }

  // factoryb-water
  {
    FixedAnimDef hemi_gradient_dest;
    hemi_gradient_dest.tex_name = "hemi-gradient-dest";
    hemi_gradient_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    hemi_gradient_dest.override_size = math::Vector2<int>(64, 64);

    auto& rim = hemi_gradient_dest.layers.emplace_back();
    rim.tex_name = "hemi-gradient-rim";
    rim.end_time = 600.f;
    rim.set_blend_b2_d1();
    rim.set_no_z_write_no_z_test();
    rim.channel_masks[3] = false;

    auto& flames = hemi_gradient_dest.layers.emplace_back();
    flames.tex_name = "hemi-gradient-flames";
    flames.end_time = 600.f;
    flames.set_blend_b2_d1();
    flames.set_no_z_write_no_z_test();
    flames.channel_masks[3] = false;

    FixedAnimDef hemi_gradient_flames_dest;
    hemi_gradient_flames_dest.tex_name = "hemi-gradient-dest";
    hemi_gradient_flames_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    hemi_gradient_flames_dest.override_size = math::Vector2<int>(64, 64);

    auto& flames0 = hemi_gradient_dest.layers.emplace_back();
    flames0.tex_name = "hemi-gradient-flames";
    flames0.end_time = 600.f;
    flames0.set_blend_b2_d1();
    flames0.set_no_z_write_no_z_test();
    flames0.channel_masks[3] = false;

    m_factoryb_water_anim_array_idx =
        create_fixed_anim_array({hemi_gradient_dest, hemi_gradient_flames_dest});
  }

  // factoryc-alpha
  {
    FixedAnimDef facc_convey_dest;
    facc_convey_dest.tex_name = "facc-convey-dest";
    facc_convey_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    facc_convey_dest.override_size = math::Vector2<int>(128, 128);

    auto& convey = facc_convey_dest.layers.emplace_back();
    convey.tex_name = "facc-convey";
    convey.end_time = 300.f;
    convey.set_blend_b2_d1();
    convey.set_no_z_write_no_z_test();
    convey.channel_masks[3] = false;

    FixedAnimDef facc_convey_02_dest;
    facc_convey_02_dest.tex_name = "facc-convey-02-dest";
    facc_convey_02_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    facc_convey_02_dest.override_size = math::Vector2<int>(128, 128);

    auto& convey_02 = facc_convey_02_dest.layers.emplace_back();
    convey_02.tex_name = "facc-convey-02";
    convey_02.end_time = 300.f;
    convey_02.set_blend_b2_d1();
    convey_02.set_no_z_write_no_z_test();
    convey_02.channel_masks[3] = false;

    m_factoryc_alpha_anim_array_idx =
        create_fixed_anim_array({facc_convey_dest, facc_convey_02_dest});
  }

  // waspala-water
  {
    FixedAnimDef waspala_water_dest;
    waspala_water_dest.tex_name = "waspala-water-dest";
    waspala_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    waspala_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = waspala_water_dest.layers.emplace_back();
      water.tex_name = "waspala-water";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef waspala_waterfall_dest;
    waspala_waterfall_dest.tex_name = "waspala-waterfall-dest";
    waspala_waterfall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    waspala_waterfall_dest.override_size = math::Vector2<int>(64, 64);

    for (int i = 0; i < 3; i++) {
      auto& waterfall = waspala_waterfall_dest.layers.emplace_back();
      waterfall.tex_name = "waspala-waterfall";
      waterfall.end_time = 600.f;
      waterfall.set_blend_b2_d1();
      waterfall.set_no_z_write_no_z_test();
    }

    m_waspal_water_anim_array_idx =
        create_fixed_anim_array({waspala_water_dest, waspala_waterfall_dest});
  }

  // rubblea-water
  {
    FixedAnimDef rub_water_dest;
    rub_water_dest.tex_name = "rub-water-dest";
    rub_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    rub_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = rub_water_dest.layers.emplace_back();
      water.tex_name = "rub-water";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_rubblea_water_anim_array_idx = create_fixed_anim_array({rub_water_dest});
  }

  // rubblea2-water
  {
    FixedAnimDef ruba2_water_dest;
    ruba2_water_dest.tex_name = "rub-water-desta2";
    ruba2_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    ruba2_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = ruba2_water_dest.layers.emplace_back();
      water.tex_name = "rub-watera2";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_rubblea2_water_anim_array_idx = create_fixed_anim_array({ruba2_water_dest});
  }

  // rubbleb-water
  {
    FixedAnimDef rubb_water_dest;
    rubb_water_dest.tex_name = "rub-water-destb";
    rubb_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    rubb_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = rubb_water_dest.layers.emplace_back();
      water.tex_name = "rub-waterb";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_rubbleb_water_anim_array_idx = create_fixed_anim_array({rubb_water_dest});
  }

  // rubblec-water
  {
    FixedAnimDef rubc_water_dest;
    rubc_water_dest.tex_name = "rub-water-destc";
    rubc_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    rubc_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = rubc_water_dest.layers.emplace_back();
      water.tex_name = "rub-waterc";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_rubblec_water_anim_array_idx = create_fixed_anim_array({rubc_water_dest});
  }

  // nstb-quicksand
  {
    FixedAnimDef quicksand_dest;
    quicksand_dest.tex_name = "nstb-quicksand-dest";
    quicksand_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};

    auto& scroll = quicksand_dest.layers.emplace_back();
    scroll.tex_name = "nstb-quicksand-scroll";
    scroll.end_time = 1200.f;
    scroll.set_blend_b2_d1();
    scroll.set_no_z_write_no_z_test();
    scroll.channel_masks[3] = false;

    m_nstb_quicksand_anim_array_idx = create_fixed_anim_array({quicksand_dest});
  }

  // ctyslumb-water
  {
    FixedAnimDef ctyslumb_water_dest;
    ctyslumb_water_dest.tex_name = "ctyslumb-water-dest";
    ctyslumb_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    ctyslumb_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = ctyslumb_water_dest.layers.emplace_back();
      water.tex_name = "ctyslumb-water";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef ctyslumb_fountain_fall_dest;
    ctyslumb_fountain_fall_dest.tex_name = "ctyslumb-fountain-fall-dest";
    ctyslumb_fountain_fall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    ctyslumb_fountain_fall_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = ctyslumb_fountain_fall_dest.layers.emplace_back();
      water.tex_name = "ctyslumb-fountain-fall";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_ctyslumb_water_anim_array_idx =
        create_fixed_anim_array({ctyslumb_water_dest, ctyslumb_fountain_fall_dest});
  }

  // ctyslumc-water
  {
    FixedAnimDef ctyslumc_water_dest;
    ctyslumc_water_dest.tex_name = "ctyslumc-water-dest";
    ctyslumc_water_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    ctyslumc_water_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = ctyslumc_water_dest.layers.emplace_back();
      water.tex_name = "ctyslumc-water";
      water.end_time = 2100.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    FixedAnimDef ctyslumc_fountain_fall_dest;
    ctyslumc_fountain_fall_dest.tex_name = "ctyslumc-fountain-fall-dest";
    ctyslumc_fountain_fall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    ctyslumc_fountain_fall_dest.override_size = math::Vector2<int>(128, 128);

    for (int i = 0; i < 3; i++) {
      auto& water = ctyslumc_fountain_fall_dest.layers.emplace_back();
      water.tex_name = "ctyslumc-fountain-fall";
      water.end_time = 600.f;
      water.set_blend_b2_d1();
      water.set_no_z_write_no_z_test();
    }

    m_ctyslumc_water_anim_array_idx =
        create_fixed_anim_array({ctyslumc_water_dest, ctyslumc_fountain_fall_dest});
  }

  // mined-tfrag
  {
    FixedAnimDef mined_pillar_side_dest;
    mined_pillar_side_dest.tex_name = "mined-pillar-side-dest";
    mined_pillar_side_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    mined_pillar_side_dest.override_size = math::Vector2<int>(128, 128);

    auto& side_cold = mined_pillar_side_dest.layers.emplace_back();
    side_cold.tex_name = "mined-pillar-side-cold";
    side_cold.end_time = 300.f;
    // no alpha
    side_cold.blend_enable = false;
    side_cold.set_no_z_write_no_z_test();
    side_cold.channel_masks[3] = false;

    auto& side_cooling = mined_pillar_side_dest.layers.emplace_back();
    side_cooling.tex_name = "mined-pillar-side-cooling";
    side_cooling.end_time = 300.f;
    side_cooling.set_blend_b1_d1();
    side_cooling.set_no_z_write_no_z_test();
    side_cooling.channel_masks[3] = false;

    auto& side_hot = mined_pillar_side_dest.layers.emplace_back();
    side_hot.tex_name = "mined-pillar-side-hot";
    side_hot.end_time = 300.f;
    side_hot.set_blend_b1_d1();
    side_hot.set_no_z_write_no_z_test();
    side_hot.channel_masks[3] = false;

    auto& molten = mined_pillar_side_dest.layers.emplace_back();
    molten.tex_name = "mined-pillar-molten";
    molten.end_time = 300.f;
    molten.set_blend_b2_d1();
    molten.set_no_z_write_no_z_test();
    molten.channel_masks[3] = false;

    auto& side_cold1 = mined_pillar_side_dest.layers.emplace_back();
    side_cold1.tex_name = "mined-pillar-side-cold";
    side_cold1.end_time = 300.f;
    side_cold1.set_blend_b1_d1();
    side_cold1.set_no_z_write_no_z_test();

    FixedAnimDef mined_pillar_top2side_dest;
    mined_pillar_top2side_dest.tex_name = "mined-pillar-top2side-dest";
    mined_pillar_top2side_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    mined_pillar_top2side_dest.override_size = math::Vector2<int>(128, 64);

    auto& top2side_cold = mined_pillar_top2side_dest.layers.emplace_back();
    top2side_cold.tex_name = "mined-pillar-top2side-cold";
    top2side_cold.end_time = 300.f;
    // no alpha
    top2side_cold.blend_enable = false;
    top2side_cold.set_no_z_write_no_z_test();
    top2side_cold.channel_masks[3] = false;

    auto& top2side_cooling = mined_pillar_top2side_dest.layers.emplace_back();
    top2side_cooling.tex_name = "mined-pillar-top2side-cooling";
    top2side_cooling.end_time = 300.f;
    top2side_cooling.set_blend_b1_d1();
    top2side_cooling.set_no_z_write_no_z_test();
    top2side_cooling.channel_masks[3] = false;

    auto& top2side_hot = mined_pillar_top2side_dest.layers.emplace_back();
    top2side_hot.tex_name = "mined-pillar-top2side-hot";
    top2side_hot.end_time = 300.f;
    top2side_hot.set_blend_b1_d1();
    top2side_hot.set_no_z_write_no_z_test();
    top2side_hot.channel_masks[3] = false;

    auto& top2side_molten = mined_pillar_top2side_dest.layers.emplace_back();
    top2side_molten.tex_name = "mined-pillar-molten";
    top2side_molten.end_time = 300.f;
    top2side_molten.set_blend_b2_d1();
    top2side_molten.set_no_z_write_no_z_test();
    top2side_molten.channel_masks[3] = false;

    auto& top2side_cold1 = mined_pillar_top2side_dest.layers.emplace_back();
    top2side_cold1.tex_name = "mined-pillar-side-cold";
    top2side_cold1.end_time = 300.f;
    top2side_cold1.set_blend_b1_d1();
    top2side_cold1.set_no_z_write_no_z_test();

    FixedAnimDef mined_pillar_top_dest;
    mined_pillar_top_dest.tex_name = "mined-pillar-top-dest";
    mined_pillar_top_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    mined_pillar_top_dest.override_size = math::Vector2<int>(128, 128);

    auto& top_cold = mined_pillar_top_dest.layers.emplace_back();
    top_cold.tex_name = "mined-pillar-top-cold";
    // no alpha
    top_cold.blend_enable = false;
    top_cold.set_no_z_write_no_z_test();
    top_cold.channel_masks[3] = false;

    auto& top_cooling = mined_pillar_top_dest.layers.emplace_back();
    top_cooling.tex_name = "mined-pillar-top-cooling";
    top_cooling.set_blend_b1_d1();
    top_cooling.set_no_z_write_no_z_test();
    top_cooling.channel_masks[3] = false;

    for (size_t i = 0; i < 4; i++) {
      auto& top_hot = mined_pillar_top_dest.layers.emplace_back();
      top_hot.tex_name = "mined-pillar-top-hot";
      top_hot.set_blend_b1_d1();
      top_hot.set_no_z_write_no_z_test();
      top_hot.channel_masks[3] = false;
    }

    auto& top_molten = mined_pillar_top_dest.layers.emplace_back();
    top_molten.tex_name = "mined-pillar-molten-top";
    top_molten.set_blend_b2_d1();
    top_molten.set_no_z_write_no_z_test();
    top_molten.channel_masks[3] = false;

    auto& top_cold1 = mined_pillar_top_dest.layers.emplace_back();
    top_cold1.tex_name = "mined-pillar-top-cold";
    top_cold1.set_blend_b1_d1();
    top_cold1.set_no_z_write_no_z_test();

    mined_pillar_top_dest.set_times({
        {0.f, 300.f},
        {0.f, 300.f},
        {0.f, 300.f},
        {0.f, 150.f},
        {150.f, 300.f},
        {0.f, 150.f},
        {150.f, 300.f},
        {0.f, 300.f},
    });

    m_mined_tfrag_anim_array_idx = create_fixed_anim_array(
        {mined_pillar_side_dest, mined_pillar_top2side_dest, mined_pillar_top_dest});
  }

  // volcanoa-alpha
  {
    FixedAnimDef vola_lava_dest;
    vola_lava_dest.tex_name = "vola-lava-01-dest";
    vola_lava_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    vola_lava_dest.override_size = math::Vector2<int>(128, 128);
    vola_lava_dest.set_alpha = true;

    for (int i = 0; i < 3; i++) {
      auto& lava_fall = vola_lava_dest.layers.emplace_back();
      lava_fall.tex_name = "vola-lava-fall";
      lava_fall.end_time = 6000.f;
      lava_fall.set_blend_b2_d1();
      lava_fall.set_no_z_write_no_z_test();
    }

    // auto& alpha = vola_lava_dest.layers.emplace_back();
    // alpha.tex_name = "";
    // alpha.end_time = 2100.f;
    // alpha.set_blend_b1_d1();
    // alpha.set_no_z_write_no_z_test();
    // alpha.channel_masks[3] = false;

    FixedAnimDef vola_lava_fall_dest;
    vola_lava_fall_dest.tex_name = "vola-lava-fall-dest";
    vola_lava_fall_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    vola_lava_fall_dest.override_size = math::Vector2<int>(128, 128);
    vola_lava_fall_dest.set_alpha = true;

    for (int i = 0; i < 3; i++) {
      auto& lava_fall = vola_lava_fall_dest.layers.emplace_back();
      lava_fall.tex_name = "vola-lava-fall";
      lava_fall.end_time = 6000.f;
      lava_fall.set_blend_b2_d1();
      lava_fall.set_no_z_write_no_z_test();
    }

    // auto& alpha1 = vola_lava_fall_dest.layers.emplace_back();
    // alpha1.tex_name = "";
    // alpha1.end_time = 2100.f;
    // alpha1.set_blend_b1_d1();
    // alpha1.set_no_z_write_no_z_test();
    // alpha1.channel_masks[3] = false;

    m_volcanoa_anim_array_idx = create_fixed_anim_array({vola_lava_dest, vola_lava_fall_dest});
  }

  // wasstada-alpha
  {
    FixedAnimDef wstd_lava_base_dest;
    wstd_lava_base_dest.tex_name = "wstd-lava-base-dest";
    wstd_lava_base_dest.color = math::Vector4<u8>{0, 0, 0, 0x80};
    wstd_lava_base_dest.override_size = math::Vector2<int>(128, 128);
    wstd_lava_base_dest.set_alpha = true;

    for (int i = 0; i < 3; i++) {
      auto& lava_base = wstd_lava_base_dest.layers.emplace_back();
      lava_base.tex_name = "wstd-lava-base";
      lava_base.end_time = 6000.f;
      lava_base.set_blend_b2_d1();
      lava_base.set_no_z_write_no_z_test();
    }

    // auto& alpha = wstd_lava_base_dest.layers.emplace_back();
    // alpha.tex_name = "";
    // alpha.end_time = 2100.f;
    // alpha.set_blend_b1_d1();
    // alpha.set_no_z_write_no_z_test();
    // alpha.channel_masks[3] = false;

    m_wasstada_alpha_anim_array_idx = create_fixed_anim_array({wstd_lava_base_dest});
  }
}

void TextureAnimator::setup_texture_anims_common() {
  // Skull Gem
  {
    FixedAnimDef skull_gem;
    skull_gem.move_to_pool = true;
    skull_gem.tex_name = "skull-gem-dest";
    skull_gem.color = math::Vector4<u8>{0, 0, 0, 0x80};
    // overriden in texture-finish.gc
    skull_gem.override_size = math::Vector2<int>(32, 32);

    auto& skull_gem_0 = skull_gem.layers.emplace_back();
    skull_gem_0.end_time = 300.;
    skull_gem_0.tex_name = "skull-gem-alpha-00";
    skull_gem_0.set_blend_b2_d1();
    skull_gem_0.set_no_z_write_no_z_test();

    auto& skull_gem_1 = skull_gem.layers.emplace_back();
    skull_gem_1.end_time = 300.;
    skull_gem_1.tex_name = "skull-gem-alpha-01";
    skull_gem_1.set_blend_b2_d1();
    skull_gem_1.set_no_z_write_no_z_test();

    auto& skull_gem_2 = skull_gem.layers.emplace_back();
    skull_gem_2.end_time = 300.;
    skull_gem_2.tex_name = "skull-gem-alpha-02";
    skull_gem_2.set_blend_b2_d1();
    skull_gem_2.set_no_z_write_no_z_test();

    m_skull_gem_fixed_anim_array_idx = create_fixed_anim_array({skull_gem});
  }

  // SECURITY
  {
    FixedAnimDef env;
    env.color = math::Vector4<u8>(0, 0, 0, 0x80);
    env.tex_name = "security-env-dest";
    for (int i = 0; i < 2; i++) {
      auto& env1 = env.layers.emplace_back();
      env1.tex_name = "security-env-uscroll";
      //    :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
      env1.set_no_z_write_no_z_test();
      env1.channel_masks[3] = false;  // no alpha writes.
      //    :alpha (new 'static 'gs-alpha :b #x2 :d #x1)
      env1.set_blend_b2_d1();
      env1.end_time = 4800.f;
    }

    FixedAnimDef dot;
    dot.color = math::Vector4<u8>(0, 0, 0, 0x80);
    dot.tex_name = "security-dot-dest";

    auto& cwhite = dot.layers.emplace_back();
    cwhite.set_blend_b2_d1();
    cwhite.set_no_z_write_no_z_test();
    cwhite.tex_name = "common-white";
    cwhite.end_time = 4800.f;

    for (int i = 0; i < 2; i++) {
      auto& dsrc = dot.layers.emplace_back();
      dsrc.set_blend_b2_d1();
      dsrc.set_no_z_write_no_z_test();
      dsrc.end_time = 600.f;
      dsrc.tex_name = "security-dot-src";
    }

    m_security_anim_array_idx = create_fixed_anim_array({env, dot});
    if (this->m_version == GameVersion::Jak3) {
      m_comb_field_anim_array_idx = create_fixed_anim_array({env, dot});
    }
  }
}

void TextureAnimator::setup_texture_anims_jak2() {
  // DARKJAK
  m_darkjak_clut_blender_idx = create_clut_blender_group(
      {"jakbsmall-eyebrow", "jakbsmall-face", "jakbsmall-finger", "jakbsmall-hair"}, "-norm",
      "-dark", {});

  // PRISON
  // MISSING EYELID
  m_jakb_prison_clut_blender_idx = create_clut_blender_group(
      {"jak-orig-arm-formorph", "jak-orig-eyebrow-formorph", "jak-orig-finger-formorph"}, "-start",
      "-end", "LDJAKBRN.DGO");
  add_to_clut_blender_group(m_jakb_prison_clut_blender_idx,
                            {"jakb-facelft", "jakb-facert", "jakb-hairtrans"}, "-norm", "-dark",
                            "LDJAKBRN.DGO");

  // ORACLE
  // MISSING FINGER
  m_jakb_oracle_clut_blender_idx = create_clut_blender_group(
      {"jakb-eyebrow", "jakb-eyelid", "jakb-facelft", "jakb-facert", "jakb-hairtrans"}, "-norm",
      "-dark", "ORACLE.DGO", true);

  // NEST
  // MISSING FINGER
  m_jakb_nest_clut_blender_idx = create_clut_blender_group(
      {"jakb-eyebrow", "jakb-eyelid", "jakb-facelft", "jakb-facert", "jakb-hairtrans"}, "-norm",
      "-dark", "NEB.DGO", true);

  // KOR (doesn't work??)
  m_kor_transform_clut_blender_idx = create_clut_blender_group(
      {
          // "kor-eyeeffect-formorph",
          // "kor-hair-formorph",
          // "kor-head-formorph",
          // "kor-head-formorph-noreflect",
          // "kor-lowercaps-formorph",
          // "kor-uppercaps-formorph",
      },
      "-start", "-end", {});

  // Bomb
  {
    FixedAnimDef bomb;
    bomb.tex_name = "bomb-gradient";
    bomb.color = math::Vector4<u8>{0, 0, 0, 0x80};

    auto& bomb_0 = bomb.layers.emplace_back();
    bomb_0.end_time = 300.;
    bomb_0.tex_name = "bomb-gradient-rim";
    bomb_0.set_blend_b2_d1();
    bomb_0.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    bomb_0.channel_masks[3] = false;  // no alpha writes.

    auto& bomb_1 = bomb.layers.emplace_back();
    bomb_1.end_time = 300.;
    bomb_1.tex_name = "bomb-gradient-flames";
    bomb_1.set_blend_b2_d1();
    bomb_1.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    bomb_1.channel_masks[3] = false;  // no alpha writes.

    m_bomb_fixed_anim_array_idx = create_fixed_anim_array({bomb});
  }

  // CAS conveyor
  {
    FixedAnimDef conveyor_0;
    conveyor_0.tex_name = "cas-conveyor-dest";
    conveyor_0.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_0.override_size = math::Vector2<int>(64, 32);
    auto& c0 = conveyor_0.layers.emplace_back();
    c0.set_blend_b2_d1();
    c0.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c0.channel_masks[3] = false;  // no alpha writes.
    c0.end_time = 300.;
    c0.tex_name = "cas-conveyor";

    FixedAnimDef conveyor_1;
    conveyor_1.tex_name = "cas-conveyor-dest-01";
    conveyor_1.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_1.override_size = math::Vector2<int>(64, 32);
    auto& c1 = conveyor_1.layers.emplace_back();
    c1.set_blend_b2_d1();
    c1.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c1.channel_masks[3] = false;  // no alpha writes.
    c1.end_time = 300.;
    c1.tex_name = "cas-conveyor";

    FixedAnimDef conveyor_2;
    conveyor_2.tex_name = "cas-conveyor-dest-02";
    conveyor_2.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_2.override_size = math::Vector2<int>(64, 32);
    auto& c2 = conveyor_2.layers.emplace_back();
    c2.set_blend_b2_d1();
    c2.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c2.channel_masks[3] = false;  // no alpha writes.
    c2.end_time = 300.;
    c2.tex_name = "cas-conveyor";

    FixedAnimDef conveyor_3;
    conveyor_3.tex_name = "cas-conveyor-dest-03";
    conveyor_3.color = math::Vector4<u8>(0, 0, 0, 0x80);
    conveyor_3.override_size = math::Vector2<int>(64, 32);
    auto& c3 = conveyor_3.layers.emplace_back();
    c3.set_blend_b2_d1();
    c3.set_no_z_write_no_z_test();
    // :test (new 'static 'gs-test :ate #x1 :afail #x3 :zte #x1 :ztst (gs-ztest always))
    c3.channel_masks[3] = false;  // no alpha writes.
    c3.end_time = 300.;
    c3.tex_name = "cas-conveyor";

    m_cas_conveyor_anim_array_idx =
        create_fixed_anim_array({conveyor_0, conveyor_1, conveyor_2, conveyor_3});
  }

  // WATERFALL
  {
    FixedAnimDef waterfall;
    waterfall.color = math::Vector4<u8>(0, 0, 0, 0x80);
    waterfall.tex_name = "waterfall-dest";
    for (int i = 0; i < 4; i++) {
      auto& src = waterfall.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 450.f;
      src.tex_name = "waterfall";
    }
    m_waterfall_anim_array_idx = create_fixed_anim_array({waterfall});
  }

  {
    FixedAnimDef waterfall;
    waterfall.color = math::Vector4<u8>(0, 0, 0, 0x80);
    waterfall.tex_name = "waterfall-dest";
    for (int i = 0; i < 4; i++) {
      auto& src = waterfall.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 450.f;
      src.tex_name = "waterfall";
    }
    m_waterfall_b_anim_array_idx = create_fixed_anim_array({waterfall});
  }

  // LAVA
  {
    FixedAnimDef lava;
    lava.color = math::Vector4<u8>(0, 0, 0, 0x80);
    lava.tex_name = "dig-lava-01-dest";
    for (int i = 0; i < 2; i++) {
      auto& src = lava.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 3600.f;
      src.tex_name = "dig-lava-01";
    }
    m_lava_anim_array_idx = create_fixed_anim_array({lava});
  }

  {
    FixedAnimDef lava;
    lava.color = math::Vector4<u8>(0, 0, 0, 0x80);
    lava.tex_name = "dig-lava-01-dest";
    for (int i = 0; i < 2; i++) {
      auto& src = lava.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 3600.f;
      src.tex_name = "dig-lava-01";
    }
    m_lava_b_anim_array_idx = create_fixed_anim_array({lava});
  }

  // Stadiumb
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "stdmb-energy-wall-01-dest";
    for (int i = 0; i < 2; i++) {
      auto& src = def.layers.emplace_back();
      src.set_blend_b1_d1();
      src.set_no_z_write_no_z_test();
      src.end_time = 300.f;
      src.tex_name = "stdmb-energy-wall-01";
    }
    m_stadiumb_anim_array_idx = create_fixed_anim_array({def});
  }

  // Fortress pris
  {
    FixedAnimDef l_tread;
    l_tread.color = math::Vector4<u8>(0, 0, 0, 0x80);
    l_tread.tex_name = "robotank-tread-l-dest";
    auto& l_src = l_tread.layers.emplace_back();
    l_src.set_blend_b1_d1();
    l_src.set_no_z_write_no_z_test();
    l_src.channel_masks[3] = false;  // no alpha writes.
    l_src.end_time = 1.f;
    l_src.tex_name = "robotank-tread";

    FixedAnimDef r_tread;
    r_tread.color = math::Vector4<u8>(0, 0, 0, 0x80);
    r_tread.tex_name = "robotank-tread-r-dest";
    auto& r_src = r_tread.layers.emplace_back();
    r_src.set_blend_b1_d1();
    r_src.set_no_z_write_no_z_test();
    r_src.channel_masks[3] = false;  // no alpha writes.
    r_src.end_time = 1.f;
    r_src.tex_name = "robotank-tread";

    m_fortress_pris_anim_array_idx = create_fixed_anim_array({l_tread, r_tread});
  }

  // Fortress Warp
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.move_to_pool = true;
    def.tex_name = "fort-roboscreen-dest";
    auto& src = def.layers.emplace_back();
    src.set_blend_b2_d1();
    src.channel_masks[3] = false;  // no alpha writes.
    src.set_no_z_write_no_z_test();
    src.end_time = 300.f;
    src.tex_name = "fort-roboscreen-env";
    m_fortress_warp_anim_array_idx = create_fixed_anim_array({def});
  }

  // metkor
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "squid-env-rim-dest";
    def.move_to_pool = true;
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-noise";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-scan";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "environment-phong-rim";
    }
    m_metkor_anim_array_idx = create_fixed_anim_array({def});
  }

  // shield
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "squid-env-rim-dest";
    def.move_to_pool = true;

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "common-white";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-uscroll";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-uscroll";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-rim-src";
    }

    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "squid-env-rim-src";
    }
    m_shield_anim_array_idx = create_fixed_anim_array({def});
  }

  // krew
  {
    FixedAnimDef def;
    def.color = math::Vector4<u8>(0, 0, 0, 0x80);
    def.tex_name = "krew-holo-dest";
    def.move_to_pool = true;
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-noise";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-scan";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-head-env-rim";
    }
    {
      auto& src = def.layers.emplace_back();
      src.set_blend_b2_d1();
      src.channel_masks[3] = false;  // no alpha writes.
      src.set_no_z_write_no_z_test();
      src.set_clamp();
      src.end_time = 1200.f;
      src.tex_name = "metkor-phong-env";
    }
    m_krew_holo_anim_array_idx = create_fixed_anim_array({def});
  }

  {
    m_slime_output_slot = output_slot_by_idx(m_version, "cas-toxic-slime-dest");
    m_slime_scroll_output_slot = output_slot_by_idx(m_version, "cas-toxic-slime-scroll-dest");
    const float max_times[4] = {600.f, 300.f, 150.f, 75.f};
    const float scales[4] = {0.55, 0.6, 0.3, 0.1f};
    for (int i = 0, dim = kFinalSlimeTextureSize >> (kNumSlimeNoiseLayers - 1);
         i < kNumSlimeNoiseLayers; i++, dim *= 2) {
      auto& tex = m_slime_noise_textures[i];
      tex.temp_data.resize(dim * dim);
      tex.max_time = max_times[i];
      tex.scale = scales[i];
      tex.dim = dim;
      glGenTextures(1, &tex.new_tex);
      m_random_index = update_opengl_noise_texture(tex.new_tex, tex.temp_data.data(),
                                                   m_random_table, dim, m_random_index);
      glGenTextures(1, &tex.old_tex);
      m_random_index = update_opengl_noise_texture(tex.old_tex, tex.temp_data.data(),
                                                   m_random_table, dim, m_random_index);
    }
  }
}