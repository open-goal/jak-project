#include "game/graphics/opengl_renderer/TextureAnimator.h"

void TextureAnimator::setup_texture_anims_jak3() {
  m_darkjak_clut_blender_idx = create_clut_blender_group(
      {"jakc-arm", "jakc-eyebrow", "jakc-face", "jakc-finger", "jakc-hair"}, "-norm", "-dark", {});
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
      "-dark", "ORACLE.DGO");

  // NEST
  // MISSING FINGER
  m_jakb_nest_clut_blender_idx = create_clut_blender_group(
      {"jakb-eyebrow", "jakb-eyelid", "jakb-facelft", "jakb-facert", "jakb-hairtrans"}, "-norm",
      "-dark", "NEB.DGO");

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