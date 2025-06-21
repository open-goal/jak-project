#pragma once

#include <optional>
#include <set>
#include <unordered_map>
#include <vector>

#include "common/custom_data/Tfrag3Data.h"
#include "common/dma/dma_chain_read.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"
#include "common/texture/texture_conversion.h"

#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/texture/TextureConverter.h"
#include "game/graphics/texture/TextureID.h"

struct GpuTexture;

struct VramEntry {
  enum class Kind {
    CLUT16_16_IN_PSM32,
    GENERIC_PSM32,
    GENERIC_PSMT8,
    GENERIC_PSMT4,
    GPU,
    INVALID
  } kind;
  std::vector<u8> data;

  int tex_width = 0;
  int tex_height = 0;
  int dest_texture_address = 0;
  int cbp = 0;
  std::optional<FramebufferTexturePair> tex;

  bool needs_pool_update = false;
  GpuTexture* pool_gpu_tex = nullptr;

  void reset() {
    data.clear();
    kind = Kind::INVALID;
    tex_height = 0;
    tex_width = 0;
    cbp = 0;
    needs_pool_update = false;
  }
};

struct ShaderContext {
  GsTex0 tex0;
  GsTex1 tex1;
  GsTest test;
  bool clamp_u, clamp_v;
  GsAlpha alpha;
  bool source_texture_set = false;
};

struct OpenGLTexturePool {
  OpenGLTexturePool(GameVersion version);
  ~OpenGLTexturePool();
  GLuint allocate(u64 w, u64 h);
  void free(GLuint texture, u64 w, u64 h);
  std::unordered_map<u64, std::vector<GLuint>> textures;
};

class ClutBlender {
 public:
  ClutBlender(const std::string& dest,
              const std::array<std::string, 2>& sources,
              const std::optional<std::string>& level_name,
              const tfrag3::Level* level,
              OpenGLTexturePool* tpool);
  GLuint run(const float* weights);
  GLuint texture() const { return m_texture; }
  bool at_default() const { return m_current_weights[0] == 1.f && m_current_weights[1] == 0.f; }

  int w() const { return m_dest->w; }
  int h() const { return m_dest->h; }
  GpuTexture* pool_gpu_tex = nullptr;

 private:
  const tfrag3::IndexTexture* m_dest;
  std::array<const std::array<math::Vector4<u8>, 256>*, 2> m_cluts;
  std::array<float, 2> m_current_weights;
  GLuint m_texture;
  std::array<math::Vector4<u8>, 256> m_temp_clut;
  std::vector<u32> m_temp_rgba;
};

struct Psm32ToPsm8Scrambler {
  Psm32ToPsm8Scrambler(int w, int h, int write_tex_width, int read_tex_width);
  std::vector<int> destinations_per_byte;
};

struct ClutReader {
  std::array<int, 256> addrs;
  ClutReader() {
    for (int i = 0; i < 256; i++) {
      u32 clut_chunk = i / 16;
      u32 off_in_chunk = i % 16;
      u8 clx = 0, cly = 0;
      if (clut_chunk & 1) {
        clx = 8;
      }
      cly = (clut_chunk >> 1) * 2;
      if (off_in_chunk >= 8) {
        off_in_chunk -= 8;
        cly++;
      }
      clx += off_in_chunk;

      // the x, y CLUT value is looked up in PSMCT32 mode
      u32 clut_addr = clx + cly * 16;
      ASSERT(clut_addr < 256);
      addrs[i] = clut_addr;
    }
  }
};

struct LayerVals {
  math::Vector4f color = math::Vector4f::zero();
  math::Vector2f scale = math::Vector2f::zero();
  math::Vector2f offset = math::Vector2f::zero();
  math::Vector2f st_scale = math::Vector2f::zero();
  math::Vector2f st_offset = math::Vector2f::zero();
  math::Vector4f qs = math::Vector4f(1, 1, 1, 1);
  float rot = 0;
  float st_rot = 0;
  u8 pad[8];
};
static_assert(sizeof(LayerVals) == 80);

/*!
 * A single layer in a FixedAnimationDef.
 */
struct FixedLayerDef {
  enum class Kind {
    DEFAULT_ANIM_LAYER,
  } kind = Kind::DEFAULT_ANIM_LAYER;
  float start_time = 0;
  float end_time = 0;
  std::string tex_name;
  bool z_writes = false;
  bool z_test = false;
  bool clamp_u = false;
  bool clamp_v = false;
  bool blend_enable = true;
  bool channel_masks[4] = {true, true, true, true};
  bool disable = false;
  GsAlpha::BlendMode blend_modes[4];  // abcd
  u8 blend_fix = 0;

  void set_blend(GsAlpha::BlendMode a,
                 GsAlpha::BlendMode b,
                 GsAlpha::BlendMode c,
                 GsAlpha::BlendMode d,
                 u8 fix) {
    blend_modes[0] = a;
    blend_modes[1] = b;
    blend_modes[2] = c;
    blend_modes[3] = d;
    blend_fix = fix;
  }

  void set_blend_b2_d1() {
    blend_modes[0] = GsAlpha::BlendMode::SOURCE;
    blend_modes[1] = GsAlpha::BlendMode::ZERO_OR_FIXED;
    blend_modes[2] = GsAlpha::BlendMode::SOURCE;
    blend_modes[3] = GsAlpha::BlendMode::DEST;
    blend_fix = 0;
  }

  void set_blend_b1_d1() {
    blend_modes[0] = GsAlpha::BlendMode::SOURCE;
    blend_modes[1] = GsAlpha::BlendMode::DEST;
    blend_modes[2] = GsAlpha::BlendMode::SOURCE;
    blend_modes[3] = GsAlpha::BlendMode::DEST;
    blend_fix = 0;
  }
  void set_no_z_write_no_z_test() {
    z_writes = false;
    z_test = false;
  }
  void set_clamp() {
    clamp_v = true;
    clamp_u = true;
  }
};

struct FixedAnimDef {
  math::Vector4<u8> color;  // clear color
  std::string tex_name;
  std::optional<math::Vector2<int>> override_size;
  // assuming (new 'static 'gs-test :ate #x1 :afail #x1 :zte #x1 :ztst (gs-ztest always))
  // alpha blend off, so alpha doesn't matter i think.
  std::vector<FixedLayerDef> layers;
  bool move_to_pool = false;
  bool set_alpha = false;
  void set_times(std::vector<std::pair<float, float>> times) {
    ASSERT(layers.size() >= times.size());
    for (size_t i = 0; i < layers.size(); i++) {
      auto& layer = layers.at(i);
      layer.start_time = times.at(i).first;
      layer.end_time = times.at(i).second;
    }
  }
};

struct DynamicLayerData {
  LayerVals start_vals, end_vals;
};

struct FixedAnimSource {
  u64 idx = 0;
  bool is_anim_slot = false;
};

struct FixedAnim {
  FixedAnimDef def;
  std::vector<DynamicLayerData> dynamic_data;
  std::optional<FramebufferTexturePair> fbt;
  int dest_slot;
  std::vector<FixedAnimSource> src_textures;
  GpuTexture* pool_gpu_tex = nullptr;
};

struct FixedAnimArray {
  std::vector<FixedAnim> anims;
};

/*
 (deftype sky-input (structure)
  ((fog-height float)
   (cloud-min  float)
   (cloud-max  float)
   (times      float 9)
   (cloud-dest int32)
   )
  )
 */

struct SkyInput {
  float fog_height;
  float cloud_min;
  float cloud_max;
  float times[11];
  float max_times[6];
  float scales[6];
  int32_t cloud_dest;
};

struct SlimeInput {
  // float alphas[4];
  float times[9];
  int32_t dest;
  int32_t scroll_dest;
};

using Vector16ub = math::Vector<u8, 16>;

struct NoiseTexturePair {
  GLuint old_tex = 0;
  GLuint new_tex = 0;
  std::vector<u8> temp_data;
  int dim = 0;
  float scale = 0;
  float last_time = 0;
  float max_time = 0;
};

class TexturePool;

class TextureAnimator {
 public:
  TextureAnimator(ShaderLibrary& shaders, const tfrag3::Level* common_level, GameVersion version);
  ~TextureAnimator();
  void handle_texture_anim_data(DmaFollower& dma,
                                const u8* ee_mem,
                                TexturePool* texture_pool,
                                u64 frame_idx);
  GLuint get_by_slot(int idx);
  void draw_debug_window();
  const std::vector<GLuint>* slots() { return &m_public_output_slots; }
  void clear_stale_textures(u64 frame_idx);

 private:
  void copy_private_to_public();
  void setup_texture_anims_common();
  void setup_texture_anims_jak2();
  void setup_texture_anims_jak3();

  void setup_sky();
  void handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem);
  void handle_generic_upload(const DmaTransfer& tf, const u8* ee_mem);
  void handle_clouds_and_fog(const DmaTransfer& tf, TexturePool* texture_pool, bool hires);
  void handle_slime(const DmaTransfer& tf, TexturePool* texture_pool);
  void handle_erase_dest(DmaFollower& dma);
  void handle_set_shader(DmaFollower& dma);
  void handle_draw(DmaFollower& dma, TexturePool& texture_pool);

  VramEntry* setup_vram_entry_for_gpu_texture(int w, int h, int tbp);
  void set_up_opengl_for_fixed(const FixedLayerDef& def, std::optional<GLint> texture);
  bool set_up_opengl_for_shader(const ShaderContext& shader,
                                std::optional<GLuint> texture,
                                bool prim_abe);
  GLuint make_temp_gpu_texture(const u32* data, u32 width, u32 height);

  GLuint make_or_get_gpu_texture_for_current_shader(TexturePool& texture_pool);
  const u32* get_clut_16_16_psm32(int cbp);
  void load_clut_to_converter();
  void force_to_gpu(int tbp);

  int create_fixed_anim_array(const std::vector<FixedAnimDef>& defs);
  void run_fixed_animation_array(int idx, const DmaTransfer& transfer, TexturePool* texture_pool);
  void run_fixed_animation(FixedAnim& anim, float time);

  struct DrawData {
    u8 tmpl1[16];
    math::Vector<u32, 4> color;

    math::Vector<float, 4> st0;
    math::Vector<u32, 4> pos0;

    math::Vector<float, 4> st1;
    math::Vector<u32, 4> pos1;

    math::Vector<float, 4> st2;
    math::Vector<u32, 4> pos2;

    math::Vector<float, 4> st3;
    math::Vector<u32, 4> pos3;
  };

  void set_uniforms_from_draw_data(const DrawData& dd, int dest_w, int dest_h);
  void set_draw_data_from_interpolated(DrawData* result, const LayerVals& vals, int w, int h);

  PcTextureId get_id_for_tbp(TexturePool* pool, u64 tbp, u64 other_id);

  VramEntry* m_tex_looking_for_clut = nullptr;
  const tfrag3::Level* m_common_level = nullptr;
  std::unordered_map<u32, VramEntry> m_textures;
  std::unordered_map<u64, PcTextureId> m_ids_by_vram;

  std::set<u32> m_force_to_gpu;  // rename? or rework to not need?

  struct TempTexture {
    GLuint tex;
    u32 w, h;
  };
  TextureConverter m_converter;
  std::vector<TempTexture> m_in_use_temp_textures;
  ShaderContext m_current_shader;
  GLuint m_vao;
  GLuint m_vertex_buffer;
  struct Vertex {
    u32 index;
    u32 pad1;
    u32 pad2;
    u32 pad3;
  };

  struct {
    GLuint rgba;
    GLuint positions;
    GLuint uvs;
    GLuint enable_tex;
    GLuint slime_scroll;
    GLuint channel_scramble;
    GLuint tcc;
    GLuint alpha_multiply;
    GLuint minimum, maximum;
    GLuint set_alpha;
  } m_uniforms;

  struct {
    bool use_fast_scrambler = true;
  } m_debug;

  GLuint m_shader_id;
  GLuint m_dummy_texture;
  GLuint m_slime_lut_texture;

  u8 m_index_to_clut_addr[256];
  OpenGLTexturePool m_opengl_texture_pool;
  int m_current_dest_tbp = -1;

  std::vector<GLuint> m_private_output_slots;
  std::vector<GLuint> m_public_output_slots;
  std::vector<int> m_skip_tbps;

  struct Bool {
    bool b = false;
  };
  std::vector<Bool> m_output_debug_flags;

  struct ClutBlenderGroup {
    std::vector<ClutBlender> blenders;
    std::vector<int> outputs;
    u64 last_updated_frame = 0;
    bool move_to_pool = false;
  };
  std::vector<ClutBlenderGroup> m_clut_blender_groups;

  int m_darkjak_clut_blender_idx = -1;
  int m_jakb_prison_clut_blender_idx = -1;
  int m_jakb_oracle_clut_blender_idx = -1;
  int m_jakb_nest_clut_blender_idx = -1;
  int m_kor_transform_clut_blender_idx = -1;

  int create_clut_blender_group(const std::vector<std::string>& textures,
                                const std::string& suffix0,
                                const std::string& suffix1,
                                const std::optional<std::string>& dgo,
                                bool send_to_pool = false);
  void add_to_clut_blender_group(int idx,
                                 const std::vector<std::string>& textures,
                                 const std::string& suffix0,
                                 const std::string& suffix1,
                                 const std::optional<std::string>& dgo);
  void run_clut_blender_group(DmaTransfer& tf, int idx, u64 frame_idx, TexturePool* texture_pool);
  GLint run_clouds(const SkyInput& input, bool hires);
  void run_slime(const SlimeInput& input);

  Psm32ToPsm8Scrambler m_psm32_to_psm8_8_8, m_psm32_to_psm8_16_16, m_psm32_to_psm8_32_32,
      m_psm32_to_psm8_64_64;
  ClutReader m_clut_table;

 public:
  // jak 2
  int m_skull_gem_fixed_anim_array_idx = -1;
  int m_bomb_fixed_anim_array_idx = -1;
  int m_cas_conveyor_anim_array_idx = -1;
  int m_security_anim_array_idx = -1;
  int m_waterfall_anim_array_idx = -1;
  int m_waterfall_b_anim_array_idx = -1;
  int m_lava_anim_array_idx = -1;
  int m_lava_b_anim_array_idx = -1;
  int m_stadiumb_anim_array_idx = -1;
  int m_fortress_pris_anim_array_idx = -1;
  int m_fortress_warp_anim_array_idx = -1;
  int m_metkor_anim_array_idx = -1;
  int m_shield_anim_array_idx = -1;
  int m_krew_holo_anim_array_idx = -1;

  // jak 3
  int m_default_water_anim_array_idx = -1;
  int m_default_warp_anim_array_idx = -1;
  int m_templea_water_anim_array_idx = -1;
  int m_templea_warp_anim_array_idx = -1;
  int m_templeb_warp_anim_array_idx = -1;
  int m_templec_water_anim_array_idx = -1;
  int m_sewc_water_anim_array_idx = -1;
  int m_sewd_water_anim_array_idx = -1;
  int m_sewe_water_anim_array_idx = -1;
  int m_sewg_water_anim_array_idx = -1;
  int m_sewh_water_anim_array_idx = -1;
  int m_sewi_water_anim_array_idx = -1;
  int m_sewj_water_anim_array_idx = -1;
  int m_sewl_water_anim_array_idx = -1;
  int m_sewm_water_anim_array_idx = -1;
  int m_sewn_water_anim_array_idx = -1;
  int m_desresc_warp_anim_array_idx = -1;
  int m_ctyslumb_water_anim_array_idx = -1;
  int m_nstb_quicksand_anim_array_idx = -1;
  int m_ctyslumc_water_anim_array_idx = -1;
  int m_factoryc_alpha_anim_array_idx = -1;
  int m_hfrag_anim_array_idx = -1;
  int m_hanga_sprite_anim_array_idx = -1;
  int m_hanga_water_anim_array_idx = -1;
  int m_desertd_water_anim_array_idx = -1;
  int m_lmhcityb_tfrag_anim_array_idx = -1;
  int m_towerb_water_anim_array_idx = -1;
  int m_comb_field_anim_array_idx = -1;
  int m_wasstada_alpha_anim_array_idx = -1;
  int m_factoryb_water_anim_array_idx = -1;
  int m_lmhcitya_tfrag_anim_array_idx = -1;
  int m_mhcitya_pris_anim_array_idx = -1;
  int m_rubblea_water_anim_array_idx = -1;
  int m_rubblea2_water_anim_array_idx = -1;
  int m_rubbleb_water_anim_array_idx = -1;
  int m_rubblec_water_anim_array_idx = -1;
  int m_foresta_water_anim_array_idx = -1;
  int m_forestb_water_anim_array_idx = -1;
  int m_lforplnt_pris_anim_array_idx = -1;
  int m_ltnfxhip_anim_array_idx = -1;
  int m_lgunnorm_water_anim_array_idx = -1;
  int m_ljkdxvin_anim_array_idx = -1;
  int m_waspal_water_anim_array_idx = -1;
  int m_mined_tfrag_anim_array_idx = -1;
  int m_volcanox_warp_anim_array_idx = -1;
  int m_templex_water_anim_array_idx = -1;
  int m_volcanoa_anim_array_idx = -1;
  int m_deshover_anim_array_idx = -1;
  int m_darkjak_highres_clut_blender_idx = -1;

  std::vector<FixedAnimArray> m_fixed_anim_arrays;

  // note: for now these can't be easily changed because each layer has its own hand-tuned
  // parameters from the original game. If you want to change it, you'll need to make up parameters
  // for those new layers.
  // must be power of 2 - number of 16-byte rows in random table. (original
  // game has 8)
  static constexpr int kRandomTableSize = 8;

  // must be power of 2 - dimensions of the final clouds textures
  static constexpr int kFinalSkyTextureSize = 128;
  static constexpr int kFinalSkyHiresTextureSize = 512;
  static constexpr int kFinalSlimeTextureSize = 128;

  // number of small sub-textures. Must be less than log2(kFinalTextureSize).
  static constexpr int kNumSkyNoiseLayers = 4;
  static constexpr int kNumSkyHiresNoiseLayers = 6;
  static constexpr int kNumSlimeNoiseLayers = 4;

 private:
  GameVersion m_version;
  Vector16ub m_random_table[kRandomTableSize];
  int m_random_index = 0;

  SkyInput m_debug_sky_input;
  NoiseTexturePair m_sky_noise_textures[kNumSkyNoiseLayers];
  FramebufferTexturePair m_sky_blend_texture;
  FramebufferTexturePair m_sky_final_texture;
  GpuTexture* m_sky_pool_gpu_tex = nullptr;
  NoiseTexturePair m_sky_hires_noise_textures[kNumSkyHiresNoiseLayers];
  FramebufferTexturePair m_sky_hires_blend_texture;
  FramebufferTexturePair m_sky_hires_final_texture;
  GpuTexture* m_sky_hires_pool_gpu_tex = nullptr;

  SlimeInput m_debug_slime_input;
  NoiseTexturePair m_slime_noise_textures[kNumSlimeNoiseLayers];
  FramebufferTexturePair m_slime_blend_texture;
  FramebufferTexturePair m_slime_final_texture, m_slime_final_scroll_texture;
  GpuTexture* m_slime_pool_gpu_tex = nullptr;
  GpuTexture* m_slime_scroll_pool_gpu_tex = nullptr;
  int m_slime_output_slot = -1;
  int m_slime_scroll_output_slot = -1;
  ShaderLibrary* m_shaders = nullptr;
};

int output_slot_by_idx(GameVersion version, const std::string& name);
int update_opengl_noise_texture(GLuint texture,
                                u8* temp,
                                Vector16ub* random_table,
                                int dim,
                                int random_index_in);
