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
  enum class Kind { CLUT16_16_IN_PSM32, GENERIC_PSM32, GENERIC_PSMT8, GPU, INVALID } kind;
  std::vector<u8> data;

  int tex_width = 0;
  int tex_height = 0;
  int dest_texture_address = 0;
  int cbp = 0;
  std::optional<FramebufferTexturePair> tex;
  // math::Vector<u8, 4> rgba_clear;

  bool needs_pool_update = false;
  GpuTexture* pool_gpu_tex = nullptr;

  void reset() {
    data.clear();
    kind = Kind::INVALID;
    tex_height = 0;
    tex_width = 0;
    cbp = 0;
    // tex.reset();
    needs_pool_update = false;
    // pool_gpu_tex = nullptr;
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
  OpenGLTexturePool();
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
  GsAlpha::BlendMode blend_modes[4];  // abcd
  u8 blend_fix = 0;

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
};

struct DynamicLayerData {
  LayerVals start_vals, end_vals;
};

struct FixedAnim {
  FixedAnimDef def;
  std::vector<DynamicLayerData> dynamic_data;
  // GLint dest_texture;
  std::optional<FramebufferTexturePair> fbt;
  int dest_slot;
  std::vector<GLint> src_textures;

  GpuTexture* pool_gpu_tex = nullptr;
};

struct FixedAnimArray {
  std::vector<FixedAnim> anims;
};

class TexturePool;

class TextureAnimator {
 public:
  TextureAnimator(ShaderLibrary& shaders, const tfrag3::Level* common_level);
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
  void setup_texture_anims();
  void handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem);
  void handle_generic_upload(const DmaTransfer& tf, const u8* ee_mem);
  void handle_erase_dest(DmaFollower& dma);
  void handle_set_shader(DmaFollower& dma);
  void handle_draw(DmaFollower& dma, TexturePool& texture_pool);
  void handle_rg_to_ba(const DmaTransfer& tf);
  void handle_set_clut_alpha(const DmaTransfer& tf);
  void handle_copy_clut_alpha(const DmaTransfer& tf);

  VramEntry* setup_vram_entry_for_gpu_texture(int w, int h, int tbp);

  bool set_up_opengl_for_shader(const ShaderContext& shader,
                                std::optional<GLuint> texture,
                                bool prim_abe);
  void set_up_opengl_for_fixed(const FixedLayerDef& def, std::optional<GLint> texture);

  void load_clut_to_converter();
  const u32* get_clut_16_16_psm32(int cbp);

  GLuint make_temp_gpu_texture(const u32* data, u32 width, u32 height);

  GLuint make_or_get_gpu_texture_for_current_shader(TexturePool& texture_pool);
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

  std::set<u32> m_erased_on_this_frame;

  struct TempTexture {
    GLuint tex;
    u32 w, h;
  };
  std::vector<TempTexture> m_in_use_temp_textures;

  ShaderContext m_current_shader;
  TextureConverter m_converter;
  int m_current_dest_tbp = -1;

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
    GLuint channel_scramble;
    GLuint tcc;
    GLuint alpha_multiply;
  } m_uniforms;

  struct {
    bool use_fast_scrambler = true;
  } m_debug;

  GLuint m_shader_id;
  GLuint m_dummy_texture;

  u8 m_index_to_clut_addr[256];
  OpenGLTexturePool m_opengl_texture_pool;

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
                                const std::optional<std::string>& dgo);
  void add_to_clut_blender_group(int idx,
                                 const std::vector<std::string>& textures,
                                 const std::string& suffix0,
                                 const std::string& suffix1,
                                 const std::optional<std::string>& dgo);
  void run_clut_blender_group(DmaTransfer& tf, int idx, u64 frame_idx);

  Psm32ToPsm8Scrambler m_psm32_to_psm8_8_8, m_psm32_to_psm8_16_16, m_psm32_to_psm8_32_32,
      m_psm32_to_psm8_64_64;
  ClutReader m_clut_table;

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

  std::vector<FixedAnimArray> m_fixed_anim_arrays;
};
