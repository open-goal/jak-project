#pragma once

#include <optional>
#include <set>
#include <unordered_map>
#include <vector>

#include "common/custom_data/Tfrag3Data.h"
#include "common/dma/dma_chain_read.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"

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
              const std::vector<std::string>& sources,
              const std::optional<std::string>& level_name,
              const tfrag3::Level* level,
              OpenGLTexturePool* tpool);
  GLuint run(const float* weights);
  GLuint texture() const { return m_texture; }

 private:
  const tfrag3::IndexTexture* m_dest;
  std::vector<const std::array<math::Vector4<u8>, 256>*> m_cluts;
  std::vector<float> m_current_weights;
  GLuint m_texture;
  std::array<math::Vector4<u8>, 256> m_temp_clut;
  std::vector<u32> m_temp_rgba;
};

class TexturePool;

class TextureAnimator {
 public:
  TextureAnimator(ShaderLibrary& shaders, const tfrag3::Level* common_level);
  ~TextureAnimator();
  void handle_texture_anim_data(DmaFollower& dma, const u8* ee_mem, TexturePool* texture_pool);
  GLuint get_by_slot(int idx);
  const std::vector<GLuint>* slots() { return &m_output_slots; }

 private:
  void handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem);
  void handle_generic_upload(const DmaTransfer& tf, const u8* ee_mem);
  void handle_erase_dest(DmaFollower& dma);
  void handle_set_shader(DmaFollower& dma);
  void handle_draw(DmaFollower& dma, TexturePool& texture_pool);
  void handle_rg_to_ba(const DmaTransfer& tf);
  void handle_set_clut_alpha(const DmaTransfer& tf);
  void handle_copy_clut_alpha(const DmaTransfer& tf);

  VramEntry* setup_vram_entry_for_gpu_texture(int w, int h, int tbp);

  void set_up_opengl_for_shader(const ShaderContext& shader,
                                std::optional<GLuint> texture,
                                bool prim_abe);

  void load_clut_to_converter();
  const u32* get_clut_16_16_psm32(int cbp);

  GLuint make_temp_gpu_texture(const u32* data, u32 width, u32 height);

  GLuint make_or_get_gpu_texture_for_current_shader(TexturePool& texture_pool);
  void force_to_gpu(int tbp);

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

  PcTextureId get_id_for_tbp(TexturePool* pool, u32 tbp);

  VramEntry* m_tex_looking_for_clut = nullptr;
  const tfrag3::Level* m_common_level = nullptr;
  std::unordered_map<u32, VramEntry> m_textures;
  std::unordered_map<u32, PcTextureId> m_ids_by_vram;

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
  } m_uniforms;

  GLuint m_shader_id;
  GLuint m_dummy_texture;

  u8 m_index_to_clut_addr[256];
  OpenGLTexturePool m_opengl_texture_pool;

  std::vector<GLuint> m_output_slots;

  struct ClutBlenderGroup {
    std::vector<ClutBlender> blenders;
    std::vector<int> outputs;
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
  void run_clut_blender_group(DmaTransfer& tf, int idx);

  //  std::vector<ClutBlender> m_darkjak_blenders;
  //  std::vector<int> m_darkjak_output_slots;
  //
  //  std::vector<ClutBlender> m_jakb_prison_blenders;
  //  std::vector<int> m_jakb_prison_output_slots;
  //
  //  std::vector<ClutBlender> m_jakb_oracle_blenders;
  //  std::vector<int> m_jakb_oracle_slots;
  //
  //  std::vector<ClutBlender> m_jakb_nest_blenders;
  //  std::vector<int> m_jakb_nest_slots;
};
