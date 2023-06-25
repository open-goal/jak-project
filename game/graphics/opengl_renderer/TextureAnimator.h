#pragma once

#include <optional>
#include <unordered_map>
#include <vector>

#include "common/dma/dma_chain_read.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"

#include "game/graphics/opengl_renderer/Shader.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/texture/TextureConverter.h"

struct VramEntry {
  enum class Kind { CLUT16_16, GENERIC_PSM32 } kind;
  int width, height;
  std::vector<u8> data_psm32;
};
struct GpuTexture;
struct DestinationTextureEntry {
  int tex_width;
  int tex_height;
  int dest_texture_address;
  std::optional<FramebufferTexturePair> tex;
  math::Vector<u8, 4> rgba_clear;

  bool needs_pool_update = false;
  bool needs_pool_creation = false;
  GpuTexture* pool_gpu_tex = nullptr;
};

struct ShaderContext {
  GsTex0 tex0;
  GsTex1 tex1;
  GsTest test;
  bool clamp_u, clamp_v;
  GsAlpha alpha;

  bool source_texture_set = false;
};

class TexturePool;

class TextureAnimator {
 public:
  TextureAnimator(ShaderLibrary& shaders);
  ~TextureAnimator();
  void handle_texture_anim_data(DmaFollower& dma, const u8* ee_mem, TexturePool* texture_pool);

 private:
  void handle_upload_clut_16_16(const DmaTransfer& tf, const u8* ee_mem);
  void handle_generic_upload(const DmaTransfer& tf, const u8* ee_mem);
  void handle_erase_dest(DmaFollower& dma);
  void handle_set_shader(DmaFollower& dma);
  void handle_draw(DmaFollower& dma);
  void handle_rg_to_ba(const DmaTransfer& tf);

  void set_up_opengl_for_shader(const ShaderContext& shader,
                                std::optional<GLuint> texture,
                                bool prim_abe);

  void load_clut_to_converter();

  GLuint make_temp_gpu_texture(const u32* data, u32 width, u32 height);

  GLuint make_or_get_gpu_texture_for_current_shader();

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

  std::unordered_map<u32, VramEntry> m_vram_entries;
  std::map<u32, DestinationTextureEntry> m_dest_textures;

  std::vector<GLuint> m_gl_textures;
  u32 m_next_gl_texture = 0;

  GLuint alloc_gl_texture() {
    ASSERT(m_next_gl_texture < m_gl_textures.size());
    return m_gl_textures[m_next_gl_texture++];
  }

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
  } m_uniforms;

  GLuint m_shader_id;
  GLuint m_dummy_texture;
};
