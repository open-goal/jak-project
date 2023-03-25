#pragma once

#include "game/common/vu.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"

class OceanTexture {
 public:
  OceanTexture(bool generate_mipmaps);
  void handle_ocean_texture_jak1(DmaFollower& dma,
                                 SharedRenderState* render_state,
                                 ScopedProfilerNode& prof);
  void handle_ocean_texture_jak2(DmaFollower& dma,
                                 SharedRenderState* render_state,
                                 ScopedProfilerNode& prof);
  void init_textures(TexturePool& pool, GameVersion version);
  void draw_debug_window();
  ~OceanTexture();

 private:
  void run_L1_PC();
  void run_L2_PC();
  void run_L3_PC();
  void run_L5_PC();
  void xgkick_PC(Vf* src);

  void run_L1_PC_jak2();
  void run_L2_PC_jak2();
  void run_L3_PC_jak2();

  void setup_renderer();
  void flush(SharedRenderState* render_state, ScopedProfilerNode& prof);

  void init_pc();
  void destroy_pc();

  void make_texture_with_mipmaps(SharedRenderState* render_state, ScopedProfilerNode& prof);

  bool m_generate_mipmaps;

  static constexpr int TEX0_SIZE = 128;
  static constexpr int NUM_MIPS = 8;
  FramebufferTexturePair m_result_texture;
  FramebufferTexturePair m_temp_texture;
  GpuTexture* m_tex0_gpu = nullptr;

  // (deftype ocean-texture-constants (structure)
  struct OceanTextureConstants {
    //  ((giftag    qword    :inline :offset-assert 0) 985
    u8 giftag[16];
    //   (buffers   vector4w :inline :offset-assert 16) 986
    math::Vector<u32, 4> buffers;
    //   (dests     vector4w :inline :offset-assert 32) 987
    math::Vector<u32, 4> dests;
    //   (start     vector   :inline :offset-assert 48) 988
    math::Vector4f start;
    //   (offsets   vector   :inline :offset-assert 64) 989
    math::Vector4f offsets;
    //   (constants vector   :inline :offset-assert 80) 990
    math::Vector4f constants;
    //   (cam-nrm   vector   :inline :offset-assert 96) 991
    math::Vector4f cam_nrm;
    //   )
  } m_texture_constants;
  static_assert(sizeof(OceanTextureConstants) == 112);

  AdGifData m_envmap_adgif;

  Vf m_texture_vertices_a[192];
  Vf m_texture_vertices_b[192];

  static constexpr int DBUF_SIZE = 99;
  Vf m_dbuf_a[DBUF_SIZE];
  Vf m_dbuf_b[DBUF_SIZE];

  Vf* m_dbuf_x;
  Vf* m_dbuf_y;

  static constexpr int TBUF_SIZE = 199;
  Vf m_tbuf_a[TBUF_SIZE];
  Vf m_tbuf_b[TBUF_SIZE];

  Vf* m_tbuf_x;
  Vf* m_tbuf_y;

  Vf* m_texture_vertices_loading = nullptr;
  Vf* m_texture_vertices_drawing = nullptr;

  Vf* swap_vu_upload_buffers() {
    std::swap(m_texture_vertices_drawing, m_texture_vertices_loading);
    return m_texture_vertices_drawing;
  }

  void swap_dbuf() { std::swap(m_dbuf_x, m_dbuf_y); }

  void swap_tbuf() { std::swap(m_tbuf_x, m_tbuf_y); }

  Vf* get_dbuf() { return m_dbuf_x; }

  Vf* get_dbuf_other() { return m_dbuf_y; }

  Vf* get_tbuf() { return m_tbuf_x; }

  struct {
    Vf startx;  //           vf14
    // Vf base_pos;          vf15
    // Vf nrm0;              vf24
    Vf* dbuf_read_a;      // vi03
    Vf* dbuf_read_b;      // vi04
    Vf* in_ptr;           // vi05
    Vf* dbuf_write;       // vi06
    Vf* dbuf_write_base;  // vi07
    Vf* tptr;             // vi08
    Vf* tbase;            // vi09
  } vu;

  static constexpr u32 NUM_STRIPS = 32;
  static constexpr u32 NUM_VERTS_PER_STRIP = 66;
  static constexpr u32 NUM_VERTS = NUM_STRIPS * NUM_VERTS_PER_STRIP;

  // note: if we used u16's for s/t, we could make this 8 bytes, but I'm afraid that some GPUs
  // will be unhappy with that format.
  struct Vertex {
    float s, t;
    math::Vector<u8, 4> rgba;
    u32 pad;
  };
  static_assert(sizeof(Vertex) == 16);
  struct {
    std::vector<math::Vector2f> vertex_positions;
    std::vector<Vertex> vertex_dynamic;
    std::vector<u32> index_buffer;
    u32 vtx_idx = 0;

    GLuint vao, static_vertex_buffer, dynamic_vertex_buffer, gl_index_buffer;
  } m_pc;

  struct MipMap {
    GLuint vao, vtx_buffer;
    struct Vertex {
      float x, y;
      float s, t;
    };
    static_assert(sizeof(Vertex) == 16);
  } m_mipmap;

  enum TexVu1Data {
    BUF0 = 384,
    BUF1 = 583,
    DEST0 = 782,
    DEST1 = 881,
    CONSTANTS = 985,
  };

  enum TexVu1Prog { START = 0, REST = 2, DONE = 4 };

  static constexpr int NUM_FRAG_LOOPS = 9;
};
