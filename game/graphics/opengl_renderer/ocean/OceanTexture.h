#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"
#include "game/common/vu.h"

class OceanTexture {
 public:
  OceanTexture();
  void handle_ocean_texture(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof);

 private:
  void handle_tex_call_start(DmaFollower& dma,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof);
  void handle_tex_call_rest(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof);
  void handle_tex_call_done(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof);

  void run_L3();
  void run_L5();

  void xgkick(Vf* src);

  static constexpr int TEX0_SIZE = 128;
  FramebufferTexturePair m_tex0;

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
