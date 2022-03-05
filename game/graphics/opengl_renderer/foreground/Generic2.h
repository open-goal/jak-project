#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class Generic2 : public BucketRenderer {
 public:
  Generic2(const std::string& name, BucketId my_id, u32 num_verts, u32 num_frags, u32 num_adgif);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  // void init_shaders(ShaderLibrary& shaders) override;

  struct Vertex {
    math::Vector<float, 3> xyz;
    math::Vector<u8, 4> rgba;
    math::Vector<float, 2> st;  // 16
    u8 tex_unit;
    u8 flags;
    u8 fog;
    u8 pad;
    u32 pad2;
  };
  static_assert(sizeof(Vertex) == 32);

 private:
  void reset_buffers();
  void process_dma(DmaFollower& dma, u32 next_bucket);
  void setup_draws();
  void do_draws();
  bool check_for_end_of_generic_data(DmaFollower& dma, u32 next_bucket);

  bool handle_bucket_setup_dma(DmaFollower& dma, u32 next_bucket);

  struct GenericDraw {
    u32 first_vert = -1;
    u32 verts = -1;
    u16 mscal = 0;
  };

  struct {
    u32 stcycl;
  } m_dma_unpack;

  struct DrawingConfig {
    bool zmsk = false;
    // horizontal, vertical, depth, fog offsets.
    math::Vector4f hvdf_offset;
    float pfog0;             // scale factor for perspective divide
    float fog_min, fog_max;  // clamp for fog

  } m_drawing_config;

  static constexpr u32 FRAG_HEADER_SIZE = 16 * 7;
  struct Fragment {
    u8 header[FRAG_HEADER_SIZE];
    u32 adgif_idx = 0;
    u32 adgif_count = 0;

    u32 vtx_idx = 0;
    u32 vtx_count = 0;
    u8 mscal_addr = 0;
  };
  u32 handle_fragments_after_unpack_v4_32(const u8* data,
                                          u32 off,
                                          u32 first_unpack_bytes,
                                          u32 next_bucket,
                                          u32 end_of_vif,
                                          Fragment* frag, bool loop);



  u32 m_next_free_frag = 0;
  std::vector<Fragment> m_fragments;

  u32 m_next_free_vert = 0;
  std::vector<Vertex> m_verts;

  struct Adgif {
    AdGifData data;
    u32 ee_mem_addr;
  };

  u32 m_next_free_adgif = 0;
  std::vector<Adgif> m_adgifs;

  Fragment& next_frag() {
    ASSERT(m_next_free_frag < m_fragments.size());
    return m_fragments[m_next_free_frag++];
  }

  Adgif& next_adgif() {
    ASSERT(m_next_free_adgif < m_adgifs.size());
    return m_adgifs[m_next_free_adgif++];
  }

  void alloc_vtx(int count) {
    m_next_free_vert += count;
    ASSERT(m_next_free_vert < m_verts.size());
  }

  std::string m_debug;

  struct Stats {
    u32 dma_tags = 0;
  } m_stats;
};
