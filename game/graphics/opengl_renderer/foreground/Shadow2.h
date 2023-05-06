#pragma once

#include "common/math/Vector.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"

class Shadow2 : public BucketRenderer {
 public:
  static constexpr int kMaxVerts = 8192 * 3;
  static constexpr int kMaxInds = 8192 * 3;
  Shadow2(const std::string& name, int my_id);
  ~Shadow2();
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;
  void init_shaders(ShaderLibrary& shaders) override;

 private:
  /*
TAG: 0x00000000 cnt  qwc 0x0073
size: 1840
vif0: FLUSH, data: 285212672
vif1: UNPACK-V4-32: 115 addr: 4 us: false tops: false, data: 115, imm: 4

dma transfer 1:
TAG: 0x00000000 cnt  qwc 0x0073
size: 1840
vif0: NOP, data: 0
vif1: UNPACK-V4-32: 115 addr: 174 us: false tops: false, data: 115, imm: 174

dma transfer 2:
TAG: 0x00000000 cnt  qwc 0x0019
size: 400
vif0: NOP, data: 0
vif1: UNPACK-V4-8: 96 addr: 344 us: true tops: false, data: 96, imm: 16728

dma transfer 3:
TAG: 0x00000000 cnt  qwc 0x001e
size: 480
vif0: NOP, data: 0
vif1: UNPACK-V4-8: 116 addr: 600 us: true tops: false, data: 116, imm: 16984
   */

  struct ShadowVu1Constants {
    math::Vector4f hmgescale;
    math::Vector4f invhscale;
    math::Vector4f texoffset;
    math::Vector4f texscale;
    math::Vector4f hvdfoff;
    math::Vector4f fog;
    math::Vector4f clrs[2];
  };
  static_assert(sizeof(ShadowVu1Constants) == 128);

  struct MysteryData {
    u8 data[4 * 16];
  };

  struct CameraMatrix {
    math::Vector4f v[4];
  };

  struct FrameConstants {
    ShadowVu1Constants constants;  // 880, 0x370
    MysteryData mystery;           // 940, 0x3ac
    CameraMatrix camera;           // 0
  };

  struct InputData {
    const u8* upload_4 = nullptr;       // always 115
    const u8* upload_174 = nullptr;     // always 115
    const u8* upload_8s_344 = nullptr;  // addr list?
    size_t size_344 = 0;

    // MSCAL 2

    const u8* upload_8s_600 = nullptr;
    size_t size_600 = 0;
  };

  struct ShadowVertex {
    math::Vector3f pos;
    math::Vector<u8, 4> rgba;
  };
  static_assert(sizeof(ShadowVertex) == 16);

  struct {
    GLuint vertex_buffer;
    GLuint index_buffer[2];
    GLuint vao;
    struct {
      GLuint hvdf_offset;
      GLuint perspective[4];
      GLuint fog;
      GLuint color;
      GLuint clear_mode;
    } uniforms;
  } m_ogl;

  std::vector<ShadowVertex> m_vertex_buffer;
  std::vector<u32> m_front_index_buffer;
  std::vector<u32> m_back_index_buffer;
  size_t m_vertex_buffer_used = 0;
  size_t m_front_index_buffer_used = 0;
  size_t m_back_index_buffer_used = 0;
  bool m_debug_draw_volume = false;

  // memory layout
  // 0 - 4    camera
  // 4 - 119  upload4

  // 174 - 289 upload174

  // 344 - ??  upload344

  // 600 - ?? upload 600

  // 880   hmgescale
  // 881   invhscale
  // 882   texoffset
  // 883   texscale
  // 884   hvdfoff
  // 885   fog
  // 886   clrs[0]
  // 887   clrs[1]

  // 940   mystery[0]
  // 941   mystery[1]
  // 942   mystery[2]
  // 943   mystery[3]

  void reset_buffers();
  void buffer_from_mscal2(const InputData& input);
  const u8* add_cap_tris(const u8* byte_data, const u8* vertex_data);
  const u8* add_wall_quads(const u8* byte_data, const u8* vertex_data_0, const u8* vertex_data_1);
  void buffer_from_mscal4(const InputData& input);
  ShadowVertex* alloc_verts(int n);
  u32* alloc_inds(int n, bool front);
  void draw_buffers(SharedRenderState* render_state,
                    ScopedProfilerNode& prof,
                    const FrameConstants& constants);
};
