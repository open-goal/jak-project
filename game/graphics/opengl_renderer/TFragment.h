#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"

using math::Matrix4f;
using math::Vector4f;

struct TFragData {
  Vector4f fog;          // 0   656
  Vector4f val;          // 1   657
  GifTag str_gif;        // 2   658
  GifTag fan_gif;        // 3   659
  GifTag ad_gif;         // 4   660
  Vector4f hvdf_offset;  // 5   661
  Vector4f hmge_scale;   // 6   662
  Vector4f invh_scale;   // 7   663
  Vector4f ambient;      // 8   664
  Vector4f guard;        // 9   665
  Vector4f k0s[2];
  Vector4f k1s[2];

  std::string print() const;
};
static_assert(sizeof(TFragData) == 0xe0, "TFragData size");

struct TFragBufferedData {
  u8 pad[328 * 16];
};
static_assert(sizeof(TFragBufferedData) == 328 * 16);

class TFragment : public BucketRenderer {
 public:
  TFragment(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  void handle_initialization(DmaFollower& dma, SharedRenderState* render_state);

  template <bool DEBUG>
  void handle_tfrag(const DmaTransfer& dma, SharedRenderState* render_state);

  int handle_unpack_v4_8_mode0(const VifCode& code,
                               const DmaTransfer& dma,
                               int offset,
                               int cl,
                               int wl);
  int handle_unpack_v4_8_mode1(const VifCode& code,
                               const DmaTransfer& dma,
                               int offset,
                               int cl,
                               int wl,
                               const u32 row[4]);
  int handle_unpack_v4_16_mode0(const VifCode& code,
                                const DmaTransfer& dma,
                                int offset,
                                int cl,
                                int wl);
  int handle_unpack_v4_16_mode1(const VifCode& code,
                                const DmaTransfer& dma,
                                int offset,
                                int cl,
                                int wl,
                                const u32 row[4]);
  int handle_unpack_v4_32(const VifCode& code, const DmaTransfer& dma, int offset, int cl, int wl);
  int handle_unpack_v3_32(const VifCode& code, const DmaTransfer& dma, int offset, int cl, int wl);

  template <bool DEBUG>
  void handle_mscal(const VifCode& code);

  std::string m_debug_string;
  bool m_extra_debug = true;
  std::string m_frag_debug;

  // GS setup data
  u8 m_test_setup[32];

  // VU data
  Matrix4f m_matrix_0;
  Matrix4f m_matrix_1;
  TFragData m_tfrag_data;

  // buffers
  TFragBufferedData m_buffered_data[2];
  int m_uploading_buffer = 0;

  u8* get_upload_buffer() { return (u8*)&m_buffered_data[m_uploading_buffer].pad[0]; }

  enum TFragDataMem {
    Buffer0_Start = 0,
    TFragMatrix0 = 5,

    Buffer1_Start = 328,
    TFragMatrix1 = TFragMatrix0 + Buffer1_Start,

    TFragFrameData = 656
  };

  enum TFragProgMem {
    TFragSetup = 0,
  };

  struct Ptrs {
    int vi01;
    int vi14;
    int vf03_x, vf03_y, vf03_z, vf03_w;
  } m_ptrs;

  struct Globals {
    Vector4f vf04_ambient;
  } m_globals;

  struct Stats {
    int tfrag_dma_packets = 0;
    int tfrag_bytes = 0;
    int error_packets = 0;
    int error_mscals = 0;
  } m_stats;
};
