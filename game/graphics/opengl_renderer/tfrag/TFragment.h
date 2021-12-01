#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/tfrag/BufferedRenderer.h"
#include "common/dma/gs.h"
#include "common/math/Vector.h"

using math::Matrix4f;
using math::Vector4f;

constexpr int KICK_ZONE_END = 1024;

struct TFragData {
  Vector4f fog;          // 0   656 (vf01)
  Vector4f val;          // 1   657 (vf02)
  GifTag str_gif;        // 2   658 (vf06)
  GifTag fan_gif;        // 3   659
  GifTag ad_gif;         // 4   660
  Vector4f hvdf_offset;  // 5   661 (vf10)
  Vector4f hmge_scale;   // 6   662 (vf11)
  Vector4f invh_scale;   // 7   663
  Vector4f ambient;      // 8   664
  Vector4f guard;        // 9   665
  Vector4f k0s[2];       // 10/11 666, 667
  Vector4f k1s[2];       // 12/13 668, 669

  std::string print() const;
};
static_assert(sizeof(TFragData) == 0xe0, "TFragData size");

struct TFragBufferedData {
  u8 pad[328 * 16];
};
static_assert(sizeof(TFragBufferedData) == 328 * 16);

struct TFragKickZone {
  u8 pad[(KICK_ZONE_END - 670) * 16];
};

class TFragment : public BucketRenderer {
 public:
  TFragment(const std::string& name, BucketId my_id, bool child_mode);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  void handle_initialization(DmaFollower& dma,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof);

  template <bool DEBUG>
  void handle_tfrag(const DmaTransfer& dma,
                    SharedRenderState* render_state,
                    ScopedProfilerNode& prof);

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
  void handle_mscal(const VifCode& code, SharedRenderState* render_state, ScopedProfilerNode& prof);

  template <bool DEBUG>
  void exec_program_6(SharedRenderState* render_state, ScopedProfilerNode& prof);

  template <bool DEBUG>
  void XGKICK(u32 addr, SharedRenderState* render_state, ScopedProfilerNode& prof);

  struct Prog6Inputs {
    Vector4f vf04_cam_mat_x;
    Vector4f vf07_cam_mat_y;
    Vector4f vf08_cam_mat_z;
  };

  struct Prog6Vars {
    // pre-set
    u16 vi03;
    u16 vi07;
    u16 vi08;
    u16 vi09;
    u16 vi14;
    Vector4f vf16_scaled_pos_0;
    Vector4f vf17_scaled_pos_1;
    Vector4f vf18_scaled_pos_2;
    Vector4f vf19_scaled_pos_3;

    // uninit
    u16 vi02;
    u16 vi04;
    u16 vi05;
    u16 vi06_kick_zone_ptr;
    u16 vi10;
    u16 vi11;
    u16 vi12;  // seems to be gs loop count (dverts) - 0x80.
    u16 vi13;
    Vector4f vf09_cam_trans;
    Vector4f vf12_root_pos_0;  // position a, 0
    Vector4f vf13_root_pos_1;
    Vector4f vf14_loop_pos_0;
    Vector4f vf15_loop_pos_1;
    Vector4f vf20;
    Vector4f vf21;
    Vector4f vf22;
    Vector4f vf23;
    Vector4f vf24;
    Vector4f vf25;  // position b, 0
    Vector4f vf26;
    Vector4f vf27;
    Vector4f vf28;
    Vector4f vf29;
    Vector4f vf30;
    Vector4f vf31;
  };

  template <bool DEBUG>
  void exec_program_6_process_first(const Prog6Inputs& in,
                                    Prog6Vars& vars,
                                    SharedRenderState* render_state,
                                    ScopedProfilerNode& prof);

  template <bool DEBUG>
  void exec_jumper_L128(const Prog6Inputs& in, Prog6Vars& vars);

  template <bool DEBUG>
  bool exec_jumper_L129(const Prog6Inputs& in,
                        Prog6Vars& vars,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  template <bool DEBUG>
  void exec_jumper_L6A1(const Prog6Inputs& in, Prog6Vars& vars);

  template <bool DEBUG>
  bool exec_jumper_L130(const Prog6Inputs& in,
                        Prog6Vars& vars,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  template <bool DEBUG>
  void exec_jumper_L6B0(const Prog6Inputs& in, Prog6Vars& vars);

  template <bool DEBUG>
  bool exec_jumper_L131(const Prog6Inputs& in,
                        Prog6Vars& vars,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  template <bool DEBUG>
  void exec_jumper_L6BF(const Prog6Inputs& in, Prog6Vars& vars);

  template <bool DEBUG>
  bool exec_jumper_L132(const Prog6Inputs& in,
                        Prog6Vars& vars,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  template <bool DEBUG>
  bool exec_jumper_L122(const Prog6Inputs& in,
                        Prog6Vars& vars,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof);

  std::string m_debug_string;
  bool m_child_mode = false;
  bool m_extra_debug = false;
  int m_max_draw = -1;
  bool m_skip_mscals = false;
  bool m_skip_xgkick = false;
  bool m_prog8_with_prog6 = true;
  bool m_prog10_with_prog6 = true;
  bool m_prog18_with_prog6 = true;
  bool m_all_with_prog6 = false;
  bool m_use_buffered_renderer = true;
  std::string m_frag_debug;

  // GS setup data
  u8 m_test_setup[32];

  // VU data
  Matrix4f m_matrix_0;
  Matrix4f m_matrix_1;
  TFragData m_tfrag_data;
  TFragKickZone m_kick_data;

  // buffers
  TFragBufferedData m_buffered_data[2];
  int m_uploading_buffer = 0;

  u8* get_upload_buffer() { return (u8*)&m_buffered_data[m_uploading_buffer].pad[0]; }
  u8* get_processing_buffer() { return (u8*)&m_buffered_data[1 - m_uploading_buffer].pad[0]; }

  void flip_buffers();
  u16 ilw_data(int offset, int xyzw);
  u16 ilw_kick_zone(int offset, int xyzw);

  Vector4f load_vector_data(int offset);
  void store_vector_kick_zone(int offset, const Vector4f& vec);
  void store_gif_kick_zone(int offset, const GifTag& tag);
  void store_u32_kick_zone(u32 value, int qw, int xyzw);

  enum TFragDataMem {
    Buffer0_Start = 0,
    TFragMatrix0 = 5,

    Buffer1_Start = 328,
    TFragMatrix1 = TFragMatrix0 + Buffer1_Start,

    TFragFrameData = 656,
    TFragKickZoneData = 670,
  };

  enum TFragJumper {
    L128_PART0_X = 0,
    L129_PART1_X = 1,
    L0x6A1_PART0_Y = 2,
    L130_PART1_Y = 3,
    L0x6B0_PART0_Z = 4,
    L131_PART1_Z = 5,
    L0x6BF_PART0_W = 6,
    L132_PART1_W = 7,
    L122_KICK = 8,
    END_PROGRAM = 9,
    INVALID = 10
  };

  TFragJumper m_next_block = TFragJumper::INVALID;
  TFragJumper m_ret_block = TFragJumper::INVALID;
  bool m_clip_and_3ffff = false;
  Vector4f m_acc;  // todo, probably rearrange this so acc stays entirely in part0 or part1?
  float m_q;       // todo, probably regroup

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

  static constexpr int NUM_PROGRAMS = 13;
  struct Stats {
    int tfrag_dma_packets = 0;
    int tfrag_bytes = 0;
    int error_packets = 0;
    int error_mscals = 0;

    struct PerProgram {
      int calls = 0;
    };

    PerProgram per_program[NUM_PROGRAMS];  // addr / 2
  } m_stats;

  DirectRenderer m_direct_renderer;
  BufferedRenderer::Builder m_buffered_renderer;
};
