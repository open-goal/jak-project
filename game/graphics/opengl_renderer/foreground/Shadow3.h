#pragma once
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/foreground/Shadow3CPU.h"
#include "game/graphics/opengl_renderer/opengl_utils.h"

struct Jak1ShadowSettings {
  math::Vector<float, 3> center;
  u32 flags;
  math::Vector<float, 3> shadow_dir;
  float dist_to_locus;
  math::Vector4f bot_plane;
  math::Vector4f top_plane;
  float fade_dist;
  float fade_start;
  s32 dummy2;
  s32 dummy3;
};
static_assert(sizeof(Jak1ShadowSettings) == 5 * 16);

struct Jak1ShadowRequest {
  u8 dma[16];
  Jak1ShadowSettings settings;
  u32 geo_name;
  u32 mtx;
  u32 num_joints;
  u32 next;
};

class Shadow3 {
 public:
  Shadow3(ShaderLibrary& shaders);
  ~Shadow3();
  void render_jak1(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window();

 private:
  struct ShadowRequest {
    ShadowRef model;
    math::Vector<float, 3> origin;
    math::Vector4f top_plane, bottom_plane;
    math::Vector3f light_dir;
    ShadowRequest* next = nullptr;
    const u8* bones = nullptr;
    u32 bone_idx = 0;
    u32 flags = 0;
  };

  struct LevelChain {
    const LevelData* level = nullptr;
    ShadowRequest* head = nullptr;
  };
  void flush_requests(SharedRenderState* render_state, ScopedProfilerNode& prof);
  void first_time_setup(SharedRenderState* render_state);
  void setup_for_level(SharedRenderState* render_state, const LevelData* level_data);
  void draw_model(SharedRenderState* render_state,
                  ShadowRequest* request,
                  ScopedProfilerNode& prof);
  void finish(SharedRenderState* render_state, ScopedProfilerNode& prof);
  std::array<ShadowRequest, 128> m_requests;
  std::array<LevelChain, 16> m_level_chains;
  int m_next_request = 0;

  static constexpr int MAX_SHADER_BONE_VECTORS = 1024 * 16;  // ??
  math::Vector4f m_shader_bone_vector_buffer[MAX_SHADER_BONE_VECTORS];
  u32 m_next_free_bone_vector = 0;

  struct {
    GLuint vao = -1;
    GLuint indices = -1;
    GLuint debug_verts = -1;
    GLuint bones_buffer = -1;
    int buffer_alignment = 0;
  } m_opengl;

  struct {
    GLuint hvdf_offset = 0;
    GLuint fog_constants = 0;
    GLuint perspective_matrix = 0;
    GLuint camera_rot = 0;
    GLuint debug_color = 0;
    GLuint bottom_plane = 0;
    GLuint top_plane = 0;
    GLuint origin = 0;
  } m_uniforms;
  bool m_did_first_time_setup = false;

  bool m_hacks = false;
  bool m_cull_back = false;
  bool m_near_plane_hack = false;
  int m_debug_tri = 0;

  ShadowCPUWorkspace m_cpu_workspace;
  ShadowCPUOutput m_cpu_output;
  FullScreenDraw m_full_screen_draw;
};