#include "DepthCue.h"

#include "game/graphics/opengl_renderer/dma_helpers.h"

#include "fmt/core.h"
#include "third-party/imgui/imgui.h"

namespace {
// Converts fixed point (with 4 bits for decimal) to floating point.
float fixed_to_floating_point(int fixed) {
  return fixed / 16.0f;
}

math::Vector2f fixed_to_floating_point(const math::Vector<s32, 2>& fixed_vec) {
  return math::Vector2f(fixed_to_floating_point(fixed_vec.x()),
                        fixed_to_floating_point(fixed_vec.y()));
}
}  // namespace

// Total number of loops depth-cue performs to draw to the framebuffer
constexpr int TOTAL_DRAW_SLICES = 16;

DepthCue::DepthCue(const std::string& name, int my_id) : BucketRenderer(name, my_id) {
  opengl_setup();

  m_draw_slices.resize(TOTAL_DRAW_SLICES);
}

void DepthCue::opengl_setup() {
  // Gen texture for sampling the framebuffer
  glGenFramebuffers(1, &m_ogl.framebuffer_sample_fbo);
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.framebuffer_sample_fbo);

  glGenTextures(1, &m_ogl.framebuffer_sample_tex);
  glBindTexture(GL_TEXTURE_2D, m_ogl.framebuffer_sample_tex);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 1, 1, 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                         m_ogl.framebuffer_sample_tex, 0);

  ASSERT(glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE);

  glBindTexture(GL_TEXTURE_2D, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  // Gen framebuffer for depth-cue-base-page
  glGenFramebuffers(1, &m_ogl.fbo);
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.fbo);

  glGenTextures(1, &m_ogl.fbo_texture);
  glBindTexture(GL_TEXTURE_2D, m_ogl.fbo_texture);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 1, 1, 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_ogl.fbo_texture, 0);

  ASSERT(glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE);

  glBindTexture(GL_TEXTURE_2D, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  // Gen vertex array for drawing to depth-cue-base-page
  glGenVertexArrays(1, &m_ogl.depth_cue_page_vao);
  glBindVertexArray(m_ogl.depth_cue_page_vao);
  glGenBuffers(1, &m_ogl.depth_cue_page_vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.depth_cue_page_vertex_buffer);

  glBufferData(GL_ARRAY_BUFFER, 4 * sizeof(SpriteVertex), nullptr, GL_STATIC_DRAW);

  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                                 // location 0 in the shader
                        2,                                 // 2 floats per vert
                        GL_FLOAT,                          // floats
                        GL_FALSE,                          // don't normalize, ignored
                        sizeof(SpriteVertex),              //
                        (void*)offsetof(SpriteVertex, xy)  // offset in array
  );
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1,                                 // location 1 in the shader
                        2,                                 // 2 floats per vert
                        GL_FLOAT,                          // floats
                        GL_FALSE,                          // don't normalize, ignored
                        sizeof(SpriteVertex),              //
                        (void*)offsetof(SpriteVertex, st)  // offset in array
  );

  // Gen vertex array for drawing to on-screen framebuffer
  glGenVertexArrays(1, &m_ogl.on_screen_vao);
  glBindVertexArray(m_ogl.on_screen_vao);
  glGenBuffers(1, &m_ogl.on_screen_vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.on_screen_vertex_buffer);

  glBufferData(GL_ARRAY_BUFFER, 4 * sizeof(SpriteVertex), nullptr, GL_STATIC_DRAW);

  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                                 // location 0 in the shader
                        2,                                 // 2 floats per vert
                        GL_FLOAT,                          // floats
                        GL_FALSE,                          // don't normalize, ignored
                        sizeof(SpriteVertex),              //
                        (void*)offsetof(SpriteVertex, xy)  // offset in array
  );
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1,                                 // location 1 in the shader
                        2,                                 // 2 floats per vert
                        GL_FLOAT,                          // floats
                        GL_FALSE,                          // don't normalize, ignored
                        sizeof(SpriteVertex),              //
                        (void*)offsetof(SpriteVertex, st)  // offset in array
  );

  // Done
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

void DepthCue::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // First thing should be a NEXT with two nops. this is a jump from buckets to depth-cue
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // depth-cue renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  // Read DMA
  {
    auto prof_node = prof.make_scoped_child("dma");
    read_dma(dma, render_state, prof_node);
  }

  if (!m_enabled) {
    // Renderer disabled, stop early
    return;
  }

  // Set up draw info
  {
    auto prof_node = prof.make_scoped_child("setup");
    setup(render_state, prof_node);
  }

  // Draw
  {
    auto prof_node = prof.make_scoped_child("drawing");
    draw(render_state, prof_node);
  }
}

/*!
 * Reads all depth-cue DMA packets.
 */
void DepthCue::read_dma(DmaFollower& dma,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& /*prof*/) {
  // First should be general GS register setup
  {
    auto gs_setup = dma.read_and_advance();
    ASSERT(gs_setup.size_bytes == sizeof(DepthCueGsSetup));
    ASSERT(gs_setup.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(gs_setup.vifcode1().kind == VifCode::Kind::DIRECT);
    memcpy(&m_gs_setup, gs_setup.data, sizeof(DepthCueGsSetup));

    ASSERT(m_gs_setup.gif_tag.nreg() == 6);
    ASSERT(m_gs_setup.gif_tag.reg(0) == GifTag::RegisterDescriptor::AD);

    ASSERT(m_gs_setup.test1.ztest() == GsTest::ZTest::ALWAYS);
    ASSERT(m_gs_setup.zbuf1.zmsk() == true);
    ASSERT(m_gs_setup.tex1.mmag() == true);
    ASSERT(m_gs_setup.tex1.mmin() == 1);
    ASSERT(m_gs_setup.miptbp1 == 0);
    ASSERT(m_gs_setup.alpha1.b_mode() == GsAlpha::BlendMode::DEST);
    ASSERT(m_gs_setup.alpha1.d_mode() == GsAlpha::BlendMode::DEST);
  }

  // Next is 64 DMAs to draw to the depth-cue-base-page and back to the on-screen framebuffer
  // We'll group these by each slice of the framebuffer being drawn to
  for (int i = 0; i < TOTAL_DRAW_SLICES; i++) {
    // Each 'slice' should be:
    // 1. GS setup for drawing from on-screen framebuffer to depth-cue-base-page
    // 2. Draw to depth-cue-base-page
    // 3. GS setup for drawing from depth-cue-base-page back to on-screen framebuffer
    // 4. Draw to on-screen framebuffer
    DrawSlice& slice = m_draw_slices.at(i);

    // depth-cue-base-page setup
    {
      auto depth_cue_page_setup = dma.read_and_advance();
      ASSERT(depth_cue_page_setup.size_bytes == sizeof(DepthCuePageGsSetup));
      ASSERT(depth_cue_page_setup.vifcode0().kind == VifCode::Kind::NOP);
      ASSERT(depth_cue_page_setup.vifcode1().kind == VifCode::Kind::DIRECT);
      memcpy(&slice.depth_cue_page_setup, depth_cue_page_setup.data, sizeof(DepthCuePageGsSetup));

      ASSERT(slice.depth_cue_page_setup.gif_tag.nreg() == 5);
      ASSERT(slice.depth_cue_page_setup.gif_tag.reg(0) == GifTag::RegisterDescriptor::AD);

      ASSERT(slice.depth_cue_page_setup.tex01.tcc() == 1);
      ASSERT(slice.depth_cue_page_setup.test1.ztest() == GsTest::ZTest::ALWAYS);
      ASSERT(slice.depth_cue_page_setup.alpha1.b_mode() == GsAlpha::BlendMode::SOURCE);
      ASSERT(slice.depth_cue_page_setup.alpha1.d_mode() == GsAlpha::BlendMode::SOURCE);
    }

    // depth-cue-base-page draw
    {
      auto depth_cue_page_draw = dma.read_and_advance();
      ASSERT(depth_cue_page_draw.size_bytes == sizeof(DepthCuePageDraw));
      ASSERT(depth_cue_page_draw.vifcode0().kind == VifCode::Kind::NOP);
      ASSERT(depth_cue_page_draw.vifcode1().kind == VifCode::Kind::DIRECT);
      memcpy(&slice.depth_cue_page_draw, depth_cue_page_draw.data, sizeof(DepthCuePageDraw));

      ASSERT(slice.depth_cue_page_draw.gif_tag.nloop() == 1);
      ASSERT(slice.depth_cue_page_draw.gif_tag.pre() == true);
      ASSERT(slice.depth_cue_page_draw.gif_tag.prim() == 6);
      ASSERT(slice.depth_cue_page_draw.gif_tag.flg() == GifTag::Format::PACKED);
      ASSERT(slice.depth_cue_page_draw.gif_tag.nreg() == 5);
      ASSERT(slice.depth_cue_page_draw.gif_tag.reg(0) == GifTag::RegisterDescriptor::RGBAQ);
    }

    // on-screen setup
    {
      auto on_screen_setup = dma.read_and_advance();
      ASSERT(on_screen_setup.size_bytes == sizeof(OnScreenGsSetup));
      ASSERT(on_screen_setup.vifcode0().kind == VifCode::Kind::NOP);
      ASSERT(on_screen_setup.vifcode1().kind == VifCode::Kind::DIRECT);
      memcpy(&slice.on_screen_setup, on_screen_setup.data, sizeof(OnScreenGsSetup));

      ASSERT(slice.on_screen_setup.gif_tag.nreg() == 5);
      ASSERT(slice.on_screen_setup.gif_tag.reg(0) == GifTag::RegisterDescriptor::AD);

      ASSERT(slice.on_screen_setup.tex01.tcc() == 0);
      ASSERT(slice.on_screen_setup.texa.ta0() == 0x80);
      ASSERT(slice.on_screen_setup.texa.ta1() == 0x80);
      ASSERT(slice.on_screen_setup.alpha1.b_mode() == GsAlpha::BlendMode::DEST);
      ASSERT(slice.on_screen_setup.alpha1.d_mode() == GsAlpha::BlendMode::DEST);
    }

    // on-screen draw
    {
      auto on_screen_draw = dma.read_and_advance();
      ASSERT(on_screen_draw.size_bytes == sizeof(OnScreenDraw));
      ASSERT(on_screen_draw.vifcode0().kind == VifCode::Kind::NOP);
      ASSERT(on_screen_draw.vifcode1().kind == VifCode::Kind::DIRECT);
      memcpy(&slice.on_screen_draw, on_screen_draw.data, sizeof(OnScreenDraw));

      ASSERT(slice.on_screen_draw.gif_tag.nloop() == 1);
      ASSERT(slice.on_screen_draw.gif_tag.pre() == true);
      ASSERT(slice.on_screen_draw.gif_tag.prim() == 6);
      ASSERT(slice.on_screen_draw.gif_tag.flg() == GifTag::Format::PACKED);
      ASSERT(slice.on_screen_draw.gif_tag.nreg() == 5);
      ASSERT(slice.on_screen_draw.gif_tag.reg(0) == GifTag::RegisterDescriptor::RGBAQ);
    }
  }

  // Finally, a packet to restore GS state
  {
    auto gs_restore = dma.read_and_advance();
    ASSERT(gs_restore.size_bytes == sizeof(DepthCueGsRestore));
    ASSERT(gs_restore.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(gs_restore.vifcode1().kind == VifCode::Kind::DIRECT);
    memcpy(&m_gs_restore, gs_restore.data, sizeof(DepthCueGsRestore));

    ASSERT(m_gs_restore.gif_tag.nreg() == 2);
    ASSERT(m_gs_restore.gif_tag.reg(0) == GifTag::RegisterDescriptor::AD);
  }

  // End with 'NEXT'
  {
    ASSERT(dma.current_tag().kind == DmaTag::Kind::NEXT);

    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
  }
}

void DepthCue::setup(SharedRenderState* render_state, ScopedProfilerNode& /*prof*/) {
  if (m_debug.cache_setup && (m_ogl.last_draw_region_w == render_state->draw_region_w &&
                              m_ogl.last_draw_region_h == render_state->draw_region_h &&
                              // Also recompute when certain debug settings change
                              m_ogl.last_override_sharpness == m_debug.override_sharpness &&
                              m_ogl.last_custom_sharpness == m_debug.sharpness &&
                              m_ogl.last_force_original_res == m_debug.force_original_res &&
                              m_ogl.last_res_scale == m_debug.res_scale)) {
    // Draw region didn't change, everything is already set up
    return;
  }

  m_ogl.last_draw_region_w = render_state->draw_region_w;
  m_ogl.last_draw_region_h = render_state->draw_region_h;
  m_ogl.last_override_sharpness = m_debug.override_sharpness;
  m_ogl.last_custom_sharpness = m_debug.sharpness;
  m_ogl.last_force_original_res = m_debug.force_original_res;
  m_ogl.last_res_scale = m_debug.res_scale;

  // ASSUMPTIONS
  // --------------------------
  // Assert some assumptions that most of the data for each depth-cue draw is the same.
  // The way the game wants to render this effect is very inefficient in OpenGL, we can use these
  // assumptions to only alter state once, do setup once, and group multiple draw calls.
  const DrawSlice& first_slice = m_draw_slices[0];

  // 1. Assume each draw slice has the exact same width of 32
  //    We'll compare each slice to the first as we go
  float slice_width = fixed_to_floating_point(first_slice.on_screen_draw.xyzf2_2.x() -
                                              first_slice.on_screen_draw.xyzf2_1.x());
  // NOTE: Y-coords will range between [0,1/2 output res], usually 224 but not always.
  // We'll capture it here so we can convert to coords to [0,1] ranges later.
  float slice_height = fixed_to_floating_point(first_slice.on_screen_draw.xyzf2_2.y());

  ASSERT(slice_width == 32.0f);
  ASSERT(first_slice.on_screen_draw.xyzf2_1.y() == 0);

  // 2. Assume that the framebuffer is sampled as a 1024x256 texel view and that the game thinks the
  // framebuffer is 512 pixels wide.
  int fb_sample_width = (int)pow(2, first_slice.depth_cue_page_setup.tex01.tw());
  int fb_sample_height = (int)pow(2, first_slice.depth_cue_page_setup.tex01.th());
  int fb_width = first_slice.depth_cue_page_setup.tex01.tbw() * 64;

  ASSERT(fb_sample_width == 1024);
  ASSERT(fb_sample_height == 256);
  ASSERT(fb_width == 512);
  ASSERT(fb_width * 2 == fb_sample_width);

  // 3. Finally, assert that all slices match the above assumptions
  for (const DrawSlice& slice : m_draw_slices) {
    float _slice_width = fixed_to_floating_point(slice.on_screen_draw.xyzf2_2.x() -
                                                 slice.on_screen_draw.xyzf2_1.x());
    float _slice_height = fixed_to_floating_point(slice.on_screen_draw.xyzf2_2.y());

    ASSERT(slice_width == _slice_width);
    ASSERT(slice_height == _slice_height);
    ASSERT(slice.on_screen_draw.xyzf2_1.y() == 0);

    int _fb_sample_width = (int)pow(2, slice.depth_cue_page_setup.tex01.tw());
    int _fb_sample_height = (int)pow(2, slice.depth_cue_page_setup.tex01.th());
    int _fb_width = slice.depth_cue_page_setup.tex01.tbw() * 64;

    ASSERT(fb_sample_width == _fb_sample_width);
    ASSERT(fb_sample_height == _fb_sample_height);
    ASSERT(fb_width == _fb_width);
  }

  // FRAMEBUFFER SAMPLE TEXTURE
  // --------------------------
  // We need a copy of the framebuffer to sample from. If the framebuffer wasn't using
  // multisampling, this wouldn't be necessary and the framebuffer could just be bound. Instead,
  // we'll just blit to a new texture.
  //
  // The original game code would have created this as a view into the framebuffer whose width is 2x
  // as large, however this isn't necessary for the effect to work.
  int pc_fb_sample_width = render_state->draw_region_w;
  int pc_fb_sample_height = render_state->draw_region_h;

  m_ogl.framebuffer_sample_width = pc_fb_sample_width;
  m_ogl.framebuffer_sample_height = pc_fb_sample_height;

  glBindTexture(GL_TEXTURE_2D, m_ogl.framebuffer_sample_tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, pc_fb_sample_width, pc_fb_sample_height, 0, GL_RGB,
               GL_UNSIGNED_BYTE, NULL);
  glBindTexture(GL_TEXTURE_2D, 0);

  // DEPTH CUE BASE PAGE FRAMEBUFFER
  // --------------------------
  // Next, we need a framebuffer to draw slices of the sample texture to. The depth-cue effect
  // usually does this in 16 vertical slices that are 32 pixels wide each. The destination
  // drawn to is smaller than the source by a very small amount (defined by sharpness in the
  // GOAL code), which kicks in the bilinear filtering effect. Normally, a 32x224 texture will
  // be reused for each slice but for the sake of efficient rendering, we'll create a framebuffer
  // that can store all 16 slices side-by-side and draw all slices to it all at once.
  int pc_depth_cue_fb_width = render_state->draw_region_w;
  int pc_depth_cue_fb_height = render_state->draw_region_h;

  if (m_debug.force_original_res) {
    pc_depth_cue_fb_width = 512;
  }

  pc_depth_cue_fb_width *= m_debug.res_scale;

  m_ogl.fbo_width = pc_depth_cue_fb_width;
  m_ogl.fbo_height = pc_depth_cue_fb_height;

  glBindTexture(GL_TEXTURE_2D, m_ogl.fbo_texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, pc_depth_cue_fb_width, pc_depth_cue_fb_height, 0, GL_RGB,
               GL_UNSIGNED_BYTE, NULL);
  glBindTexture(GL_TEXTURE_2D, 0);

  // DEPTH CUE BASE PAGE VERTEX DATA
  // --------------------------
  // Now that we have a framebuffer to draw each slice to, we need the actual vertex data.
  // We'll take the exact data DMA'd and scale it up the PC dimensions.
  std::vector<SpriteVertex> depth_cue_page_vertices;

  // U-coordinates here will range from [0,512], but the maximum U value in the original is
  // 1024 since the sample texel width is usually 1024. Since we're not using a texture with
  // 2x the width, the maximum U value used to convert UVs to [0,1] should be 512.
  float max_u = fb_sample_width / 2.0f;
  ASSERT(max_u == 512.0f);

  for (const auto& slice : m_draw_slices) {
    math::Vector2f xyoffset = fixed_to_floating_point(
        math::Vector2<s32>((s32)slice.depth_cue_page_setup.xyoffset1.ofx(),
                           (s32)slice.depth_cue_page_setup.xyoffset1.ofy()));

    math::Vector2f xy1 = fixed_to_floating_point(slice.depth_cue_page_draw.xyzf2_1.xy());
    math::Vector2f xy2 = fixed_to_floating_point(slice.depth_cue_page_draw.xyzf2_2.xy());
    math::Vector2f uv1 = fixed_to_floating_point(slice.depth_cue_page_draw.uv_1.xy());
    math::Vector2f uv2 = fixed_to_floating_point(slice.depth_cue_page_draw.uv_2.xy());

    ASSERT(xy1.x() == 0);
    ASSERT(xy1.y() == 0);
    ASSERT(xy2.x() <= 32.0f);
    ASSERT(xy2.y() <= slice_height);

    if (m_debug.override_sharpness) {
      // Undo sharpness from GOAL code and apply custom
      xy2.x() = 32.0f * m_debug.sharpness;
      xy2.y() = 224.0f * m_debug.sharpness;
    }

    // Apply xyoffset GS register
    xy1.x() += xyoffset.x() / 4096.0f;
    xy1.y() += xyoffset.y() / 4096.0f;
    xy2.x() += xyoffset.x() / 4096.0f;
    xy2.y() += xyoffset.y() / 4096.0f;

    // U-coord will range from [0,512], which is half of the original framebuffer sample width
    // Let's also use it to determine the X offset into the depth-cue framebuffer since the
    // original draw assumes each slice is at 0,0.
    float x_offset = (uv1.x() / 512.0f) * (xy2.x() / 32.0f);

    build_sprite(depth_cue_page_vertices,
                 // Top-left
                 (xy1.x() / 512.0f) + x_offset,  // x1
                 xy1.y() / slice_height,         // y1
                 uv1.x() / max_u,                // s1
                 uv1.y() / slice_height,         // t1
                 // Bottom-right
                 (xy2.x() / 512.0f) + x_offset,  // x2
                 xy2.y() / slice_height,         // y2
                 uv2.x() / max_u,                // s2
                 uv2.y() / slice_height          // t2
    );
  }

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.depth_cue_page_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, depth_cue_page_vertices.size() * sizeof(SpriteVertex),
               depth_cue_page_vertices.data(), GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  // ON SCREEN VERTEX DATA
  // --------------------------
  // Finally, we need to draw pixels from the depth-cue-base-page back to the on-screen
  // framebuffer. We'll take the same approach as above.
  std::vector<SpriteVertex> on_screen_vertices;

  for (const auto& slice : m_draw_slices) {
    math::Vector2f xyoffset = fixed_to_floating_point(math::Vector2<s32>(
        (s32)slice.on_screen_setup.xyoffset1.ofx(), (s32)slice.on_screen_setup.xyoffset1.ofy()));

    math::Vector2f xy1 = fixed_to_floating_point(slice.on_screen_draw.xyzf2_1.xy());
    math::Vector2f xy2 = fixed_to_floating_point(slice.on_screen_draw.xyzf2_2.xy());
    math::Vector2f uv1 = fixed_to_floating_point(slice.on_screen_draw.uv_1.xy());
    math::Vector2f uv2 = fixed_to_floating_point(slice.on_screen_draw.uv_2.xy());

    ASSERT(uv1.x() == 0);
    ASSERT(uv1.y() == 0);
    ASSERT(uv2.x() <= 32.0f);
    ASSERT(uv2.y() <= slice_height);

    if (m_debug.override_sharpness) {
      // Undo sharpness from GOAL code and apply custom
      uv2.x() = 32.0f * m_debug.sharpness;
      uv2.y() = 224.0f * m_debug.sharpness;
    }

    // Apply xyoffset GS register
    xy1.x() += xyoffset.x() / 4096.0f;
    xy1.y() += xyoffset.y() / 4096.0f;
    xy2.x() += xyoffset.x() / 4096.0f;
    xy2.y() += xyoffset.y() / 4096.0f;

    // X-coord will range from [0,512], which is half of the original framebuffer sample width
    // Let's also use it to determine the U offset into the on-screen framebuffer since the
    // original draw assumes each slice is at 0,0.
    float u_offset = (xy1.x() / 512.0f) * (uv2.x() / 32.0f);

    build_sprite(on_screen_vertices,
                 // Top-left
                 xy1.x() / 512.0f,               // x1
                 xy1.y() / slice_height,         // y1
                 (uv1.x() / 512.0f) + u_offset,  // s1
                 uv1.y() / slice_height,         // t1
                 // Bottom-right
                 xy2.x() / 512.0f,               // x2
                 xy2.y() / slice_height,         // y2
                 (uv2.x() / 512.0f) + u_offset,  // s2
                 uv2.y() / slice_height          // t2
    );
  }

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.on_screen_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, on_screen_vertices.size() * sizeof(SpriteVertex),
               on_screen_vertices.data(), GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void DepthCue::draw(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // Disable depth writing but keep test
  glEnable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);

  // Activate shader
  auto shader = &render_state->shaders[ShaderId::DEPTH_CUE];
  shader->activate();

  glUniform1i(glGetUniformLocation(shader->id(), "tex"), 0);

  // First, we need to copy the framebuffer into the framebuffer sample texture
  glBindFramebuffer(GL_READ_FRAMEBUFFER, render_state->render_fb);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_ogl.framebuffer_sample_fbo);

  glBlitFramebuffer(render_state->draw_offset_x,                                // srcX0
                    render_state->draw_offset_y,                                // srcY0
                    render_state->draw_offset_x + render_state->draw_region_w,  // srcX1
                    render_state->draw_offset_y + render_state->draw_region_h,  // srcY1
                    0,                                                          // dstX0
                    0,                                                          // dstY0
                    m_ogl.framebuffer_sample_width,                             // dstX1
                    m_ogl.framebuffer_sample_height,                            // dstY1
                    GL_COLOR_BUFFER_BIT,                                        // mask
                    GL_NEAREST                                                  // filter
  );

  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);

  // Next, we need to draw from the framebuffer sample texture to the depth-cue-base-page
  // framebuffer
  {
    const auto& depth_cue_page_draw = m_draw_slices[0].depth_cue_page_draw;

    math::Vector4f colorf = math::Vector4f(
        depth_cue_page_draw.rgbaq.x() / 255.0f, depth_cue_page_draw.rgbaq.y() / 255.0f,
        depth_cue_page_draw.rgbaq.z() / 255.0f, depth_cue_page_draw.rgbaq.w() / 255.0f);
    glUniform4fv(glGetUniformLocation(shader->id(), "u_color"), 1, colorf.data());

    glUniform1f(glGetUniformLocation(shader->id(), "u_depth"), 1.0f);

    glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.fbo);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, m_ogl.framebuffer_sample_tex);

    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_ONE, GL_ZERO);

    glViewport(0, 0, m_ogl.fbo_width, m_ogl.fbo_height);

    prof.add_draw_call();
    prof.add_tri(2 * TOTAL_DRAW_SLICES);

    glBindVertexArray(m_ogl.depth_cue_page_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6 * TOTAL_DRAW_SLICES);  // 6 verts per slice
  }

  // Finally, the contents of depth-cue-base-page need to be overlayed onto the on-screen
  // framebuffer
  {
    const auto& on_screen_draw = m_draw_slices[0].on_screen_draw;

    math::Vector4f colorf =
        math::Vector4f(on_screen_draw.rgbaq.x() / 255.0f, on_screen_draw.rgbaq.y() / 255.0f,
                       on_screen_draw.rgbaq.z() / 255.0f, on_screen_draw.rgbaq.w() / 255.0f);
    if (m_debug.override_alpha) {
      colorf.w() = m_debug.draw_alpha / 2.0f;
    }
    glUniform4fv(glGetUniformLocation(shader->id(), "u_color"), 1, colorf.data());

    if (m_debug.depth == 1.0f) {
      glUniform1f(glGetUniformLocation(shader->id(), "u_depth"), m_debug.depth);
    } else {
      // Scale debug depth expontentially to make the slider easier to use
      glUniform1f(glGetUniformLocation(shader->id(), "u_depth"), pow(m_debug.depth, 8));
    }

    glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, m_ogl.fbo_texture);

    glBlendEquation(GL_FUNC_ADD);
    glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ZERO, GL_ONE);

    glViewport(render_state->draw_offset_x, render_state->draw_offset_y,
               render_state->draw_region_w, render_state->draw_region_h);

    prof.add_draw_call();
    prof.add_tri(2 * TOTAL_DRAW_SLICES);

    glBindVertexArray(m_ogl.on_screen_vao);
    glDrawArrays(GL_TRIANGLES, 0, 6 * TOTAL_DRAW_SLICES);  // 6 verts per slice
  }

  // Done
  glDepthMask(GL_TRUE);
  glBindTexture(GL_TEXTURE_2D, 0);
  glBindVertexArray(0);
}

void DepthCue::build_sprite(std::vector<SpriteVertex>& vertices,
                            float x1,
                            float y1,
                            float s1,
                            float t1,
                            float x2,
                            float y2,
                            float s2,
                            float t2) {
  // First triangle
  // -------------
  // Top-left
  vertices.push_back(SpriteVertex(x1, y1, s1, t1));

  // Top-right
  vertices.push_back(SpriteVertex(x2, y1, s2, t1));

  // Bottom-left
  vertices.push_back(SpriteVertex(x1, y2, s1, t2));

  // Second triangle
  // -------------
  // Top-right
  vertices.push_back(SpriteVertex(x2, y1, s2, t1));

  // Bottom-left
  vertices.push_back(SpriteVertex(x1, y2, s1, t2));

  // Bottom-right
  vertices.push_back(SpriteVertex(x2, y2, s2, t2));
}

void DepthCue::draw_debug_window() {
  ImGui::Text("NOTE: depth-cue may be disabled by '*vu1-enable-user-menu*'!");

  ImGui::Checkbox("Cache setup", &m_debug.cache_setup);
  ImGui::Checkbox("Force original resolution", &m_debug.force_original_res);

  ImGui::Checkbox("Override alpha", &m_debug.override_alpha);
  if (m_debug.override_alpha) {
    ImGui::SliderFloat("Alpha", &m_debug.draw_alpha, 0.0f, 1.0f);
  }

  ImGui::Checkbox("Override sharpness", &m_debug.override_sharpness);
  if (m_debug.override_sharpness) {
    ImGui::SliderFloat("Sharpness", &m_debug.sharpness, 0.001f, 1.0f);
  }

  ImGui::SliderFloat("Depth", &m_debug.depth, 0.0f, 1.0f);
  ImGui::SliderFloat("Resolution scale", &m_debug.res_scale, 0.001f, 2.0f);

  if (ImGui::Button("Reset")) {
    m_debug.draw_alpha = 0.4f;
    m_debug.sharpness = 0.999f;
    m_debug.depth = 1.0f;
    m_debug.res_scale = 1.0f;
  }
}
