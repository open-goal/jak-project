

#include "Sprite3.h"

#include "common/log/log.h"

#include "game/graphics/opengl_renderer/background/background_common.h"
#include "game/graphics/opengl_renderer/dma_helpers.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

namespace {

/*!
 * Does the next DMA transfer look like it could be the start of a 2D group?
 */
bool looks_like_2d_chunk_start(const DmaFollower& dma) {
  return dma.current_tag().qwc == 1 && dma.current_tag().kind == DmaTag::Kind::CNT;
}

/*!
 * Read the header. Asserts if it's bad.
 * Returns the number of sprites.
 * Advances 1 dma transfer
 */
u32 process_sprite_chunk_header(DmaFollower& dma) {
  auto transfer = dma.read_and_advance();
  // note that flg = true, this should use double buffering
  bool ok = verify_unpack_with_stcycl(transfer, VifCode::Kind::UNPACK_V4_32, 4, 4, 1,
                                      SpriteDataMem::Header, false, true);
  ASSERT(ok);
  u32 header[4];
  memcpy(header, transfer.data, 16);
  ASSERT(header[0] <= Sprite3::SPRITES_PER_CHUNK);
  return header[0];
}

/*!
 * Does the next DMA transfer look like the frame data for sprite distort?
 */
bool looks_like_distort_frame_data(const DmaFollower& dma) {
  return dma.current_tag().kind == DmaTag::Kind::CNT &&
         dma.current_tag_vifcode0().kind == VifCode::Kind::NOP &&
         dma.current_tag_vifcode1().kind == VifCode::Kind::UNPACK_V4_32;
}
}  // namespace

constexpr int SPRITE_RENDERER_MAX_SPRITES = 1920 * 10;
constexpr int SPRITE_RENDERER_MAX_DISTORT_SPRITES =
    256 * 10;  // size of sprite-aux-list in GOAL code * SPRITE_MAX_AMOUNT_MULT

Sprite3::Sprite3(const std::string& name, int my_id)
    : BucketRenderer(name, my_id), m_direct(name, my_id, 1024) {
  opengl_setup();
}

void Sprite3::opengl_setup() {
  // Set up OpenGL for 'normal' sprites
  opengl_setup_normal();

  // Set up OpenGL for distort sprites
  opengl_setup_distort();
}

void Sprite3::opengl_setup_normal() {
  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenVertexArrays(1, &m_ogl.vao);
  glBindVertexArray(m_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  auto verts = SPRITE_RENDERER_MAX_SPRITES * 4;
  auto bytes = verts * sizeof(SpriteVertex3D);
  glBufferData(GL_ARRAY_BUFFER, bytes, nullptr, GL_STREAM_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(
      0,                                       // location 0 in the shader
      4,                                       // 4 floats per vert (w unused)
      GL_FLOAT,                                // floats
      GL_TRUE,                                 // normalized, ignored,
      sizeof(SpriteVertex3D),                  //
      (void*)offsetof(SpriteVertex3D, xyz_sx)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(1);
  glVertexAttribPointer(
      1,                                        // location 1 in the shader
      4,                                        // 4 color components
      GL_FLOAT,                                 // floats
      GL_TRUE,                                  // normalized, ignored,
      sizeof(SpriteVertex3D),                   //
      (void*)offsetof(SpriteVertex3D, quat_sy)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(2);
  glVertexAttribPointer(
      2,                                     // location 2 in the shader
      4,                                     // 4 color components
      GL_FLOAT,                              // floats
      GL_TRUE,                               // normalized, ignored,
      sizeof(SpriteVertex3D),                //
      (void*)offsetof(SpriteVertex3D, rgba)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(3);
  glVertexAttribIPointer(
      3,                                             // location 3 in the shader
      2,                                             // 4 color components
      GL_UNSIGNED_SHORT,                             // floats
      sizeof(SpriteVertex3D),                        //
      (void*)offsetof(SpriteVertex3D, flags_matrix)  // offset in array (why is this a pointer...)
  );

  glEnableVertexAttribArray(4);
  glVertexAttribIPointer(
      4,                                     // location 4 in the shader
      4,                                     // 3 floats per vert
      GL_UNSIGNED_SHORT,                     // floats
      sizeof(SpriteVertex3D),                //
      (void*)offsetof(SpriteVertex3D, info)  // offset in array (why is this a pointer...)
  );
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  u32 idx_buffer_len = SPRITE_RENDERER_MAX_SPRITES * 5;
  glGenBuffers(1, &m_ogl.index_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_len * sizeof(u32), nullptr, GL_STREAM_DRAW);

  glBindVertexArray(0);

  m_vertices_3d.resize(verts);
  m_index_buffer_data.resize(idx_buffer_len);

  m_default_mode.disable_depth_write();
  m_default_mode.set_depth_test(GsTest::ZTest::GEQUAL);
  m_default_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
  m_default_mode.set_aref(38);
  m_default_mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
  m_default_mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
  m_default_mode.set_at(true);
  m_default_mode.set_zt(true);
  m_default_mode.set_ab(true);

  m_current_mode = m_default_mode;
}

void Sprite3::opengl_setup_distort() {
  // Create framebuffer to snapshot current render to a texture that can be bound for the distort
  // shader This will represent tex0 from the original GS data
  glGenFramebuffers(1, &m_distort_ogl.fbo);
  glBindFramebuffer(GL_FRAMEBUFFER, m_distort_ogl.fbo);

  glGenTextures(1, &m_distort_ogl.fbo_texture);
  glBindTexture(GL_TEXTURE_2D, m_distort_ogl.fbo_texture);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, m_distort_ogl.fbo_width, m_distort_ogl.fbo_height, 0,
               GL_RGB, GL_UNSIGNED_BYTE, NULL);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  // Texture clamping here matches the GS init data for distort
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                         m_distort_ogl.fbo_texture, 0);

  ASSERT(glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE);

  glBindTexture(GL_TEXTURE_2D, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  // Non-instancing
  // ----------------------
  glGenBuffers(1, &m_distort_ogl.vertex_buffer);
  glGenVertexArrays(1, &m_distort_ogl.vao);
  glBindVertexArray(m_distort_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_distort_ogl.vertex_buffer);
  // note: each sprite shares a single vertex per slice, account for that here
  int distort_vert_buffer_len =
      SPRITE_RENDERER_MAX_DISTORT_SPRITES *
      ((5 - 1) * 11 + 1);  // max * ((verts_per_slice - 1) * max_slices + 1)
  glBufferData(GL_ARRAY_BUFFER, distort_vert_buffer_len * sizeof(SpriteDistortVertex), nullptr,
               GL_DYNAMIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                                         // location 0 in the shader
                        3,                                         // 3 floats per vert
                        GL_FLOAT,                                  // floats
                        GL_FALSE,                                  // don't normalize, ignored
                        sizeof(SpriteDistortVertex),               //
                        (void*)offsetof(SpriteDistortVertex, xyz)  // offset in array
  );
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1,                                        // location 1 in the shader
                        2,                                        // 2 floats per vert
                        GL_FLOAT,                                 // floats
                        GL_FALSE,                                 // don't normalize, ignored
                        sizeof(SpriteDistortVertex),              //
                        (void*)offsetof(SpriteDistortVertex, st)  // offset in array
  );

  // note: add one extra element per sprite that marks the end of a triangle strip
  int distort_idx_buffer_len = SPRITE_RENDERER_MAX_DISTORT_SPRITES *
                               ((5 * 11) + 1);  // max * ((verts_per_slice * max_slices) + 1)
  glGenBuffers(1, &m_distort_ogl.index_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_distort_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, distort_idx_buffer_len * sizeof(u32), nullptr,
               GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  m_sprite_distorter_vertices.resize(distort_vert_buffer_len);
  m_sprite_distorter_indices.resize(distort_idx_buffer_len);
  m_sprite_distorter_frame_data.resize(SPRITE_RENDERER_MAX_DISTORT_SPRITES);

  // Instancing
  // ----------------------
  glGenVertexArrays(1, &m_distort_instanced_ogl.vao);
  glBindVertexArray(m_distort_instanced_ogl.vao);

  int distort_max_sprite_slices = 0;
  for (int i = 3; i < 12; i++) {
    // For each 'resolution', there can be that many slices
    distort_max_sprite_slices += i;
  }

  glGenBuffers(1, &m_distort_instanced_ogl.vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_distort_instanced_ogl.vertex_buffer);

  int distort_instanced_vert_buffer_len = distort_max_sprite_slices * 5;  // 5 vertices per slice
  glBufferData(GL_ARRAY_BUFFER, distort_instanced_vert_buffer_len * sizeof(SpriteDistortVertex),
               nullptr, GL_STREAM_DRAW);

  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                                         // location 0 in the shader
                        3,                                         // 3 floats per vert
                        GL_FLOAT,                                  // floats
                        GL_FALSE,                                  // don't normalize, ignored
                        sizeof(SpriteDistortVertex),               //
                        (void*)offsetof(SpriteDistortVertex, xyz)  // offset in array
  );
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1,                                        // location 1 in the shader
                        2,                                        // 2 floats per vert
                        GL_FLOAT,                                 // floats
                        GL_FALSE,                                 // don't normalize, ignored
                        sizeof(SpriteDistortVertex),              //
                        (void*)offsetof(SpriteDistortVertex, st)  // offset in array
  );

  glGenBuffers(1, &m_distort_instanced_ogl.instance_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_distort_instanced_ogl.instance_buffer);

  int distort_instance_buffer_len = SPRITE_RENDERER_MAX_DISTORT_SPRITES;
  glBufferData(GL_ARRAY_BUFFER, distort_instance_buffer_len * sizeof(SpriteDistortInstanceData),
               nullptr, GL_DYNAMIC_DRAW);

  glEnableVertexAttribArray(2);
  glVertexAttribPointer(2,                                  // location 2 in the shader
                        4,                                  // 4 floats per vert
                        GL_FLOAT,                           // floats
                        GL_FALSE,                           // normalized, ignored,
                        sizeof(SpriteDistortInstanceData),  //
                        (void*)offsetof(SpriteDistortInstanceData, x_y_z_s)  // offset in array
  );
  glEnableVertexAttribArray(3);
  glVertexAttribPointer(3,                                  // location 3 in the shader
                        4,                                  // 4 floats per vert
                        GL_FLOAT,                           // floats
                        GL_FALSE,                           // normalized, ignored,
                        sizeof(SpriteDistortInstanceData),  //
                        (void*)offsetof(SpriteDistortInstanceData, sx_sy_sz_t)  // offset in array
  );

  glVertexAttribDivisor(2, 1);
  glVertexAttribDivisor(3, 1);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  m_sprite_distorter_vertices_instanced.resize(distort_instanced_vert_buffer_len);

  for (int i = 3; i < 12; i++) {
    auto vec = std::vector<SpriteDistortInstanceData>();
    vec.resize(distort_instance_buffer_len);

    m_sprite_distorter_instances_by_res[i] = vec;
  }
}

/*!
 * Run the sprite distorter.
 */
void Sprite3::render_distorter(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  // Skip to distorter DMA
  m_direct.reset_state();
  while (dma.current_tag().qwc != 7) {
    auto direct_data = dma.read_and_advance();
    m_direct.render_vif(direct_data.vif0(), direct_data.vif1(), direct_data.data,
                        direct_data.size_bytes, render_state, prof);
  }
  m_direct.flush_pending(render_state, prof);

  // Read DMA
  {
    auto prof_node = prof.make_scoped_child("dma");
    distort_dma(dma, prof_node);
  }

  if (!m_enabled || !m_distort_enable) {
    // Distort disabled, we can stop here since all the DMA has been read
    return;
  }

  // Set up vertex data
  {
    auto prof_node = prof.make_scoped_child("setup");
    if (m_enable_distort_instancing) {
      distort_setup_instanced(prof_node);
    } else {
      distort_setup(prof_node);
    }
  }

  // Draw
  {
    auto prof_node = prof.make_scoped_child("drawing");
    if (m_enable_distort_instancing) {
      distort_draw_instanced(render_state, prof_node);
    } else {
      distort_draw(render_state, prof_node);
    }
  }
}

/*!
 * Reads all sprite distort related DMA packets.
 */
void Sprite3::distort_dma(DmaFollower& dma, ScopedProfilerNode& /*prof*/) {
  // First should be the GS setup
  auto sprite_distorter_direct_setup = dma.read_and_advance();
  ASSERT(sprite_distorter_direct_setup.vifcode0().kind == VifCode::Kind::NOP);
  ASSERT(sprite_distorter_direct_setup.vifcode1().kind == VifCode::Kind::DIRECT);
  ASSERT(sprite_distorter_direct_setup.vifcode1().immediate == 7);
  memcpy(&m_sprite_distorter_setup, sprite_distorter_direct_setup.data, 7 * 16);

  auto gif_tag = m_sprite_distorter_setup.gif_tag;
  ASSERT(gif_tag.nloop() == 1);
  ASSERT(gif_tag.eop() == 1);
  ASSERT(gif_tag.nreg() == 6);
  ASSERT(gif_tag.reg(0) == GifTag::RegisterDescriptor::AD);

  auto zbuf1 = m_sprite_distorter_setup.zbuf;
  ASSERT(zbuf1.zbp() == 0x1c0);
  ASSERT(zbuf1.zmsk() == true);
  ASSERT(zbuf1.psm() == TextureFormat::PSMZ24);

  auto tex0 = m_sprite_distorter_setup.tex0;
  ASSERT(tex0.tbw() == 8);
  ASSERT(tex0.tw() == 9);
  ASSERT(tex0.th() == 8);

  auto tex1 = m_sprite_distorter_setup.tex1;
  ASSERT(tex1.mmag() == true);
  ASSERT(tex1.mmin() == 1);

  auto alpha = m_sprite_distorter_setup.alpha;
  ASSERT(alpha.a_mode() == GsAlpha::BlendMode::SOURCE);
  ASSERT(alpha.b_mode() == GsAlpha::BlendMode::DEST);
  ASSERT(alpha.c_mode() == GsAlpha::BlendMode::SOURCE);
  ASSERT(alpha.d_mode() == GsAlpha::BlendMode::DEST);

  // Next is the aspect used by the sine tables (PC only)
  //
  // This was added to let the renderer reliably detect when the sine tables changed,
  // which is whenever the aspect ratio changed. However, the tables aren't always
  // updated on the same frame that the aspect changed, so this just lets the game
  // easily notify the renderer when it finally does get updated.
  auto sprite_distort_tables_aspect = dma.read_and_advance();
  ASSERT(sprite_distort_tables_aspect.size_bytes == 16);
  ASSERT(sprite_distort_tables_aspect.vifcode1().kind == VifCode::Kind::PC_PORT);
  memcpy(&m_sprite_distorter_sine_tables_aspect, sprite_distort_tables_aspect.data,
         sizeof(math::Vector4f));

  // Next thing should be the sine tables
  auto sprite_distorter_tables = dma.read_and_advance();
  unpack_to_stcycl(&m_sprite_distorter_sine_tables, sprite_distorter_tables,
                   VifCode::Kind::UNPACK_V4_32, 4, 4, 0x8b * 16, 0x160, false, false);

  ASSERT(GsPrim(m_sprite_distorter_sine_tables.gs_gif_tag.prim()).kind() ==
         GsPrim::Kind::TRI_STRIP);

  // Finally, should be frame data packets (containing sprites)
  // Up to 170 sprites will be DMA'd at a time followed by a mscalf,
  // and this process can happen twice up to a maximum of 256 sprites DMA'd
  // (256 is the size of sprite-aux-list which drives this).
  int sprite_idx = 0;
  m_distort_stats.total_sprites = 0;

  while (looks_like_distort_frame_data(dma)) {
    math::Vector<u32, 4> num_sprites_vec;

    // Read sprite packets
    do {
      int qwc = dma.current_tag().qwc;
      int dest = dma.current_tag_vifcode1().immediate;
      auto distort_data = dma.read_and_advance();

      if (dest == 511) {
        // VU address 511 specifies the number of sprites
        unpack_to_no_stcycl(&num_sprites_vec, distort_data, VifCode::Kind::UNPACK_V4_32, 16, dest,
                            false, false);
      } else {
        // VU address >= 512 is the actual vertex data
        ASSERT(dest >= 512);
        ASSERT(sprite_idx + (qwc / 3) <= (int)m_sprite_distorter_frame_data.capacity());

        unpack_to_no_stcycl(&m_sprite_distorter_frame_data.at(sprite_idx), distort_data,
                            VifCode::Kind::UNPACK_V4_32, qwc * 16, dest, false, false);

        sprite_idx += qwc / 3;
      }
    } while (looks_like_distort_frame_data(dma));

    // Sprite packets should always end with a mscalf flush
    ASSERT(dma.current_tag().kind == DmaTag::Kind::CNT);
    ASSERT(dma.current_tag_vifcode0().kind == VifCode::Kind::MSCALF);
    ASSERT(dma.current_tag_vifcode1().kind == VifCode::Kind::FLUSH);
    dma.read_and_advance();

    m_distort_stats.total_sprites += num_sprites_vec.x();
  }

  // Done
  ASSERT(m_distort_stats.total_sprites <= SPRITE_RENDERER_MAX_DISTORT_SPRITES);
}

/*!
 * Sets up OpenGL data for each distort sprite.
 */
void Sprite3::distort_setup(ScopedProfilerNode& /*prof*/) {
  m_distort_stats.total_tris = 0;

  m_sprite_distorter_vertices.clear();
  m_sprite_distorter_indices.clear();

  int sprite_idx = 0;
  int sprites_left = m_distort_stats.total_sprites;

  // This part is mostly ripped from the VU program
  while (sprites_left != 0) {
    // flag seems to represent the 'resolution' of the circle sprite used to create the distortion
    // effect For example, a flag value of 3 will create a circle using 3 "pie-slice" shapes
    u32 flag = m_sprite_distorter_frame_data.at(sprite_idx).flag;
    u32 slices_left = flag;

    // flag has a minimum value of 3 which represents the first ientry
    // Additionally, the ientry index has 352 added to it (which is the start of the entry array
    // in VU memory), so we need to subtract that as well
    int entry_index = m_sprite_distorter_sine_tables.ientry[flag - 3].x() - 352;

    // Here would be adding the giftag, but we don't need that

    // Get the frame data for the next distort sprite
    SpriteDistortFrameData frame_data = m_sprite_distorter_frame_data.at(sprite_idx);
    sprite_idx++;

    // Build the OpenGL data for the sprite
    math::Vector2f vf03 = frame_data.st;
    math::Vector3f vf14 = frame_data.xyz;

    // Each slice shares a center vertex, we can use this fact and cut out duplicate vertices
    u32 center_vert_idx = m_sprite_distorter_vertices.size();
    m_sprite_distorter_vertices.push_back({vf14, vf03});

    do {
      math::Vector3f vf06 = m_sprite_distorter_sine_tables.entry[entry_index++].xyz();
      math::Vector2f vf07 = m_sprite_distorter_sine_tables.entry[entry_index++].xy();
      math::Vector3f vf08 = m_sprite_distorter_sine_tables.entry[entry_index + 0].xyz();
      math::Vector2f vf09 = m_sprite_distorter_sine_tables.entry[entry_index + 1].xy();

      slices_left--;

      math::Vector2f vf11 = (vf07 * frame_data.rgba.z()) + frame_data.st;
      math::Vector2f vf13 = (vf09 * frame_data.rgba.z()) + frame_data.st;
      math::Vector3f vf06_2 = (vf06 * frame_data.rgba.x()) + frame_data.xyz;
      math::Vector2f vf07_2 = (vf07 * frame_data.rgba.x()) + frame_data.st;
      math::Vector3f vf08_2 = (vf08 * frame_data.rgba.x()) + frame_data.xyz;
      math::Vector2f vf09_2 = (vf09 * frame_data.rgba.x()) + frame_data.st;
      math::Vector3f vf10 = (vf06 * frame_data.rgba.y()) + frame_data.xyz;
      math::Vector3f vf12 = (vf08 * frame_data.rgba.y()) + frame_data.xyz;
      math::Vector3f vf06_3 = vf06_2;
      math::Vector3f vf08_3 = vf08_2;

      m_sprite_distorter_indices.push_back(m_sprite_distorter_vertices.size());
      m_sprite_distorter_vertices.push_back({vf06_3, vf07_2});

      m_sprite_distorter_indices.push_back(m_sprite_distorter_vertices.size());
      m_sprite_distorter_vertices.push_back({vf08_3, vf09_2});

      m_sprite_distorter_indices.push_back(m_sprite_distorter_vertices.size());
      m_sprite_distorter_vertices.push_back({vf10, vf11});

      m_sprite_distorter_indices.push_back(m_sprite_distorter_vertices.size());
      m_sprite_distorter_vertices.push_back({vf12, vf13});

      // Originally, would add the shared center vertex, but in our case we can just add the index
      m_sprite_distorter_indices.push_back(center_vert_idx);
      // m_sprite_distorter_vertices.push_back({vf14, vf03});

      m_distort_stats.total_tris += 2;
    } while (slices_left != 0);

    // Mark the end of the triangle strip
    m_sprite_distorter_indices.push_back(UINT32_MAX);

    sprites_left--;
  }
}

/*!
 * Sets up OpenGL data for rendering distort sprites using instanced rendering.
 *
 * A mesh is built once for each possible sprite resolution and is only re-built
 * when the dimensions of the window are changed. These meshes are built just like
 * the triangle strips in the VU program, but with the sprite-specific data removed.
 *
 * Required sprite-specific frame data is kept as is and is grouped by resolution.
 */
void Sprite3::distort_setup_instanced(ScopedProfilerNode& /*prof*/) {
  if (m_distort_instanced_ogl.last_aspect_x != m_sprite_distorter_sine_tables_aspect.x() ||
      m_distort_instanced_ogl.last_aspect_y != m_sprite_distorter_sine_tables_aspect.y()) {
    m_distort_instanced_ogl.last_aspect_x = m_sprite_distorter_sine_tables_aspect.x();
    m_distort_instanced_ogl.last_aspect_y = m_sprite_distorter_sine_tables_aspect.y();
    // Aspect ratio changed, which means we have a new sine table
    m_sprite_distorter_vertices_instanced.clear();

    // Build a mesh for every possible distort sprite resolution
    auto vf03 = math::Vector2f(0, 0);
    auto vf14 = math::Vector3f(0, 0, 0);

    for (int res = 3; res < 12; res++) {
      int entry_index = m_sprite_distorter_sine_tables.ientry[res - 3].x() - 352;

      for (int i = 0; i < res; i++) {
        math::Vector3f vf06 = m_sprite_distorter_sine_tables.entry[entry_index++].xyz();
        math::Vector2f vf07 = m_sprite_distorter_sine_tables.entry[entry_index++].xy();
        math::Vector3f vf08 = m_sprite_distorter_sine_tables.entry[entry_index + 0].xyz();
        math::Vector2f vf09 = m_sprite_distorter_sine_tables.entry[entry_index + 1].xy();

        // Normally, there would be a bunch of transformations here against the sprite data.
        // Instead, we'll let the shader do it and just store the sine table specific parts here.

        m_sprite_distorter_vertices_instanced.push_back({vf06, vf07});
        m_sprite_distorter_vertices_instanced.push_back({vf08, vf09});
        m_sprite_distorter_vertices_instanced.push_back({vf06, vf07});
        m_sprite_distorter_vertices_instanced.push_back({vf08, vf09});
        m_sprite_distorter_vertices_instanced.push_back({vf14, vf03});
      }
    }

    m_distort_instanced_ogl.vertex_data_changed = true;
  }

  // Set up instance data for each sprite
  m_distort_stats.total_tris = 0;

  for (auto& [res, vec] : m_sprite_distorter_instances_by_res) {
    vec.clear();
  }

  for (int i = 0; i < m_distort_stats.total_sprites; i++) {
    SpriteDistortFrameData frame_data = m_sprite_distorter_frame_data.at(i);

    // Shader just needs the position, tex coords, and scale
    auto x_y_z_s = math::Vector4f(frame_data.xyz.x(), frame_data.xyz.y(), frame_data.xyz.z(),
                                  frame_data.st.x());
    auto sx_sy_sz_t = math::Vector4f(frame_data.rgba.x(), frame_data.rgba.y(), frame_data.rgba.z(),
                                     frame_data.st.y());

    int res = frame_data.flag;

    m_sprite_distorter_instances_by_res[res].push_back({x_y_z_s, sx_sy_sz_t});

    m_distort_stats.total_tris += res * 2;
  }
}

/*!
 * Draws each distort sprite.
 */
void Sprite3::distort_draw(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // First, make sure the distort framebuffer is the correct size
  distort_setup_framebuffer_dims(render_state);

  if (m_distort_stats.total_tris == 0) {
    // No distort sprites to draw, we can end early
    return;
  }

  // Do common distort drawing logic
  distort_draw_common(render_state, prof);

  // Set up shader
  auto shader = &render_state->shaders[ShaderId::SPRITE_DISTORT];
  shader->activate();

  Vector4f colorf = Vector4f(m_sprite_distorter_sine_tables.color.x() / 255.0f,
                             m_sprite_distorter_sine_tables.color.y() / 255.0f,
                             m_sprite_distorter_sine_tables.color.z() / 255.0f,
                             m_sprite_distorter_sine_tables.color.w() / 255.0f);
  glUniform4fv(glGetUniformLocation(shader->id(), "u_color"), 1, colorf.data());

  // Bind vertex array
  glBindVertexArray(m_distort_ogl.vao);

  // Enable prim restart, we need this to break up the triangle strips
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  // Upload vertex data
  glBindBuffer(GL_ARRAY_BUFFER, m_distort_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_sprite_distorter_vertices.size() * sizeof(SpriteDistortVertex),
               m_sprite_distorter_vertices.data(), GL_DYNAMIC_DRAW);

  // Upload element data
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_distort_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_sprite_distorter_indices.size() * sizeof(u32),
               m_sprite_distorter_indices.data(), GL_DYNAMIC_DRAW);

  // Draw
  prof.add_draw_call();
  prof.add_tri(m_distort_stats.total_tris);

  glDrawElements(GL_TRIANGLE_STRIP, m_sprite_distorter_indices.size(), GL_UNSIGNED_INT, (void*)0);

  // Done
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

/*!
 * Draws each distort sprite using instanced rendering.
 */
void Sprite3::distort_draw_instanced(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // First, make sure the distort framebuffer is the correct size
  distort_setup_framebuffer_dims(render_state);

  if (m_distort_stats.total_tris == 0) {
    // No distort sprites to draw, we can end early
    return;
  }

  // Do common distort drawing logic
  distort_draw_common(render_state, prof);

  // Set up shader
  auto shader = &render_state->shaders[ShaderId::SPRITE_DISTORT_INSTANCED];
  shader->activate();

  Vector4f colorf = Vector4f(m_sprite_distorter_sine_tables.color.x() / 255.0f,
                             m_sprite_distorter_sine_tables.color.y() / 255.0f,
                             m_sprite_distorter_sine_tables.color.z() / 255.0f,
                             m_sprite_distorter_sine_tables.color.w() / 255.0f);
  glUniform4fv(glGetUniformLocation(shader->id(), "u_color"), 1, colorf.data());

  // Bind vertex array
  glBindVertexArray(m_distort_instanced_ogl.vao);

  // Upload vertex data (if it changed)
  if (m_distort_instanced_ogl.vertex_data_changed) {
    m_distort_instanced_ogl.vertex_data_changed = false;

    glBindBuffer(GL_ARRAY_BUFFER, m_distort_instanced_ogl.vertex_buffer);
    glBufferData(GL_ARRAY_BUFFER,
                 m_sprite_distorter_vertices_instanced.size() * sizeof(SpriteDistortVertex),
                 m_sprite_distorter_vertices_instanced.data(), GL_STREAM_DRAW);
  }

  // Draw each resolution group
  glBindBuffer(GL_ARRAY_BUFFER, m_distort_instanced_ogl.instance_buffer);
  prof.add_tri(m_distort_stats.total_tris);

  int vert_offset = 0;
  for (int res = 3; res < 12; res++) {
    auto& instances = m_sprite_distorter_instances_by_res[res];
    int num_verts = res * 5;

    if (instances.size() > 0) {
      // Upload instance data
      glBufferData(GL_ARRAY_BUFFER, instances.size() * sizeof(SpriteDistortInstanceData),
                   instances.data(), GL_DYNAMIC_DRAW);

      // Draw
      prof.add_draw_call();

      glDrawArraysInstanced(GL_TRIANGLE_STRIP, vert_offset, num_verts, instances.size());
    }

    vert_offset += num_verts;
  }

  // Done
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

void Sprite3::distort_draw_common(SharedRenderState* render_state, ScopedProfilerNode& /*prof*/) {
  // The distort effect needs to read the current framebuffer, so copy what's been rendered so far
  // to a texture that we can then pass to the shader
  glBindFramebuffer(GL_READ_FRAMEBUFFER, render_state->render_fb);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_distort_ogl.fbo);

  glBlitFramebuffer(render_state->render_fb_x,                              // srcX0
                    render_state->render_fb_y,                              // srcY0
                    render_state->render_fb_x + render_state->render_fb_w,  // srcX1
                    render_state->render_fb_y + render_state->render_fb_h,  // srcY1
                    0,                                                      // dstX0
                    0,                                                      // dstY0
                    m_distort_ogl.fbo_width,                                // dstX1
                    m_distort_ogl.fbo_height,                               // dstY1
                    GL_COLOR_BUFFER_BIT,                                    // mask
                    GL_NEAREST                                              // filter
  );

  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);

  // Set up OpenGL state
  m_current_mode.set_depth_write_enable(!m_sprite_distorter_setup.zbuf.zmsk());  // zbuf
  glBindTexture(GL_TEXTURE_2D, m_distort_ogl.fbo_texture);                       // tex0
  m_current_mode.set_filt_enable(m_sprite_distorter_setup.tex1.mmag());          // tex1
  update_mode_from_alpha1(m_sprite_distorter_setup.alpha.data, m_current_mode);  // alpha1
  // note: clamp and miptbp are skipped since that is set up ahead of time with the distort
  // framebuffer texture

  setup_opengl_from_draw_mode(m_current_mode, GL_TEXTURE0, false);
}

void Sprite3::distort_setup_framebuffer_dims(SharedRenderState* render_state) {
  // Distort framebuffer must be the same dimensions as the default window framebuffer
  if (m_distort_ogl.fbo_width != render_state->render_fb_w ||
      m_distort_ogl.fbo_height != render_state->render_fb_h) {
    m_distort_ogl.fbo_width = render_state->render_fb_w;
    m_distort_ogl.fbo_height = render_state->render_fb_h;

    glBindTexture(GL_TEXTURE_2D, m_distort_ogl.fbo_texture);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, m_distort_ogl.fbo_width, m_distort_ogl.fbo_height, 0,
                 GL_RGB, GL_UNSIGNED_BYTE, NULL);

    glBindTexture(GL_TEXTURE_2D, 0);
  }
}

/*!
 * Handle DMA data that does the per-frame setup.
 * This should get the dma chain immediately after the call to sprite-draw-distorters.
 * It ends right before the sprite-add-matrix-data for the 3d's
 */
void Sprite3::handle_sprite_frame_setup(DmaFollower& dma, GameVersion version) {
  // first is some direct data
  auto direct_data = dma.read_and_advance();
  ASSERT(direct_data.size_bytes == 3 * 16);
  memcpy(m_sprite_direct_setup, direct_data.data, 3 * 16);

  // next would be the program, but it's 0 size on the PC and isn't sent.

  // next is the "frame data"
  switch (version) {
    case GameVersion::Jak1: {
      auto frame_data = dma.read_and_advance();
      ASSERT(frame_data.size_bytes == (int)sizeof(SpriteFrameDataJak1));  // very cool
      ASSERT(frame_data.vifcode0().kind == VifCode::Kind::STCYCL);
      VifCodeStcycl frame_data_stcycl(frame_data.vifcode0());
      ASSERT(frame_data_stcycl.cl == 4);
      ASSERT(frame_data_stcycl.wl == 4);
      ASSERT(frame_data.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
      VifCodeUnpack frame_data_unpack(frame_data.vifcode1());
      ASSERT(frame_data_unpack.addr_qw == SpriteDataMem::FrameData);
      ASSERT(frame_data_unpack.use_tops_flag == false);
      SpriteFrameDataJak1 jak1_data;
      memcpy(&jak1_data, frame_data.data, sizeof(SpriteFrameDataJak1));
      m_frame_data.from_jak1(jak1_data);
    } break;
    case GameVersion::Jak2: {
      auto frame_data = dma.read_and_advance();
      ASSERT(frame_data.size_bytes == (int)sizeof(SpriteFrameData));  // very cool
      ASSERT(frame_data.vifcode0().kind == VifCode::Kind::STCYCL);
      VifCodeStcycl frame_data_stcycl(frame_data.vifcode0());
      ASSERT(frame_data_stcycl.cl == 4);
      ASSERT(frame_data_stcycl.wl == 4);
      ASSERT(frame_data.vifcode1().kind == VifCode::Kind::UNPACK_V4_32);
      VifCodeUnpack frame_data_unpack(frame_data.vifcode1());
      ASSERT(frame_data_unpack.addr_qw == SpriteDataMem::FrameData);
      ASSERT(frame_data_unpack.use_tops_flag == false);
      memcpy(&m_frame_data, frame_data.data, sizeof(SpriteFrameData));
    } break;
    default:
      ASSERT_NOT_REACHED();
  }

  // next, a MSCALF.
  auto mscalf = dma.read_and_advance();
  ASSERT(mscalf.size_bytes == 0);
  ASSERT(mscalf.vifcode0().kind == VifCode::Kind::MSCALF);
  ASSERT(mscalf.vifcode0().immediate == SpriteProgMem::Init);
  ASSERT(mscalf.vifcode1().kind == VifCode::Kind::FLUSHE);

  // next base and offset
  auto base_offset = dma.read_and_advance();
  ASSERT(base_offset.size_bytes == 0);
  ASSERT(base_offset.vifcode0().kind == VifCode::Kind::BASE);
  ASSERT(base_offset.vifcode0().immediate == SpriteDataMem::Buffer0);
  ASSERT(base_offset.vifcode1().kind == VifCode::Kind::OFFSET);
  ASSERT(base_offset.vifcode1().immediate == SpriteDataMem::Buffer1);
}

void Sprite3::render_3d(DmaFollower& dma) {
  // one time matrix data
  auto matrix_data = dma.read_and_advance();
  ASSERT(matrix_data.size_bytes == sizeof(Sprite3DMatrixData));

  bool unpack_ok = verify_unpack_with_stcycl(matrix_data, VifCode::Kind::UNPACK_V4_32, 4, 4, 5,
                                             SpriteDataMem::Matrix, false, false);
  ASSERT(unpack_ok);
  static_assert(sizeof(m_3d_matrix_data) == 5 * 16);
  memcpy(&m_3d_matrix_data, matrix_data.data, sizeof(m_3d_matrix_data));
  // TODO
}

void Sprite3::render_2d_group0(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  // opengl sprite frame setup
  auto shid = render_state->shaders[ShaderId::SPRITE3].id();
  glUniform4fv(glGetUniformLocation(shid, "hvdf_offset"), 1, m_3d_matrix_data.hvdf_offset.data());
  glUniform1f(glGetUniformLocation(shid, "pfog0"), m_frame_data.pfog0);
  glUniform1f(glGetUniformLocation(shid, "min_scale"), m_frame_data.min_scale);
  glUniform1f(glGetUniformLocation(shid, "max_scale"), m_frame_data.max_scale);
  glUniform1f(glGetUniformLocation(shid, "fog_min"), m_frame_data.fog_min);
  glUniform1f(glGetUniformLocation(shid, "fog_max"), m_frame_data.fog_max);
  // glUniform1f(glGetUniformLocation(shid, "bonus"), m_frame_data.bonus);
  // glUniform4fv(glGetUniformLocation(shid, "hmge_scale"), 1, m_frame_data.hmge_scale.data());
  glUniform1f(glGetUniformLocation(shid, "deg_to_rad"), m_frame_data.deg_to_rad);
  glUniform1f(glGetUniformLocation(shid, "inv_area"), m_frame_data.inv_area);
  glUniformMatrix4fv(glGetUniformLocation(shid, "camera"), 1, GL_FALSE,
                     m_3d_matrix_data.camera.data());
  glUniform4fv(glGetUniformLocation(shid, "xy_array"), 8, m_frame_data.xy_array[0].data());
  glUniform4fv(glGetUniformLocation(shid, "xyz_array"), 4, m_frame_data.xyz_array[0].data());
  glUniform4fv(glGetUniformLocation(shid, "st_array"), 4, m_frame_data.st_array[0].data());
  glUniform4fv(glGetUniformLocation(shid, "basis_x"), 1, m_frame_data.basis_x.data());
  glUniform4fv(glGetUniformLocation(shid, "basis_y"), 1, m_frame_data.basis_y.data());

  u16 last_prog = -1;

  while (looks_like_2d_chunk_start(dma)) {
    m_debug_stats.blocks_2d_grp0++;
    // 4 packets per chunk

    // first is the header
    u32 sprite_count = process_sprite_chunk_header(dma);
    m_debug_stats.count_2d_grp0 += sprite_count;

    // second is the vector data
    u32 expected_vec_size = sizeof(SpriteVecData2d) * sprite_count;
    auto vec_data = dma.read_and_advance();
    ASSERT(expected_vec_size <= sizeof(m_vec_data_2d));
    unpack_to_no_stcycl(&m_vec_data_2d, vec_data, VifCode::Kind::UNPACK_V4_32, expected_vec_size,
                        SpriteDataMem::Vector, false, true);

    // third is the adgif data
    u32 expected_adgif_size = sizeof(AdGifData) * sprite_count;
    auto adgif_data = dma.read_and_advance();
    ASSERT(expected_adgif_size <= sizeof(m_adgif));
    unpack_to_no_stcycl(&m_adgif, adgif_data, VifCode::Kind::UNPACK_V4_32, expected_adgif_size,
                        SpriteDataMem::Adgif, false, true);

    // fourth is the actual run!!!!!
    auto run = dma.read_and_advance();
    ASSERT(run.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(run.vifcode1().kind == VifCode::Kind::MSCAL);

    if (m_enabled) {
      if (run.vifcode1().immediate != last_prog) {
        // one-time setups and flushing
        flush_sprites(render_state, prof, false);
      }

      if (run.vifcode1().immediate == SpriteProgMem::Sprites2dGrp0) {
        if (m_2d_enable) {
          do_block_common(SpriteMode::Mode2D, sprite_count, render_state, prof);
        }
      } else {
        if (m_3d_enable) {
          do_block_common(SpriteMode::Mode3D, sprite_count, render_state, prof);
        }
      }
      last_prog = run.vifcode1().immediate;
    }
  }
}

void Sprite3::render_fake_shadow(DmaFollower& dma) {
  // TODO
  // nop + flushe
  auto nop_flushe = dma.read_and_advance();
  ASSERT(nop_flushe.vifcode0().kind == VifCode::Kind::NOP);
  ASSERT(nop_flushe.vifcode1().kind == VifCode::Kind::FLUSHE);
}

/*!
 * Handle DMA data for group1 2d's (HUD)
 */
void Sprite3::render_2d_group1(DmaFollower& dma,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  // one time matrix data upload
  auto mat_upload = dma.read_and_advance();
  bool mat_ok = verify_unpack_with_stcycl(mat_upload, VifCode::Kind::UNPACK_V4_32, 4, 4, 80,
                                          SpriteDataMem::Matrix, false, false);
  ASSERT(mat_ok);
  ASSERT(mat_upload.size_bytes == sizeof(m_hud_matrix_data));
  memcpy(&m_hud_matrix_data, mat_upload.data, sizeof(m_hud_matrix_data));

  // opengl sprite frame setup
  glUniform4fv(
      glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "hud_hvdf_offset"), 1,
      m_hud_matrix_data.hvdf_offset.data());
  glUniform4fv(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "hud_hvdf_user"),
               75, m_hud_matrix_data.user_hvdf[0].data());
  glUniformMatrix4fv(
      glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "hud_matrix"), 1,
      GL_FALSE, m_hud_matrix_data.matrix.data());

  // loop through chunks.
  while (looks_like_2d_chunk_start(dma)) {
    m_debug_stats.blocks_2d_grp1++;
    // 4 packets per chunk

    // first is the header
    u32 sprite_count = process_sprite_chunk_header(dma);
    m_debug_stats.count_2d_grp1 += sprite_count;

    // second is the vector data
    u32 expected_vec_size = sizeof(SpriteVecData2d) * sprite_count;
    auto vec_data = dma.read_and_advance();
    ASSERT(expected_vec_size <= sizeof(m_vec_data_2d));
    unpack_to_no_stcycl(&m_vec_data_2d, vec_data, VifCode::Kind::UNPACK_V4_32, expected_vec_size,
                        SpriteDataMem::Vector, false, true);

    // third is the adgif data
    u32 expected_adgif_size = sizeof(AdGifData) * sprite_count;
    auto adgif_data = dma.read_and_advance();
    ASSERT(expected_adgif_size <= sizeof(m_adgif));
    unpack_to_no_stcycl(&m_adgif, adgif_data, VifCode::Kind::UNPACK_V4_32, expected_adgif_size,
                        SpriteDataMem::Adgif, false, true);

    // fourth is the actual run!!!!!
    auto run = dma.read_and_advance();
    ASSERT(run.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(run.vifcode1().kind == VifCode::Kind::MSCAL);
    ASSERT(run.vifcode1().immediate == SpriteProgMem::Sprites2dHud);
    if (m_enabled && m_2d_enable) {
      do_block_common(SpriteMode::ModeHUD, sprite_count, render_state, prof);
    }
  }
}

void Sprite3::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  switch (render_state->version) {
    case GameVersion::Jak1:
      render_jak1(dma, render_state, prof);
      break;
    case GameVersion::Jak2:
      render_jak2(dma, render_state, prof);
      break;
    default:
      ASSERT_NOT_REACHED();
  }
}

void Sprite3::render_jak2(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  m_debug_stats = {};
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0 || data0.vifcode1().kind == VifCode::Kind::NOP);
  ASSERT(data0.vif0() == 0 || data0.vifcode0().kind == VifCode::Kind::MARK);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag_offset() == render_state->next_bucket) {
    fmt::print("early exit!");
    return;
  }

  // First is the distorter (temporarily disabled for jak 2)
  {
    // auto child = prof.make_scoped_child("distorter");
    // render_distorter(dma, render_state, child);
  }

  // next, the normal sprite stuff
  render_state->shaders[ShaderId::SPRITE3].activate();
  handle_sprite_frame_setup(dma, render_state->version);

  // 3d sprites
  render_3d(dma);

  // 2d draw
  // m_sprite_renderer.reset_state();
  {
    auto child = prof.make_scoped_child("2d-group0");
    render_2d_group0(dma, render_state, child);
    flush_sprites(render_state, prof, false);
  }

  // shadow draw
  render_fake_shadow(dma);

  // 2d draw (HUD)
  {
    auto child = prof.make_scoped_child("2d-group1");
    render_2d_group1(dma, render_state, child);
    flush_sprites(render_state, prof, true);
  }

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBlendEquation(GL_FUNC_ADD);

  // TODO finish this up.
  // fmt::print("next bucket is 0x{}\n", render_state->next_bucket);
  while (dma.current_tag_offset() != render_state->next_bucket) {
    //    auto tag = dma.current_tag();
    // fmt::print("@ 0x{:x} tag: {}", dma.current_tag_offset(), tag.print());
    auto data = dma.read_and_advance();
    VifCode code(data.vif0());
    // fmt::print(" vif0: {}\n", code.print());
    if (code.kind == VifCode::Kind::NOP) {
      // fmt::print(" vif1: {}\n", VifCode(data.vif1()).print());
    }
  }
}

void Sprite3::render_jak1(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  m_debug_stats = {};
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sprite renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  // First is the distorter
  {
    auto child = prof.make_scoped_child("distorter");
    render_distorter(dma, render_state, child);
  }

  render_state->shaders[ShaderId::SPRITE3].activate();

  // next, sprite frame setup.
  handle_sprite_frame_setup(dma, render_state->version);

  // 3d sprites
  render_3d(dma);

  // 2d draw
  // m_sprite_renderer.reset_state();
  {
    auto child = prof.make_scoped_child("2d-group0");
    render_2d_group0(dma, render_state, child);
    flush_sprites(render_state, prof, false);
  }

  // shadow draw
  render_fake_shadow(dma);

  // 2d draw (HUD)
  {
    auto child = prof.make_scoped_child("2d-group1");
    render_2d_group1(dma, render_state, child);
    flush_sprites(render_state, prof, true);
  }

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBlendEquation(GL_FUNC_ADD);

  // TODO finish this up.
  // fmt::print("next bucket is 0x{}\n", render_state->next_bucket);
  while (dma.current_tag_offset() != render_state->next_bucket) {
    //    auto tag = dma.current_tag();
    // fmt::print("@ 0x{:x} tag: {}", dma.current_tag_offset(), tag.print());
    auto data = dma.read_and_advance();
    VifCode code(data.vif0());
    // fmt::print(" vif0: {}\n", code.print());
    if (code.kind == VifCode::Kind::NOP) {
      // fmt::print(" vif1: {}\n", VifCode(data.vif1()).print());
    }
  }
}

void Sprite3::draw_debug_window() {
  ImGui::Separator();
  ImGui::Text("Distort sprites: %d", m_distort_stats.total_sprites);
  ImGui::Text("2D Group 0 (World) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp0,
              m_debug_stats.count_2d_grp0);
  ImGui::Text("2D Group 1 (HUD) blocks: %d sprites: %d", m_debug_stats.blocks_2d_grp1,
              m_debug_stats.count_2d_grp1);
  ImGui::Checkbox("Culling", &m_enable_culling);
  ImGui::Checkbox("2d", &m_2d_enable);
  ImGui::SameLine();
  ImGui::Checkbox("3d", &m_3d_enable);
  ImGui::Checkbox("Distort", &m_distort_enable);
  ImGui::Checkbox("Distort instancing", &m_enable_distort_instancing);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Render (for real)

void Sprite3::flush_sprites(SharedRenderState* render_state,
                            ScopedProfilerNode& prof,
                            bool double_draw) {
  glBindVertexArray(m_ogl.vao);

  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  // upload vertex buffer
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_sprite_idx * sizeof(SpriteVertex3D) * 4, m_vertices_3d.data(),
               GL_STREAM_DRAW);

  // two passes through the buckets. first to build the index buffer
  u32 idx_offset = 0;
  for (const auto bucket : m_bucket_list) {
    memcpy(&m_index_buffer_data[idx_offset], bucket->ids.data(), bucket->ids.size() * sizeof(u32));
    bucket->offset_in_idx_buffer = idx_offset;
    idx_offset += bucket->ids.size();
  }

  // now upload it
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_offset * sizeof(u32), m_index_buffer_data.data(),
               GL_STREAM_DRAW);

  // now do draws!
  for (const auto bucket : m_bucket_list) {
    u32 tbp = bucket->key >> 32;
    DrawMode mode;
    mode.as_int() = bucket->key & 0xffffffff;

    std::optional<u64> tex;
    tex = render_state->texture_pool->lookup(tbp);

    if (!tex) {
      lg::warn("Failed to find texture at {}, using random", tbp);
      tex = render_state->texture_pool->get_placeholder_texture();
    }
    ASSERT(tex);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, *tex);

    auto settings = setup_opengl_from_draw_mode(mode, GL_TEXTURE0, false);

    glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_min"),
                double_draw ? settings.aref_first : 0.016);
    glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_max"),
                10.f);
    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "tex_T0"), 0);

    prof.add_draw_call();
    prof.add_tri(2 * (bucket->ids.size() / 5));

    glDrawElements(GL_TRIANGLE_STRIP, bucket->ids.size(), GL_UNSIGNED_INT,
                   (void*)(bucket->offset_in_idx_buffer * sizeof(u32)));

    if (double_draw) {
      switch (settings.kind) {
        case DoubleDrawKind::NONE:
          break;
        case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
          prof.add_draw_call();
          prof.add_tri(2 * (bucket->ids.size() / 5));
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_min"),
              -10.f);
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::SPRITE3].id(), "alpha_max"),
              settings.aref_second);
          glDepthMask(GL_FALSE);
          glDrawElements(GL_TRIANGLE_STRIP, bucket->ids.size(), GL_UNSIGNED_INT,
                         (void*)(bucket->offset_in_idx_buffer * sizeof(u32)));
          break;
        default:
          ASSERT(false);
      }
    }
  }

  m_sprite_buckets.clear();
  m_bucket_list.clear();
  m_last_bucket_key = UINT64_MAX;
  m_last_bucket = nullptr;
  m_sprite_idx = 0;
  glBindVertexArray(0);
}

void Sprite3::handle_tex0(u64 val,
                          SharedRenderState* /*render_state*/,
                          ScopedProfilerNode& /*prof*/) {
  GsTex0 reg(val);

  // update tbp
  m_current_tbp = reg.tbp0();
  m_current_mode.set_tcc(reg.tcc());

  // tbw: assume they got it right
  // psm: assume they got it right
  // tw: assume they got it right
  // th: assume they got it right

  ASSERT(reg.tfx() == GsTex0::TextureFunction::MODULATE);
  ASSERT(reg.psm() != GsTex0::PSM::PSMT4HH);

  // cbp: assume they got it right
  // cpsm: assume they got it right
  // csm: assume they got it right
}

void Sprite3::handle_tex1(u64 val,
                          SharedRenderState* /*render_state*/,
                          ScopedProfilerNode& /*prof*/) {
  GsTex1 reg(val);
  m_current_mode.set_filt_enable(reg.mmag());
}

void Sprite3::handle_zbuf(u64 val,
                          SharedRenderState* /*render_state*/,
                          ScopedProfilerNode& /*prof*/) {
  // note: we can basically ignore this. There's a single z buffer that's always configured the same
  // way - 24-bit, at offset 448.
  GsZbuf x(val);
  ASSERT(x.psm() == TextureFormat::PSMZ24);
  ASSERT(x.zbp() == 448 || x.zbp() == 304);  // 304 for jak 2.

  m_current_mode.set_depth_write_enable(!x.zmsk());
}

void Sprite3::handle_clamp(u64 val,
                           SharedRenderState* /*render_state*/,
                           ScopedProfilerNode& /*prof*/) {
  if (!(val == 0b101 || val == 0 || val == 1 || val == 0b100)) {
    ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", val));
  }

  m_current_mode.set_clamp_s_enable(val & 0b001);
  m_current_mode.set_clamp_t_enable(val & 0b100);
}

void Sprite3::update_mode_from_alpha1(u64 val, DrawMode& mode) {
  GsAlpha reg(val);
  if (reg.a_mode() == GsAlpha::BlendMode::SOURCE && reg.b_mode() == GsAlpha::BlendMode::DEST &&
      reg.c_mode() == GsAlpha::BlendMode::SOURCE && reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - Cd) * As + Cd
    // Cs * As  + (1 - As) * Cd
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);

  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - 0) * As + Cd
    // Cs * As + (1) * CD
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_SRC_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    ASSERT(reg.fix() == 128);
    // Cv = (Cs - 0) * FIX + Cd
    // if fix = 128, it works out to 1.0
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_FIX_DST);
    // src plus dest
  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::DEST &&
             reg.c_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // Cv = (Cs - Cd) * FIX + Cd
    ASSERT(reg.fix() == 64);
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_FIX_DST);
  } else if (reg.a_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.b_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (0 - Cs) * As + Cd
    // Cd - Cs * As
    // s, d
    mode.set_alpha_blend(DrawMode::AlphaBlend::ZERO_SRC_SRC_DST);
  }

  else {
    lg::error("unsupported blend: a {} b {} c {} d {}", (int)reg.a_mode(), (int)reg.b_mode(),
              (int)reg.c_mode(), (int)reg.d_mode());
    mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
    ASSERT(false);
  }
}

void Sprite3::handle_alpha(u64 val,
                           SharedRenderState* /*render_state*/,
                           ScopedProfilerNode& /*prof*/) {
  update_mode_from_alpha1(val, m_current_mode);
}

void Sprite3::do_block_common(SpriteMode mode,
                              u32 count,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof) {
  m_current_mode = m_default_mode;
  for (u32 sprite_idx = 0; sprite_idx < count; sprite_idx++) {
    if (m_sprite_idx == SPRITE_RENDERER_MAX_SPRITES) {
      flush_sprites(render_state, prof, mode == ModeHUD);
    }

    if (mode == Mode2D && render_state->has_pc_data && m_enable_culling) {
      // we can skip sprites that are out of view
      // it's probably possible to do this for 3D as well.
      auto bsphere = m_vec_data_2d[sprite_idx].xyz_sx;
      bsphere.w() = std::max(bsphere.w(), m_vec_data_2d[sprite_idx].sy());
      if (bsphere.w() == 0 || !sphere_in_view_ref(bsphere, render_state->camera_planes)) {
        continue;
      }
    }

    auto& adgif = m_adgif[sprite_idx];
    handle_tex0(adgif.tex0_data, render_state, prof);
    handle_tex1(adgif.tex1_data, render_state, prof);
    if (GsRegisterAddress(adgif.clamp_addr) == GsRegisterAddress::ZBUF_1) {
      handle_zbuf(adgif.clamp_data, render_state, prof);
    } else {
      handle_clamp(adgif.clamp_data, render_state, prof);
    }
    handle_alpha(adgif.alpha_data, render_state, prof);

    u64 key = (((u64)m_current_tbp) << 32) | m_current_mode.as_int();
    Bucket* bucket;
    if (key == m_last_bucket_key) {
      bucket = m_last_bucket;
    } else {
      auto it = m_sprite_buckets.find(key);
      if (it == m_sprite_buckets.end()) {
        bucket = &m_sprite_buckets[key];
        bucket->key = key;
        m_bucket_list.push_back(bucket);
      } else {
        bucket = &it->second;
      }
    }
    u32 start_vtx_id = m_sprite_idx * 4;
    bucket->ids.push_back(start_vtx_id);
    bucket->ids.push_back(start_vtx_id + 1);
    bucket->ids.push_back(start_vtx_id + 2);
    bucket->ids.push_back(start_vtx_id + 3);
    bucket->ids.push_back(UINT32_MAX);

    auto& vert1 = m_vertices_3d.at(start_vtx_id + 0);

    vert1.xyz_sx = m_vec_data_2d[sprite_idx].xyz_sx;
    vert1.quat_sy = m_vec_data_2d[sprite_idx].flag_rot_sy;
    vert1.rgba = m_vec_data_2d[sprite_idx].rgba / 255;
    vert1.flags_matrix[0] = m_vec_data_2d[sprite_idx].flag();
    vert1.flags_matrix[1] = m_vec_data_2d[sprite_idx].matrix();
    vert1.info[0] = 0;  // hack
    vert1.info[1] = m_current_mode.get_tcc_enable();
    vert1.info[2] = 0;
    vert1.info[3] = mode;

    m_vertices_3d.at(start_vtx_id + 1) = vert1;
    m_vertices_3d.at(start_vtx_id + 2) = vert1;
    m_vertices_3d.at(start_vtx_id + 3) = vert1;

    m_vertices_3d.at(start_vtx_id + 1).info[2] = 1;
    m_vertices_3d.at(start_vtx_id + 2).info[2] = 3;
    m_vertices_3d.at(start_vtx_id + 3).info[2] = 2;

    ++m_sprite_idx;
  }
}
