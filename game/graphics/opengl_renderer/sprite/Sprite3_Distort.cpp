#include "Sprite3.h"
#include "game/graphics/opengl_renderer/dma_helpers.h"

namespace {
/*!
 * Does the next DMA transfer look like the frame data for sprite distort?
 */
bool looks_like_distort_frame_data(const DmaFollower& dma) {
  return dma.current_tag().kind == DmaTag::Kind::CNT &&
         dma.current_tag_vifcode0().kind == VifCode::Kind::NOP &&
         dma.current_tag_vifcode1().kind == VifCode::Kind::UNPACK_V4_32;
}

constexpr int SPRITE_RENDERER_MAX_DISTORT_SPRITES =
    256 * 12;  // size of sprite-aux-list in GOAL code * SPRITE_MAX_AMOUNT_MULT
}  // namespace

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
  // Read DMA
  {
    auto prof_node = prof.make_scoped_child("dma");
    distort_dma(render_state->version, dma, prof_node);
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
void Sprite3::distort_dma(GameVersion version, DmaFollower& dma, ScopedProfilerNode& /*prof*/) {
  // set the expected values per game version first
  u32 expect_zbp, expect_th;
  switch (version) {
    case GameVersion::Jak1:
      expect_zbp = 0x1c0;
      expect_th = 8;
      break;
    case GameVersion::Jak2:
      expect_zbp = 0x130;
      expect_th = 9;
      break;
    default:
      ASSERT(false);
      return;
  }

  // First should be the GS setup
  auto sprite_distorter_direct_setup = dma.read_and_advance();
  ASSERT(sprite_distorter_direct_setup.vifcode0().kind == VifCode::Kind::NOP);
  ASSERT(sprite_distorter_direct_setup.vifcode1().kind == VifCode::Kind::DIRECT);
  ASSERT(sprite_distorter_direct_setup.vifcode1().immediate == 7);
  memcpy(&m_sprite_distorter_setup, sprite_distorter_direct_setup.data, 7 * 16);

  auto gif_tag = m_sprite_distorter_setup.gif_tag;
  ASSERT(gif_tag.nloop() == 1);
  ASSERT(gif_tag.eop() == true);
  ASSERT(gif_tag.nreg() == 6);
  ASSERT(gif_tag.reg(0) == GifTag::RegisterDescriptor::AD);

  auto zbuf1 = m_sprite_distorter_setup.zbuf;
  ASSERT(zbuf1.zbp() == expect_zbp);
  ASSERT(zbuf1.zmsk() == true);
  ASSERT(zbuf1.psm() == TextureFormat::PSMZ24);

  auto tex0 = m_sprite_distorter_setup.tex0;
  ASSERT(tex0.tbw() == 8);
  ASSERT(tex0.tw() == 9);
  ASSERT(tex0.th() == expect_th);

  auto tex1 = m_sprite_distorter_setup.tex1;
  ASSERT(tex1.mmag() == 1);
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
