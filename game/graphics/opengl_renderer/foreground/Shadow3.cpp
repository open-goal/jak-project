#include "Shadow3.h"

#include "game/runtime.h"

Shadow3::Shadow3(ShaderLibrary& shaders) {
  glGenVertexArrays(1, &m_opengl.vao);
  glBindVertexArray(m_opengl.vao);

  glGenBuffers(1, &m_opengl.bones_buffer);
  glBindBuffer(GL_UNIFORM_BUFFER, m_opengl.bones_buffer);

  GLint val;
  glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, &val);
  if (val <= 16) {
    m_opengl.buffer_alignment = 1;
  } else {
    m_opengl.buffer_alignment = val / 16;
    if (m_opengl.buffer_alignment * 16 != (u32)val) {
      ASSERT_MSG(false,
                 fmt::format("opengl uniform buffer alignment is {}, which is strange\n", val));
    }
  }
  {
    auto& shader = shaders.at(ShaderId::SHADOW3);
    shader.activate();
    auto id = shader.id();
    m_uniforms.camera_rot = glGetUniformLocation(id, "camera_rot");
    m_uniforms.fog_constants = glGetUniformLocation(id, "fog_constants");
    m_uniforms.hvdf_offset = glGetUniformLocation(id, "hvdf_offset");
    m_uniforms.perspective_matrix = glGetUniformLocation(id, "perspective_matrix");
    m_uniforms.debug_color = glGetUniformLocation(id, "debug_color");
    m_uniforms.origin = glGetUniformLocation(id, "origin");
    m_uniforms.top_plane = glGetUniformLocation(id, "top_plane");
    m_uniforms.bottom_plane = glGetUniformLocation(id, "bottom_plane");
    m_uniforms.bottom_cap = glGetUniformLocation(id, "bottom_cap");
  }

  std::vector<u8> temp(MAX_SHADER_BONE_VECTORS * sizeof(math::Vector4f));
  glBufferData(GL_UNIFORM_BUFFER, MAX_SHADER_BONE_VECTORS * sizeof(math::Vector4f), temp.data(),
               GL_DYNAMIC_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

Shadow3::~Shadow3() {
  glDeleteBuffers(1, &m_opengl.bones_buffer);
  glDeleteVertexArrays(1, &m_opengl.vao);
}

void Shadow3::setup_for_level(SharedRenderState* render_state, const LevelData* level_data) {
  glBindVertexArray(m_opengl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, level_data->shadow_vertices);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, level_data->shadow_indices);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glEnableVertexAttribArray(3);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GEQUAL);

  glVertexAttribPointer(0,                                          // location 0 in the shader
                        3,                                          // 3 values per vert
                        GL_FLOAT,                                   // floats
                        GL_FALSE,                                   // normalized
                        sizeof(tfrag3::ShadowVertex),               // stride
                        (void*)offsetof(tfrag3::ShadowVertex, pos)  // offset (0)
  );

  glVertexAttribPointer(1,                                             // location 1 in the
                        1,                                             // 3 values per vert
                        GL_FLOAT,                                      // floats
                        GL_FALSE,                                      // normalized
                        sizeof(tfrag3::ShadowVertex),                  // stride
                        (void*)offsetof(tfrag3::ShadowVertex, weight)  // offset (0)
  );

  glVertexAttribIPointer(2,                                              // location 2 in the
                         2,                                              //
                         GL_UNSIGNED_BYTE,                               // u8's
                         sizeof(tfrag3::ShadowVertex),                   //
                         (void*)offsetof(tfrag3::ShadowVertex, mats[0])  // offset in array
  );

  glVertexAttribIPointer(3,                                            // location 2 in the
                         1,                                            //
                         GL_UNSIGNED_BYTE,                             // u8's
                         sizeof(tfrag3::ShadowVertex),                 //
                         (void*)offsetof(tfrag3::ShadowVertex, flags)  // offset in array
  );
}

namespace {
void set_uniform(GLuint uniform, const math::Vector3f& val) {
  glUniform3f(uniform, val.x(), val.y(), val.z());
}
void set_uniform(GLuint uniform, const math::Vector4f& val) {
  glUniform4f(uniform, val.x(), val.y(), val.z(), val.w());
}
}  // namespace

void Shadow3::draw_model(SharedRenderState* render_state,
                         ShadowRequest* request,
                         ScopedProfilerNode& prof) {
  glBindBufferRange(GL_UNIFORM_BUFFER, 1, m_opengl.bones_buffer,
                    sizeof(math::Vector4f) * request->bone_idx, 128 * 16 * 4);
  const auto* geo = request->model.model;

  set_uniform(m_uniforms.origin, request->origin);
  set_uniform(m_uniforms.top_plane, request->top_plane);
  set_uniform(m_uniforms.bottom_plane, request->bottom_plane);

  // enable stencil!
  glEnable(GL_STENCIL_TEST);
  glStencilMask(0xFF);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDepthFunc(GL_GEQUAL);
  // glDepthMask(GL_FALSE);  // no depth writes.

  auto do_draw = [&](const tfrag3::ShadowModel::Run& run, const math::Vector3f& color) {
    set_uniform(m_uniforms.debug_color, color);
    glDrawElements(GL_TRIANGLES, run.count, GL_UNSIGNED_INT,
                   (void*)(sizeof(u32) * run.first_index));
    glDisable(GL_BLEND);
    set_uniform(m_uniforms.debug_color,color);
    // glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    // glDrawElements(GL_TRIANGLES, run.count, GL_UNSIGNED_INT,
    //                (void*)(sizeof(u32) * run.first_index));
    // glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  };

  auto do_all_draws = [&]( const math::Vector3f& color) {
    glUniform1i(m_uniforms.bottom_cap, 0);
    do_draw(geo->single_tris, color);
    // do_draw(geo->double_tris, math::Vector3f(0.8, 0.5, 0.5));
    do_draw(geo->single_edges, color);
    // do_draw(geo->double_edges, math::Vector3f(0.5, 0.8, 0.5));
    // glUniform1i(m_uniforms.bottom_cap, 1);
    // do_draw(geo->single_tris, math::Vector3f(0.5, 0.5, 0.8));
    // do_draw(geo->double_tris, math::Vector3f(0.5, 0.5, 0.8));
  };

  // using glCullFace(GL_FRONT) seems to give us back faces.

  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);
  glStencilFunc(GL_ALWAYS, 0, 0);          // always pass stencil
  glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);  // increment on depth pass.
  do_all_draws({0.1f, 0.1f, 0.8f});
  glCullFace(GL_FRONT);
  glStencilFunc(GL_ALWAYS, 0, 0);
  glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);  // decrement on depth pass.
  do_all_draws({0.8f, 0.1f, 0.1f});
  glDisable(GL_CULL_FACE);
}

void Shadow3::finish(SharedRenderState* render_state, ScopedProfilerNode& prof) {

}

void Shadow3::flush_requests(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_next_request == 0) {
    return;
  }

  if (!m_did_first_time_setup) {
    first_time_setup(render_state);
    m_did_first_time_setup = true;
  }

  glBindBuffer(GL_UNIFORM_BUFFER, m_opengl.bones_buffer);
  glBufferSubData(GL_UNIFORM_BUFFER, 0, m_next_free_bone_vector * sizeof(math::Vector4f),
                  m_shader_bone_vector_buffer);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);

  for (auto& c : m_level_chains) {
    if (!c.head)
      continue;
    setup_for_level(render_state, c.level);
    ShadowRequest* iter = c.head;
    while (iter) {
      draw_model(render_state, iter, prof);
      iter = iter->next;
    }
  }

  for (auto& c : m_level_chains) {
    c.level = nullptr;
    c.head = nullptr;
  }
  m_next_request = 0;
  m_next_free_bone_vector = 0;
}

void Shadow3::first_time_setup(SharedRenderState* render_state) {
  glClearStencil(0);
  glClear(GL_STENCIL_BUFFER_BIT);

  render_state->shaders[ShaderId::SHADOW3].activate();
  glUniformMatrix4fv(m_uniforms.camera_rot, 1, GL_FALSE, &render_state->camera_rot[0].x());
  glUniformMatrix4fv(m_uniforms.perspective_matrix, 1, GL_FALSE, &render_state->perspective[0].x());
  set_uniform(m_uniforms.fog_constants, render_state->camera_fog);
  set_uniform(m_uniforms.hvdf_offset, render_state->camera_hvdf_off);
}

void Shadow3::draw_debug_window() {}

void Shadow3::render_jak1(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  printf("Jak1 shadow render\n");
  m_did_first_time_setup = false;

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto dmatag = dma.current_tag();
    auto data = dma.read_and_advance();
    int run_idx = 0;
    if (data.vifcode0().kind == VifCode::Kind::PC_PORT) {
      printf(" Run %d start\n", run_idx);
      u32 next = data.data_offset;
      while (next) {
        Jak1ShadowRequest game_request;
        memcpy(&game_request, g_ee_main_mem + next, sizeof(Jak1ShadowRequest));
        next = game_request.next;

        char name[128];
        strncpy(name, (const char*)(g_ee_main_mem) + 4 + game_request.geo_name, 128);
        name[127] = 0;
        printf("  draw %s\n", name);

        auto model = render_state->loader->get_shadow_model(name);
        if (!model) {
          printf("   SKIP: no model data\n");
          continue;
        }
        /*
        *  (shdf00) ;; unused
        (disable-fade)
        (shdf02) ;; only set, never used.
        (shdf03)
        (shdf04) ;; unused
        (disable-draw)
        */
        constexpr u32 kCullWhenUnderPlane = 1;
        constexpr u32 kDisableFade = 2;
        constexpr u32 kAbsolutePlanes = 4;
        constexpr u32 kFlag3 = 8;
        constexpr u32 kFlag4 = 16;
        constexpr u32 kDisableDraw = 32;

        if (game_request.settings.flags & kDisableDraw) {
          printf("   SKIP: disable flag set\n");
          continue;
        }

        if (game_request.num_joints * 4 + m_next_free_bone_vector >= MAX_SHADER_BONE_VECTORS) {
          flush_requests(render_state, prof);
        }

        if (m_next_request == m_requests.size()) {
          flush_requests(render_state, prof);
        }

        LevelChain* chain = nullptr;
        for (auto& c : m_level_chains) {
          if (c.level == model->level || !c.level) {
            chain = &c;
            chain->level = model->level;
            break;
          }
        }

        if (!chain) {
          ASSERT_NOT_REACHED();
        }

        // grab the next request and link it to the chain for the level.
        auto& request = m_requests[m_next_request++];
        request.next = chain->head;
        chain->head = &request;

        request.model = *model;
        // the origin of "light" for the shadow is found by starting at the "center" point
        // (somewhere in the model) and following the shadow direction backward.
        request.origin = game_request.settings.center +
                         game_request.settings.shadow_dir * game_request.settings.dist_to_locus;

        // copy bones to buffer
        constexpr int in_stride = 8 * 4 * sizeof(float);
        constexpr int out_stride = 4 * 4 * sizeof(float);
        constexpr int in_offset = 3 * in_stride;
        request.bone_idx = m_next_free_bone_vector;
        for (int i = 0; i < game_request.num_joints; i++) {
          memcpy(&m_shader_bone_vector_buffer[m_next_free_bone_vector].x(),
                 g_ee_main_mem + game_request.mtx + in_offset + i * in_stride, out_stride);
          m_next_free_bone_vector += 4;
        }

        // the clipping planes for the shadow
        request.top_plane = game_request.settings.top_plane;
        request.bottom_plane = game_request.settings.bot_plane;
        if (!(kAbsolutePlanes & game_request.settings.flags)) {
          printf("relative plane mode, base is %f, move by %f\n",
                 -request.bottom_plane.w() / 4096.0, game_request.settings.center.y() / 4096.0);
          // in relative planes mode, the height of the plane is adjusted to be relative to the
          // height of the center, so the planes move and down with the model
          request.top_plane.w() -= game_request.settings.center.y();
          request.bottom_plane.w() -= game_request.settings.center.y();
        }

        // skip drawing if the camera is below the lower clipping plane
        if (kCullWhenUnderPlane & game_request.settings.flags) {
          if (render_state->camera_pos.xyz().dot(request.bottom_plane.xyz()) +
                  request.bottom_plane.w() <
              0) {
            printf("   SKIP: camera below lower clipping plane.\n");
            m_next_request--;
            continue;
          }
        }

        // detect if the origin is below the clipping plane and if so, move it up.
        const float dot = request.bottom_plane.xyz().dot(request.origin);
        if (dot + request.bottom_plane.w() > 0) {
          printf("   the origin is below the clipping plane, moving it up.\n");
          printf("    center was %s\n", game_request.settings.center.to_string_aligned().c_str());
          printf("    dir was %s\n", game_request.settings.shadow_dir.to_string_aligned().c_str());
          printf("    locus %f\n", game_request.settings.dist_to_locus);
          printf("    bottom plane was %s\n",
                 game_request.settings.bot_plane.to_string_aligned().c_str());
          printf("    adjusted bottom plane was %s\n",
                 request.bottom_plane.to_string_aligned().c_str());
          printf("    abs flag %d\n", game_request.settings.flags & kAbsolutePlanes);

          request.bottom_plane.w() = -dot;
        }

        const auto& cam_rot = render_state->camera_rot;
        const auto& cam_pos = render_state->camera_pos;

        request.light_dir = game_request.settings.shadow_dir;

        // transform to camera frame
        auto rotate = [&](const math::Vector3f& in) {
          return (cam_rot[0] * in[0] + cam_rot[1] * in[1] + cam_rot[2] * in[2]).xyz();
        };

        auto transform = [&](const math::Vector3f& in) {
          return (cam_rot[0] * in[0] + cam_rot[1] * in[1] + cam_rot[2] * in[2] + cam_rot[3]).xyz();
        };

        auto rotate_plane = [&](const math::Vector4f& in) {
          auto xyz = rotate(in.xyz());
          return math::Vector4f(xyz.x(), xyz.y(), xyz.z(), in.w() - xyz.dot(cam_rot[3].xyz()));
        };

        printf("plane offset before: %f\n",
               game_request.settings.center.dot(request.bottom_plane.xyz()) +
                   request.bottom_plane.w());

        request.light_dir = rotate(request.light_dir);
        request.top_plane = rotate_plane(request.top_plane);
        request.bottom_plane = rotate_plane(request.bottom_plane);
        request.origin = transform(request.origin);

        printf("plane offset after: %f\n",
               transform(game_request.settings.center).dot(request.bottom_plane.xyz()) +
                   request.bottom_plane.w());
        printf("rot3: %s\n", cam_rot[3].to_string_aligned().c_str());
        printf("   2: %s\n", cam_pos.to_string_aligned().c_str());

        // printf("  origin: %s\n", (request.origin / 4096.f).to_string_aligned().c_str());
      }
    }
  }

  flush_requests(render_state, prof);
  finish(render_state, prof);
}
