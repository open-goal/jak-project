#include "Shadow3.h"

#include "game/runtime.h"

Shadow3::Shadow3(ShaderLibrary& shaders) {
  glGenVertexArrays(1, &m_opengl.vao);
  glBindVertexArray(m_opengl.vao);

  glGenBuffers(1, &m_opengl.indices);
  glGenBuffers(1, &m_opengl.debug_verts);

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
    m_uniforms.scissor_top = glGetUniformLocation(id, "scissor_top");
  }

  std::vector<u8> temp(MAX_SHADER_BONE_VECTORS * sizeof(math::Vector4f));
  glBufferData(GL_UNIFORM_BUFFER, MAX_SHADER_BONE_VECTORS * sizeof(math::Vector4f), temp.data(),
               GL_DYNAMIC_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

Shadow3::~Shadow3() {
  glDeleteBuffers(1, &m_opengl.bones_buffer);
  glDeleteBuffers(1, &m_opengl.indices);
  glDeleteBuffers(1, &m_opengl.debug_verts);
  glDeleteVertexArrays(1, &m_opengl.vao);
}

void Shadow3::setup_for_level(SharedRenderState* render_state, const LevelData* level_data) {
  glBindVertexArray(m_opengl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_hacks ? m_opengl.debug_verts : level_data->shadow_vertices);
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
void set_uniform(GLint uniform, const math::Vector3f& val) {
  glUniform3f(uniform, val.x(), val.y(), val.z());
}
void set_uniform(GLint uniform, const math::Vector4f& val) {
  glUniform4f(uniform, val.x(), val.y(), val.z(), val.w());
}

}  // namespace

void Shadow3::draw_model(SharedRenderState* render_state,
                         ShadowRequest* request,
                         ScopedProfilerNode& prof) {
  for (const auto& frag : request->model.model->fragments) {
    ShadowCPUInput input{
        .origin = request->origin,
        .top_plane = request->top_plane,
        .bottom_plane = request->bottom_plane,
        .light_dir = request->light_dir,
        .bones = request->bones,
        .model = &frag,
        .vertices = &request->model.level->level->shadow_data.vertices,
        .scissor_top = request->scissor_top,
        .debug_highlight_tri = m_debug_tri,
    };
    calc_shadow_indices(input, &m_cpu_workspace, &m_cpu_output);
    glBindBuffer(GL_UNIFORM_BUFFER, m_opengl.bones_buffer);
    glBindBufferRange(GL_UNIFORM_BUFFER, 1, m_opengl.bones_buffer,
                      sizeof(math::Vector4f) * request->bone_idx, 128 * 16 * 4);
    // const auto* geo = request->model.model;
    // printf("draw %s\n", geo->name.c_str());

    set_uniform(m_uniforms.origin, request->origin);
    set_uniform(m_uniforms.top_plane, request->top_plane);
    set_uniform(m_uniforms.bottom_plane, request->bottom_plane);
    glUniform1i(m_uniforms.scissor_top, request->scissor_top);

    if (m_hacks) {
      int num_verts = frag.num_one_bone_vertices + frag.num_two_bone_vertices;
      std::vector<tfrag3::ShadowVertex> verts;
      for (size_t i = 0; i < num_verts; ++i) {
        auto& out = verts.emplace_back();
        out.flags = 255;
        out.mats[0] = 255;
        out.mats[1] = 255;
        out.pos[0] = m_cpu_workspace.vertices[i].x();
        out.pos[1] = m_cpu_workspace.vertices[i].y();
        out.pos[2] = m_cpu_workspace.vertices[i].z();
        out.weight = m_cpu_workspace.vertices[i].w();
      }

      for (size_t i = 0; i < num_verts; ++i) {
        auto& out = verts.emplace_back();
        out.flags = 255;
        out.mats[0] = 255;
        out.mats[1] = 255;
        out.pos[0] = m_cpu_workspace.dual_vertices[i].x();
        out.pos[1] = m_cpu_workspace.dual_vertices[i].y();
        out.pos[2] = m_cpu_workspace.dual_vertices[i].z();
        out.weight = m_cpu_workspace.dual_vertices[i].w();
      }

      glEnable(GL_DEPTH_TEST);
      glDisable(GL_BLEND);
      glDepthFunc(GL_GEQUAL);
      glDepthMask(GL_TRUE);
      glEnable(GL_CULL_FACE);
      glCullFace(m_cull_back ? GL_BACK : GL_FRONT);
      glBindBuffer(GL_ARRAY_BUFFER, m_opengl.debug_verts);
      glBufferData(GL_ARRAY_BUFFER, num_verts * 2 * sizeof(tfrag3::ShadowVertex), verts.data(),
                   GL_DYNAMIC_DRAW);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_opengl.indices);
      set_uniform(m_uniforms.debug_color, math::Vector3f(0.5f, 0.5f, 0.5f));
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_cpu_output.num_f0_indices * sizeof(u32),
                   m_cpu_output.f0_indices, GL_DYNAMIC_DRAW);
      glDrawElements(GL_TRIANGLES, m_cpu_output.num_f0_indices, GL_UNSIGNED_INT, nullptr);
      set_uniform(m_uniforms.debug_color, math::Vector3f(0.f, 0.f, 0.f));
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawElements(GL_TRIANGLES, m_cpu_output.num_f0_indices, GL_UNSIGNED_INT, nullptr);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

      glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_cpu_output.num_f1_indices * sizeof(u32),
                   m_cpu_output.f1_indices, GL_DYNAMIC_DRAW);
      set_uniform(m_uniforms.debug_color, math::Vector3f(0.f, 0.f, 0.f));
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawElements(GL_TRIANGLES, m_cpu_output.num_f1_indices, GL_UNSIGNED_INT, nullptr);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

      glEnable(GL_BLEND);
      glBlendEquation(GL_FUNC_ADD);
      glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ZERO, GL_ONE);
      set_uniform(m_uniforms.debug_color, math::Vector3f(0.5f, 0.78f, 0.5f));
      glDrawElements(GL_TRIANGLES, m_cpu_output.num_f1_indices, GL_UNSIGNED_INT, nullptr);

      glBindBuffer(GL_ARRAY_BUFFER, request->model.level->shadow_vertices);
      glDisable(GL_CULL_FACE);

    } else {
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_opengl.indices);
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_cpu_output.num_indices * sizeof(u32),
                   m_cpu_output.indices, GL_DYNAMIC_DRAW);
      // enable stencil!
      glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);  // no color writes.
      glEnable(GL_STENCIL_TEST);
      glStencilMask(0xFF);
      glEnable(GL_DEPTH_TEST);
      glDisable(GL_BLEND);
      glDepthFunc(GL_GEQUAL);
      glDepthMask(GL_FALSE);  // no depth writes.

      if (false) {
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        glStencilFunc(GL_ALWAYS, 0, 0);          // always pass stencil
        glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);  // increment on depth fail
        glDrawElements(GL_TRIANGLES, m_cpu_output.num_indices, GL_UNSIGNED_INT, nullptr);
        glCullFace(GL_FRONT);
        glStencilFunc(GL_ALWAYS, 0, 0);
        glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);  // decrement on depth pass.
        glDrawElements(GL_TRIANGLES, m_cpu_output.num_indices, GL_UNSIGNED_INT, nullptr);
      } else {
        glEnable(GL_CULL_FACE);
        glCullFace(GL_FRONT);
        glStencilFunc(GL_ALWAYS, 0, 0);          // always pass stencil
        glStencilOp(GL_KEEP, GL_INCR, GL_KEEP);  // increment on depth fail
        glDrawElements(GL_TRIANGLES, m_cpu_output.num_indices, GL_UNSIGNED_INT, nullptr);
        glCullFace(GL_BACK);
        glStencilFunc(GL_ALWAYS, 0, 0);
        glStencilOp(GL_KEEP, GL_DECR, GL_KEEP);  // decrement on depth pass.
        glDrawElements(GL_TRIANGLES, m_cpu_output.num_indices, GL_UNSIGNED_INT, nullptr);
      }

      glDisable(GL_CULL_FACE);
    }
  }
}

void Shadow3::finish(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // finally, draw shadow.
  if (!m_hacks) {
    if (render_state->version == GameVersion::Jak1) {
      glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_FALSE);
      glStencilFunc(GL_NOTEQUAL, 0, 0xFF);
      glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
      glDepthFunc(GL_ALWAYS);
      glEnable(GL_BLEND);
      glBlendEquation(GL_FUNC_ADD);
      glBlendFuncSeparate(GL_DST_COLOR, GL_ZERO, GL_ONE, GL_ZERO);
      m_full_screen_draw.draw(m_color, render_state, prof);

    } else {
      glStencilFunc(GL_NOTEQUAL, 0, 0xFF);
      glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
      glDepthFunc(GL_ALWAYS);

      glEnable(GL_BLEND);
      glBlendFuncSeparate(GL_ONE, GL_ONE, GL_ONE, GL_ZERO);

      bool have_darken = false;
      bool have_lighten = false;
      bool lighten_channel[3] = {false, false, false};
      bool darken_channel[3] = {false, false, false};
      for (int i = 0; i < 3; i++) {
        if (m_color[i] > 128) {
          have_lighten = true;
          lighten_channel[i] = true;
        } else if (m_color[i] < 128) {
          have_darken = true;
          darken_channel[i] = true;
        }
      }

      if (have_darken) {
        glColorMask(darken_channel[0], darken_channel[1], darken_channel[2], false);
        glBlendEquation(GL_FUNC_REVERSE_SUBTRACT);
        m_full_screen_draw.draw(
            math::Vector4f((m_color[3] - m_color[0]) / 256.f, (m_color[3] - m_color[1]) / 256.f,
                           (m_color[3] - m_color[2]) / 256.f, 0) *
                0.5f,
            render_state, prof);
      }

      if (have_lighten) {
        glColorMask(lighten_channel[0], lighten_channel[1], lighten_channel[2], false);
        glBlendEquation(GL_FUNC_ADD);
        m_full_screen_draw.draw(
            math::Vector4f((m_color[0] - m_color[3]) / 256.f, (m_color[1] - m_color[3]) / 256.f,
                           (m_color[2] - m_color[3]) / 256.f, 0) *
                0.5f,
            render_state, prof);
      }
    }
  }

  // restore
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glBlendEquation(GL_FUNC_ADD);
  glDepthMask(GL_TRUE);
  glDisable(GL_STENCIL_TEST);

  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);
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
  render_state->stencil_dirty = true;

  render_state->shaders[ShaderId::SHADOW3].activate();
  glUniformMatrix4fv(m_uniforms.camera_rot, 1, GL_FALSE, &render_state->camera_rot[0].x());
  glUniformMatrix4fv(m_uniforms.perspective_matrix, 1, GL_FALSE, &render_state->perspective[0].x());
  set_uniform(m_uniforms.fog_constants, render_state->camera_fog);
  set_uniform(m_uniforms.hvdf_offset, render_state->camera_hvdf_off);
}

void Shadow3::draw_debug_window() {
  ImGui::Checkbox("hacks", &m_hacks);
  ImGui::Checkbox("near_plane", &m_near_plane_hack);
  ImGui::Checkbox("back?", &m_cull_back);
  ImGui::InputInt("Tri", &m_debug_tri);
}

void Shadow3::render_jak1(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  m_did_first_time_setup = false;
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();

    if (data.vifcode0().kind == VifCode::Kind::PC_PORT) {
      u32 next = data.data_offset;
      while (next) {
        Jak1ShadowRequest game_request;
        memcpy(&game_request, g_ee_main_mem + next, sizeof(Jak1ShadowRequest));
        next = game_request.next;

        char name[128];
        strncpy(name, (const char*)(g_ee_main_mem) + 4 + game_request.geo_name, 128);
        name[127] = 0;
        // printf("  draw %s\n", name);

        auto model = render_state->loader->get_shadow_model(name);
        if (!model) {
          // printf("   SKIP: no model data\n");
          continue;
        }

        constexpr u32 kCullWhenUnderPlane = 1;
        // constexpr u32 kDisableFade = 2;
        constexpr u32 kAbsolutePlanes = 4;
        constexpr u32 kUpperClip = 8;
        // constexpr u32 kFlag4 = 16;
        constexpr u32 kDisableDraw = 32;

        if (game_request.settings.flags & kDisableDraw) {
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

        request.model = *model;
        // the origin of "light" for the shadow is found by starting at the "center" point
        // (somewhere in the model) and following the shadow direction backward.
        request.origin = game_request.settings.center +
                         game_request.settings.shadow_dir * game_request.settings.dist_to_locus;
        request.bones = g_ee_main_mem + game_request.mtx;
        request.scissor_top = game_request.settings.flags & kUpperClip;
        request.color = game_request.color;
        request.dist_to_locus = game_request.settings.dist_to_locus;
        m_color = request.color;
        // m_color = {0.2, 0.8, 0.2, 1.};

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
          // in relative planes mode, the height of the plane is adjusted to be relative to the
          // height of the center, so the planes move and down with the model
          request.top_plane.w() -= game_request.settings.center.y();
          if (m_near_plane_hack) {
            request.bottom_plane.w() = 4096;
          }
          request.bottom_plane.w() -= game_request.settings.center.y();
        }

        // skip drawing if the camera is below the lower clipping plane
        if (!m_hacks && (kCullWhenUnderPlane & game_request.settings.flags)) {
          if (render_state->camera_pos.xyz().dot(request.bottom_plane.xyz()) +
                  request.bottom_plane.w() <
              0) {
            m_next_request--;
            continue;
          }
        }

        request.next = chain->head;
        chain->head = &request;

        // detect if the origin is below the clipping plane and if so, move it up.
        // the logic for this changed in jak2, to support shadows with negative dist_from_locus
        if (render_state->version == GameVersion::Jak1) {
          const float dot = request.bottom_plane.xyz().dot(request.origin);
          if (dot + request.bottom_plane.w() > 0) {
            request.bottom_plane.w() = -dot;
          }
        } else {
          const float bot_offset = request.origin.dot(request.bottom_plane.xyz());
          const float top_offset = request.origin.dot(request.top_plane.xyz());
          if ((request.bottom_plane.w() < bot_offset) && (top_offset < request.top_plane.w())) {
            if (request.dist_to_locus > 0) {
              request.bottom_plane.w() = -bot_offset;
            } else {
              request.top_plane.w() = -top_offset;
            }
          }
        }

        const auto& cam_rot = render_state->camera_rot;

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

        request.light_dir = rotate(request.light_dir);
        request.top_plane = rotate_plane(request.top_plane);
        request.bottom_plane = rotate_plane(request.bottom_plane);
        request.origin = transform(request.origin);
      }
    }
  }

  flush_requests(render_state, prof);
  finish(render_state, prof);
}
