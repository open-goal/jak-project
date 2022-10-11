#include "Shrub.h"

#include "common/log/log.h"

Shrub::Shrub(const std::string& name, int my_id) : BucketRenderer(name, my_id) {
  m_color_result.resize(TIME_OF_DAY_COLOR_COUNT);
}

Shrub::~Shrub() {
  discard_tree_cache();
}

void Shrub::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0 || data0.vifcode1().kind == VifCode::Kind::NOP);
  ASSERT(data0.vif0() == 0 || data0.vifcode0().kind == VifCode::Kind::NOP ||
         data0.vifcode0().kind == VifCode::Kind::MARK);
  ASSERT(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }
  if (dma.current_tag_offset() == render_state->next_bucket) {
    return;
  }

  auto pc_port_data = dma.read_and_advance();
  ASSERT(pc_port_data.size_bytes == sizeof(TfragPcPortData));
  memcpy(&m_pc_port_data, pc_port_data.data, sizeof(TfragPcPortData));
  m_pc_port_data.level_name[11] = '\0';

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }

  TfragRenderSettings settings;
  settings.hvdf_offset = m_pc_port_data.hvdf_off;
  settings.fog = m_pc_port_data.fog;

  memcpy(settings.math_camera.data(), m_pc_port_data.camera[0].data(), 64);
  settings.tree_idx = 0;

  for (int i = 0; i < 4; i++) {
    settings.itimes[i] = m_pc_port_data.itimes[i];
  }

  update_render_state_from_pc_settings(render_state, m_pc_port_data);

  for (int i = 0; i < 4; i++) {
    settings.planes[i] = m_pc_port_data.planes[i];
  }

  m_has_level = setup_for_level(m_pc_port_data.level_name, render_state);
  render_all_trees(settings, render_state, prof);
}

void Shrub::update_load(const LevelData* loader_data) {
  const tfrag3::Level* lev_data = loader_data->level.get();
  // We changed level!
  discard_tree_cache();
  m_trees.resize(lev_data->shrub_trees.size());

  size_t max_draws = 0;
  size_t time_of_day_count = 0;
  size_t max_num_grps = 0;
  size_t max_inds = 0;

  for (u32 l_tree = 0; l_tree < lev_data->shrub_trees.size(); l_tree++) {
    size_t num_grps = 0;

    const auto& tree = lev_data->shrub_trees[l_tree];
    max_draws = std::max(tree.static_draws.size(), max_draws);
    for (auto& draw : tree.static_draws) {
      (void)draw;
      // num_grps += draw.vis_groups.size(); TODO
      max_num_grps += 1;
    }
    max_num_grps = std::max(max_num_grps, num_grps);

    time_of_day_count = std::max(tree.time_of_day_colors.size(), time_of_day_count);
    max_inds = std::max(tree.indices.size(), max_inds);
    u32 verts = tree.unpacked.vertices.size();
    glGenVertexArrays(1, &m_trees[l_tree].vao);
    glBindVertexArray(m_trees[l_tree].vao);
    m_trees[l_tree].vertex_buffer = loader_data->shrub_vertex_data[l_tree];
    m_trees[l_tree].vert_count = verts;
    m_trees[l_tree].draws = &tree.static_draws;
    m_trees[l_tree].colors = &tree.time_of_day_colors;
    m_trees[l_tree].index_data = tree.indices.data();
    m_trees[l_tree].tod_cache = swizzle_time_of_day(tree.time_of_day_colors);
    glBindBuffer(GL_ARRAY_BUFFER, m_trees[l_tree].vertex_buffer);
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glEnableVertexAttribArray(3);

    glVertexAttribPointer(0,                                          // location 0 in the shader
                          3,                                          // 3 values per vert
                          GL_FLOAT,                                   // floats
                          GL_FALSE,                                   // normalized
                          sizeof(tfrag3::ShrubGpuVertex),             // stride
                          (void*)offsetof(tfrag3::ShrubGpuVertex, x)  // offset (0)
    );

    glVertexAttribPointer(1,                                          // location 1 in the shader
                          2,                                          // 3 values per vert
                          GL_FLOAT,                                   // floats
                          GL_FALSE,                                   // normalized
                          sizeof(tfrag3::ShrubGpuVertex),             // stride
                          (void*)offsetof(tfrag3::ShrubGpuVertex, s)  // offset (0)
    );

    glVertexAttribPointer(2,                               // location 1 in the shader
                          3,                               // 4 color components
                          GL_UNSIGNED_BYTE,                // u8
                          GL_TRUE,                         // normalized (255 becomes 1)
                          sizeof(tfrag3::ShrubGpuVertex),  //
                          (void*)offsetof(tfrag3::ShrubGpuVertex, rgba_base)  //
    );

    glVertexAttribIPointer(3,                               // location 2 in the shader
                           1,                               // 1 values per vert
                           GL_UNSIGNED_SHORT,               // u16
                           sizeof(tfrag3::ShrubGpuVertex),  // stride
                           (void*)offsetof(tfrag3::ShrubGpuVertex, color_index)  // offset (0)
    );

    glGenBuffers(1, &m_trees[l_tree].single_draw_index_buffer);
    glGenBuffers(1, &m_trees[l_tree].index_buffer);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_trees[l_tree].index_buffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, tree.indices.size() * sizeof(u32), tree.indices.data(),
                 GL_STATIC_DRAW);

    glActiveTexture(GL_TEXTURE10);
    glGenTextures(1, &m_trees[l_tree].time_of_day_texture);
    glBindTexture(GL_TEXTURE_1D, m_trees[l_tree].time_of_day_texture);
    glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, TIME_OF_DAY_COLOR_COUNT, 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8, nullptr);
    glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    glBindVertexArray(0);
  }

  m_cache.multidraw_offset_per_stripdraw.resize(max_draws);
  m_cache.multidraw_count_buffer.resize(max_num_grps);
  m_cache.multidraw_index_offset_buffer.resize(max_num_grps);
  m_cache.draw_idx_temp.resize(max_draws);
  m_cache.index_temp.resize(max_inds);
  ASSERT(time_of_day_count <= TIME_OF_DAY_COLOR_COUNT);
}

bool Shrub::setup_for_level(const std::string& level, SharedRenderState* render_state) {
  // make sure we have the level data.
  Timer tfrag3_setup_timer;
  auto lev_data = render_state->loader->get_tfrag3_level(level);
  if (!lev_data || (m_has_level && lev_data->load_id != m_load_id)) {
    m_has_level = false;
    m_textures = nullptr;
    m_level_name = "";
    discard_tree_cache();
    return false;
  }
  m_textures = &lev_data->textures;
  m_load_id = lev_data->load_id;

  if (m_level_name != level) {
    update_load(lev_data);
    m_has_level = true;
    m_level_name = level;
  } else {
    m_has_level = true;
  }

  if (tfrag3_setup_timer.getMs() > 5) {
    lg::info("Shrub setup: {:.1f}ms", tfrag3_setup_timer.getMs());
  }

  return m_has_level;
}

void Shrub::discard_tree_cache() {
  for (auto& tree : m_trees) {
    glBindTexture(GL_TEXTURE_1D, tree.time_of_day_texture);
    glDeleteTextures(1, &tree.time_of_day_texture);
    glDeleteBuffers(1, &tree.index_buffer);
    glDeleteBuffers(1, &tree.single_draw_index_buffer);
    glDeleteVertexArrays(1, &tree.vao);
  }

  m_trees.clear();
}

void Shrub::render_all_trees(const TfragRenderSettings& settings,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof) {
  for (u32 i = 0; i < m_trees.size(); i++) {
    render_tree(i, settings, render_state, prof);
  }
}

void Shrub::render_tree(int idx,
                        const TfragRenderSettings& settings,
                        SharedRenderState* render_state,
                        ScopedProfilerNode& prof) {
  Timer tree_timer;
  auto& tree = m_trees.at(idx);
  tree.perf.draws = 0;
  tree.perf.wind_draws = 0;
  if (!m_has_level) {
    return;
  }

  if (m_color_result.size() < tree.colors->size()) {
    m_color_result.resize(tree.colors->size());
  }

  Timer interp_timer;
  interp_time_of_day_fast(settings.itimes, tree.tod_cache, m_color_result.data());
  tree.perf.tod_time.add(interp_timer.getSeconds());

  Timer setup_timer;
  glActiveTexture(GL_TEXTURE10);
  glBindTexture(GL_TEXTURE_1D, tree.time_of_day_texture);
  glTexSubImage1D(GL_TEXTURE_1D, 0, 0, tree.colors->size(), GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                  m_color_result.data());

  first_tfrag_draw_setup(settings, render_state, ShaderId::SHRUB);

  glBindVertexArray(tree.vao);
  glBindBuffer(GL_ARRAY_BUFFER, tree.vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,
               render_state->no_multidraw ? tree.single_draw_index_buffer : tree.index_buffer);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  tree.perf.tod_time.add(setup_timer.getSeconds());

  int last_texture = -1;

  tree.perf.cull_time.add(0);
  Timer index_timer;
  if (render_state->no_multidraw) {
    u32 idx_buffer_size = make_all_visible_index_list(
        m_cache.draw_idx_temp.data(), m_cache.index_temp.data(), *tree.draws, tree.index_data);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_size * sizeof(u32), m_cache.index_temp.data(),
                 GL_STREAM_DRAW);
  } else {
    make_all_visible_multidraws(m_cache.multidraw_offset_per_stripdraw.data(),
                                m_cache.multidraw_count_buffer.data(),
                                m_cache.multidraw_index_offset_buffer.data(), *tree.draws);
  }

  tree.perf.index_time.add(index_timer.getSeconds());

  Timer draw_timer;

  for (size_t draw_idx = 0; draw_idx < tree.draws->size(); draw_idx++) {
    const auto& draw = tree.draws->operator[](draw_idx);
    const auto& multidraw_indices = m_cache.multidraw_offset_per_stripdraw[draw_idx];
    const auto& singledraw_indices = m_cache.draw_idx_temp[draw_idx];

    if (render_state->no_multidraw) {
      if (singledraw_indices.second == 0) {
        continue;
      }
    } else {
      if (multidraw_indices.second == 0) {
        continue;
      }
    }

    if ((int)draw.tree_tex_id != last_texture) {
      glBindTexture(GL_TEXTURE_2D, m_textures->at(draw.tree_tex_id));
      last_texture = draw.tree_tex_id;
    }

    auto double_draw = setup_tfrag_shader(render_state, draw.mode, ShaderId::SHRUB);

    prof.add_draw_call();
    prof.add_tri(draw.num_triangles);

    tree.perf.draws++;

    if (render_state->no_multidraw) {
      glDrawElements(GL_TRIANGLE_STRIP, singledraw_indices.second, GL_UNSIGNED_INT,
                     (void*)(singledraw_indices.first * sizeof(u32)));
    } else {
      glMultiDrawElements(GL_TRIANGLE_STRIP,
                          &m_cache.multidraw_count_buffer[multidraw_indices.first], GL_UNSIGNED_INT,
                          &m_cache.multidraw_index_offset_buffer[multidraw_indices.first],
                          multidraw_indices.second);
    }

    switch (double_draw.kind) {
      case DoubleDrawKind::NONE:
        break;
      case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
        tree.perf.draws++;
        prof.add_draw_call();
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SHRUB].id(), "alpha_min"),
                    -10.f);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::SHRUB].id(), "alpha_max"),
                    double_draw.aref_second);
        glDepthMask(GL_FALSE);
        if (render_state->no_multidraw) {
          glDrawElements(GL_TRIANGLE_STRIP, singledraw_indices.second, GL_UNSIGNED_INT,
                         (void*)(singledraw_indices.first * sizeof(u32)));
        } else {
          glMultiDrawElements(
              GL_TRIANGLE_STRIP, &m_cache.multidraw_count_buffer[multidraw_indices.first],
              GL_UNSIGNED_INT, &m_cache.multidraw_index_offset_buffer[multidraw_indices.first],
              multidraw_indices.second);
        }
        break;
      default:
        ASSERT(false);
    }
  }

  glBindVertexArray(0);
  tree.perf.draw_time.add(draw_timer.getSeconds());
  tree.perf.tree_time.add(tree_timer.getSeconds());
}

void Shrub::draw_debug_window() {}
