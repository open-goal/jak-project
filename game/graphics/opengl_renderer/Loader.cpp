#include "Loader.h"
#include "common/util/Timer.h"
#include "common/util/FileUtil.h"
#include "common/util/compress.h"

namespace {
std::string uppercase_string(const std::string& s) {
  std::string result;
  for (auto c : s) {
    result.push_back(toupper(c));
  }
  return result;
}
}  // namespace

Loader::Loader() {
  m_loader_thread = std::thread(&Loader::loader_thread, this);
}

Loader::~Loader() {
  {
    std::lock_guard<std::mutex> lk(m_loader_mutex);
    m_want_shutdown = true;
    m_loader_cv.notify_all();
  }
  m_loader_thread.join();
}

/*!
 * Try to get a loaded level by name. It may fail and return nullptr.
 * Getting a level will reset the counter for the level and prevent it from being kicked out
 * for a little while.
 *
 * This is safe to call from the graphics thread
 */
const Loader::LevelData* Loader::get_tfrag3_level(const std::string& level_name) {
  std::unique_lock<std::mutex> lk(m_loader_mutex);
  const auto& existing = m_loaded_tfrag3_levels.find(level_name);
  if (existing == m_loaded_tfrag3_levels.end()) {
    return nullptr;
  } else {
    existing->second->frames_since_last_used = 0;
    return existing->second.get();
  }
}

/*!
 * The game calls this to give the loader a hint on which levels we want.
 * If the loader is not busy, it will begin loading the level.
 * This should be called on every frame.
 */
void Loader::set_want_levels(const std::vector<std::string>& levels) {
  std::unique_lock<std::mutex> lk(m_loader_mutex);
  m_desired_levels = levels;
  if (!m_level_to_load.empty()) {
    // can't do anything, we're loading a level right now
    return;
  }

  if (!m_initializing_tfrag3_levels.empty()) {
    // can't do anything, we're initializing a level right now
    return;
  }

  // loader isn't busy, try to load one of the requested levels.
  for (auto& lev : levels) {
    auto it = m_loaded_tfrag3_levels.find(lev);
    if (it == m_loaded_tfrag3_levels.end()) {
      // we haven't loaded it yet. Request this level to load and wake up the thread.
      m_level_to_load = lev;
      lk.unlock();
      m_loader_cv.notify_all();
      return;
    }
  }
}

/*!
 * Get all levels that are in memory and used very recently.
 */
std::vector<Loader::LevelData*> Loader::get_in_use_levels() {
  std::vector<Loader::LevelData*> result;
  std::unique_lock<std::mutex> lk(m_loader_mutex);

  for (auto& lev : m_loaded_tfrag3_levels) {
    if (lev.second->frames_since_last_used < 5) {
      result.push_back(lev.second.get());
    }
  }
  return result;
}

/*!
 * Loader function that runs in a completely separate thread.
 * This is used for file I/O and unpacking.
 */
void Loader::loader_thread() {
  while (!m_want_shutdown) {
    std::unique_lock<std::mutex> lk(m_loader_mutex);

    // this will keep us asleep until we've got a level to load.
    m_loader_cv.wait(lk, [&] { return !m_level_to_load.empty() || m_want_shutdown; });
    if (m_want_shutdown) {
      return;
    }
    std::string lev = m_level_to_load;
    // don't hold the lock while reading the file.
    lk.unlock();

    // simulate slower hard drive (so that the loader thread can lose to the game loads)
    // std::this_thread::sleep_for(std::chrono::milliseconds(1500));

    // load the fr3 file
    Timer disk_timer;
    auto data = file_util::read_binary_file(
        file_util::get_file_path({fmt::format("assets/{}.fr3", uppercase_string(lev))}));
    double disk_load_time = disk_timer.getSeconds();

    // the FR3 files are compressed
    Timer decomp_timer;
    auto decomp_data = compression::decompress_zstd(data.data(), data.size());
    double decomp_time = decomp_timer.getSeconds();

    // Read back into the tfrag3::Level structure
    Timer import_timer;
    auto result = std::make_unique<tfrag3::Level>();
    Serializer ser(decomp_data.data(), decomp_data.size());
    result->serialize(ser);
    double import_time = import_timer.getSeconds();

    // and finally "unpack", which creates the vertex data we'll upload to the GPU
    Timer unpack_timer;
    for (auto& tie_tree : result->tie_trees) {
      for (auto& tree : tie_tree) {
        tree.unpack();
      }
    }
    for (auto& t_tree : result->tfrag_trees) {
      for (auto& tree : t_tree) {
        tree.unpack();
      }
    }

    for (auto& shrub_tree : result->shrub_trees) {
      shrub_tree.unpack();
    }
    fmt::print(
        "------------> Load from file: {:.3f}s, import {:.3f}s, decomp {:.3f}s unpack {:.3f}s\n",
        disk_load_time, import_time, decomp_time, unpack_timer.getSeconds());

    // grab the lock again
    lk.lock();
    // move this level to "initializing" state.
    m_initializing_tfrag3_levels[lev] = std::make_unique<LevelData>();  // reset load state
    m_initializing_tfrag3_levels[lev]->level = std::move(result);
    m_level_to_load = "";
    m_file_load_done_cv.notify_all();
  }
}

/*!
 * Load a "common" FR3 file that has non-level textures.
 * This should be called during initialization, before any threaded loading goes on.
 */
void Loader::load_common(TexturePool& tex_pool, const std::string& name) {
  auto data =
      file_util::read_binary_file(file_util::get_file_path({fmt::format("assets/{}.fr3", name)}));

  auto decomp_data = compression::decompress_zstd(data.data(), data.size());
  Serializer ser(decomp_data.data(), decomp_data.size());
  m_common_level.level = std::make_unique<tfrag3::Level>();
  m_common_level.level->serialize(ser);
  for (auto& tex : m_common_level.level->textures) {
    m_common_level.textures.push_back(add_texture(tex_pool, tex, true));
  }

  Timer tim;
  init_merc(tim, m_common_level);
}

/*!
 * Upload a texture to the GPU, and give it to the pool.
 */
u64 Loader::add_texture(TexturePool& pool, const tfrag3::Texture& tex, bool is_common) {
  GLuint gl_tex;
  glGenTextures(1, &gl_tex);
  glBindTexture(GL_TEXTURE_2D, gl_tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex.w, tex.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               tex.data.data());
  glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, gl_tex);
  glGenerateMipmap(GL_TEXTURE_2D);
  float aniso = 0.0f;
  glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY, &aniso);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, aniso);
  if (tex.load_to_pool) {
    TextureInput in;
    in.debug_page_name = tex.debug_tpage_name;
    in.debug_name = tex.debug_name;
    in.w = tex.w;
    in.h = tex.h;
    in.gpu_texture = gl_tex;
    in.common = is_common;
    in.id = PcTextureId::from_combo_id(tex.combo_id);
    in.src_data = (const u8*)tex.data.data();
    pool.give_texture(in);
  }

  return gl_tex;
}

bool Loader::init_tfrag(Timer& timer, LevelData& data) {
  if (data.tfrag_load_done) {
    return true;
  }

  if (data.level->tfrag_trees.front().empty()) {
    data.tfrag_load_done = true;
    return true;
  }

  if (!data.tfrag_opengl_created) {
    for (int geo = 0; geo < tfrag3::TFRAG_GEOS; geo++) {
      auto& in_trees = data.level->tfrag_trees[geo];
      for (auto& in_tree : in_trees) {
        GLuint& tree_out = data.tfrag_vertex_data[geo].emplace_back();
        glGenBuffers(1, &tree_out);
        glBindBuffer(GL_ARRAY_BUFFER, tree_out);
        glBufferData(GL_ARRAY_BUFFER,
                     in_tree.unpacked.vertices.size() * sizeof(tfrag3::PreloadedVertex), nullptr,
                     GL_STATIC_DRAW);
      }
    }
    data.tfrag_opengl_created = true;
    return false;
  }

  constexpr u32 CHUNK_SIZE = 32768;
  u32 uploaded_bytes = 0;

  while (true) {
    const auto& tree = data.level->tfrag_trees[data.tfrag_next_geo][data.tfrag_next_tree];
    u32 end_vert_in_tree = tree.unpacked.vertices.size();
    // the number of vertices we'd need to finish the tree right now
    size_t num_verts_left_in_tree = end_vert_in_tree - data.tfrag_next_vert;
    size_t start_vert_for_chunk;
    size_t end_vert_for_chunk;

    bool complete_tree;

    if (num_verts_left_in_tree > CHUNK_SIZE) {
      complete_tree = false;
      // should only do partial
      start_vert_for_chunk = data.tfrag_next_vert;
      end_vert_for_chunk = start_vert_for_chunk + CHUNK_SIZE;
      data.tfrag_next_vert += CHUNK_SIZE;
    } else {
      // should do all!
      start_vert_for_chunk = data.tfrag_next_vert;
      end_vert_for_chunk = end_vert_in_tree;
      complete_tree = true;
    }

    // glBindVertexArray(m_trees[m_load_state.vert_geo][m_load_state.vert_tree].vao);
    glBindBuffer(GL_ARRAY_BUFFER,
                 data.tfrag_vertex_data[data.tfrag_next_geo][data.tfrag_next_tree]);
    u32 upload_size = (end_vert_for_chunk - start_vert_for_chunk) * sizeof(tfrag3::PreloadedVertex);
    glBufferSubData(GL_ARRAY_BUFFER, start_vert_for_chunk * sizeof(tfrag3::PreloadedVertex),
                    upload_size, tree.unpacked.vertices.data() + start_vert_for_chunk);
    uploaded_bytes += upload_size;

    if (complete_tree) {
      // and move on to next tree
      data.tfrag_next_vert = 0;
      data.tfrag_next_tree++;
      if (data.tfrag_next_tree >= data.level->tfrag_trees[data.tfrag_next_geo].size()) {
        data.tfrag_next_tree = 0;
        data.tfrag_next_geo++;
        if (data.tfrag_next_geo >= tfrag3::TFRAG_GEOS) {
          data.tfrag_load_done = true;
          data.tfrag_next_tree = 0;
          data.tfrag_next_geo = 0;
          data.tfrag_next_vert = 0;
          return true;
        }
      }
    }

    if (timer.getMs() > Loader::TIE_LOAD_BUDGET || (uploaded_bytes / 1024) > 2048) {
      return false;
    }
  }
}

bool Loader::init_shrub(Timer& timer, LevelData& data) {
  if (data.shrub_load_done) {
    return true;
  }

  if (data.level->shrub_trees.empty()) {
    data.shrub_load_done = true;
    return true;
  }

  if (!data.shrub_opengl_created) {
    for (auto& in_tree : data.level->shrub_trees) {
      GLuint& tree_out = data.shrub_vertex_data.emplace_back();
      glGenBuffers(1, &tree_out);
      glBindBuffer(GL_ARRAY_BUFFER, tree_out);
      glBufferData(GL_ARRAY_BUFFER,
                   in_tree.unpacked.vertices.size() * sizeof(tfrag3::ShrubGpuVertex), nullptr,
                   GL_STATIC_DRAW);
    }
    data.shrub_opengl_created = true;
    return false;
  }

  constexpr u32 CHUNK_SIZE = 32768;
  u32 uploaded_bytes = 0;

  while (true) {
    const auto& tree = data.level->shrub_trees[data.shrub_next_tree];
    u32 end_vert_in_tree = tree.unpacked.vertices.size();
    // the number of vertices we'd need to finish the tree right now
    size_t num_verts_left_in_tree = end_vert_in_tree - data.shrub_next_vert;
    size_t start_vert_for_chunk;
    size_t end_vert_for_chunk;

    bool complete_tree;

    if (num_verts_left_in_tree > CHUNK_SIZE) {
      complete_tree = false;
      // should only do partial
      start_vert_for_chunk = data.shrub_next_vert;
      end_vert_for_chunk = start_vert_for_chunk + CHUNK_SIZE;
      data.shrub_next_vert += CHUNK_SIZE;
    } else {
      // should do all!
      start_vert_for_chunk = data.shrub_next_vert;
      end_vert_for_chunk = end_vert_in_tree;
      complete_tree = true;
    }

    // glBindVertexArray(m_trees[m_load_state.vert_geo][m_load_state.vert_tree].vao);
    glBindBuffer(GL_ARRAY_BUFFER, data.shrub_vertex_data[data.shrub_next_tree]);
    u32 upload_size = (end_vert_for_chunk - start_vert_for_chunk) * sizeof(tfrag3::ShrubGpuVertex);
    glBufferSubData(GL_ARRAY_BUFFER, start_vert_for_chunk * sizeof(tfrag3::ShrubGpuVertex),
                    upload_size, tree.unpacked.vertices.data() + start_vert_for_chunk);
    uploaded_bytes += upload_size;

    if (complete_tree) {
      // and move on to next tree
      data.shrub_next_vert = 0;
      data.shrub_next_tree++;
      if (data.shrub_next_tree >= data.level->shrub_trees.size()) {
        data.shrub_load_done = true;
        return true;
      }
    }

    if (timer.getMs() > Loader::TIE_LOAD_BUDGET || (uploaded_bytes / 1024) > 2048) {
      return false;
    }
  }
}

bool Loader::init_tie(Timer& timer, LevelData& data) {
  if (data.tie_load_done) {
    return true;
  }

  if (data.level->tie_trees.front().empty()) {
    data.tie_load_done = true;
    return true;
  }

  if (!data.tie_opengl_created) {
    for (int geo = 0; geo < tfrag3::TIE_GEOS; geo++) {
      auto& in_trees = data.level->tie_trees[geo];
      for (auto& in_tree : in_trees) {
        LevelData::TieOpenGL& tree_out = data.tie_data[geo].emplace_back();
        glGenBuffers(1, &tree_out.vertex_buffer);
        glBindBuffer(GL_ARRAY_BUFFER, tree_out.vertex_buffer);
        glBufferData(GL_ARRAY_BUFFER,
                     in_tree.unpacked.vertices.size() * sizeof(tfrag3::PreloadedVertex), nullptr,
                     GL_STATIC_DRAW);
      }
    }
    data.tie_opengl_created = true;
    return false;
  }

  if (!data.tie_verts_done) {
    constexpr u32 CHUNK_SIZE = 32768;
    u32 uploaded_bytes = 0;

    while (true) {
      const auto& tree = data.level->tie_trees[data.tie_next_geo][data.tie_next_tree];
      u32 end_vert_in_tree = tree.unpacked.vertices.size();
      // the number of vertices we'd need to finish the tree right now
      size_t num_verts_left_in_tree = end_vert_in_tree - data.tie_next_vert;
      size_t start_vert_for_chunk;
      size_t end_vert_for_chunk;

      bool complete_tree;

      if (num_verts_left_in_tree > CHUNK_SIZE) {
        complete_tree = false;
        // should only do partial
        start_vert_for_chunk = data.tie_next_vert;
        end_vert_for_chunk = start_vert_for_chunk + CHUNK_SIZE;
        data.tie_next_vert += CHUNK_SIZE;
      } else {
        // should do all!
        start_vert_for_chunk = data.tie_next_vert;
        end_vert_for_chunk = end_vert_in_tree;
        complete_tree = true;
      }

      // glBindVertexArray(m_trees[m_load_state.vert_geo][m_load_state.vert_tree].vao);
      glBindBuffer(GL_ARRAY_BUFFER,
                   data.tie_data[data.tie_next_geo][data.tie_next_tree].vertex_buffer);
      u32 upload_size =
          (end_vert_for_chunk - start_vert_for_chunk) * sizeof(tfrag3::PreloadedVertex);
      glBufferSubData(GL_ARRAY_BUFFER, start_vert_for_chunk * sizeof(tfrag3::PreloadedVertex),
                      upload_size, tree.unpacked.vertices.data() + start_vert_for_chunk);
      uploaded_bytes += upload_size;

      if (complete_tree) {
        // and move on to next tree
        data.tie_next_vert = 0;
        data.tie_next_tree++;
        if (data.tie_next_tree >= data.level->tie_trees[data.tie_next_geo].size()) {
          data.tie_next_tree = 0;
          data.tie_next_geo++;
          if (data.tie_next_geo >= tfrag3::TIE_GEOS) {
            data.tie_verts_done = true;
            data.tie_next_tree = 0;
            data.tie_next_geo = 0;
            data.tie_next_vert = 0;
            return false;
          }
        }
      }

      if (timer.getMs() > Loader::TIE_LOAD_BUDGET || (uploaded_bytes / 1024) > 2048) {
        return false;
      }
    }
  }

  if (!data.tie_wind_indices_done) {
    bool abort = false;
    for (; data.tie_next_geo < tfrag3::TIE_GEOS; data.tie_next_geo++) {
      auto& geo_trees = data.level->tie_trees[data.tie_next_geo];
      for (; data.tie_next_tree < geo_trees.size(); data.tie_next_tree++) {
        if (abort) {
          return false;
        }
        auto& in_tree = geo_trees[data.tie_next_tree];
        auto& out_tree = data.tie_data[data.tie_next_geo][data.tie_next_tree];
        size_t wind_idx_buffer_len = 0;
        for (auto& draw : in_tree.instanced_wind_draws) {
          wind_idx_buffer_len += draw.vertex_index_stream.size();
        }
        if (wind_idx_buffer_len > 0) {
          out_tree.has_wind = true;
          glGenBuffers(1, &out_tree.wind_indices);
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, out_tree.wind_indices);
          std::vector<u32> temp;
          temp.resize(wind_idx_buffer_len);
          u32 off = 0;
          for (auto& draw : in_tree.instanced_wind_draws) {
            memcpy(temp.data() + off, draw.vertex_index_stream.data(),
                   draw.vertex_index_stream.size() * sizeof(u32));
            off += draw.vertex_index_stream.size();
          }

          glBufferData(GL_ELEMENT_ARRAY_BUFFER, wind_idx_buffer_len * sizeof(u32), temp.data(),
                       GL_STATIC_DRAW);
          abort = true;
        }
      }
      data.tie_next_tree = 0;
    }

    data.tie_wind_indices_done = true;
    data.tie_load_done = true;
    return true;
  }

  return false;
}

bool Loader::init_collide(Timer& /*timer*/, LevelData& data) {
  Timer t;
  glGenBuffers(1, &data.collide_vertices);
  glBindBuffer(GL_ARRAY_BUFFER, data.collide_vertices);
  glBufferData(GL_ARRAY_BUFFER,
               data.level->collision.vertices.size() * sizeof(tfrag3::CollisionMesh::Vertex),
               data.level->collision.vertices.data(), GL_STATIC_DRAW);
  fmt::print("init_collide took {:.2f} ms\n", t.getMs());
  return true;
}

bool Loader::init_merc(Timer& /*timer*/, LevelData& data) {
  Timer t;
  glGenBuffers(1, &data.merc_indices);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, data.merc_indices);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, data.level->merc_data.indices.size() * sizeof(u32),
               data.level->merc_data.indices.data(), GL_STATIC_DRAW);

  glGenBuffers(1, &data.merc_vertices);
  glBindBuffer(GL_ARRAY_BUFFER, data.merc_vertices);
  glBufferData(GL_ARRAY_BUFFER, data.level->merc_data.vertices.size() * sizeof(tfrag3::MercVertex),
               data.level->merc_data.vertices.data(), GL_STATIC_DRAW);

  for (auto& model : data.level->merc_data.models) {
    data.merc_model_lookup[model.name] = &model;
    m_all_merc_models[model.name].push_back({&model, data.load_id, &data});
  }
  fmt::print("init_merc took {:.2f} ms\n", t.getMs());
  return true;
}

bool Loader::upload_textures(Timer& timer, LevelData& data, TexturePool& texture_pool) {
  // try to move level from initializing to initialized:

  constexpr int MAX_TEX_BYTES_PER_FRAME = 1024 * 128;

  int bytes_this_run = 0;
  int tex_this_run = 0;
  if (data.textures.size() < data.level->textures.size()) {
    std::unique_lock<std::mutex> tpool_lock(texture_pool.mutex());
    while (data.textures.size() < data.level->textures.size()) {
      auto& tex = data.level->textures[data.textures.size()];
      data.textures.push_back(add_texture(texture_pool, tex, false));
      bytes_this_run += tex.w * tex.h * 4;
      tex_this_run++;
      if (tex_this_run > 20) {
        break;
      }
      if (bytes_this_run > MAX_TEX_BYTES_PER_FRAME || timer.getMs() > SHARED_TEXTURE_LOAD_BUDGET) {
        break;
      }
    }
  }
  return data.textures.size() == data.level->textures.size();
}

void Loader::update_blocking(TexturePool& tex_pool) {
  fmt::print("NOTE: coming out of blackout on next frame, doing all loads now...\n");

  bool missing_levels = true;
  while (missing_levels) {
    bool needs_run = true;

    while (needs_run) {
      needs_run = false;
      {
        std::unique_lock<std::mutex> lk(m_loader_mutex);
        if (!m_level_to_load.empty()) {
          m_file_load_done_cv.wait(lk, [&]() { return m_level_to_load.empty(); });
        }
      }
    }

    needs_run = true;

    while (needs_run) {
      needs_run = false;
      {
        std::unique_lock<std::mutex> lk(m_loader_mutex);
        if (!m_initializing_tfrag3_levels.empty()) {
          needs_run = true;
        }
      }

      if (needs_run) {
        update(tex_pool);
      }
    }

    {
      std::unique_lock<std::mutex> lk(m_loader_mutex);
      missing_levels = false;
      for (auto& des : m_desired_levels) {
        if (m_loaded_tfrag3_levels.find(des) == m_loaded_tfrag3_levels.end()) {
          fmt::print("blackout loader doing additional level {}...\n", des);
          missing_levels = true;
        }
      }
    }

    if (missing_levels) {
      set_want_levels(m_desired_levels);
    }
  }

  fmt::print("Blackout loads done. Current status:");
  std::unique_lock<std::mutex> lk(m_loader_mutex);
  for (auto& ld : m_loaded_tfrag3_levels) {
    fmt::print("  {} is loaded.\n", ld.first);
  }
}

void Loader::update(TexturePool& texture_pool) {
  Timer loader_timer;

  // only main thread can touch this.
  for (auto& lev : m_loaded_tfrag3_levels) {
    lev.second->frames_since_last_used++;
  }

  bool did_gpu_stuff = false;

  // work on moving initializing to initialized.
  {
    // accessing initializing, should lock
    std::unique_lock<std::mutex> lk(m_loader_mutex);
    // grab the first initializing level:
    const auto& it = m_initializing_tfrag3_levels.begin();
    if (it != m_initializing_tfrag3_levels.end()) {
      std::string name = it->first;
      auto& lev = it->second;
      it->second->load_id = m_id++;

      // we're the only place that erases, so it's okay to unlock and hold a reference
      lk.unlock();
      if (!lev->tfrag_load_done) {
        did_gpu_stuff = true;
      }
      if (upload_textures(loader_timer, *lev, texture_pool)) {
        if (init_tie(loader_timer, *lev)) {
          if (init_tfrag(loader_timer, *lev)) {
            if (init_shrub(loader_timer, *lev)) {
              if (init_collide(loader_timer, *lev)) {
                if (init_merc(loader_timer, *lev)) {
                  // we're done! lock before removing from loaded.
                  lk.lock();

                  m_loaded_tfrag3_levels[name] = std::move(lev);
                  m_initializing_tfrag3_levels.erase(it);
                }
              }
            }
          }
        }
      }
    }
  }

  if (!did_gpu_stuff) {
    // try to remove levels.
    if (m_loaded_tfrag3_levels.size() >= 3) {
      for (auto& lev : m_loaded_tfrag3_levels) {
        if (lev.second->frames_since_last_used > 180) {
          std::unique_lock<std::mutex> lk(texture_pool.mutex());
          fmt::print("------------------------- PC unloading {}\n", lev.first);
          for (size_t i = 0; i < lev.second->level->textures.size(); i++) {
            auto& tex = lev.second->level->textures[i];
            if (tex.load_to_pool) {
              texture_pool.unload_texture(PcTextureId::from_combo_id(tex.combo_id),
                                          lev.second->textures.at(i));
            }
          }
          lk.unlock();
          for (auto tex : lev.second->textures) {
            if (EXTRA_TEX_DEBUG) {
              for (auto& slot : texture_pool.all_textures()) {
                if (slot.source) {
                  ASSERT(slot.gpu_texture != tex);
                } else {
                  ASSERT(slot.gpu_texture != tex);
                }
              }
            }

            glBindTexture(GL_TEXTURE_2D, tex);
            glDeleteTextures(1, &tex);
          }

          for (auto& tie_geo : lev.second->tie_data) {
            for (auto& tie_tree : tie_geo) {
              glDeleteBuffers(1, &tie_tree.vertex_buffer);
              if (tie_tree.has_wind) {
                glDeleteBuffers(1, &tie_tree.wind_indices);
              }
            }
          }

          for (auto& tfrag_geo : lev.second->tfrag_vertex_data) {
            for (auto& tfrag_buff : tfrag_geo) {
              glDeleteBuffers(1, &tfrag_buff);
            }
          }

          glDeleteBuffers(1, &lev.second->collide_vertices);
          glDeleteBuffers(1, &lev.second->merc_vertices);
          glDeleteBuffers(1, &lev.second->merc_indices);

          for (auto& model : lev.second->level->merc_data.models) {
            auto& mercs = m_all_merc_models.at(model.name);
            MercRef ref{&model, lev.second->load_id};
            auto it = std::find(mercs.begin(), mercs.end(), ref);
            ASSERT_MSG(it != mercs.end(), fmt::format("missing merc: {}\n", model.name));
            mercs.erase(it);
          }

          m_loaded_tfrag3_levels.erase(lev.first);
          break;
        }
      }
    }
  }

  if (loader_timer.getMs() > 5) {
    fmt::print("Loader::update slow setup: {:.1f}ms\n", loader_timer.getMs());
  }
}

std::optional<Loader::MercRef> Loader::get_merc_model(const char* model_name) {
  // don't think we need to lock here...
  const auto& it = m_all_merc_models.find(model_name);
  if (it != m_all_merc_models.end() && !it->second.empty()) {
    // it->second.front().parent_level->frames_since_last_used = 0;
    return it->second.front();
  } else {
    return std::nullopt;
  }
}