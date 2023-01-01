#include "Loader.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/compress.h"

#include "game/graphics/opengl_renderer/loader/LoaderStages.h"

namespace {
std::string uppercase_string(const std::string& s) {
  std::string result;
  for (auto c : s) {
    result.push_back(toupper(c));
  }
  return result;
}
}  // namespace

Loader::Loader(const fs::path& base_path, int max_levels)
    : m_base_path(base_path), m_max_levels(max_levels) {
  m_loader_thread = std::thread(&Loader::loader_thread, this);
  m_loader_stages = make_loader_stages();
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
const LevelData* Loader::get_tfrag3_level(const std::string& level_name) {
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
std::vector<LevelData*> Loader::get_in_use_levels() {
  std::vector<LevelData*> result;
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
  try {
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
      auto data =
          file_util::read_binary_file(m_base_path / fmt::format("{}.fr3", uppercase_string(lev)));
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
  } catch (std::exception& e) {
    ASSERT_MSG(false, fmt::format("Exception {} encountered in loader_thread", e.what()));
  }
}

/*!
 * Load a "common" FR3 file that has non-level textures.
 * This should be called during initialization, before any threaded loading goes on.
 */
void Loader::load_common(TexturePool& tex_pool, const std::string& name) {
  auto data = file_util::read_binary_file(m_base_path / fmt::format("{}.fr3", name));

  auto decomp_data = compression::decompress_zstd(data.data(), data.size());
  Serializer ser(decomp_data.data(), decomp_data.size());
  m_common_level.level = std::make_unique<tfrag3::Level>();
  m_common_level.level->serialize(ser);
  for (auto& tex : m_common_level.level->textures) {
    m_common_level.textures.push_back(add_texture(tex_pool, tex, true));
  }

  Timer tim;
  MercLoaderStage mls;
  LoaderInput input;
  input.tex_pool = &tex_pool;
  input.mercs = &m_all_merc_models;
  input.lev_data = &m_common_level;
  bool done = false;
  while (!done) {
    done = mls.run(tim, input);
  }
}

bool Loader::upload_textures(Timer& timer, LevelData& data, TexturePool& texture_pool) {
  // try to move level from initializing to initialized:

  auto evt = scoped_prof("upload-textures");
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
      did_gpu_stuff = true;
      std::string name = it->first;
      auto& lev = it->second;
      if (it->second->load_id == UINT64_MAX) {
        it->second->load_id = m_id++;
      }

      // we're the only place that erases, so it's okay to unlock and hold a reference
      lk.unlock();
      bool done = true;
      LoaderInput loader_input;
      loader_input.lev_data = lev.get();
      loader_input.mercs = &m_all_merc_models;
      loader_input.tex_pool = &texture_pool;

      for (auto& stage : m_loader_stages) {
        auto evt = scoped_prof(fmt::format("stage-{}", stage->name()).c_str());
        Timer stage_timer;
        done = stage->run(loader_timer, loader_input);
        if (stage_timer.getMs() > 5.f) {
          fmt::print("stage {} took {:.2f} ms\n", stage->name(), stage_timer.getMs());
        }
        if (!done) {
          break;
        }
      }

      if (done) {
        auto evt = scoped_prof("finish-stages");
        lk.lock();
        m_loaded_tfrag3_levels[name] = std::move(lev);
        m_initializing_tfrag3_levels.erase(it);

        for (auto& stage : m_loader_stages) {
          stage->reset();
        }
      }
    }
  }

  if (!did_gpu_stuff) {
    auto evt = scoped_prof("gpu-unload");
    // try to remove levels.
    Timer unload_timer;
    if ((int)m_loaded_tfrag3_levels.size() >= m_max_levels) {
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
              glDeleteBuffers(1, &tie_tree.index_buffer);
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

    if (unload_timer.getMs() > 5.f) {
      fmt::print("Unload took {:.2f}\n", unload_timer.getMs());
    }
  }

  if (loader_timer.getMs() > 5) {
    fmt::print("Loader::update slow setup: {:.1f}ms\n", loader_timer.getMs());
  }
}

std::optional<MercRef> Loader::get_merc_model(const char* model_name) {
  // don't think we need to lock here...
  const auto& it = m_all_merc_models.find(model_name);
  if (it != m_all_merc_models.end() && !it->second.empty()) {
    // it->second.front().parent_level->frames_since_last_used = 0;
    return it->second.front();
  } else {
    return std::nullopt;
  }
}
