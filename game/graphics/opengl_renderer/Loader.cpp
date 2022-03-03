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

const Loader::LevelData* Loader::get_tfrag3_level(const std::string& level_name) {
  std::unique_lock<std::mutex> lk(m_loader_mutex);
  const auto& existing = m_loaded_tfrag3_levels.find(level_name);
  if (existing == m_loaded_tfrag3_levels.end()) {
    return nullptr;
  } else {
    existing->second.frames_since_last_used = 0;
    return &existing->second.data;
  }
}

void Loader::set_want_levels(const std::vector<std::string>& levels) {
  std::unique_lock<std::mutex> lk(m_loader_mutex);
  if (!m_level_to_load.empty()) {
    // can't do anything, we're loading a level right now
    return;
  }

  if (!m_initializing_tfrag3_levels.empty()) {
    // can't do anything, we're initializing a level right now
    return;
  }

  for (auto& lev : levels) {
    auto it = m_loaded_tfrag3_levels.find(lev);
    if (it == m_loaded_tfrag3_levels.end()) {
      m_level_to_load = lev;
      lk.unlock();
      m_loader_cv.notify_all();
      return;
    }
  }
}

void Loader::loader_thread() {
  while (!m_want_shutdown) {
    std::unique_lock<std::mutex> lk(m_loader_mutex);
    m_loader_cv.wait(lk, [&] { return !m_level_to_load.empty() || m_want_shutdown; });
    if (m_want_shutdown) {
      return;
    }
    std::string lev = m_level_to_load;
    lk.unlock();

    Timer disk_timer;
    auto data = file_util::read_binary_file(
        file_util::get_file_path({fmt::format("assets/{}.fr3", uppercase_string(lev))}));
    double disk_load_time = disk_timer.getSeconds();

    Timer decomp_timer;
    auto decomp_data = compression::decompress_zstd(data.data(), data.size());
    double decomp_time = decomp_timer.getSeconds();

    Timer import_timer;
    auto result = std::make_unique<tfrag3::Level>();
    Serializer ser(decomp_data.data(), decomp_data.size());
    result->serialize(ser);
    double import_time = import_timer.getSeconds();

    Timer unpack_timer;
    for (auto& tie_tree : result->tie_trees) {
      for (auto& tree : tie_tree) {
        tree.unpack();
        for (auto& d : tree.static_draws) {
          d.unpack();
        }
      }
    }
    for (auto& t_tree : result->tfrag_trees) {
      for (auto& tree : t_tree) {
        tree.unpack();
        for (auto& d : tree.draws) {
          d.unpack();
        }
      }
    }
    fmt::print(
        "------------> Load from file: {:.3f}s, import {:.3f}s, decomp {:.3f}s unpack {:.3f}s\n",
        disk_load_time, import_time, decomp_time, unpack_timer.getSeconds());

    lk.lock();
    m_initializing_tfrag3_levels[lev].data.level = std::move(result);
    m_level_to_load = "";
  }
}

void Loader::load_common(TexturePool& tex_pool, const std::string& name) {
  auto data =
      file_util::read_binary_file(file_util::get_file_path({fmt::format("assets/{}.fr3", name)}));

  auto decomp_data = compression::decompress_zstd(data.data(), data.size());
  Serializer ser(decomp_data.data(), decomp_data.size());
  m_common_level.serialize(ser);
  for (auto& tex : m_common_level.textures) {
    add_texture(tex_pool, tex, true);
  }
}

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
    in.page_name = tex.debug_tpage_name;
    in.name = tex.debug_name;
    in.w = tex.w;
    in.h = tex.h;
    in.gpu_texture = gl_tex;
    in.common = is_common;
    in.combo_id = tex.combo_id;
    in.src_data = (const u8*)tex.data.data();
    pool.give_texture(in);
  }

  return gl_tex;
}

Loader::Loader() {
  m_loader_thread = std::thread(&Loader::loader_thread, this);
}

void Loader::update(std::string& status_out, TexturePool& texture_pool) {
  Timer loader_timer;

  // only main thread can touch this.
  for (auto& lev : m_loaded_tfrag3_levels) {
    lev.second.frames_since_last_used++;
  }

  bool did_gpu_stuff = false;

  {
    // try to move level from initializing to initialized:
    std::unique_lock<std::mutex> lk(m_loader_mutex);
    const auto& it = m_initializing_tfrag3_levels.begin();
    if (it != m_initializing_tfrag3_levels.end()) {
      did_gpu_stuff = true;
      constexpr int MAX_TEX_BYTES_PER_FRAME = 1024 * 128;
      auto& data = it->second.data;

      int bytes_this_run = 0;
      int tex_this_run = 0;
      std::unique_lock<std::mutex> tpool_lock(texture_pool.mutex());
      while (data.textures.size() < data.level->textures.size()) {
        auto& tex = data.level->textures[data.textures.size()];
        it->second.data.textures.push_back(add_texture(texture_pool, tex, false));
        bytes_this_run += tex.w * tex.h * 4;
        tex_this_run++;
        if (tex_this_run > 20) {
          status_out += fmt::format("LOAD tex {} kB\n", bytes_this_run / 1024);
          break;
        }
        if (bytes_this_run > MAX_TEX_BYTES_PER_FRAME ||
            loader_timer.getMs() > SHARED_TEXTURE_LOAD_BUDGET) {
          status_out += fmt::format("LOAD tex {} kB\n", bytes_this_run / 1024);
          break;
        }
      }
      if (data.textures.size() == data.level->textures.size()) {
        fmt::print("Loader texture complete: {}\n", it->first);
        it->second.data.load_id = m_id++;

        m_loaded_tfrag3_levels[it->first] = std::move(it->second);
        m_initializing_tfrag3_levels.erase(it);
      }
    }
  }

  if (!did_gpu_stuff) {
    // try to remove levels.
    if (m_loaded_tfrag3_levels.size() >= 3) {
      for (const auto& lev : m_loaded_tfrag3_levels) {
        if (lev.second.frames_since_last_used > 180) {
          std::unique_lock<std::mutex> lk(texture_pool.mutex());
          fmt::print("------------------------- PC unloading {}\n", lev.first);
          for (size_t i = 0; i < lev.second.data.level->textures.size(); i++) {
            auto& tex = lev.second.data.level->textures[i];
            if (tex.load_to_pool) {
              texture_pool.unload_texture(tex.debug_name, lev.second.data.textures.at(i));
            }
          }
          for (auto tex : lev.second.data.textures) {
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

Loader::~Loader() {
  {
    std::lock_guard<std::mutex> lk(m_loader_mutex);
    m_want_shutdown = true;
    m_loader_cv.notify_all();
  }
  m_loader_thread.join();
}

void Loader::hack_scramble_textures() {
  for (auto& it : m_loaded_tfrag3_levels) {
    int n = it.second.data.textures.size();
    for (int i = 0; i < n; i++) {
      int a = rand() % n;
      int b = rand() % n;
      std::swap(it.second.data.textures[a], it.second.data.textures[b]);
    }
  }
}