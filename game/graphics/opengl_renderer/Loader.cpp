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
    if (m_level_to_load.empty() && m_initializing_tfrag3_levels.count(level_name) == 0) {
      fmt::print("[pc loader] starting load for {}\n", level_name);
      m_level_to_load = level_name;
      lk.unlock();
      m_loader_cv.notify_all();
      return nullptr;
    } else {
      return nullptr;
    }
  } else {
    existing->second.frames_since_last_used = 0;
    return &existing->second.data;
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

Loader::Loader() {
  m_loader_thread = std::thread(&Loader::loader_thread, this);
}

void Loader::update() {
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
      constexpr int MAX_TEX_BYTES_PER_FRAME = 1024 * 1024;
      auto& data = it->second.data;

      int bytes_this_run = 0;
      while (data.textures.size() < data.level->textures.size()) {
        auto& tex = data.level->textures[data.textures.size()];
        GLuint gl_tex;
        glGenTextures(1, &gl_tex);
        glBindTexture(GL_TEXTURE_2D, gl_tex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex.w, tex.h, 0, GL_RGBA,
                     GL_UNSIGNED_INT_8_8_8_8_REV, tex.data.data());
        glBindTexture(GL_TEXTURE_2D, 0);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, gl_tex);
        glGenerateMipmap(GL_TEXTURE_2D);

        float aniso = 0.0f;
        glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY, &aniso);
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, aniso);
        it->second.data.textures.push_back(gl_tex);
        bytes_this_run += tex.w * tex.h * 4;
        if (bytes_this_run > MAX_TEX_BYTES_PER_FRAME) {
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
          fmt::print("------------------------- PC unloading {}\n", lev.first);
          for (auto tex : lev.second.data.textures) {
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