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

tfrag3::Level* Loader::get_tfrag3_level(const std::string& level_name) {
  std::unique_lock<std::mutex> lk(m_loader_mutex);
  const auto& existing = m_tfrag3_levels.find(level_name);
  if (existing == m_tfrag3_levels.end()) {
    if (m_level_to_load.empty()) {
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
    return existing->second.level.get();
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
    fmt::print("------------> Load from file: {:.3f}s, import {:.3f}s, decomp {:.3f}s\n",
               disk_load_time, import_time, decomp_time);

    lk.lock();
    m_level_to_load = "";
    m_tfrag3_levels[lev].level = std::move(result);
  }
}

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