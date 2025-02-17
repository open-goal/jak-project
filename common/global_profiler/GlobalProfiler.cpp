// clang-format off
#include "GlobalProfiler.h"

#include <chrono>
#include <cstring>
#include <thread>

#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/common_types.h"

#include "fmt/core.h"
#include "third-party/json.hpp"

#ifdef OS_POSIX
u64 get_current_tid() {
  return (u64)pthread_self();
}
#else
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "Processthreadsapi.h"
u64 get_current_tid() {
  return (u64)GetCurrentThreadId();
}
#endif
#include "common/log/log.h"
#include "common/util/compress.h"
#include "common/util/string_util.h"
// clang-format on

u64 get_current_ts() {
  return std::chrono::steady_clock::now().time_since_epoch().count();
}

GlobalProfiler::GlobalProfiler() {
  m_t0 = get_current_ts();
  m_nodes.resize(m_max_events);
}

void GlobalProfiler::update_event_buffer_size(size_t new_size) {
  m_max_events = new_size;
  m_nodes.resize(m_max_events);
}

void GlobalProfiler::set_waiting_for_event(const std::string& event_name) {
  if (!event_name.empty()) {
    m_waiting_for_event = event_name;
  }
}

void GlobalProfiler::event(const char* name, ProfNode::Kind kind) {
  if (m_waiting_for_event && m_waiting_for_event.value() == name) {
    m_ignore_events = true;
  }
  if (!m_enabled || m_ignore_events) {
    return;
  }
  size_t my_idx = (m_next_idx++ % m_nodes.size());
  auto& node = m_nodes[my_idx];
  node.ts = get_current_ts() - m_t0;
  node.tid = get_current_tid();
  node.kind = kind;
  strncpy(node.name, name, sizeof(node.name));
  node.name[sizeof(node.name) - 1] = '\0';
}

void GlobalProfiler::instant_event(const char* name) {
  event(name, ProfNode::INSTANT);
}

void GlobalProfiler::root_event() {
  instant_event("ROOT");
}

size_t GlobalProfiler::get_next_idx() {
  return (m_next_idx % m_nodes.size());
}

void GlobalProfiler::begin_event(const char* name) {
  event(name, ProfNode::BEGIN);
}

void GlobalProfiler::end_event() {
  if (!m_enabled || m_ignore_events) {
    return;
  }
  size_t my_idx = (m_next_idx++ % m_nodes.size());
  auto& node = m_nodes[my_idx];
  node.ts = get_current_ts() - m_t0;
  node.tid = get_current_tid();
  node.kind = ProfNode::END;
  node.name[0] = '\0';
}

void GlobalProfiler::clear() {
  m_next_idx = 0;
}

void GlobalProfiler::set_enable(bool en) {
  m_enabled = en;
}

void GlobalProfiler::dump_to_json() {
  if (m_enabled) {
    set_enable(false);
  }

  nlohmann::json json;
  auto& trace_events = json["traceEvents"];
  json["displayTimeUnit"] = "ms";

  u64 lowest_ts = UINT64_MAX;
  struct ThreadInfo {
    size_t lowest_at_target = UINT64_MAX;
    size_t highest_at_target = 0;
    size_t debug = 0;
    u32 short_id = 0;
  };
  std::unordered_map<u32, ThreadInfo> info_per_thread;

  // first, find all the threads
  std::string kRootName = "ROOT";
  for (size_t offset = m_nodes.size(); offset-- > 0;) {
    size_t idx = (m_next_idx + offset) % m_nodes.size();
    const auto& event = m_nodes[idx];

    if (event.kind != ProfNode::UNUSED) {
      lowest_ts = std::min(event.ts, lowest_ts);
    }
    if (event.kind == ProfNode::INSTANT && kRootName == event.name) {
      auto& info = info_per_thread[event.tid];
      info.lowest_at_target = std::min(info.lowest_at_target, offset);
      info.highest_at_target = std::max(info.highest_at_target, offset);
    }
  }

  u32 i = 0;
  for (auto& info : info_per_thread) {
    info.second.short_id = i++;
  }

  for (size_t event_idx = 0; event_idx < m_nodes.size(); event_idx++) {
    auto& event = m_nodes[(event_idx + m_next_idx) % m_nodes.size()];
    if (event.kind == ProfNode::UNUSED) {
      continue;
    }
    auto& info = info_per_thread.at(event.tid);
    if (event_idx < info.lowest_at_target || event_idx > info.highest_at_target) {
      continue;
    }

    auto& json_event = trace_events.emplace_back();
    // name
    if (event.kind != ProfNode::END) {
      json_event["name"] = event.name;
    }

    // cat
    // json_event["cat"] = "a";  // ??
    // ph BEi
    switch (event.kind) {
      case ProfNode::END:
        json_event["ph"] = "E";
        break;
      case ProfNode::BEGIN:
        json_event["ph"] = "B";
        break;
      case ProfNode::INSTANT:
        json_event["ph"] = "i";
        break;
      default:
        ASSERT(false);
    }
    // pid
    json_event["pid"] = 1;
    // tid
    json_event["tid"] = info.short_id;
    // ts
    json_event["ts"] = (event.ts - lowest_ts) / 1000.f;
    if (event.ts < info.debug) {
      lg::debug("out of order: {} {} {} ms", event.ts / 1000.f, info.debug / 1000.f,
                (info.debug - event.ts) / 1000000.f);
      lg::debug("  idx: {}, range {} {}", event_idx, info.lowest_at_target, info.highest_at_target);
      lg::debug("  now: {}", m_next_idx.load());
    }
    info.debug = event.ts;
  }

  for (auto& t : info_per_thread) {
    lg::debug("thread: {}: {} -> {}", t.first, t.second.lowest_at_target,
              t.second.highest_at_target);
  }

  if (m_enable_compression) {
    const auto json_str = json.dump();
    const auto compressed_data =
        compression::compress_zstd_no_header(json_str.data(), json_str.size());
    auto file_path = file_util::get_jak_project_dir() / "profile_data" /
                     fmt::format("prof-{}.json.zst", str_util::current_local_timestamp_no_colons());
    file_util::create_dir_if_needed_for_file(file_path);
    file_util::write_binary_file(file_path, compressed_data.data(), compressed_data.size());
  } else {
    auto file_path = file_util::get_jak_project_dir() / "profile_data" /
                     fmt::format("prof-{}.json", str_util::current_local_timestamp_no_colons());
    file_util::create_dir_if_needed_for_file(file_path);
    file_util::write_text_file(file_path, json.dump());
  }
}

GlobalProfiler gprof;
GlobalProfiler& prof() {
  return gprof;
}

ScopedEvent scoped_prof(const char* name) {
  auto& p = prof();
  p.begin_event(name);
  return {&p};
}
