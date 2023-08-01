#pragma once

#include <atomic>
#include <string>
#include <vector>

#include "common/common_types.h"

struct ProfNode {
  u64 ts;
  u64 tid;
  char name[128];
  enum Kind : u8 { BEGIN, END, INSTANT, UNUSED } kind = UNUSED;
};

class GlobalProfiler {
 public:
  GlobalProfiler();
  void set_max_events(size_t event_count);
  void instant_event(const char* name);
  void begin_event(const char* name);
  void event(const char* name, ProfNode::Kind kind);
  void end_event();
  void clear();
  void set_enable(bool en);
  void dump_to_json(const std::string& path);
  void root_event();

 private:
  std::atomic_bool m_enabled = false;
  u64 m_t0 = 0;
  std::atomic_size_t m_next_idx = 0;
  std::vector<ProfNode> m_nodes;
};

struct ScopedEvent {
  ScopedEvent(const ScopedEvent&) = delete;
  ScopedEvent& operator=(const ScopedEvent&) = delete;
  GlobalProfiler* prof = nullptr;
  ~ScopedEvent() {
    if (prof) {
      prof->end_event();
    }
  }
};

GlobalProfiler& prof();
ScopedEvent scoped_prof(const char* name);
