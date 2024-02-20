#pragma once

#include <atomic>
#include <optional>
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
  void set_waiting_for_event(const std::string& event_name);
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
  // this is very niche, but sometimes you want to capture up to a given event (ie. long startup)
  // instead of having to make the user quit and record as fast as possible, we can instead just
  // stop capturing events once we have received what we are looking for
  std::optional<std::string> m_waiting_for_event = {};
  bool m_ignore_events = false;
};

struct ScopedEvent {
  ScopedEvent(GlobalProfiler* _prof) : prof(_prof){};
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
