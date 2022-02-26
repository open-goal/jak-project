#pragma once

#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/util/Timer.h"

enum class ProfilerSort { NONE = 0, TIME = 1, DRAW_CALLS = 2, TRIANGLES = 3 };

struct ProfilerStats {
  float duration = 0;  // seconds
  u32 draw_calls = 0;
  u32 triangles = 0;

  void add_draw_stats(const ProfilerStats& other) {
    draw_calls += other.draw_calls;
    triangles += other.triangles;
  }
};

class ScopedProfilerNode;

class ProfilerNode {
 public:
  ProfilerNode(const std::string& name);
  ProfilerNode* make_child(const std::string& name);
  ScopedProfilerNode make_scoped_child(const std::string& name);
  void sort(ProfilerSort mode);
  void finish();

  bool finished() const { return m_finished; }
  const std::string& name() const { return m_name; }

  void add_draw_call(int count = 1) { m_stats.draw_calls += count; }
  void add_tri(int count = 1) { m_stats.triangles += count; }

 private:
  friend class Profiler;
  std::string m_name;
  ProfilerStats m_stats;
  std::vector<ProfilerNode> m_children;
  Timer m_timer;
  bool m_finished = false;
};

class ScopedProfilerNode {
 public:
  ScopedProfilerNode(ProfilerNode* node) : m_node(node) {}
  ScopedProfilerNode(const ScopedProfilerNode& other) = delete;
  ScopedProfilerNode& operator=(const ScopedProfilerNode& other) = delete;
  ProfilerNode* make_child(const std::string& name) { return m_node->make_child(name); }
  ScopedProfilerNode make_scoped_child(const std::string& name) {
    return m_node->make_scoped_child(name);
  }
  ~ScopedProfilerNode() { m_node->finish(); }

  void add_draw_call(int count = 1) { m_node->add_draw_call(count); }
  void add_tri(int count = 1) { m_node->add_tri(count); }

 private:
  ProfilerNode* m_node;
};

class Profiler {
 public:
  Profiler();
  void clear();
  void draw();
  void draw_small_window();
  void finish();
  ProfilerNode* root() { return &m_root; }

 private:
  void draw_node(ProfilerNode& node, bool expand, int depth, float start_time);

  struct BarEntry {
    float duration;
    float rgba[4];
  };

  int m_mode_selector = 0;
  bool m_small_window_open = true;
  ProfilerNode m_root;
};