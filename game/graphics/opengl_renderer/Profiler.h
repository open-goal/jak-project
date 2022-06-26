#pragma once

#include <array>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/global_profiler/GlobalProfiler.h"
#include "common/util/Timer.h"

#include "game/graphics/opengl_renderer/buckets.h"

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
  float get_elapsed_time() const { return m_timer.getSeconds(); }
  const ProfilerStats& stats() const { return m_stats; }

 private:
  friend class Profiler;
  void to_string_helper(std::string& str, int depth) const;

  std::string m_name;
  ProfilerStats m_stats;
  std::vector<ProfilerNode> m_children;
  Timer m_timer;
  bool m_finished = false;
};

class ScopedProfilerNode {
 public:
  ScopedProfilerNode(ProfilerNode* node)
      : m_node(node), m_global_event(scoped_prof(node->name().c_str())) {}
  ScopedProfilerNode(const ScopedProfilerNode& other) = delete;
  ScopedProfilerNode& operator=(const ScopedProfilerNode& other) = delete;
  ProfilerNode* make_child(const std::string& name) { return m_node->make_child(name); }
  ScopedProfilerNode make_scoped_child(const std::string& name) {
    return m_node->make_scoped_child(name);
  }
  ~ScopedProfilerNode() { m_node->finish(); }

  void add_draw_call(int count = 1) { m_node->add_draw_call(count); }
  void add_tri(int count = 1) { m_node->add_tri(count); }
  float get_elapsed_time() const { return m_node->get_elapsed_time(); }

 private:
  ProfilerNode* m_node;
  ScopedEvent m_global_event;
};

class Profiler {
 public:
  Profiler();
  void clear();
  void draw();
  void finish();

  float root_time() const { return m_root.m_stats.duration; }

  std::string to_string();
  ProfilerNode* root() { return &m_root; }

 private:
  void draw_node(ProfilerNode& node, bool expand, int depth, float start_time);

  struct BarEntry {
    float duration;
    float rgba[4];
  };

  int m_mode_selector = 0;
  ProfilerNode m_root;
};

class FramePlot {
 public:
  void push(float val);
  void draw(float max);

 private:
  static constexpr int SIZE = 60 * 5;
  float m_buffer[SIZE] = {};
  int m_idx = 0;
};

struct SmallProfilerStats {
  int triangles, draw_calls;
  float time_per_category[(int)BucketCategory::MAX_CATEGORIES];
};

class SmallProfiler {
 public:
  void draw(const std::string& load_status, const SmallProfilerStats& stats);

 private:
  std::array<FramePlot, (int)BucketCategory::MAX_CATEGORIES> m_plots;
};