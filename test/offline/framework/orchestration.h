#pragma once

#include <future>
#include <mutex>
#include <set>
#include <string>
#include <vector>

#include "file_management.h"

#include "common/util/Timer.h"

#include "test/offline/config/config.h"

struct OfflineTestCompareResult {
  struct Fail {
    std::string filename;
    std::string diff;
  };
  std::vector<Fail> failing_files;
  int total_files = 0;
  int ok_files = 0;
  int total_lines = 0;
  bool total_pass = true;

  void add(const OfflineTestCompareResult& other) {
    failing_files.insert(failing_files.end(), other.failing_files.begin(),
                         other.failing_files.end());
    total_files += other.total_files;
    ok_files += other.ok_files;
    total_lines += other.total_lines;
    if (!other.total_pass) {
      total_pass = false;
    }
  }
};

struct OfflineTestCompileResult {
  bool ok = true;
  struct Fail {
    std::string filename;
    std::string error;
  };
  std::vector<Fail> failing_files;
  int num_lines = 0;
  void add(const OfflineTestCompileResult& other) {
    failing_files.insert(failing_files.end(), other.failing_files.begin(),
                         other.failing_files.end());
    num_lines += other.num_lines;
    if (!other.ok) {
      ok = false;
    }
  }
};

/// @brief A simple struct to contain the reason for failure from a thread
struct OfflineTestThreadResult {
  int exit_code = 0;
  std::string reason;

  float time_spent_compiling = 0;
  float time_spent_decompiling = 0;
  float total_time = 0;

  OfflineTestCompareResult compare;
  OfflineTestCompileResult compile;

  void add(const OfflineTestThreadResult& other) {
    if (other.exit_code) {
      exit_code = other.exit_code;
    }
    time_spent_compiling += other.time_spent_compiling;
    time_spent_decompiling += other.time_spent_decompiling;
    total_time += other.total_time;
    compare.add(other.compare);
    compile.add(other.compile);
  }
};

class OfflineTestThreadStatus {
 public:
  enum class Stage { IDLE, PREPARING, DECOMPILING, COMPARING, COMPILING, FAILED, FINISHED };

  OfflineTestThreadStatus(const OfflineTestConfig& _config) : config(_config){};

  Stage stage = Stage::IDLE;
  uint32_t total_steps = 0;
  uint32_t curr_step = 0;
  std::set<std::string> dgos;
  std::string curr_file;
  OfflineTestConfig config;

  void update_stage(Stage new_stage);
  void update_curr_file(const std::string& _curr_file);
  void complete_step();
  bool in_progress();
};

struct OfflineTestWorkCollection {
  std::vector<OfflineTestSourceFile> source_files;
};

struct OfflineTestWorkGroup {
  std::set<std::string> dgo_set;
  OfflineTestWorkCollection work_collection;
  std::shared_ptr<OfflineTestThreadStatus> status;

  int work_size() const { return work_collection.source_files.size(); }
};

class OfflineTestThreadManager {
 public:
  std::vector<std::shared_ptr<OfflineTestThreadStatus>> statuses = {};

  int num_threads_pending();
  int num_threads_succeeded();
  int num_threads_failed();

  void print_current_test_status(const OfflineTestConfig& config);

 private:
  std::mutex print_lock;
};

extern OfflineTestThreadManager g_offline_test_thread_manager;

std::vector<std::future<OfflineTestThreadResult>> distribute_work(
    const OfflineTestConfig& offline_config,
    const std::vector<OfflineTestSourceFile>& files);
