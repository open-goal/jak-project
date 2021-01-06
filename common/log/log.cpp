#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <mutex>
#include "third-party/fmt/color.h"
#include "log.h"

namespace lg {
struct Logger {
  Logger() = default;

  bool initialized = false;
  FILE* fp = nullptr;
  level stdout_log_level = level::trace;
  level file_log_level = level::trace;
  level flush_level = level::trace;
  std::mutex mutex;

  ~Logger() {
    // will run when program exits.
    if (fp) {
      fclose(fp);
    }
  }
};

Logger gLogger;

namespace internal {
const char* log_level_names[] = {"trace", "debug", "info", "warn", "error", "die"};
const fmt::color log_colors[] = {fmt::color::gray,   fmt::color::turquoise, fmt::color::light_green,
                                 fmt::color::yellow, fmt::color::red,       fmt::color::hot_pink};

void log_message(level log_level, LogTime& now, const char* message) {
#ifdef __linux__
  char date_time_buffer[128];
  time_t now_seconds = now.tv.tv_sec;
  auto now_milliseconds = now.tv.tv_usec / 1000;
  strftime(date_time_buffer, 128, "%Y-%m-%d %H:%M:%S", localtime(&now_seconds));
  std::string date_string = fmt::format("[{}:{:03d}]", date_time_buffer, now_milliseconds);
#else
  char date_time_buffer[128];
  strftime(date_time_buffer, 128, "%Y-%m-%d %H:%M:%S", localtime(&now.tim));
  std::string date_string = fmt::format("[{}]", date_time_buffer);
#endif

  {
    std::lock_guard<std::mutex> lock(gLogger.mutex);
    if (gLogger.fp && log_level >= gLogger.file_log_level) {
      // log to file
      std::string file_string =
          fmt::format("{} [{}] {}\n", date_string, log_level_names[int(log_level)], message);
      fwrite(file_string.c_str(), file_string.length(), 1, gLogger.fp);
      if (log_level >= gLogger.flush_level) {
        fflush(gLogger.fp);
      }
    }

    if (log_level >= gLogger.stdout_log_level) {
      fmt::print("{} [", date_string);
      fmt::print(fg(log_colors[int(log_level)]), "{}", log_level_names[int(log_level)]);
      fmt::print("] {}\n", message);
      if (log_level >= gLogger.flush_level) {
        fflush(stdout);
      }
    }
  }

  if (log_level == level::die) {
    exit(-1);
  }
}
}  // namespace internal

void set_file(const std::string& filename) {
  assert(!gLogger.fp);
  gLogger.fp = fopen(filename.c_str(), "w");
  assert(gLogger.fp);
}

void set_flush_level(level log_level) {
  gLogger.flush_level = log_level;
}

void set_file_level(level log_level) {
  gLogger.file_log_level = log_level;
}

void set_stdout_level(level log_level) {
  gLogger.stdout_log_level = log_level;
}

void set_max_debug_levels() {
  gLogger.flush_level = level::trace;
  gLogger.stdout_log_level = level::trace;
  gLogger.file_log_level = level::trace;
}

void initialize() {
  assert(!gLogger.initialized);
  gLogger.initialized = true;
}

void finish() {
  {
    std::lock_guard<std::mutex> lock(gLogger.mutex);
    if (gLogger.fp) {
      fclose(gLogger.fp);
      gLogger.fp = nullptr;
    }
  }
}

}  // namespace lg