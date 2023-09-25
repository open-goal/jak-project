#pragma once

#include <ctime>

#ifdef __linux__
#include <sys/time.h>
#endif
#include <string>

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"

namespace lg {

#ifdef __linux__
struct LogTime {
  timeval tv;
};
#else
struct LogTime {
  time_t tim;
};
#endif

// Logging API
enum class level {
  trace = 0,
  debug = 1,
  info = 2,
  warn = 3,
  error = 4,
  die = 5,
  off_unless_die = 6,
  off = 7
};

namespace internal {
// log implementation stuff, not to be called by the user
void log_message(level log_level, LogTime& now, const char* message);
void log_print(const char* message);
}  // namespace internal

void set_file(const std::string& filename,
              const bool should_rotate = true,
              const bool append = false,
              const std::string& dir = "");
void set_flush_level(level log_level);
void set_file_level(level log_level);
void set_stdout_level(level log_level);
void set_max_debug_levels();
void disable_ansi_colors();
void initialize();
void finish();

template <typename... Args>
void log(level log_level, const std::string& format, Args&&... args) {
  LogTime now;
#ifdef __linux__
  gettimeofday(&now.tv, nullptr);
#else
  now.tim = time(nullptr);
#endif
  std::string formatted_message = fmt::format(format, std::forward<Args>(args)...);
  internal::log_message(log_level, now, formatted_message.c_str());
}

template <typename... Args>
void print(const std::string& format, Args&&... args) {
  std::string formatted_message = fmt::format(format, std::forward<Args>(args)...);
  internal::log_print(formatted_message.c_str());
}
template <typename... Args>
void print(const fmt::text_style& ts, const std::string& format, Args&&... args) {
  std::string formatted_message = fmt::format(ts, format, std::forward<Args>(args)...);
  internal::log_print(formatted_message.c_str());
}

template <typename... Args>
void trace(const std::string& format, Args&&... args) {
  log(level::trace, format, std::forward<Args>(args)...);
}

template <typename... Args>
void debug(const std::string& format, Args&&... args) {
  log(level::debug, format, std::forward<Args>(args)...);
}

template <typename... Args>
void info(const std::string& format, Args&&... args) {
  log(level::info, format, std::forward<Args>(args)...);
}

template <typename... Args>
void warn(const std::string& format, Args&&... args) {
  log(level::warn, format, std::forward<Args>(args)...);
}

template <typename... Args>
void error(const std::string& format, Args&&... args) {
  log(level::error, format, std::forward<Args>(args)...);
}

template <typename... Args>
void die(const std::string& format, Args&&... args) {
  log(level::die, format, std::forward<Args>(args)...);
}
}  // namespace lg
