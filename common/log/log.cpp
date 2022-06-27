#include "log.h"

#include <cstdio>
#include <cstdlib>
#include <mutex>

#include "third-party/fmt/color.h"
#ifdef _WIN32  // see lg::initialize
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

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
  ASSERT(!gLogger.fp);
  file_util::create_dir_if_needed_for_file(filename);
  gLogger.fp = fopen(filename.c_str(), "w");
  ASSERT(gLogger.fp);
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
  ASSERT(!gLogger.initialized);

#ifdef _WIN32
  // Enable some console terminal flags. Some of these may not be enabled by default, unless
  // the user edits their registry. This is clearly unwanted, so we just set them here ourselves.

  // VIRTUAL_TERMINAL_PROCESSING enables support for ANSI colors in the stdout text, used by the
  // logging tool.
  // QUICK_EDIT_MODE enables various mouse-related stdin functions, such as right-click for
  // copy-paste, scroll wheel to - shocker - scroll, etc.

  // Get handle to stdout
  HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
  // get current stdout mode
  DWORD modeStdOut;
  GetConsoleMode(hStdOut, &modeStdOut);
  // printf("stdout mode is: %08x", modeStdOut);

  // Get handle to stdin
  HANDLE hStdIn = GetStdHandle(STD_INPUT_HANDLE);
  // get current stdin mode
  DWORD modeStdIn;
  GetConsoleMode(hStdIn, &modeStdIn);
  // printf("stdin mode is: %08x", modeStdIn);

  // enable VIRTUAL_TERMINAL_PROCESSING on stdout
  modeStdOut |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  SetConsoleMode(hStdOut, modeStdOut);

  // enable ENABLE_QUICK_EDIT_MODE on stdin
  modeStdIn |= ENABLE_QUICK_EDIT_MODE;
  SetConsoleMode(hStdIn, modeStdIn);
#endif

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
