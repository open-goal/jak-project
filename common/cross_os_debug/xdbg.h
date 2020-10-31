#pragma once

#include <string>
#include <cstdint>

#ifdef __linux
#include <sys/types.h>
#elif _WIN32
// todo - windows includes
#endif

namespace xdbg {
#ifdef __linux

struct ThreadID {
  pid_t id = 0;

  std::string to_string() const;
  explicit ThreadID(const std::string& str);
  explicit ThreadID(pid_t _id);
  ThreadID() = default;
};

#elif _WIN32
struct ThreadID {
  // todo - whatever windows uses to identify a thread
  std::string to_string() const;
  ThreadID(const std::string& str);
  ThreadID();  // todo - add id type here, like in linux version
};
#endif

struct DebugContext {
  ThreadID tid;
  uintptr_t base;
  uint32_t s7;
  bool valid = false;
  bool running = true;
};

// Functions
ThreadID get_current_thread_id();
bool attach_and_break(const ThreadID& tid);
void allow_debugging();
bool detach_and_resume(const ThreadID& tid);

}  // namespace xdbg
