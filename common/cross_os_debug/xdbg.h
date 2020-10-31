#pragma once

#include <string>
#include <cstdint>
#include "common/common_types.h"

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
};

struct Regs {
  u64 gprs[16];
  u128 xmms[16];

  u64 rip;

  std::string print_gprs() const;
  std::string print_xmms_as_flt() const;
  std::string print_xmms_as_int() const;
  std::string print_xmms_as_flt_vec() const;
};

// Functions
ThreadID get_current_thread_id();
bool attach_and_break(const ThreadID& tid);
void allow_debugging();
bool detach_and_resume(const ThreadID& tid);
bool get_regs_now(const ThreadID& tid, Regs* out);
bool break_now(const ThreadID& tid);
bool cont_now(const ThreadID& tid);

}  // namespace xdbg
