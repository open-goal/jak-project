#include <cstring>
#include "xdbg.h"

#ifdef __linux
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/ptrace.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/wait.h>
#elif _WIN32

#endif

namespace xdbg {
#ifdef __linux

ThreadID::ThreadID(pid_t _id) : id(_id) {}

ThreadID::ThreadID(const std::string& str) {
  id = std::stoi(str);
}

std::string ThreadID::to_string() const {
  return std::to_string(id);
}

ThreadID get_current_thread_id() {
  return ThreadID(syscall(SYS_gettid));
}

bool attach_and_break(const ThreadID& tid) {
  auto rv = ptrace(PTRACE_ATTACH, tid.id, nullptr, nullptr);
  if (rv == -1) {
    printf("[Debugger] Failed to attach %s\n", strerror(errno));
    return false;
  } else {
    printf("[Debugger] PTRACE_ATTACHED! Waiting for process to stop...\n");

    // we could technically hang here forever if runtime ignores the signal.
    int status;
    if (waitpid(tid.id, &status, 0) < 0) {
      printf("[Debugger] Failed to waitpid: %s. The runtime is probably in a bad state now.\n",
             strerror(errno));
      return false;
    }

    if (!WIFSTOPPED(status)) {
      printf("[Debugger] Failed to STOP: %s. The runtime is probably in a bad state now.\n",
             strerror(errno));
      return false;
    }
    return true;
  }
}

void allow_debugging() {
  // modern Linux has "security features" which prevent processes from accessing memory of others.
  // we disable these for the GOAL runtime process so the debugger can connect.
  if (prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY) < 0) {
    printf("[Debugger] Failed to PR_SET_PTRACER %s\n", strerror(errno));
  }
}

bool detach_and_resume(const ThreadID& tid) {
  if (ptrace(PTRACE_DETACH, tid.id, nullptr, nullptr) < 0) {
    printf("[Debugger] Failed to detach: %s\n", strerror(errno));
    return false;
  }
  return true;
}

#elif _WIN32

ThreadID::ThreadID() {}  // todo

ThreadID::ThreadID(const std::string& str) {
  // todo
}

std::string ThreadID::to_string() const {
  // todo
  return "0";
}

ThreadID get_current_thread_id() {
  // todo
  return {};
}

bool attach_and_break(const ThreadID& tid) {
  return false;
}

bool detach_and_resume(const ThreadID& tid) {
  return false;
}

void allow_debugging() {}

#endif
}  // namespace xdbg
