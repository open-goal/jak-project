#include <cstring>
#include "third-party/fmt/core.h"
#include "xdbg.h"

#ifdef __linux
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/ptrace.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/user.h>
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
  auto rv = ptrace(PTRACE_SEIZE, tid.id, nullptr, nullptr);
  if (rv == -1) {
    printf("[Debugger] Failed to attach %s\n", strerror(errno));
    return false;
  } else {
    printf("[Debugger] PTRACE_ATTACHED! Waiting for process to stop...\n");
    if (ptrace(PTRACE_INTERRUPT, tid.id, nullptr, nullptr) < 0) {
      printf("[Debugger] Failed to PTRACE_INTERRUPT %s\n", strerror(errno));
      return false;
    }

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

bool get_regs_now(const ThreadID& tid, Regs* out) {
  user regs = {};
  if (ptrace(PTRACE_GETREGS, tid.id, nullptr, &regs) < 0) {
    printf("[Debugger] Failed to PTRACE_GETREGS %s\n", strerror(errno));
    return false;
  }

  out->gprs[0] = regs.regs.rax;
  out->gprs[1] = regs.regs.rcx;
  out->gprs[2] = regs.regs.rdx;
  out->gprs[3] = regs.regs.rbx;
  out->gprs[4] = regs.regs.rsp;
  out->gprs[5] = regs.regs.rbp;
  out->gprs[6] = regs.regs.rsi;
  out->gprs[7] = regs.regs.rdi;
  out->gprs[8] = regs.regs.r8;
  out->gprs[9] = regs.regs.r9;
  out->gprs[10] = regs.regs.r10;
  out->gprs[11] = regs.regs.r11;
  out->gprs[12] = regs.regs.r12;
  out->gprs[13] = regs.regs.r13;
  out->gprs[14] = regs.regs.r14;
  out->gprs[15] = regs.regs.r15;
  out->rip = regs.regs.rip;

  // todo, get fprs.
  return true;
}

bool break_now(const ThreadID& tid) {
  if (ptrace(PTRACE_INTERRUPT, tid.id, nullptr, nullptr) < 0) {
    printf("[Debugger] Failed to PTRACE_INTERRUPT %s\n", strerror(errno));
    return false;
  }

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

bool cont_now(const ThreadID& tid) {
  if (ptrace(PTRACE_CONT, tid.id, nullptr, nullptr) < 0) {
    printf("[Debugger] Failed to PTRACE_CONT %s\n", strerror(errno));
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

bool break_now(const ThreadID& tid) {
  return false;
}

bool cont_now(const ThreadID& tid) {
  return false;
}

bool get_regs_now(const ThreadID& tid, Regs* out) {
  return false;
}

#endif

const char* gpr_names[] = {"rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
                           " r8", " r9", "r10", "r11", "r12", "r13", "r14", "r15"};

std::string Regs::print_gprs() const {
  std::string result;
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      int idx = i * 4 + j;
      result += fmt::format("{}: 0x{:016x} ", gpr_names[idx], gprs[idx]);
    }
    result += "\n";
  }
  result += fmt::format("rip: 0x{:016x}\n", rip);
  return result;
}
}  // namespace xdbg
