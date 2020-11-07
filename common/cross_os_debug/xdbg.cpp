/*!
 * @file xdbg.cpp
 * Debugging utility library. This hides the platform specific details of the debugger.
 * Nothing in here should hold state, that should all be managed in Debugger.
 */

#include <cstring>
#include "common/goal_constants.h"
#include "common/util/Timer.h"
#include "third-party/fmt/core.h"
#include "xdbg.h"

#ifdef __linux
#include <unistd.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/ptrace.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <fcntl.h>
#elif _WIN32

#endif

namespace xdbg {
#ifdef __linux

/*!
 * In Linux, a ThreadID is just the pid_t of the thread.
 */
ThreadID::ThreadID(pid_t _id) : id(_id) {}

/*!
 * In Linux, the string representation of a ThreadID is just the number printed in base 10
 */
ThreadID::ThreadID(const std::string& str) {
  id = std::stoi(str);
}

std::string ThreadID::to_string() const {
  return std::to_string(id);
}

/*!
 * Get the ThreadID of whatever called this function.
 * The runtime calls this to get the Thread to be debugged.
 */
ThreadID get_current_thread_id() {
  return ThreadID(syscall(SYS_gettid));
}

/*!
 * Called by the target to do any setup required for the debugger to attach (allowing tracing)
 * Will be called from the GOAL thread.
 */
void allow_debugging() {
  // modern Linux has "security features" which prevent processes from accessing memory of others.
  // we disable these for the GOAL runtime process so the debugger can connect.
  if (prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY) < 0) {
    printf("[Debugger] Failed to PR_SET_PTRACER %s\n", strerror(errno));
  }
}

/*!
 * Attach to the given thread ID and halt it.
 */
bool attach_and_break(const ThreadID& tid) {
  // SEIZE attaches without halting, but is required to use PTRACE_INTERRUPT in the future.
  auto rv = ptrace(PTRACE_SEIZE, tid.id, nullptr, nullptr);
  if (rv == -1) {
    printf("[Debugger] Failed to attach %s\n", strerror(errno));
    return false;
  } else {
    // we attached, now send break
    printf("[Debugger] PTRACE_ATTACHED! Waiting for process to stop...\n");
    if (ptrace(PTRACE_INTERRUPT, tid.id, nullptr, nullptr) < 0) {
      printf("[Debugger] Failed to PTRACE_INTERRUPT %s\n", strerror(errno));
      return false;
    }

    return true;
  }
}

/*!
 * Has the given thread transitioned from running to stopped?
 * If the thread has transitioned to stop, check_stopped should only return true once.
 * If true, populates out with information about why it stopped.
 * This shouldn't hang if the thread doesn't stop.
 */
bool check_stopped(const ThreadID& tid, SignalInfo* out) {
  int status;
  if (waitpid(tid.id, &status, WNOHANG) < 0) {
    printf("[Debugger] Failed to waitpid: %s.\n", strerror(errno));
    //    assert(false);  // todo, temp because I think we should never hit this.
    return false;
  }

  if (WIFSTOPPED(status)) {
    auto sig = WSTOPSIG(status);
    if (out) {
      switch (sig) {
        case SIGSEGV:
          out->kind = SignalInfo::SEGFAULT;
          break;
        case SIGFPE:
          out->kind = SignalInfo::MATH_EXCEPTION;
          break;
        case SIGTRAP:
          out->kind = SignalInfo::BREAK;
          break;

        default:
          out->kind = SignalInfo::UNKNOWN;
      }
    }

    return true;
  }

  return false;
}

/*!
 * Open memory of target. Assumes we are already connected and halted.
 * If successful returns true and populates out with a "handle" to the memory.
 */
bool open_memory(const ThreadID& tid, MemoryHandle* out) {
  int fd = open(fmt::format("/proc/{}/mem", tid.id).c_str(), O_RDWR);
  if (fd < -1) {
    printf("[Debugger] Failed to open memory: %s.\n", strerror(errno));
    return false;
  }
  out->fd = fd;
  return true;
}

/*!
 * Close memory of target.
 */
bool close_memory(const ThreadID& tid, MemoryHandle* handle) {
  (void)tid;
  if (close(handle->fd) < 0) {
    printf("[Debugger] Failed to close memory: %s.\n", strerror(errno));
    return false;
  }
  return true;
}

/*!
 * Read data from target's EE memory
 */
bool read_goal_memory(u8* dest_buffer,
                      int size,
                      u32 goal_addr,
                      const DebugContext& context,
                      const MemoryHandle& mem) {
  if (pread(mem.fd, dest_buffer, size, context.base + goal_addr) != size) {
    printf("[Debugger] Failed to read memory: %s.\n", strerror(errno));
    return false;
  }
  return true;
}

/*!
 * Write data into target's EE memory
 */
bool write_goal_memory(const u8* src_buffer,
                       int size,
                       u32 goal_addr,
                       const DebugContext& context,
                       const MemoryHandle& mem) {
  if (pwrite(mem.fd, src_buffer, size, context.base + goal_addr) != size) {
    printf("[Debugger] Failed to write memory: %s.\n", strerror(errno));
    return false;
  }
  return true;
}

/*!
 * Detach from the given thread and resume it if it's halted.
 */
bool detach_and_resume(const ThreadID& tid) {
  if (ptrace(PTRACE_DETACH, tid.id, nullptr, nullptr) < 0) {
    printf("[Debugger] Failed to detach: %s\n", strerror(errno));
    return false;
  }
  return true;
}

/*!
 * Get all registers now. Must be attached and stopped
 */
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

/*!
 * Set all registers now. Must be attached and stopped
 */
bool set_regs_now(const ThreadID& tid, const Regs& out) {
  user regs = {};
  if (ptrace(PTRACE_GETREGS, tid.id, nullptr, &regs) < 0) {
    printf("[Debugger] Failed to PTRACE_GETREGS %s\n", strerror(errno));
    return false;
  }

  regs.regs.rax = out.gprs[0];
  regs.regs.rcx = out.gprs[1];
  regs.regs.rdx = out.gprs[2];
  regs.regs.rbx = out.gprs[3];
  regs.regs.rsp = out.gprs[4];
  regs.regs.rbp = out.gprs[5];
  regs.regs.rsi = out.gprs[6];
  regs.regs.rdi = out.gprs[7];
  regs.regs.r8 = out.gprs[8];
  regs.regs.r9 = out.gprs[9];
  regs.regs.r10 = out.gprs[10];
  regs.regs.r11 = out.gprs[11];
  regs.regs.r12 = out.gprs[12];
  regs.regs.r13 = out.gprs[13];
  regs.regs.r14 = out.gprs[14];
  regs.regs.r15 = out.gprs[15];
  regs.regs.rip = out.rip;

  if (ptrace(PTRACE_SETREGS, tid.id, nullptr, &regs) < 0) {
    printf("[Debugger] Failed to PTRACE_SETREGS %s\n", strerror(errno));
    return false;
  }
  // todo, set fprs.
  return true;
}

/*!
 * Break the given thread.  Must be attached and running.
 * Does not wait for the thread to stop.
 * Eventually check_stop should return true with a reason of BREAK, unless the target gets really
 * lucky and manages to crash before the SIGTRAP reaches the target
 */
bool break_now(const ThreadID& tid) {
  if (ptrace(PTRACE_INTERRUPT, tid.id, nullptr, nullptr) < 0) {
    printf("[Debugger] Failed to PTRACE_INTERRUPT %s\n", strerror(errno));
    return false;
  }

  return true;
}

/*!
 * Continue the given thread. Must be attached and not running.
 */
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

bool open_memory(const ThreadID& tid, MemoryHandle* out) {
  return false;
}

bool close_memory(const ThreadID& tid, MemoryHandle* handle) {
  return false;
}

bool read_goal_memory(u8* dest_buffer,
                      int size,
                      u32 goal_addr,
                      const DebugContext& context,
                      const MemoryHandle& mem) {
  return false;
}

bool write_goal_memory(const u8* src_buffer,
                       int size,
                       u32 goal_addr,
                       const DebugContext& context,
                       const MemoryHandle& mem) {
  return false;
}

bool check_stopped(const ThreadID& tid, SignalInfo* out) {
  return false;
}

bool set_regs_now(const ThreadID& tid, const Regs& out) {
  return false;
}
#endif

const char* gpr_names[] = {"rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
                           " r8", " r9", "r10", "r11", "r12", "r13", "r14", "r15"};

/*!
 * Print GPR register values, including rip.
 * Splits into 5 lines.
 */
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
