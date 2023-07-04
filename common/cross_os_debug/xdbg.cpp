/*!
 * @file xdbg.cpp
 * Debugging utility library. This hides the platform specific details of the debugger.
 * Nothing in here should hold state, that should all be managed in Debugger.
 */

#include "xdbg.h"

#include <cstring>

#include "common/goal_constants.h"
#include "common/util/Timer.h"

#include "third-party/fmt/core.h"

#ifdef __linux
#include <fcntl.h>
#include <unistd.h>

#include <sys/prctl.h>
#include <sys/ptrace.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>
#elif _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <condition_variable>
#include <mutex>
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
ThreadID::ThreadID(const std::string& str) : id(std::stoi(str)) {}

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
  int rv = waitpid(tid.id, &status, WNOHANG);
  if (rv < 0) {
    if (errno == ECHILD) {
      // the thing died.
      out->kind = SignalInfo::DISAPPEARED;
      return true;
    }
    printf("[Debugger] Failed to waitpid: %s.\n", strerror(errno));
    //    ASSERT(false);  // todo, temp because I think we should never hit this.
    return false;
  }

  if (rv > 0) {
    // status has actually changed
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
          case SIGILL:
            out->kind = SignalInfo::ILLEGAL_INSTR;
            break;

          default:
            out->kind = SignalInfo::UNKNOWN;
        }
      }
      return true;
    }
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

ThreadID::ThreadID(DWORD _pid, DWORD _tid) : pid(_pid), tid(_tid) {}

ThreadID::ThreadID(const std::string& str) {
  auto sep = str.find('-');
  pid = std::stoi(str.substr(0, sep));
  tid = std::stoi(str.substr(sep + 1));
}

std::string ThreadID::to_string() const {
  return fmt::format("{}-{}", pid, tid);
}

ThreadID get_current_thread_id() {
  return ThreadID(GetCurrentProcessId(), GetCurrentThreadId());
}

void win_print_last_error(const std::string& msg) {
  LPSTR lpMsgBuf;

  FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&lpMsgBuf, 0, NULL);

  printf("[Debugger] %s Win Err: %s", msg.c_str(), lpMsgBuf);
}

/*!
 * Cross-thread things.
 */
int cont_status = -1;  // hack? -1 = ignore; 0 = waiting for cont; 1 = cont'd, will resume
std::mutex m;
std::condition_variable cv;

bool attach_and_break(const ThreadID& tid) {
  cont_status = -1;

  if (!DebugActiveProcess(tid.pid)) {
    win_print_last_error(fmt::format("DebugActiveProcess w/ TID {}", tid.to_string()));
    return false;
  }

  // by default, windows debuggers will kill their debuggees on detach
  DebugSetProcessKillOnExit(FALSE);

  return true;
}

bool detach_and_resume(const ThreadID& tid) {
  if (!DebugActiveProcessStop(tid.pid)) {
    win_print_last_error("DebugActiveProcessStop");
    return false;
  }

  return true;
}

void allow_debugging() {}

bool break_now(const ThreadID& tid) {
  HANDLE hProc = OpenProcess(PROCESS_ALL_ACCESS, FALSE, tid.pid);

  auto result = DebugBreakProcess(hProc);

  CloseHandle(hProc);

  if (!result) {
    win_print_last_error("DebugBreakProcess");
    return false;
  }

  return true;
}

bool cont_now(const ThreadID& tid) {
  if (cont_status != 0) {
    return false;
  }

  {
    std::unique_lock<std::mutex> lk(m);
    cont_status = 1;
    cv.notify_all();
  }
  return true;
}

DEBUG_EVENT debugEvent;
void ignore_debug_event() {
  if (!ContinueDebugEvent(debugEvent.dwProcessId, debugEvent.dwThreadId, DBG_CONTINUE)) {
    win_print_last_error("ContinueDebugEvent ignore_debug_event");
  }
  cont_status = -1;
}

const char* win32_exception_code_to_charp(DWORD exc) {
  switch (exc) {
    case EXCEPTION_ACCESS_VIOLATION:
      return "EXCEPTION_ACCESS_VIOLATION";
    case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
      return "EXCEPTION_ARRAY_BOUNDS_EXCEEDED";
    case EXCEPTION_BREAKPOINT:
      return "EXCEPTION_BREAKPOINT";
    case EXCEPTION_DATATYPE_MISALIGNMENT:
      return "EXCEPTION_DATATYPE_MISALIGNMENT";
    case EXCEPTION_FLT_DENORMAL_OPERAND:
      return "EXCEPTION_FLT_DENORMAL_OPERAND";
    case EXCEPTION_FLT_DIVIDE_BY_ZERO:
      return "EXCEPTION_FLT_DIVIDE_BY_ZERO";
    case EXCEPTION_FLT_INEXACT_RESULT:
      return "EXCEPTION_FLT_INEXACT_RESULT";
    case EXCEPTION_FLT_INVALID_OPERATION:
      return "EXCEPTION_FLT_INVALID_OPERATION";
    case EXCEPTION_FLT_OVERFLOW:
      return "EXCEPTION_FLT_OVERFLOW";
    case EXCEPTION_FLT_STACK_CHECK:
      return "EXCEPTION_FLT_STACK_CHECK";
    case EXCEPTION_FLT_UNDERFLOW:
      return "EXCEPTION_FLT_UNDERFLOW";
    case EXCEPTION_ILLEGAL_INSTRUCTION:
      return "EXCEPTION_ILLEGAL_INSTRUCTION";
    case EXCEPTION_IN_PAGE_ERROR:
      return "EXCEPTION_IN_PAGE_ERROR";
    case EXCEPTION_INT_DIVIDE_BY_ZERO:
      return "EXCEPTION_INT_DIVIDE_BY_ZERO";
    case EXCEPTION_INT_OVERFLOW:
      return "EXCEPTION_INT_OVERFLOW";
    case EXCEPTION_INVALID_DISPOSITION:
      return "EXCEPTION_INVALID_DISPOSITION";
    case EXCEPTION_NONCONTINUABLE_EXCEPTION:
      return "EXCEPTION_NONCONTINUABLE_EXCEPTION";
    case EXCEPTION_PRIV_INSTRUCTION:
      return "EXCEPTION_PRIV_INSTRUCTION";
    case EXCEPTION_SINGLE_STEP:
      return "EXCEPTION_SINGLE_STEP";
    case EXCEPTION_STACK_OVERFLOW:
      return "EXCEPTION_STACK_OVERFLOW";
    case STATUS_STACK_BUFFER_OVERRUN:
      return "STATUS_STACK_BUFFER_OVERRUN";
    case STATUS_HEAP_CORRUPTION:
      return "STATUS_HEAP_CORRUPTION";
    case STATUS_GUARD_PAGE_VIOLATION:
      return "STATUS_GUARD_PAGE_VIOLATION";
    default:
      return "UNKNOWN (please contact developers)";
  }
}

bool check_stopped(const ThreadID& tid, SignalInfo* out) {
  {
    std::unique_lock<std::mutex> lk(m);
    if (cont_status != -1) {
      cv.wait(lk, [&] { return cont_status == 1; });
      if (!ContinueDebugEvent(debugEvent.dwProcessId, debugEvent.dwThreadId, DBG_CONTINUE)) {
        win_print_last_error("ContinueDebugEvent check_stopped");
      }
      cont_status = -1;
    }
  }

  if (WaitForDebugEvent(&debugEvent, INFINITE)) {
    bool is_other = tid.pid != debugEvent.dwProcessId || tid.tid != debugEvent.dwThreadId;

    cont_status = 0;
    switch (debugEvent.dwDebugEventCode) {
      case EXCEPTION_DEBUG_EVENT:  // 1
      {
        auto exc = debugEvent.u.Exception.ExceptionRecord.ExceptionCode;
        if (is_other) {
          if (exc == EXCEPTION_BREAKPOINT) {
            out->kind = SignalInfo::BREAK;
          } else {
            // ignore exceptions outside goal thread
            ignore_debug_event();
          }
        } else {
          switch (exc) {
            case EXCEPTION_BREAKPOINT:
              out->kind = SignalInfo::BREAK;
              break;
            case EXCEPTION_ILLEGAL_INSTRUCTION:
              out->kind = SignalInfo::ILLEGAL_INSTR;
              break;
            case EXCEPTION_INT_DIVIDE_BY_ZERO:
            case EXCEPTION_FLT_DENORMAL_OPERAND:
            case EXCEPTION_FLT_DIVIDE_BY_ZERO:
            case EXCEPTION_FLT_INEXACT_RESULT:
            case EXCEPTION_FLT_INVALID_OPERATION:
            case EXCEPTION_FLT_OVERFLOW:
            case EXCEPTION_FLT_UNDERFLOW:
            case EXCEPTION_FLT_STACK_CHECK:
              out->kind = SignalInfo::MATH_EXCEPTION;
              break;
            case EXCEPTION_INT_OVERFLOW:
              ignore_debug_event();
              break;
            default:
              out->kind = SignalInfo::EXCEPTION;
              out->msg = fmt::format("{} [0x{:X}]", win32_exception_code_to_charp(exc), exc);
              break;
          }
        }
      } break;
      case CREATE_THREAD_DEBUG_EVENT:   // 2
      case CREATE_PROCESS_DEBUG_EVENT:  // 3
      case EXIT_THREAD_DEBUG_EVENT:     // 4
      case LOAD_DLL_DEBUG_EVENT:        // 6
      case UNLOAD_DLL_DEBUG_EVENT:      // 7
      case OUTPUT_DEBUG_STRING_EVENT:   // 8
        // don't care about these
        // out->kind = SignalInfo::NOTHING;
        ignore_debug_event();
        break;
      case EXIT_PROCESS_DEBUG_EVENT:  // 5
      case RIP_EVENT:                 // 9
        out->kind = SignalInfo::DISAPPEARED;
        break;
      default:
        printf("[Debugger] unhandled debug event %lu\n", debugEvent.dwDebugEventCode);
        out->kind = SignalInfo::UNKNOWN;
        break;
    }
  } else if (GetLastError() != 0x79) {  // semaphore timeout error, irrelevant.
    win_print_last_error("WaitForDebugEvent");
  }

  return cont_status != -1;
}

bool open_memory(const ThreadID& tid, MemoryHandle* out) {
  return true;
}

bool close_memory(const ThreadID& tid, MemoryHandle* handle) {
  return true;
}

bool read_goal_memory(u8* dest_buffer,
                      int size,
                      u32 goal_addr,
                      const DebugContext& context,
                      const MemoryHandle& mem) {
  SIZE_T read;
  HANDLE hProc = OpenProcess(PROCESS_VM_READ, FALSE, context.tid.pid);

  if (hProc == NULL) {
    win_print_last_error("OpenProcess read_goal_memory");
    return false;
  }

  auto result =
      ReadProcessMemory(hProc, (LPCVOID)(context.base + goal_addr), dest_buffer, size, &read);

  CloseHandle(hProc);

  if (!result || read != size) {
    win_print_last_error("ReadProcessMemory");
    return false;
  }
  return true;
}

bool write_goal_memory(const u8* src_buffer,
                       int size,
                       u32 goal_addr,
                       const DebugContext& context,
                       const MemoryHandle& mem) {
  SIZE_T written;
  HANDLE hProc = OpenProcess(PROCESS_VM_WRITE, FALSE, context.tid.pid);

  if (hProc == NULL) {
    win_print_last_error("OpenProcess write_goal_memory");
    return false;
  }

  auto result =
      WriteProcessMemory(hProc, (LPVOID)(context.base + goal_addr), src_buffer, size, &written);

  CloseHandle(hProc);

  if (!result || written != size) {
    win_print_last_error("WriteProcessMemory");
    return false;
  }
  return true;
}

bool get_regs_now(const ThreadID& tid, Regs* out) {
  CONTEXT context = {};
  context.ContextFlags = CONTEXT_FULL;
  HANDLE hThr = OpenThread(THREAD_GET_CONTEXT, FALSE, tid.tid);

  if (hThr == NULL) {
    win_print_last_error("OpenThread get_regs_now");
    return false;
  }

  auto result = GetThreadContext(hThr, &context);
  CloseHandle(hThr);

  if (!result) {
    win_print_last_error("GetThreadContext get_regs_now");
    return false;
  }

  out->gprs[0] = context.Rax;
  out->gprs[1] = context.Rcx;
  out->gprs[2] = context.Rdx;
  out->gprs[3] = context.Rbx;
  out->gprs[4] = context.Rsp;
  out->gprs[5] = context.Rbp;
  out->gprs[6] = context.Rsi;
  out->gprs[7] = context.Rdi;
  out->gprs[8] = context.R8;
  out->gprs[9] = context.R9;
  out->gprs[10] = context.R10;
  out->gprs[11] = context.R11;
  out->gprs[12] = context.R12;
  out->gprs[13] = context.R13;
  out->gprs[14] = context.R14;
  out->gprs[15] = context.R15;
  out->rip = context.Rip;

  // todo, get fprs.
  return true;
}

bool set_regs_now(const ThreadID& tid, const Regs& out) {
  CONTEXT context = {};
  context.ContextFlags = CONTEXT_FULL;
  HANDLE hThr = OpenThread(THREAD_GET_CONTEXT, FALSE, tid.tid);

  if (hThr == NULL) {
    win_print_last_error("OpenThread set_regs_now");
    return false;
  }

  auto result = GetThreadContext(hThr, &context);
  CloseHandle(hThr);

  if (!result) {
    win_print_last_error("GetThreadContext set_regs_now");
    return false;
  }

  context.Rax = out.gprs[0];
  context.Rcx = out.gprs[1];
  context.Rdx = out.gprs[2];
  context.Rbx = out.gprs[3];
  context.Rsp = out.gprs[4];
  context.Rbp = out.gprs[5];
  context.Rsi = out.gprs[6];
  context.Rdi = out.gprs[7];
  context.R8 = out.gprs[8];
  context.R9 = out.gprs[9];
  context.R10 = out.gprs[10];
  context.R11 = out.gprs[11];
  context.R12 = out.gprs[12];
  context.R13 = out.gprs[13];
  context.R14 = out.gprs[14];
  context.R15 = out.gprs[15];
  context.Rip = out.rip;

  hThr = OpenThread(THREAD_SET_CONTEXT, FALSE, tid.tid);

  if (hThr == NULL) {
    win_print_last_error("OpenThread set_regs_now set");
    return false;
  }

  result = SetThreadContext(hThr, &context);
  CloseHandle(hThr);

  if (!result) {
    win_print_last_error("SetThreadContext set_regs_now set");
    return false;
  }
  // todo, set fprs.
  return true;
}
#elif __APPLE__
ThreadID::ThreadID(const std::string& str) {}

std::string ThreadID::to_string() const {
  return "invalid";
}

ThreadID get_current_thread_id() {
  return ThreadID("not implemented on macOS");
}

bool attach_and_break(const ThreadID& tid);

void allow_debugging() {
  printf("allow_debugging not implemented on macOS\n");
}

bool detach_and_resume(const ThreadID& tid) {
  return false;
}
bool get_regs_now(const ThreadID& tid, Regs* out) {
  return false;
}
bool set_regs_now(const ThreadID& tid, const Regs& in) {
  return false;
}
bool break_now(const ThreadID& tid) {
  return false;
}
bool cont_now(const ThreadID& tid) {
  return false;
}
bool open_memory(const ThreadID& tid, MemoryHandle* out);
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
