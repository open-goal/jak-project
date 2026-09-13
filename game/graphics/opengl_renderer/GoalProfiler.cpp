#include "GoalProfiler.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <map>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

#include "common/goal_constants.h"
#include "common/symbols.h"
#include "common/versions/versions.h"

#include "game/kernel/common/kscheme.h"
#include "game/kernel/jak3/kscheme.h"
#include "game/kernel/jakx/kscheme.h"
#include "game/runtime.h"

#include "fmt/format.h"
#include "third-party/imgui/imgui.h"

#ifdef OS_POSIX
#include <pthread.h>
#include <signal.h>
#include <ucontext.h>
#elif _WIN32
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#endif

namespace GoalProfiler {
namespace {

constexpr size_t kMaxSamples = 120000;
static_assert(std::atomic<uintptr_t>::is_always_lock_free,
              "The sampling signal handler requires lock-free atomics");
#ifdef OS_POSIX
constexpr int kSamplingSignal = SIGUSR2;
#endif

enum class Status { Unavailable, Idle, Sampling, Complete, Failed };

struct Result {
  std::string name;
  u32 address = 0;
  size_t hits = 0;
};

struct NamedObject {
  std::string name;
  u32 value = 0;
};

class Profiler {
 public:
  void register_thread();
  void unregister_thread();
  void update();
  void draw(bool* open);

  void record(uintptr_t rip) {
    if (!m_collecting.load(std::memory_order_relaxed)) {
      return;
    }
    const size_t idx = m_sample_count.fetch_add(1, std::memory_order_relaxed);
    const size_t target = m_target_samples.load(std::memory_order_relaxed);
    if (idx < target && idx < kMaxSamples) {
      m_samples[idx].store(rip, std::memory_order_relaxed);
    }
  }

 private:
  void start();
  void stop(bool keep_partial);
  void sampling_thread();
  void analyze();

  bool in_goal_memory(u32 offset, size_t size = 1) const {
    return g_ee_main_mem && offset >= EE_MAIN_MEM_LOW_PROTECT && offset <= EE_MAIN_MEM_SIZE &&
           size <= EE_MAIN_MEM_SIZE - offset;
  }

  template <typename T>
  T read(u32 offset) const {
    T result;
    memcpy(&result, g_ee_main_mem + offset, sizeof(T));
    return result;
  }

  std::string read_string(u32 offset) const;
  std::vector<NamedObject> read_symbols() const;

  std::atomic<Status> m_status{Status::Unavailable};
  std::atomic<bool> m_collecting{false};
  std::atomic<size_t> m_sample_count{0};
  std::atomic<size_t> m_target_samples{10000};
  std::array<std::atomic<uintptr_t>, kMaxSamples> m_samples{};
  std::thread m_thread;
  std::mutex m_thread_mutex;

#ifdef OS_POSIX
  pthread_t m_goal_thread{};
  struct sigaction m_old_action {};
  bool m_installed_handler = false;
  std::vector<u8> m_signal_stack;
#elif _WIN32
  HANDLE m_goal_thread = nullptr;
#endif

  int m_requested_samples = 10000;
  int m_frequency_hz = 1000;
  std::atomic<bool> m_registered{false};
  bool m_analyzed = true;
  size_t m_goal_samples = 0;
  size_t m_outside_samples = 0;
  size_t m_unresolved_samples = 0;
  std::vector<Result> m_results;
  char m_filter[128] = {};
  std::string m_error;
};

Profiler& profiler() {
  static Profiler instance;
  return instance;
}

Profiler* g_signal_profiler = nullptr;

#ifdef OS_POSIX
void sample_signal_handler(int, siginfo_t*, void* raw_context) {
  auto* context = static_cast<ucontext_t*>(raw_context);
  uintptr_t pc = 0;
#if defined(__linux__) && defined(__x86_64__)
  pc = context->uc_mcontext.gregs[REG_RIP];
#elif defined(__APPLE__) && defined(__x86_64__)
  pc = context->uc_mcontext->__ss.__rip;
#elif defined(__APPLE__) && defined(__aarch64__)
  pc = context->uc_mcontext->__ss.__pc;
#elif defined(__linux__) && defined(__aarch64__)
  pc = context->uc_mcontext.pc;
#endif
  if (g_signal_profiler) {
    g_signal_profiler->record(pc);
  }
}
#endif

void Profiler::register_thread() {
  std::lock_guard<std::mutex> lock(m_thread_mutex);
#ifdef OS_POSIX
  g_signal_profiler = this;
  if (m_signal_stack.empty()) {
    m_signal_stack.resize(std::max<size_t>(SIGSTKSZ, 64 * 1024));
  }
  stack_t signal_stack{};
  signal_stack.ss_sp = m_signal_stack.data();
  signal_stack.ss_size = m_signal_stack.size();
  if (sigaltstack(&signal_stack, nullptr) != 0) {
    m_error = "Could not install the alternate sampling signal stack";
    m_status = Status::Failed;
    return;
  }
  if (!m_installed_handler) {
    struct sigaction current {};
    sigaction(kSamplingSignal, nullptr, &current);
    if (current.sa_handler != SIG_DFL && current.sa_handler != SIG_IGN) {
      m_error = "SIGUSR2 already has a handler";
      m_status = Status::Failed;
      return;
    }

    struct sigaction action {};
    action.sa_sigaction = sample_signal_handler;
    action.sa_flags = SA_SIGINFO | SA_RESTART | SA_ONSTACK;
    sigemptyset(&action.sa_mask);
    if (sigaction(kSamplingSignal, &action, &m_old_action) != 0) {
      m_error = "Could not install the sampling signal handler";
      m_status = Status::Failed;
      return;
    }
    m_installed_handler = true;
  }
  m_goal_thread = pthread_self();
#elif _WIN32
  m_goal_thread = OpenThread(THREAD_SUSPEND_RESUME | THREAD_GET_CONTEXT | THREAD_QUERY_INFORMATION,
                             FALSE, GetCurrentThreadId());
  if (!m_goal_thread) {
    m_error = "Could not open the GOAL thread";
    m_status = Status::Failed;
    return;
  }
#endif
  m_registered = true;
  m_status = Status::Idle;
}

void Profiler::unregister_thread() {
  stop(false);
  std::lock_guard<std::mutex> lock(m_thread_mutex);
  m_registered = false;
#ifdef OS_POSIX
  // Keep the harmless handler installed until process exit. Restoring SIG_DFL here could race with
  // a signal that was queued immediately before the sampling thread stopped.
#elif _WIN32
  if (m_goal_thread) {
    CloseHandle(m_goal_thread);
    m_goal_thread = nullptr;
  }
#endif
  m_status = Status::Unavailable;
}

void Profiler::start() {
  stop(false);
  if (!m_registered || !g_ee_main_mem || !s7.offset) {
    m_error = "The GOAL thread is not ready";
    m_status = Status::Failed;
    return;
  }

  m_requested_samples = std::clamp(m_requested_samples, 1, static_cast<int>(kMaxSamples));
  m_frequency_hz = std::clamp(m_frequency_hz, 1, 10000);
  m_target_samples = static_cast<size_t>(m_requested_samples);
  m_sample_count = 0;
  m_results.clear();
  m_error.clear();
  m_analyzed = false;
  m_collecting = true;
  m_status = Status::Sampling;
  m_thread = std::thread(&Profiler::sampling_thread, this);
}

void Profiler::stop(bool keep_partial) {
  m_collecting = false;
  if (m_thread.joinable()) {
    m_thread.join();
  }
  if (keep_partial && m_sample_count.load() > 0) {
    m_status = Status::Complete;
    m_analyzed = false;
  } else if (m_registered && m_status != Status::Failed) {
    m_status = Status::Idle;
  }
}

void Profiler::sampling_thread() {
  using Clock = std::chrono::steady_clock;
  const auto period = std::chrono::nanoseconds(1000000000ll / m_frequency_hz);
  auto next = Clock::now();

  while (m_collecting.load(std::memory_order_relaxed) &&
         m_sample_count.load(std::memory_order_relaxed) < m_target_samples.load()) {
#ifdef OS_POSIX
    if (pthread_kill(m_goal_thread, kSamplingSignal) != 0) {
      m_error = "The GOAL thread disappeared while profiling";
      m_status = Status::Failed;
      m_collecting = false;
      return;
    }
#elif _WIN32
    if (SuspendThread(m_goal_thread) == static_cast<DWORD>(-1)) {
      m_error = "Could not suspend the GOAL thread";
      m_status = Status::Failed;
      m_collecting = false;
      return;
    }
    CONTEXT context{};
    context.ContextFlags = CONTEXT_CONTROL;
    if (GetThreadContext(m_goal_thread, &context)) {
#if defined(_M_X64)
      record(context.Rip);
#elif defined(_M_ARM64)
      record(context.Pc);
#endif
    }
    ResumeThread(m_goal_thread);
#endif
    next = std::max(next + period, Clock::now());
    std::this_thread::sleep_until(next);
  }

  if (m_status != Status::Failed) {
    m_collecting = false;
    m_status = Status::Complete;
  }
}

std::string Profiler::read_string(u32 offset) const {
  if (!in_goal_memory(offset, sizeof(u32))) {
    return {};
  }
  const u32 length = read<u32>(offset);
  if (length == 0 || length > 1024 || !in_goal_memory(offset + 4, length)) {
    return {};
  }
  const char* data = reinterpret_cast<const char*>(g_ee_main_mem + offset + 4);
  return std::string(data, strnlen(data, length));
}

std::vector<NamedObject> Profiler::read_symbols() const {
  std::vector<NamedObject> result;
  if (!g_ee_main_mem || !s7.offset || !SymbolTable2.offset || !LastSymbol.offset) {
    return result;
  }

  const u32 stride = g_game_version == GameVersion::Jak1 ? 8 : 4;
  auto visit_range = [&](u32 begin, u32 end) {
    for (u32 sym = begin; sym < end; sym += stride) {
      u32 value = 0;
      u32 string_ptr = 0;
      if (g_game_version == GameVersion::Jak1) {
        if (!in_goal_memory(sym, 4) || !in_goal_memory(sym + jak1::SYM_INFO_OFFSET + 4, 4)) {
          continue;
        }
        value = read<u32>(sym);
        string_ptr = read<u32>(sym + jak1::SYM_INFO_OFFSET + 4);
      } else {
        if (!in_goal_memory(sym - 1, 4)) {
          continue;
        }
        value = read<u32>(sym - 1);
        if (g_game_version == GameVersion::Jak2) {
          if (!in_goal_memory(sym + jak2::SYM_TO_STRING_OFFSET, 4)) {
            continue;
          }
          string_ptr = read<u32>(sym + jak2::SYM_TO_STRING_OFFSET);
        } else {
          const u32 strings = g_game_version == GameVersion::Jak3 ? jak3::SymbolString.offset
                                                                  : jakx::SymbolString.offset;
          const u32 entry = strings + sym - s7.offset;
          if (!strings || !in_goal_memory(entry, 4)) {
            continue;
          }
          string_ptr = read<u32>(entry);
        }
      }
      auto name = read_string(string_ptr);
      if (!name.empty()) {
        result.push_back({std::move(name), value});
      }
    }
  };

  visit_range(SymbolTable2.offset, s7.offset);
  visit_range(s7.offset, LastSymbol.offset);
  return result;
}

void Profiler::analyze() {
  m_results.clear();
  m_goal_samples = 0;
  m_outside_samples = 0;
  m_unresolved_samples = 0;

  const auto symbols = read_symbols();
  if (symbols.empty()) {
    m_error = "The GOAL symbol table is not available";
    m_status = Status::Failed;
    return;
  }

  const u32 function_type = g_game_version == GameVersion::Jak1
                                ? read<u32>(s7.offset + jak1_symbols::FIX_SYM_FUNCTION_TYPE)
                                : read<u32>(s7.offset + jak2_symbols::FIX_SYM_FUNCTION_TYPE - 1);
  const u32 type_type = g_game_version == GameVersion::Jak1
                            ? read<u32>(s7.offset + jak1_symbols::FIX_SYM_TYPE_TYPE)
                            : read<u32>(s7.offset + jak2_symbols::FIX_SYM_TYPE_TYPE - 1);

  u32 state_type = 0;
  for (const auto& symbol : symbols) {
    if (symbol.name == "state") {
      state_type = symbol.value;
      break;
    }
  }

  struct FunctionName {
    std::string name;
    bool direct = false;
  };
  std::map<u32, FunctionName> functions;
  auto add_function = [&](u32 address, std::string name, bool direct) {
    if (!in_goal_memory(address) || !in_goal_memory(address - 4, 4) ||
        read<u32>(address - 4) != function_type) {
      return;
    }
    auto [it, inserted] = functions.emplace(address, FunctionName{std::move(name), direct});
    if (!inserted && direct && !it->second.direct) {
      it->second = {std::move(name), true};
    }
  };

  auto is_state_type = [&](u32 type) {
    for (int depth = 0; state_type && type && depth < 128; ++depth) {
      if (type == state_type) {
        return true;
      }
      if (!in_goal_memory(type + 4, 4) || read<u32>(type - 4) != type_type) {
        return false;
      }
      type = read<u32>(type + 4);
    }
    return false;
  };

  for (const auto& symbol : symbols) {
    const u32 value = symbol.value;
    if (!in_goal_memory(value) || !in_goal_memory(value - 4, 4)) {
      continue;
    }
    const u32 tag = read<u32>(value - 4);
    if (tag == function_type) {
      add_function(value, symbol.name, true);
    } else if (tag == type_type && in_goal_memory(value + 16, 4)) {
      const u16 method_count = read<u16>(value + 14);
      if (method_count < 4096 && in_goal_memory(value + 16, size_t(method_count) * 4)) {
        for (u16 method = 0; method < method_count; ++method) {
          add_function(read<u32>(value + 16 + method * 4),
                       fmt::format("{}::method-{}", symbol.name, method), false);
        }
      }
    } else if (is_state_type(tag) && in_goal_memory(tag + 10, 2)) {
      const u16 size = read<u16>(tag + 10);
      if (size < 4096 && in_goal_memory(value, size)) {
        for (u16 offset = 0; offset + 4 <= size; offset += 4) {
          add_function(read<u32>(value + offset),
                       fmt::format("{}::handler+0x{:x}", symbol.name, offset), false);
        }
      }
    }
  }

  if (functions.empty()) {
    m_error = "No GOAL functions were found in the symbol table";
    m_status = Status::Failed;
    return;
  }

  std::map<u32, size_t> hits;
  const size_t count = std::min(m_sample_count.load(), m_target_samples.load());
  const uintptr_t memory_start = reinterpret_cast<uintptr_t>(g_ee_main_mem);
  const uintptr_t memory_end = memory_start + EE_MAIN_MEM_SIZE;
  for (size_t i = 0; i < count; ++i) {
    const uintptr_t rip = m_samples[i].load(std::memory_order_relaxed);
    if (rip < memory_start || rip >= memory_end) {
      ++m_outside_samples;
      continue;
    }
    ++m_goal_samples;
    const u32 offset = static_cast<u32>(rip - memory_start);
    auto it = functions.upper_bound(offset);
    if (it == functions.begin()) {
      ++m_unresolved_samples;
      continue;
    }
    --it;
    ++hits[it->first];
  }

  for (const auto& [address, count_for_function] : hits) {
    m_results.push_back({functions.at(address).name, address, count_for_function});
  }
  std::sort(m_results.begin(), m_results.end(),
            [](const Result& a, const Result& b) { return a.hits > b.hits; });
  m_analyzed = true;
}

void Profiler::update() {
  if (m_status == Status::Complete && !m_analyzed) {
    if (m_thread.joinable()) {
      m_thread.join();
    }
    analyze();
  }
}

void Profiler::draw(bool* open) {
  update();

  if (!ImGui::Begin("GOAL Sampling Profiler", open)) {
    ImGui::End();
    return;
  }

  ImGui::InputInt("Samples", &m_requested_samples);
  ImGui::InputInt("Frequency (Hz)", &m_frequency_hz);
  const Status status = m_status.load();
  if (status == Status::Sampling) {
    if (ImGui::Button("Stop")) {
      stop(true);
    }
    const float fraction = float(std::min(m_sample_count.load(), m_target_samples.load())) /
                           float(m_target_samples.load());
    ImGui::ProgressBar(fraction, ImVec2(-1, 0));
  } else if (ImGui::Button("Profile")) {
    start();
  }

  if (status == Status::Unavailable) {
    ImGui::TextUnformatted("Waiting for the GOAL thread...");
  } else if (status == Status::Failed) {
    ImGui::TextColored(ImVec4(1.f, 0.35f, 0.35f, 1.f), "%s", m_error.c_str());
  }

  if (m_analyzed && (m_goal_samples || m_outside_samples)) {
    ImGui::Separator();
    ImGui::Text("Samples: %zu GOAL, %zu outside GOAL, %zu unresolved", m_goal_samples,
                m_outside_samples, m_unresolved_samples);
    ImGui::InputText("Filter", m_filter, sizeof(m_filter));
    if (ImGui::BeginTable("goal-profile-results", 4,
                          ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg |
                              ImGuiTableFlags_ScrollY | ImGuiTableFlags_Resizable,
                          ImVec2(0, 420))) {
      ImGui::TableSetupColumn("Function");
      ImGui::TableSetupColumn("Samples", ImGuiTableColumnFlags_WidthFixed, 80);
      ImGui::TableSetupColumn("GOAL %", ImGuiTableColumnFlags_WidthFixed, 70);
      ImGui::TableSetupColumn("Address", ImGuiTableColumnFlags_WidthFixed, 90);
      ImGui::TableHeadersRow();
      for (const auto& result : m_results) {
        if (m_filter[0] && result.name.find(m_filter) == std::string::npos) {
          continue;
        }
        ImGui::TableNextRow();
        ImGui::TableNextColumn();
        ImGui::TextUnformatted(result.name.c_str());
        ImGui::TableNextColumn();
        ImGui::Text("%zu", result.hits);
        ImGui::TableNextColumn();
        ImGui::Text("%.2f", m_goal_samples ? 100. * result.hits / m_goal_samples : 0.);
        ImGui::TableNextColumn();
        ImGui::Text("0x%08x", result.address);
      }
      ImGui::EndTable();
    }
  }
  ImGui::End();
}

}  // namespace

void register_goal_thread() {
  profiler().register_thread();
}

void unregister_goal_thread() {
  profiler().unregister_thread();
}

void update() {
  profiler().update();
}

void draw_window(bool* open) {
  profiler().draw(open);
}

}  // namespace GoalProfiler
