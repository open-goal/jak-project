/*!
 * @file CodeTester.cpp
 * The CodeTester is a utility to run the output of the compiler as part of a unit test.
 * This is effective for tests which try all combinations of registers, etc.
 *
 * The CodeTester can't be used for tests requiring the full GOAL language/linking.
 */

#include <stdexcept>

#include "common/common_types.h"

#include "goalc/emitter/Instruction.h"
#include "goalc/emitter/InstructionSet.h"
#include "goalc/emitter/Register.h"
#ifdef OS_POSIX
#include <sys/mman.h>
#elif _WIN32
#include "third-party/mman/mman.h"
#endif
#if defined(__APPLE__) && defined(__aarch64__)
#include <pthread.h>  // pthread_jit_write_protect_np
#endif
#if defined(__SWITCH__)
// See the matching comment in CodeTester.h -- avoids a struct-u128 vs typedef-u128 collision.
#define u128 nx_u128
#include <switch.h>
#undef u128
#endif

#include <cstdio>

#include "CodeTester.h"
#include "IGen.h"

#include "capstone/capstone.h"

namespace emitter {

CodeTester::CodeTester() : m_info(RegisterInfo::make_register_info()), m_gen(GameVersion::Jak1) {}

CodeTester::CodeTester(InstructionSet instruction_set)
    : m_info(instruction_set == InstructionSet::ARM64 ? RegisterInfo::make_register_info_arm64()
                                                      : RegisterInfo::make_register_info()),
      m_gen(GameVersion::Jak1, instruction_set) {}

/*!
 * Convert to a string for comparison against an assembler or tests.
 */
std::string CodeTester::dump_to_hex_string(bool nospace) {
  std::string result;
  char buff[32];
  for (int i = 0; i < code_buffer_size; i++) {
    if (nospace) {
      sprintf(buff, "%02X", code_buffer[i]);
    } else {
      sprintf(buff, "%02x ", code_buffer[i]);
    }

    result += buff;
  }

  // remove trailing space
  if (!nospace && !result.empty()) {
    result.pop_back();
  }
  return result;
}

void CodeTester::print_hex_dump() {
  printf("%s\n", dump_to_hex_string(true).data());
}
void CodeTester::print_asm_dump() {
  printf("%s\n", dump_to_asm_string().data());
}

static constexpr int first_saved_gpr = 1;
static constexpr int first_saved_simd = 0;

static std::optional<size_t> match_push_gprs(const std::vector<CodeTester::DisasmLine>& lines,
                                             int last_gpr,
                                             size_t start) {
  const int count = last_gpr - first_saved_gpr + 1;

  if (start + count > lines.size()) {
    return std::nullopt;
  }

  for (int i = 0; i < count; i++) {
    int reg = first_saved_gpr + i;

    std::string expected = "str\tx" + std::to_string(reg) + ", [sp, #-0x10]!";

    if (lines[start + i].text != expected) {
      return std::nullopt;
    }
  }

  return static_cast<size_t>(count);
}

static std::optional<size_t> match_pop_gprs(const std::vector<CodeTester::DisasmLine>& lines,
                                            int last_gpr,
                                            size_t start) {
  const int count = last_gpr - first_saved_gpr + 1;

  if (start + count > lines.size()) {
    return std::nullopt;
  }

  for (int i = 0; i < count; i++) {
    int reg = last_gpr - i;

    std::string expected = "ldr\tx" + std::to_string(reg) + ", [sp], #0x10";

    if (lines[start + i].text != expected) {
      return std::nullopt;
    }
  }

  return static_cast<size_t>(count);
}

static std::optional<size_t> match_push_simd(const std::vector<CodeTester::DisasmLine>& lines,
                                             int last_simd,
                                             size_t start) {
  const int count = last_simd - first_saved_simd + 1;
  const size_t line_count = static_cast<size_t>(count) * 2;

  if (start + line_count > lines.size()) {
    return std::nullopt;
  }

  for (int i = 0; i < count; i++) {
    int reg = first_saved_simd + i;

    std::string expected_sub = "sub\tsp, sp, #0x10";
    std::string expected_str = "str\tq" + std::to_string(reg) + ", [sp]";

    if (lines[start + i * 2].text != expected_sub ||
        lines[start + i * 2 + 1].text != expected_str) {
      return std::nullopt;
    }
  }

  return line_count;
}

static std::optional<size_t> match_pop_simd(const std::vector<CodeTester::DisasmLine>& lines,
                                            int last_simd,
                                            size_t start) {
  const int count = last_simd - first_saved_simd + 1;
  const size_t line_count = static_cast<size_t>(count) * 2;

  if (start + line_count > lines.size()) {
    return std::nullopt;
  }

  for (int i = 0; i < count; i++) {
    int reg = last_simd - i;

    std::string expected_ldr = "ldr\tq" + std::to_string(reg) + ", [sp]";
    std::string expected_add = "add\tsp, sp, #0x10";

    if (lines[start + i * 2].text != expected_ldr ||
        lines[start + i * 2 + 1].text != expected_add) {
      return std::nullopt;
    }
  }

  return line_count;
}

std::vector<CodeTester::DisasmLine> CodeTester::disassemble() {
  std::vector<DisasmLine> result;

  csh handle;
  if (cs_open(CS_ARCH_AARCH64, CS_MODE_ARM, &handle) != CS_ERR_OK) {
    return result;
  }

  cs_insn* insn = nullptr;
  size_t count = cs_disasm(handle, code_buffer, code_buffer_size, 0, 0, &insn);

  for (size_t i = 0; i < count; i++) {
    DisasmLine line;
    line.address = insn[i].address;

    // Keep only the mnemonic + operands for pattern matching
    line.text = std::string(insn[i].mnemonic) + "\t" + insn[i].op_str;

    result.push_back(std::move(line));
  }

  cs_free(insn, count);
  cs_close(&handle);

  return result;
}

std::string CodeTester::dump_to_asm_string() {
  auto lines = disassemble();

  // x31 is SP/ZR, not a GPR. The last real GPR is one before it.
  const int last_gpr = get_reg_count() - 1;

  // SIMD registers are V0-V31.
  const int last_simd = get_simd_reg_count() - 1;

  std::string result;
  for (size_t i = 0; i < lines.size();) {
    if (auto count = match_push_gprs(lines, last_gpr, i)) {
      result += "\033[2m<push all GPRs>\033[0m\n";
      i += *count;
      continue;
    }

    if (auto count = match_pop_gprs(lines, last_gpr, i)) {
      result += "\033[2m<pop all GPRs>\033[0m\n";
      i += *count;
      continue;
    }

    if (auto count = match_push_simd(lines, last_simd, i)) {
      result += "\033[2m<push all SIMDs>\033[0m\n";
      i += *count;
      continue;
    }

    if (auto count = match_pop_simd(lines, last_simd, i)) {
      result += "\033[2m<pop all SIMDs>\033[0m\n";
      i += *count;
      continue;
    }

    char buff[128];
    snprintf(buff, sizeof(buff), "%08llx:\t%s\n", lines[i].address, lines[i].text.c_str());
    result += buff;
    i++;
  }

  return result;
}

/*!
 * Add an instruction to the buffer.
 */
void CodeTester::emit(const emitter::Instruction& instr) {
  u8* start = code_buffer + code_buffer_size;
  code_buffer_size += instr.emit(start);
  ASSERT(code_buffer_size <= code_buffer_capacity);
}
/*!
 * Add a return instruction to the buffer.
 */
void CodeTester::emit_return() {
  emit(IGen::ret(m_gen));
}

/*!
 * Pop all GPRs off of the stack. Optionally exclude rax.
 * Pops RSP always, which is weird, but doesn't cause issues.
 */
void CodeTester::emit_pop_all_gprs(bool exclude_return_register) {
  if (m_gen.instr_set() == InstructionSet::X86) {
    for (int i = 16; i-- > 0;) {
      if (i != RAX || !exclude_return_register) {
        emit(IGen::pop_gpr64(m_gen, i));
      }
    }
  } else if (m_gen.instr_set() == InstructionSet::ARM64) {
    for (int i = 31; i-- > 0;) {
      if (i != X0 || !exclude_return_register) {
        emit(IGen::pop_gpr64(m_gen, i));
      }
    }
  } else {
    throw std::runtime_error("CodeTester::emit_pop_all_gprs unhandled instruction set");
  }
}

/*!
 * Push all GPRs onto the stack. Optionally exclude RAX.
 * Pushes RSP always, which is weird, but doesn't cause issues.
 */
void CodeTester::emit_push_all_gprs(bool exclude_return_register) {
  if (m_gen.instr_set() == InstructionSet::X86) {
    for (int i = 0; i < 16; i++) {
      if (i != RAX || !exclude_return_register) {
        emit(IGen::push_gpr64(m_gen, i));
      }
    }
  } else if (m_gen.instr_set() == InstructionSet::ARM64) {
    for (int i = 0; i < 31; i++) {
      if (i != X0 || !exclude_return_register) {
        emit(IGen::push_gpr64(m_gen, i));
      }
    }
  } else {
    throw std::runtime_error("CodeTester::emit_push_all_gprs unhandled instruction set");
  }
}

/*!
 * Push all SIMD registers (all 128 bits) to the stack.
 */
void CodeTester::emit_push_all_simd() {
  if (m_gen.instr_set() == InstructionSet::X86) {
    emit(IGen::sub_gpr64_imm8s(m_gen, RSP, 8));
    for (int i = 0; i < 16; i++) {
      emit(IGen::sub_gpr64_imm8s(m_gen, RSP, 16));
      emit(IGen::store128_gpr64_simd128(m_gen, RSP, XMM0 + i));
    }
  } else if (m_gen.instr_set() == InstructionSet::ARM64) {
    for (int i = 0; i < 32; i++) {
      emit(IGen::sub_gpr64_imm8s(m_gen, SP, 16));
      emit(IGen::store128_gpr64_simd128(m_gen, SP, V0 + i));
    }
  } else {
    throw std::runtime_error("CodeTester::emit_push_all_simd unhandled instruction set");
  }
}

/*!
 * Pop all SIMD registers (all 128 bits) from the stack.
 */
void CodeTester::emit_pop_all_simd() {
  if (m_gen.instr_set() == InstructionSet::X86) {
    for (int i = 15; i >= 0; i--) {
      emit(IGen::load128_simd128_gpr64(m_gen, XMM0 + i, RSP));
      emit(IGen::add_gpr64_imm8s(m_gen, RSP, 16));
    }
    emit(IGen::add_gpr64_imm8s(m_gen, RSP, 8));
  } else if (m_gen.instr_set() == InstructionSet::ARM64) {
    for (int i = 31; i >= 0; i--) {
      emit(IGen::load128_simd128_gpr64(m_gen, V0 + i, SP));
      emit(IGen::add_gpr64_imm8s(m_gen, SP, 16));
    }
  } else {
    throw std::runtime_error("CodeTester::emit_pop_all_simd unhandled instruction set");
  }
}

/*!
 * Remove everything from the code buffer
 */
void CodeTester::clear() {
  code_buffer_size = 0;
}

/*!
 * Execute the buffered code with no arguments, return the value of RAX.
 */
u64 CodeTester::execute() {
#if defined(__aarch64__)
  // allegedly needed because ARM requires flushing after writing new instructions
  // on x86 it does nothing
  __builtin___clear_cache((char*)code_buffer, (char*)code_buffer + code_buffer_size);
#endif
  // clang-format off
#if defined(__APPLE__) && defined(__aarch64__)
  // block writes while this thread runs the MAP_JIT buffer
  pthread_jit_write_protect_np(1);
  auto ret = ((u64(*)())code_buffer)();
  pthread_jit_write_protect_np(0);
  return ret;
#elif defined(__SWITCH__)
  jitTransitionToExecutable(&m_jit);
  auto ret = ((u64(*)())code_buffer_rx)();
  jitTransitionToWritable(&m_jit);
  return ret;
#else
  return ((u64(*)())code_buffer)();
#endif
  // clang-format on
}

/*!
 * Execute code buffer with arguments. Use get_c_abi_arg to figure out which registers the
 * arguments will appear in (will handle windows/linux differences)
 */
u64 CodeTester::execute(u64 in0, u64 in1, u64 in2, u64 in3) {
#if defined(__aarch64__)
  __builtin___clear_cache((char*)code_buffer, (char*)code_buffer + code_buffer_size);
#endif
  // clang-format off
#if defined(__APPLE__) && defined(__aarch64__)
  pthread_jit_write_protect_np(1);
  auto ret = ((u64(*)(u64, u64, u64, u64))code_buffer)(in0, in1, in2, in3);
  pthread_jit_write_protect_np(0);
  return ret;
#elif defined(__SWITCH__)
  jitTransitionToExecutable(&m_jit);
  auto ret = ((u64(*)(u64, u64, u64, u64))code_buffer_rx)(in0, in1, in2, in3);
  jitTransitionToWritable(&m_jit);
  return ret;
#else
  return ((u64(*)(u64, u64, u64, u64))code_buffer)(in0, in1, in2, in3);
#endif
  // clang-format on
}

/*!
 * Allocate a code buffer of the given size.
 */
void CodeTester::init_code_buffer(int capacity) {
  // MAP_JIT write protection is per thread
#if defined(__APPLE__) && defined(__aarch64__)
  code_buffer = (u8*)mmap(nullptr, capacity, PROT_READ | PROT_WRITE | PROT_EXEC,
                          MAP_ANONYMOUS | MAP_PRIVATE | MAP_JIT, -1, 0);
#elif defined(__SWITCH__)
  // Horizon enforces W^X: jitCreate gives back separate RW/RX aliases of the same pages instead
  // of one RWX mapping. Round up to a page (0x1000) since jitCreate requires page-aligned size.
  size_t jit_size = (size_t(capacity) + 0xfff) & ~size_t(0xfff);
  Result rc = jitCreate(&m_jit, jit_size);
  if (R_FAILED(rc)) {
    ASSERT_MSG(false, "[CodeTester] jitCreate failed!");
  }
  jitTransitionToWritable(&m_jit);
  code_buffer = (u8*)jitGetRwAddr(&m_jit);
  code_buffer_rx = (u8*)jitGetRxAddr(&m_jit);
#else
  code_buffer = (u8*)mmap(nullptr, capacity, PROT_EXEC | PROT_READ | PROT_WRITE,
                          MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
#endif
#if !defined(__SWITCH__)
  if (code_buffer == (u8*)(-1)) {
    ASSERT_MSG(false, "[CodeTester] Failed to map memory!");
  }
#endif
#if defined(__APPLE__) && defined(__aarch64__)
  // allow writes before the first instruction
  pthread_jit_write_protect_np(0);
#endif

  code_buffer_capacity = capacity;
  code_buffer_size = 0;
}

CodeTester::~CodeTester() {
  if (code_buffer_capacity) {
#if defined(__SWITCH__)
    jitClose(&m_jit);
#else
    munmap(code_buffer, code_buffer_capacity);
#endif
  }
}
}  // namespace emitter
