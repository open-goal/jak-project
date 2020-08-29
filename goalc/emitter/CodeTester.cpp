#include <sys/mman.h>
#include "CodeTester.h"
#include "IGen.h"

namespace emitter {

CodeTester::CodeTester() : m_info(RegisterInfo::make_register_info()) {

}

std::string CodeTester::dump_to_hex_string(bool nospace) {
  std::string result;
  char buff[32];
  for (int i = 0; i < code_buffer_size; i++) {
    if(nospace) {
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

void CodeTester::emit(const Instruction& instr) {
  code_buffer_size += instr.emit(code_buffer + code_buffer_size);
  assert(code_buffer_size <= code_buffer_capacity);
}

void CodeTester::emit_set_gpr_as_return(Register gpr) {
  assert(gpr.is_gpr());
  emit(IGen::mov_gpr64_gpr64(RAX, gpr));
}

void CodeTester::emit_return() {
  emit(IGen::ret());
}

void CodeTester::emit_pop_all_gprs(bool exclude_rax) {
  for (int i = 16; i-- > 0;) {
    if (i != RAX || !exclude_rax) {
      emit(IGen::pop_gpr64(i));
    }
  }
}

void CodeTester::emit_push_all_gprs(bool exclude_rax) {
  for (int i = 0; i < 16; i++) {
    if (i != RAX || !exclude_rax) {
      emit(IGen::push_gpr64(i));
    }
  }
}

void CodeTester::emit_push_all_xmms() {
  emit(IGen::sub_gpr64_imm8s(RSP, 8));
  for(int i = 0; i < 16; i++) {
    emit(IGen::sub_gpr64_imm8s(RSP, 16));
    emit(IGen::store128_gpr64_xmm128(RSP, XMM0 + i));
  }
}

void CodeTester::emit_pop_all_xmms() {
  for(int i = 0; i < 16; i++) {
    emit(IGen::load128_xmm128_gpr64(XMM0 + i, RSP));
    emit(IGen::add_gpr64_imm8s(RSP, 16));
  }
  emit(IGen::add_gpr64_imm8s(RSP, 8));
}

void CodeTester::clear() {
  code_buffer_size = 0;
}

u64 CodeTester::execute() {
  return ((u64(*)())code_buffer)();
}

u64 CodeTester::execute(u64 in0, u64 in1, u64 in2, u64 in3) {
  return ((u64(*)(u64, u64, u64, u64))code_buffer)(in0, in1, in2, in3);
}

void CodeTester::init_code_buffer(int capacity) {
  code_buffer = (u8*)mmap(nullptr, capacity, PROT_EXEC | PROT_READ | PROT_WRITE,
                          MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);
  if (code_buffer == (u8*)(-1)) {
    printf("[CodeTester] Failed to map memory!\n");
    assert(false);
  }

  code_buffer_capacity = capacity;
  code_buffer_size = 0;
}

CodeTester::~CodeTester() {
  if (code_buffer_capacity) {
    munmap(code_buffer, code_buffer_capacity);
  }
}
}