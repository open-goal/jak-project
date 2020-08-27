#include <third-party/mman.h>
#include <cstdio>
#include "CodeTester.h"
#include "Instruction.h"
#include "IGen.h"

namespace goal {

std::string CodeTester::dump_to_hex_string() {
  std::string result;
  char buff[32];
  for (int i = 0; i < code_buffer_size; i++) {
    sprintf(buff, "%02x ", code_buffer[i]);
    result += buff;
  }

  // remove trailing space
  if (!result.empty()) {
    result.pop_back();
  }
  return result;
}

void CodeTester::emit(const Instruction& instr) {
  code_buffer_size += instr.emit(code_buffer + code_buffer_size);
  assert(code_buffer_size <= code_buffer_capacity);
}

void CodeTester::emit_set_gpr_as_return(X86R gpr) {
  assert(is_gpr(gpr));
  emit(IGen::mov_gpr64_gpr64(RAX, gpr));
}

void CodeTester::emit_return() {
  emit(IGen::ret());
}

void CodeTester::emit_pop_all_gprs(bool exclude_rax) {
  for(int i = 16; i-- > 0;) {
    if(i != RAX || !exclude_rax) {
      emit(IGen::pop_gpr64(i));
    }
  }
}

void CodeTester::emit_push_all_gprs(bool exclude_rax) {
  for (int i = 0; i < 16; i++) {
    if(i != RAX || !exclude_rax) {
      emit(IGen::push_gpr64(i));
    }

  }
}

void CodeTester::clear() {
  code_buffer_size = 0;
}

u64 CodeTester::execute() {
  return ((u64(*)())code_buffer)();
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

}  // namespace goal