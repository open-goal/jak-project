#ifdef __aarch64__

#pragma once

#include <cstring>
#include "Instruction.h"

namespace emitter {
struct InstructionARM64 : Instruction {
    // The ARM instruction stream is a sequence of word-aligned words. Each ARM instruction is a single 32-bit word in that stream. 
    // The encoding of an ARM instruction is:
    // TODO
    // https://iitd-plos.github.io/col718/ref/arm-instructionset.pdf
    u32 instruction_encoding;

    InstructionARM64(u32 encoding) : instruction_encoding(encoding) {}

    uint8_t emit(uint8_t* buffer) const override {
        memcpy(buffer, &instruction_encoding, 4);
        return 4;
    }

    uint8_t length() const override {
        return 4;
    }

};
}  // namespace emitter
#endif