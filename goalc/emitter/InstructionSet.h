#pragma once

namespace emitter {
enum class InstructionSet { X86, ARM64 };

//! The instruction set that the current process can execute.
constexpr InstructionSet kNativeInstructionSet =
#ifdef __aarch64__
    InstructionSet::ARM64;
#else
    InstructionSet::X86;
#endif
};  // namespace emitter
