#pragma once

namespace emitter {
enum class InstructionSet { X86, ARM64 };

//! Native target for tests that execute generated code.
//! Compiler targets must be selected explicitly.
constexpr InstructionSet kNativeInstructionSet =
#ifdef __aarch64__
    InstructionSet::ARM64;
#else
    InstructionSet::X86;
#endif
};  // namespace emitter
