add_library(compiler
        SHARED
        emitter/CodeTester.cpp
        emitter/ObjectFileData.cpp
        emitter/ObjectGenerator.cpp
        emitter/Register.cpp
        debugger/disassemble.cpp
        compiler/Compiler.cpp
        compiler/Env.cpp
        compiler/Val.cpp
        compiler/IR.cpp
        compiler/CompilerSettings.cpp
        compiler/CodeGenerator.cpp
        compiler/StaticObject.cpp
        compiler/compilation/Atoms.cpp
        compiler/compilation/CompilerControl.cpp
        compiler/compilation/Block.cpp
        compiler/compilation/Macro.cpp
        compiler/compilation/Math.cpp
        compiler/compilation/Define.cpp
        compiler/compilation/Debug.cpp
        compiler/compilation/Function.cpp
        compiler/compilation/ControlFlow.cpp
        compiler/compilation/Type.cpp
        compiler/compilation/Static.cpp
        compiler/Util.cpp
        debugger/Debugger.cpp
        debugger/DebugInfo.cpp
        logger/Logger.cpp
        listener/Listener.cpp
        listener/MemoryMap.cpp
        regalloc/IRegister.cpp
        regalloc/Allocator.cpp
        regalloc/allocate.cpp
        regalloc/allocate_common.cpp
        compiler/Compiler.cpp)

add_executable(goalc main.cpp)

IF (WIN32)
    target_link_libraries(compiler goos type_system mman common_util spdlog cross_os_debug cross_sockets Zydis)

ELSE ()
    target_link_libraries(compiler goos type_system common_util spdlog cross_os_debug cross_sockets Zydis)
ENDIF ()

target_link_libraries(goalc goos compiler type_system)
