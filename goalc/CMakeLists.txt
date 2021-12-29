add_library(compiler
        SHARED
        emitter/CallingConvention.cpp
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
        compiler/compilation/Asm.cpp
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
        compiler/compilation/State.cpp
        compiler/compilation/Static.cpp
        compiler/Util.cpp
        data_compiler/game_text.cpp
        data_compiler/dir_tpages.cpp
        data_compiler/DataObjectGenerator.cpp
        data_compiler/game_count.cpp
        debugger/Debugger.cpp
        debugger/DebugInfo.cpp
        listener/Listener.cpp
        listener/MemoryMap.cpp
        make/MakeSystem.cpp
        make/Tool.cpp
        make/Tools.cpp
        regalloc/IRegister.cpp
        regalloc/Allocator.cpp
        regalloc/allocator_interface.cpp
        regalloc/Allocator_v2.cpp
        )

target_link_libraries(compiler common Zydis)

if (WIN32)
    target_link_libraries(compiler mman)
endif ()

if (UNIX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2")
elseif (WIN32)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2")
endif ()

add_executable(goalc main.cpp)
target_link_libraries(goalc common Zydis compiler)

