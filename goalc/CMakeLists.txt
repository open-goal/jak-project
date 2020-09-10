add_subdirectory(util)
add_subdirectory(goos)
add_subdirectory(listener)

add_library(compiler
        SHARED
        emitter/CodeTester.cpp
        emitter/ObjectFileData.cpp
        emitter/ObjectGenerator.cpp
        emitter/Register.cpp
        compiler/Compiler.cpp
        compiler/Env.cpp
        compiler/Val.cpp
        compiler/IR.cpp
        compiler/CodeGenerator.cpp
        compiler/compilation/Atoms.cpp
        compiler/compilation/CompilerControl.cpp
        compiler/compilation/Block.cpp
        compiler/compilation/Macro.cpp
        compiler/compilation/Define.cpp
        compiler/Util.cpp
        logger/Logger.cpp
        regalloc/IRegister.cpp
        regalloc/Allocator.cpp
        regalloc/allocate.cpp
        compiler/Compiler.cpp)

add_executable(goalc main.cpp)

IF (WIN32)
    target_link_libraries(compiler util goos type_system listener mman)

ELSE ()
    target_link_libraries(compiler util goos type_system listener)
ENDIF ()

target_link_libraries(goalc util goos compiler type_system)
