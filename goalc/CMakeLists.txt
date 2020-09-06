add_subdirectory(util)
add_subdirectory(goos)

IF (WIN32)
    # TODO - implement windows listener
    message("Windows Listener Not Implemented!")
ELSE ()
    add_subdirectory(listener)
ENDIF ()

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
        compiler/Util.cpp
        logger/Logger.cpp
        regalloc/IRegister.cpp
        regalloc/Allocator.cpp
        regalloc/allocate.cpp
        compiler/Compiler.cpp
        )

add_executable(goalc main.cpp
        )

IF (WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    target_link_libraries(compiler util goos type_system mman)

ELSE ()
    target_link_libraries(compiler util goos type_system listener)
ENDIF ()

target_link_libraries(goalc util goos compiler type_system)
