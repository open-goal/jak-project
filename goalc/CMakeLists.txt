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
        logger/Logger.cpp
        regalloc/IRegister.cpp
        regalloc/Allocator.cpp
        regalloc/allocate.cpp
        compiler/Compiler.cpp
        )

add_executable(goalc main.cpp)

IF (WIN32)
  target_link_libraries(compiler cross_sockets util goos type_system mman)
ELSE()
	target_link_libraries(compiler cross_sockets util goos goalc type_system)
ENDIF()
