add_subdirectory(util)
add_subdirectory(goos)
add_subdirectory(listener)

add_library(compiler
        SHARED
        emitter/CodeTester.cpp
        emitter/ObjectFileData.cpp
        emitter/ObjectGenerator.cpp
        emitter/Register.cpp
        )
add_executable(goalc main.cpp
        compiler/Compiler.cpp)

target_link_libraries(goalc util goos type_system)