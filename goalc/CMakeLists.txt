add_subdirectory(util)
add_subdirectory(goos)
IF (WIN32)
  # TODO - implement windows listener
  message("Windows Listener Not Implemented!")
ELSE()
  add_subdirectory(listener)
ENDIF()

add_library(compiler
        SHARED
        emitter/CodeTester.cpp
        emitter/ObjectFileData.cpp
        emitter/ObjectGenerator.cpp
        emitter/Register.cpp
        )

add_executable(goalc main.cpp
        compiler/Compiler.cpp)

IF (WIN32)
  set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
ENDIF()

target_link_libraries(goalc util goos type_system)
