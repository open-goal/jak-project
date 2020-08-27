add_subdirectory(util)
add_subdirectory(goos)
add_subdirectory(listener)
add_subdirectory(emitter)

add_executable(goalc main.cpp)

IF (WIN32)
  set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
ENDIF()

target_link_libraries(goalc util goos)