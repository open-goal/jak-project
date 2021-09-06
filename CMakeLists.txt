# Top Level CMakeLists.txt
cmake_minimum_required(VERSION 3.10)

project(jak)
include(CTest)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Debug")
endif()

set(CMAKE_CXX_STANDARD 17)

# Set default compile flags for GCC
# optimization level can be set here. Note that game/ overwrites this for building game C++ code.
if(UNIX)
    message(STATUS "GCC detected, adding compile flags")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} \
        -Wall \
        -Winit-self \
        -ggdb \
        -Wextra \
        -Wno-cast-align \
        -Wcast-qual \
        -Wdisabled-optimization \
        -Wformat \
        -Wmissing-include-dirs \
        -Woverloaded-virtual \
        -Wredundant-decls \
        -Wshadow \
        -Wsign-promo")
else()
    set(CMAKE_CXX_FLAGS "/EHsc")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /STACK:10000000")
endif(UNIX)

if(WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
endif()

if(ASAN_BUILD)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -O1")
    message(STATUS "Doing ASAN build")
endif()

# if(WIN32)
#   find_program(buildcache_program buildcache ${PROJECT_SOURCE_DIR}/bin/windows/)
#   if(buildcache_program)
#     message("Found Windows BuildCache, gonna use it!")
#     set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${buildcache_program}")
#   else()
#     message("Windows Buildcache not found!")
#   endif()
# else()
#   find_program(buildcache_program buildcache ${PROJECT_SOURCE_DIR}/bin/linux/)
#   if(buildcache_program)
#     message("Found Linux BuildCache, gonna use it!")
#     set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${buildcache_program}")
#   else()
#     message("Linux Buildcache not found!")
#   endif()
# endif()

option(CODE_COVERAGE "Enable Code Coverage Compiler Flags" OFF)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/third-party/cmake/modules/)

if(UNIX AND CODE_COVERAGE)
  include(CodeCoverage)
  append_coverage_compiler_flags()
  message("Code Coverage build is enabled!")
else()
  message("Code Coverage build is disabled!")
endif()

# includes relative to top level jak-project folder
include_directories(./)

include_directories(SYSTEM third-party/inja)

# build repl library
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
add_subdirectory(third-party/replxx EXCLUDE_FROM_ALL)

# build common library
add_subdirectory(common)

# build decompiler
add_subdirectory(decompiler)

# build glfw library
add_subdirectory(third-party/glfw)

# build libzip
add_subdirectory(third-party/11zip EXCLUDE_FROM_ALL)

# build imgui
include_directories(third-party/glad/include)
include_directories(third-party/glfw/include)
add_subdirectory(third-party/imgui)

# build the game code in C++
add_subdirectory(game)

# build the compiler
add_subdirectory(goalc)

# build standalone tools
add_subdirectory(tools)

# build the gtest libraries
if(WIN32)
  set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
endif()
add_subdirectory(third-party/googletest EXCLUDE_FROM_ALL)
include(GoogleTest)

# build tests
include(test/CMakeLists.txt)

# build lzokay library
add_subdirectory(third-party/lzokay EXCLUDE_FROM_ALL)

# build format library
add_subdirectory(third-party/fmt EXCLUDE_FROM_ALL)

# build zydis third party library for disassembling x86
# NOTE: Once under CMake 3.13's policy CMP0077, override with `set()` instead
option(ZYDIS_BUILD_TOOLS "Zydis: Build tools" OFF)
option(ZYDIS_BUILD_EXAMPLES "Zydis: Build examples" OFF)
option(ZYDIS_BUILD_SHARED_LIB "Zydis: Build shared library" ON)
add_subdirectory(third-party/zydis EXCLUDE_FROM_ALL)


# windows memory management lib
if(WIN32)
    add_subdirectory(third-party/mman)
endif()
