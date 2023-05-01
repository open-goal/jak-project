# Top Level CMakeLists.txt
cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_STANDARD 17)
project(jak)
include(CTest)

# Include third-party modules
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/third-party/cmake/modules/)

# Default to Release mode
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release")
endif()

# Potentially statically build the project
option(STATICALLY_LINK "Build for release purposes (statically link everything)" OFF)
message(STATUS "Statically Link? ${STATICALLY_LINK}")
if(STATICALLY_LINK)
    set(BUILD_SHARED_LIBS OFF)
else()
    set(BUILD_SHARED_LIBS ON)
endif()

# Setup compiler flags
# TODO - consider moving most of the configuration into presets
# - https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html
# - requires atleast CMake 3.19
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    message(STATUS "Clang Detected - Setting Defaults")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} \
        -fcxx-exceptions \
        -fexceptions \
        -fdiagnostics-color=always \
        -std=c++17 \
        -mavx \
        -Wall \
        -Wno-c++11-narrowing \
        -Wno-c++98-compat \
        -O3 \
        -D_CRT_SECURE_NO_WARNINGS")

    # Increase stack size for windows, who's default is too low
    if(WIN32)
        # Increase the reserved stack size for all threads to 16MB
        # Note: this is only _reserved_ memory, not necessarily _committed_ memory
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LDFLAGS} -Xlinker /STACK:16000000")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb -g -Wextra")
    endif()

    # additional c++ and linker flags for release mode for our projects
    if(CMAKE_BUILD_TYPE MATCHES "Release" OR CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
    endif()

    if(CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LDFLAGS} -Xlinker /debug")
    endif()

    # Fuzzing
    if(NOT STATICALLY_LINK AND ASAN_BUILD)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address -O1")
        message(STATUS "Doing ASAN build")
    endif()

    # Warnings
    set(THIRDPARTY_IGNORED_WARNINGS "-Wno-everything")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    message(STATUS "GCC detected - Setting Defaults")
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
        -Wextra \
        -Wmissing-include-dirs \
        -Woverloaded-virtual \
        -Wredundant-decls \
        -Wshadow \
        -Wsign-promo \
        -fdiagnostics-color=always \
        -mavx")

    # additional c++ flags for release mode for our projects
    if(CMAKE_BUILD_TYPE MATCHES "Release")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
    endif()
    set(THIRDPARTY_IGNORED_WARNINGS "-w")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
    message(STATUS "AppleClang detected - Setting Defaults")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} \
        -march=native \
        -Wall \
        -Winit-self \
        -ggdb \
        -Wextra \
        -Wno-cast-align \
        -Wcast-qual \
        -Wdisabled-optimization \
        -Wformat \
        -Wextra \
        -Wmissing-include-dirs \
        -Woverloaded-virtual \
        -Wredundant-decls \
        -Wshadow \
        -Wsign-promo \
        -O3 \
        -fdiagnostics-color=always"
        )

    # additional c++ flags for release mode for our projects
    if(CMAKE_BUILD_TYPE MATCHES "Release")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
    endif()
    set(THIRDPARTY_IGNORED_WARNINGS "-w")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-stack_size -Wl,0x20000000")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    message(STATUS "MSVC detected - Setting Defaults")

    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc /utf-8 /arch:AVX")

    # Increase stack size for windows, who's default is too low
    # Increase the reserved stack size for all threads to 16MB
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /STACK:16000000,16384")

    # additional c++ and linker flags for specific build types
    if(CMAKE_BUILD_TYPE MATCHES "Release" OR CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /O2 /Ob2")
    endif()

    if(CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /DEBUG")
    endif()
    set(THIRDPARTY_IGNORED_WARNINGS "/w")
else()
    message(FATAL_ERROR "Unknown Compiler '${CMAKE_CXX_COMPILER_ID}', get out!")
endif()

# Platform Specific Settings
if(WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
endif()

# Code Coverage
option(CODE_COVERAGE "Enable Code Coverage Compiler Flags" OFF)

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND CODE_COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags()
    message(STATUS "Code Coverage build is enabled!")
endif()

# Dependencies and Libraries
# includes relative to top level jak-project folder
include_directories(./)

# build templating engine library
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${THIRDPARTY_IGNORED_WARNINGS} ")
include_directories(SYSTEM third-party/inja)

# build repl library
add_subdirectory(third-party/replxx EXCLUDE_FROM_ALL)

# SQLite - Jak 2/3's built in editor
add_definitions(-DHAVE_USLEEP=1)
add_definitions(-DSQLITE_THREADSAFE=1)
add_definitions(-DSQLITE_ENABLE_RTREE)
include_directories(third-party/SQLiteCpp/include)
add_subdirectory(third-party/SQLiteCpp)
string(REPLACE " ${THIRDPARTY_IGNORED_WARNINGS} " "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

# build tree-sitter parser
include_directories(third-party/tree-sitter/tree-sitter/lib/include)
include_directories(third-party/tree-sitter/tree-sitter-opengoal/include)
add_subdirectory(third-party/tree-sitter EXCLUDE_FROM_ALL)

# build common library
add_subdirectory(common)

# build decompiler
add_subdirectory(decompiler)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${THIRDPARTY_IGNORED_WARNINGS} ")
add_subdirectory(third-party/cubeb EXCLUDE_FROM_ALL)

# build LSP
add_subdirectory(lsp)

# build libco
add_subdirectory(third-party/libco)

add_subdirectory(third-party/zstd EXCLUDE_FROM_ALL)

# build SDL
include(SDLOptions)
add_subdirectory(third-party/SDL EXCLUDE_FROM_ALL)

# build imgui
include_directories(third-party/glad/include)
include_directories(third-party/SDL/include)
add_subdirectory(third-party/imgui EXCLUDE_FROM_ALL)
string(REPLACE " ${THIRDPARTY_IGNORED_WARNINGS} " "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

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

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${THIRDPARTY_IGNORED_WARNINGS} ")
add_subdirectory(third-party/googletest EXCLUDE_FROM_ALL)
include(GoogleTest)
string(REPLACE " ${THIRDPARTY_IGNORED_WARNINGS} " "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

# build tests
include(test/CMakeLists.txt)

# build lzokay library
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${THIRDPARTY_IGNORED_WARNINGS} ")
add_subdirectory(third-party/lzokay EXCLUDE_FROM_ALL)

# build format library
add_subdirectory(third-party/fmt EXCLUDE_FROM_ALL)

add_subdirectory(third-party/stb_image EXCLUDE_FROM_ALL)
add_subdirectory(third-party/tiny_gltf EXCLUDE_FROM_ALL)
add_subdirectory(third-party/xdelta3 EXCLUDE_FROM_ALL)

# discord rich presence
include_directories(third-party/discord-rpc/include)
add_subdirectory(third-party/discord-rpc EXCLUDE_FROM_ALL)

# build zydis third party library for disassembling x86
# TODO - Once under CMake 3.13's policy CMP0077, override with `set()` instead
option(ZYDIS_BUILD_TOOLS "Zydis: Build tools" OFF)
option(ZYDIS_BUILD_EXAMPLES "Zydis: Build examples" OFF)

if(STATICALLY_LINK)
    message(STATUS "Statically linking zydis")
    option(ZYDIS_BUILD_SHARED_LIB "Zydis: Build shared library" OFF)
else()
    message(STATUS "dynamically linking zydis")
    option(ZYDIS_BUILD_SHARED_LIB "Zydis: Build shared library" ON)
endif()

add_subdirectory(third-party/zydis EXCLUDE_FROM_ALL)

# windows memory management lib
if(WIN32)
    add_subdirectory(third-party/mman)
endif()
string(REPLACE " ${THIRDPARTY_IGNORED_WARNINGS} " "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
