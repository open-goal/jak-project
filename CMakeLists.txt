# Top Level CMakeLists.txt
cmake_minimum_required(VERSION 3.16)
project(jak)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Debug")
endif()

set(CMAKE_CXX_STANDARD 17)

# Set default compile flags for GCC
# optimization level can be set here. Note that game/ overwrites this for building game C++ code.
if (CMAKE_COMPILER_IS_GNUCXX)
    message(STATUS "GCC detected, adding compile flags")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} \
        -Wall \
        -Winit-self \
        -ggdb \
        -Wextra \
        -Wcast-align \
        -Wcast-qual \
        -Wdisabled-optimization \
        -Wformat=2 \
        -Wmissing-include-dirs \
        -Woverloaded-virtual \
        -Wredundant-decls \
        -Wshadow \
        -Wsign-promo")
else ()
    set(CMAKE_CXX_FLAGS "/EHsc")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /STACK:10000000")
endif (CMAKE_COMPILER_IS_GNUCXX)

IF (WIN32)
    set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
ENDIF ()

option(CODE_COVERAGE "Enable Code Coverage Compiler Flags" OFF)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/modules/)

if(CMAKE_COMPILER_IS_GNUCXX AND CODE_COVERAGE)
  include(CodeCoverage)
  append_coverage_compiler_flags()
  message("Code Coverage build is enabled!")
else()
  message("Code Coverage build is disabled!")
endif()

# includes relative to top level jak-project folder
include_directories(./)

include_directories(SYSTEM third-party/inja)

# build spdlog as a shared library to improve compile times
# adding this as a SYSTEM include suppresses all the terrible warnings in spdlog
include_directories(SYSTEM third-party/spdlog/include)
# this makes spdlog generate a shared library that we can link against
set(SPDLOG_BUILD_SHARED ON CACHE BOOL "a" FORCE)
# this makes the spdlog includes not use the header only version, making compiling faster
add_definitions(-DSPDLOG_COMPILED_LIB)

# build asset packer/unpacker
add_subdirectory(asset_tool)

# build goos
add_subdirectory(common/goos)

# build type_system library for compiler/decompiler
add_subdirectory(common/type_system)

# build common_util library
add_subdirectory(common/util)

# build cross platform socket library
add_subdirectory(common/cross_sockets)

# build cross platform debug library
add_subdirectory(common/cross_os_debug)

# build decompiler
add_subdirectory(decompiler)

# build the game code in C++
add_subdirectory(game)

# build the compiler
add_subdirectory(goalc)

# build the gtest libraries
add_subdirectory(third-party/googletest)

# build tests
add_subdirectory(test)

# build minilzo library
add_subdirectory(third-party/minilzo)

# build format library
add_subdirectory(third-party/fmt)

# build spdlog library
add_subdirectory(third-party/spdlog)

# windows memory management lib
IF (WIN32)
    add_subdirectory(third-party/mman)
ENDIF ()