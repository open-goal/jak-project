# Top Level CMakeLists.txt
cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
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
    set(BUILD_STATIC_LIBS ON)
else()
    set(BUILD_SHARED_LIBS ON)
    set(BUILD_STATIC_LIBS OFF)
endif()

# For clangd
# file(COPY), not configure_file(): configure_file registers its input as a
# configure dependency, putting the ever-rewritten compile_commands.json into
# the Ninja regen rule and causing an endless reconfigure loop.
if (EXISTS "${CMAKE_CURRENT_BINARY_DIR}/compile_commands.json" )
    file(COPY
        "${CMAKE_CURRENT_BINARY_DIR}/compile_commands.json"
        DESTINATION "${PROJECT_SOURCE_DIR}/build")
endif()

# NOTE - for SDL, i think SDL3 regressed this - https://github.com/libsdl-org/SDL/pull/6455
# a more recent issue - https://github.com/libsdl-org/SDL/issues/12078
if (APPLE)
  enable_language(OBJC)
  execute_process(
    COMMAND xcrun --show-sdk-path
        OUTPUT_VARIABLE MACOSX_SYSROOT
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    # Tell CMake to use it
    set(CMAKE_OSX_SYSROOT "${MACOSX_SYSROOT}" CACHE PATH "macOS SDK path" FORCE)
endif()

# Setup compiler flags
if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    message(STATUS "Clang Detected - Setting Defaults")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} \
        -fcxx-exceptions \
        -fexceptions \
        -fdiagnostics-color=always \
        -Wall \
        -Wno-c++11-narrowing \
        -Wno-c++98-compat \
        -Wno-deprecated-this-capture \
        -Wno-missing-field-initializers \
        -Wno-sign-compare \
        -Wno-ignored-qualifiers \
        -O3 \
        -D_CRT_SECURE_NO_WARNINGS")

    # AVX is an x86-only ISA extension; cross builds (e.g. Switch's aarch64) set their own
    # arch flags via CMAKE_CXX_FLAGS_INIT in their toolchain file instead.
    if(NOT NINTENDO_SWITCH AND CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|AMD64|i.86)$")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mavx")
    endif()

    # Increase stack size for windows, who's default is too low
    if(WIN32)
        # Increase the reserved stack size for all threads to 16MB
        # Note: this is only _reserved_ memory, not necessarily _committed_ memory
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${LDFLAGS} -Xlinker /STACK:16000000")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb -g")
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
        -Woverloaded-virtual \
        -Wredundant-decls \
        -Wshadow \
        -Wsign-promo \
        -fdiagnostics-color=always")

    # AVX is an x86-only ISA extension; cross builds (e.g. Switch's aarch64) set their own
    # arch flags via CMAKE_CXX_FLAGS_INIT in their toolchain file instead.
    if(NOT NINTENDO_SWITCH AND CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|AMD64|i.86)$")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mavx")
    endif()

    # additional c++ flags for release mode for our projects
    if(CMAKE_BUILD_TYPE MATCHES "Release")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
    endif()
    set(THIRDPARTY_IGNORED_WARNINGS "-w")
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
    message(STATUS "AppleClang detected - Setting Defaults")
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
        -Woverloaded-virtual \
        -Wredundant-decls \
        -Wshadow \
        -Wsign-promo \
        -fdiagnostics-color=always"
        )

    # TODO - make a proper flag for arm compiling
    if (CMAKE_APPLE_SILICON_PROCESSOR STREQUAL "x86_64")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mavx")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native -mcrc")
    endif()

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

function(build_third_party_lib dir_name target_name)
    add_subdirectory(third-party/${dir_name} EXCLUDE_FROM_ALL)
    # Treat third-party headers as system headers.
    get_target_property(INCLUDES ${target_name} INTERFACE_INCLUDE_DIRECTORIES)
    if(INCLUDES)
        set_target_properties(${target_name} PROPERTIES
            INTERFACE_SYSTEM_INCLUDE_DIRECTORIES "${INCLUDES}"
        )
    endif()
    # Suppress all warnings while compiling the third-party library itself.
    target_compile_options(${target_name} PRIVATE
        -w
    )
endfunction()

# Dependencies and Libraries
# - for reasons outside of my understanding, on some distros CMake can't find a library if it's
# in `/usr/lib64` instead of `/usr/lib`.  This problem was encountered for libcurl not being able
# to find OpenSSL even though its most definitely installed!
if(UNIX AND NOT APPLE)
    list(APPEND CMAKE_LIBRARY_PATH "/usr/lib64")
endif()

# includes relative to top level jak-project folder
include_directories(./)

# build templating engine library
include_directories(SYSTEM third-party/inja)

# libcurl for HTTP requests
# Enable SSL Support, most URLs now-a-days use SSL!
# TODO - probably integrate with ZSTD since we have it already
set(CURL_USE_LIBSSH2 OFF)
# another change in the macOS test runner environment that suddenly has broken
# but this is another part of libcurl we probably don't need (multi-threading/parallelism)
set(CURL_USE_LIBPSL OFF)
set(CURL_ZLIB OFF)
# these default to AUTO since curl 8.10 and would silently pick up system libs
set(CURL_BROTLI OFF CACHE BOOL "Disable Brotli support" FORCE)
set(CURL_ZSTD OFF CACHE BOOL "Disable zstd support" FORCE)
set(USE_NGHTTP2 OFF CACHE BOOL "Disable nghttp2 support" FORCE)
# curl's BUILD_EXAMPLES cache option (default ON) would otherwise leak into
# discord-rpc's identically named option and break its examples add_subdirectory
set(BUILD_EXAMPLES OFF)
# suddenly became enabled on macOS atleast, only needed for unicode URLs
# wasn't being properly statically linked
set(USE_LIBIDN2 OFF)
if(WIN32)
    set(CURL_USE_SCHANNEL ON) # native Windows SSL support
else()
    # curl 8.15 removed the native macOS backend (CURL_USE_SECTRANSP), so macOS
    # uses OpenSSL like linux (Homebrew openssl@3, see the macOS build workflows)
    if(STATICALLY_LINK)
        set(OPENSSL_USE_STATIC_LIBS TRUE)
    endif()
    set(CURL_USE_OPENSSL ON)
endif()
include_directories(third-party/curl/include)
if(NINTENDO_SWITCH)
    # No OpenSSL/mbedTLS portlib and no use for desktop update-check HTTP on a homebrew NRO.
    message(STATUS "Switch: skipping libcurl")
else()
    if(STATICALLY_LINK)
        build_third_party_lib(curl libcurl_static)
    else()
        build_third_party_lib(curl libcurl_shared)
    endif()
endif()

# build repl library
if(NOT NINTENDO_SWITCH)
    build_third_party_lib(replxx replxx)
endif()

# SQLite - Jak 2/3's built in editor
add_definitions(-DHAVE_USLEEP=1)
add_definitions(-DSQLITE_THREADSAFE=1)
add_definitions(-DSQLITE_ENABLE_JSON1)
add_definitions(-DSQLITE_ENABLE_RTREE)
if(NINTENDO_SWITCH)
    # newlib on devkitA64 has no sys/mman.h. sqlite3.c's guard is
    # `!defined(SQLITE_OMIT_WAL) || SQLITE_MAX_MMAP_SIZE>0`, so MAX_MMAP_SIZE=0 alone
    # doesn't skip the include -- WAL must be omitted too.
    add_definitions(-DSQLITE_MAX_MMAP_SIZE=0)
    add_definitions(-DSQLITE_OMIT_WAL=1)
    # No dynamic loader on Horizon OS -- everything is statically linked.
    add_definitions(-DSQLITE_OMIT_LOAD_EXTENSION=1)
endif()
build_third_party_lib(sqlite3 sqlite3)

# build tree-sitter parser
include_directories(third-party/tree-sitter/tree-sitter/lib/include)
build_third_party_lib(tree-sitter tree-sitter)

# native OS dialogs for error messages (needs termios/a real windowing system; unused on Switch)
if(NOT NINTENDO_SWITCH)
    build_third_party_lib(libtinyfiledialogs libtinyfiledialogs)
endif()

# build format library
include_directories(third-party/fmt/include)
build_third_party_lib(fmt fmt)

# build common library
add_subdirectory(common)

# build decompiler
add_subdirectory(decompiler)

# cubeb - audio
build_third_party_lib(cubeb cubeb)

# build LSP
if(NOT NINTENDO_SWITCH)
    add_subdirectory(lsp)
endif()

# build libco and zstd
build_third_party_lib(libco libco)
build_third_party_lib(zstd libzstd_static)

# build SDL
if(NINTENDO_SWITCH)
    # No SDL3 port exists for Switch -- devkitPro only ships SDL2. game/'s SDL3 call sites are
    # routed through game/switch/sdl3_compat.h (a real compat shim, not a rebuild) instead, so
    # SDL3::SDL3 just needs to resolve to devkitPro's actual SDL2 + its Switch-specific deps.
    add_library(SDL3_switch_compat INTERFACE)
    target_link_libraries(SDL3_switch_compat INTERFACE SDL2 SDL2main EGL glapi drm_nouveau)
    add_library(SDL3::SDL3 ALIAS SDL3_switch_compat)
else()
    include(SDLOptions)
    if(STATICALLY_LINK)
        build_third_party_lib(SDL SDL3-static)
    else()
        build_third_party_lib(SDL SDL3-shared)
    endif()
endif()

# build imgui
include_directories(third-party/glad/include)
include_directories(third-party/SDL/include)
if(NOT NINTENDO_SWITCH)
    build_third_party_lib(imgui imgui)
endif()

# build the game code in C++
add_subdirectory(game)

# build the compiler
add_subdirectory(goalc)

# build standalone tools
if(NOT NINTENDO_SWITCH)
    add_subdirectory(tools)
endif()

# build the gtest libraries
if(WIN32)
    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
endif()

build_third_party_lib(googletest gtest)
include(GoogleTest)

# build tests
include(test/CMakeLists.txt)

# build lzokay library
build_third_party_lib(lzokay lzokay)

build_third_party_lib(stb_image stb_image)
# build draco library for tinygltf compression support
include_directories(third-party/draco/src)
add_subdirectory(third-party/draco)
add_compile_definitions(TINYGLTF_ENABLE_DRACO)
build_third_party_lib(tiny_gltf tiny_gltf)
build_third_party_lib(xdelta3 xdelta3)

# discord rich presence
include_directories(third-party/discord-rpc/include)
if(NOT NINTENDO_SWITCH)
    build_third_party_lib(discord-rpc discord-rpc)
endif()

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

build_third_party_lib(zydis Zydis)

# Capstone (zydis replacement eventually)
option(CAPSTONE_ARCHITECTURE_DEFAULT "Capstone: Disable all by default" OFF)
option(CAPSTONE_X86_SUPPORT "Capstone: x86 support" ON)
option(CAPSTONE_ARM_SUPPORT "Capstone: ARM support" ON)
option(CAPSTONE_AARCH64_SUPPORT "Capstone: ARM64 support" ON)

if(STATICALLY_LINK)
    message(STATUS "Statically linking capstone")
    option(CAPSTONE_BUILD_SHARED_LIBS "Capstone: Build shared library" OFF)
    option(CAPSTONE_BUILD_STATIC_LIBS "Capstone: Build static library" ON)
else()
    message(STATUS "dynamically linking capstone")
    option(CAPSTONE_BUILD_SHARED_LIBS "Capstone: Build shared library" ON)
    option(CAPSTONE_BUILD_STATIC_LIBS "Capstone: Build static library" OFF)
endif()

include_directories(third-party/capstone/include)
build_third_party_lib(capstone capstone)

# windows memory management lib
if(WIN32)
    build_third_party_lib(mman mman)
endif()
