if (TARGET Zycore)
    return()
endif ()

cmake_minimum_required(VERSION 3.9 FATAL_ERROR)

if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15")
    # Enable runtime library selection via CMAKE_MSVC_RUNTIME_LIBRARY
    cmake_policy(SET CMP0091 NEW)
endif ()

project(Zycore VERSION 1.4.0.0 LANGUAGES C)

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

# =============================================================================================== #
# Overridable options                                                                             #
# =============================================================================================== #

# Global configuration
option(ZYAN_WHOLE_PROGRAM_OPTIMIZATION
    "Enable whole program optimization (all targets)"
    OFF)
option(ZYAN_NO_LIBC
    "Don't use any C standard library functions (for exotic build-envs like kernel drivers)"
    OFF)
option(ZYAN_DEV_MODE
    "Enable developer mode (-Wall, -Werror, ...)"
    OFF)
option(ZYAN_FORCE_ASSERTS
    "Forces asserts in release builds."
    OFF)

# Build configuration
option(ZYCORE_BUILD_SHARED_LIB
    "Build shared library"
    OFF)
option(ZYCORE_BUILD_EXAMPLES
    "Build examples"
    OFF)
option(ZYCORE_BUILD_TESTS
    "Build tests"
    OFF)

# =============================================================================================== #
# Forced assertions hack                                                                          #
# =============================================================================================== #

# The code for this is adapted from how LLVM forces asserts.
if (ZYAN_FORCE_ASSERTS)
    set(vars
        CMAKE_CXX_FLAGS_RELEASE
        CMAKE_CXX_FLAGS_RELWITHDEBINFO
        CMAKE_CXX_FLAGS_MINSIZEREL
        CMAKE_C_FLAGS_RELEASE
        CMAKE_C_FLAGS_RELWITHDEBINFO
        CMAKE_C_FLAGS_MINSIZEREL)

    foreach (var ${vars})
        string (REGEX REPLACE "(^| )[/-]D *NDEBUG($| )" " " "${var}" "${${var}}")
        set("${var}" "${${var}} -UNDEBUG" CACHE STRING "" FORCE)
    endforeach()
endif ()

# =============================================================================================== #
# GoogleTest                                                                                      #
# =============================================================================================== #

# Search for GoogleTest, and fallback to downloading it if not found
if (ZYCORE_BUILD_TESTS)
    find_package(GTest QUIET)
    if (GTest_FOUND)
        # CMake 3.20 and upstream GTestConfig.cmake
        if (TARGET GTest::gtest)
            add_library(gtest ALIAS GTest::gtest)
        # Older FindGTest
        else ()
            add_library(gtest ALIAS GTest::GTest)
        endif ()
    else ()
        if (NOT DEFINED ZYCORE_DOWNLOADED_GTEST)
            configure_file("CMakeLists.txt.in" "${CMAKE_BINARY_DIR}/gtest/download/CMakeLists.txt")
            execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
                RESULT_VARIABLE result
                WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/gtest/download")
            if (result)
                message(FATAL_ERROR "CMake step for googletest failed: ${result}")
            endif()
            execute_process(COMMAND ${CMAKE_COMMAND} --build .
                RESULT_VARIABLE result
                WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/gtest/download")
            if (result)
                message(FATAL_ERROR "Build step for googletest failed: ${result}")
            endif()

            set(ZYCORE_DOWNLOADED_GTEST TRUE CACHE BOOL "")
            mark_as_advanced(ZYCORE_DOWNLOADED_GTEST)
        endif ()

    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

    add_subdirectory("${CMAKE_BINARY_DIR}/gtest/src" "${CMAKE_BINARY_DIR}/gtest/build"
        EXCLUDE_FROM_ALL)
    endif ()
endif ()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(zyan-functions)

# =============================================================================================== #
# Library configuration                                                                           #
# =============================================================================================== #

if (ZYCORE_BUILD_SHARED_LIB)
    add_library("Zycore" SHARED)
else ()
    add_library("Zycore" STATIC)
    target_compile_definitions("Zycore" PUBLIC "ZYCORE_STATIC_BUILD")
endif ()

set_target_properties("Zycore" PROPERTIES
    LINKER_LANGUAGE C
    VERSION "${Zycore_VERSION}"
    SOVERSION "${Zycore_VERSION_MAJOR}.${Zycore_VERSION_MINOR}"
    DEFINE_SYMBOL "ZYCORE_SHOULD_EXPORT")
target_include_directories("Zycore"
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    PRIVATE "src")
target_compile_definitions("Zycore" PRIVATE "_CRT_SECURE_NO_WARNINGS")
zyan_set_common_flags("Zycore")
zyan_maybe_enable_wpo("Zycore")

if (ZYAN_NO_LIBC)
    target_compile_definitions("Zycore" PUBLIC "ZYAN_NO_LIBC")
    if (UNIX)
        set_target_properties("Zycore" PROPERTIES LINK_FLAGS "-nostdlib -nodefaultlibs")
    endif ()
endif ()

target_sources("Zycore"
    PRIVATE
        # API
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/API/Memory.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/API/Process.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/API/Synchronization.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/API/Terminal.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/API/Thread.h"
        # Common
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Allocator.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/ArgParse.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Atomic.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Bitset.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Comparison.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Defines.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Format.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/LibC.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/List.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Object.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Status.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/String.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Types.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Vector.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Zycore.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Internal/AtomicGNU.h"
        "${CMAKE_CURRENT_LIST_DIR}/include/Zycore/Internal/AtomicMSVC.h"
        # API
        "src/API/Memory.c"
        "src/API/Process.c"
        "src/API/Synchronization.c"
        "src/API/Terminal.c"
        "src/API/Thread.c"
        # Common
        "src/Allocator.c"
        "src/ArgParse.c"
        "src/Bitset.c"
        "src/Format.c"
        "src/List.c"
        "src/String.c"
        "src/Vector.c"
        "src/Zycore.c")

if (ZYCORE_BUILD_SHARED_LIB AND WIN32)
    target_sources("Zycore" PRIVATE "resources/VersionInfo.rc")
endif ()

zyan_set_source_group("Zycore")

if (${CMAKE_SYSTEM_NAME} STREQUAL "Linux" AND NOT ZYAN_NO_LIBC)
    target_compile_definitions("Zycore" PRIVATE "_GNU_SOURCE")
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries("Zycore" Threads::Threads)
endif ()

configure_package_config_file(cmake/zycore-config.cmake.in
    "${CMAKE_CURRENT_BINARY_DIR}/zycore-config.cmake"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/zycore"
)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/zycore-config-version.cmake"
    COMPATIBILITY SameMajorVersion
)
install(FILES
    "cmake/zyan-functions.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/zycore-config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/zycore-config-version.cmake"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/zycore"
)

install(TARGETS "Zycore" EXPORT "zycore-targets"
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(EXPORT "zycore-targets"
    NAMESPACE "Zycore::"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/zycore")
install(DIRECTORY "include/" DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# =============================================================================================== #
# Doxygen documentation                                                                           #
# =============================================================================================== #

find_package(Doxygen)
if (DOXYGEN_FOUND)
    set(DOXYGEN_GENERATE_MAN YES)
    doxygen_add_docs(ZycoreDoc "include/Zycore/" ALL)
    install(
        DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html/"
        DESTINATION "${CMAKE_INSTALL_DOCDIR}/api"
        COMPONENT Documentation
    )
    install(
        DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/man/man3/"
        DESTINATION "${CMAKE_INSTALL_MANDIR}/man3"
        COMPONENT Documentation
    )
endif()

# =============================================================================================== #
# Developer mode                                                                                  #
# =============================================================================================== #

if (ZYAN_DEV_MODE)
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
endif ()

# =============================================================================================== #
# Examples                                                                                        #
# =============================================================================================== #

if (ZYCORE_BUILD_EXAMPLES)
    add_executable("String" "examples/String.c")
    zyan_set_common_flags("String" "Zycore")
    target_link_libraries("String" "Zycore")
    set_target_properties("String" PROPERTIES FOLDER "Examples")
    target_compile_definitions("String" PRIVATE "_CRT_SECURE_NO_WARNINGS")
    zyan_maybe_enable_wpo("String")

    add_executable("Vector" "examples/Vector.c")
    zyan_set_common_flags("Vector" "Zycore")
    target_link_libraries("Vector" "Zycore")
    set_target_properties("Vector" PROPERTIES FOLDER "Examples")
    target_compile_definitions("Vector" PRIVATE "_CRT_SECURE_NO_WARNINGS")
    zyan_maybe_enable_wpo("Vector")
endif ()

# =============================================================================================== #
# Tests                                                                                           #
# =============================================================================================== #

function (zyan_add_test test)
    add_executable("Test${test}" "tests/${test}.cpp")
    target_link_libraries("Test${test}" "Zycore" "gtest")
    set_target_properties("Test${test}" PROPERTIES
        LANGUAGE CXX
        CXX_STANDARD 17
        CXX_EXTENSIONS OFF
        CXX_STANDARD_REQUIRED ON
        FOLDER "Tests"
    )
    target_compile_definitions("Test${test}" PRIVATE "_CRT_SECURE_NO_WARNINGS")
    zyan_maybe_enable_wpo("Test${test}")
    add_test("${test}" "Test${test}")
endfunction ()

if (ZYCORE_BUILD_TESTS)
    enable_language(CXX)
    enable_testing()
    zyan_add_test("String")
    zyan_add_test("Vector")
    zyan_add_test("ArgParse")
endif ()

# =============================================================================================== #
