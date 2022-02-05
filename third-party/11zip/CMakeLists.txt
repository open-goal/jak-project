cmake_minimum_required(VERSION 3.12)

project(elzip)

option(ELZIP_EXCLUDE_MINIZIP "Use this to exclude internal minizip copy." OFF)
option(ELZIP_TESTS "includes tests for 11Zip." OFF)

if(NOT ELZIP_EXCLUDE_MINIZIP)
    set(MZ_BZIP2 OFF)
    set(MZ_ICONV OFF)
    set(MZ_LZMA OFF)
    set(MZ_PKCRYPT OFF)
    set(MZ_SIGNING OFF)
    set(MZ_WZAES OFF)
    set(MZ_ZSTD OFF)
    # Enabling ZLIB and MZ_COMPAT
    set(MZ_FETCH_LIBS ON)
    set(MZ_ZLIB ON)
    set(MZ_COMPAT ON)
    add_subdirectory(extlibs/minizip EXCLUDE_FROM_ALL)
endif()

file(GLOB LZIP_HEADERS include/elzip/*.hpp)

if (USE_FILESYSTEM_FALLBACK)
    set(ELZIP_CPP src/elzip_fs_fallback.cpp)
else()
    set(ELZIP_CPP src/elzip.cpp)
endif()

file(GLOB LZIP_SOURCES
    ${ELZIP_CPP}
    src/unzipper.cpp
    src/zipper.cpp
)

add_library(elzip ${LZIP_HEADERS} ${LZIP_SOURCES})
target_link_libraries(elzip minizip)

if (USE_FILESYSTEM_FALLBACK)
    target_link_libraries(elzip tinydir)
elseif (CMAKE_COMPILER_IS_GNUCXX)
    target_link_libraries(elzip stdc++fs)
endif()

target_include_directories(elzip
    PUBLIC
        $<INSTALL_INTERFACE:include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/elzip>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/extlibs>
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/src
)

set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 17)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_EXTENSIONS OFF)

if(ELZIP_TESTS)
    set(BUILD_TESTING ON)
    include(CTest)
    add_subdirectory(test)
endif()
