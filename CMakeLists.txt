# Top Level CMakeLists.txt
cmake_minimum_required(VERSION 3.0) # todo - this was picked randomly
project(jak)

set(CMAKE_CXX_STANDARD 14)

# optimization level can be set here. Note that game/ overwrites this for building game C++ code.
set(CMAKE_CXX_FLAGS "-O3 -ggdb /EHsc")

# Set default compile flags for GCC
if(CMAKE_COMPILER_IS_GNUCXX)
    message(STATUS "GCC detected, adding compile flags")
    set(CMAKE_CXX_FLAGS 
        "${CMAKE_CXX_FLAGS} 
        -Wall-Winit-self 
        -Wextra
        -Wcast-align
        -Wcast-qual
        -Wdisabled-optimization
        -Wformat=2
        -Wmissing-include-dirs
        -Woverloaded-virtual
        -Wredundant-decls
        -Wshadow
        -Wsign-promo")
endif(CMAKE_COMPILER_IS_GNUCXX)

IF (WIN32)
  set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
ENDIF()

# includes relative to top level jak-project folder
include_directories(./)

# build asset packer/unpacker
add_subdirectory(asset_tool)

# build type_system library for compiler/decompiler
add_subdirectory(common/type_system)

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

# windows memory management lib
IF (WIN32)
  add_subdirectory(third-party/mman)
ENDIF()