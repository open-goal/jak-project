# Top Level CMakeLists.txt
cmake_minimum_required(VERSION 3.16)
project(jak)

set(CMAKE_CXX_STANDARD 14)

# Set default compile flags for GCC
# optimization level can be set here. Note that game/ overwrites this for building game C++ code.
if(CMAKE_COMPILER_IS_GNUCXX)
    message(STATUS "GCC detected, adding compile flags")
    set(CMAKE_CXX_FLAGS 
        "${CMAKE_CXX_FLAGS} \
        -Wall \
				-Winit-self \
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
else()
		set(CMAKE_CXX_FLAGS "/EHsc")
endif(CMAKE_COMPILER_IS_GNUCXX)

IF (WIN32)
  set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS ON)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
ENDIF()


add_library(util SHARED FileIO.cpp)

include_directories(/.)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../A ${CMAKE_CURRENT_BINARY_DIR}/A)