# Example CMake file for compiling & linking a project with the the SQLiteCpp wrapper
#
# Copyright (c) 2012-2021 Sebastien Rombauts (sebastien.rombauts@gmail.com)
#
# Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
# or copy at http://opensource.org/licenses/MIT) 
cmake_minimum_required(VERSION 3.1) # for "CMAKE_CXX_STANDARD" version
project(SQLiteCpp_Example VERSION 2.0)

# SQLiteC++ 3.x now requires C++11 compiler
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Add SQLite3 C++ wrapper around sqlite3 library (and sqlite3 itself provided for ease of use)
# Here you can set CMake variables to avoid building Example, as well as cpplint, cppcheck...
# or set them in the cmake command line (see for instance provided build.bat/build.sh scripts)
set(SQLITECPP_RUN_CPPCHECK OFF CACHE BOOL "" FORCE)
set(SQLITECPP_RUN_CPPLINT OFF CACHE BOOL "" FORCE)
set(SQLITECPP_USE_STATIC_RUNTIME OFF CACHE BOOL "" FORCE)
add_subdirectory(../.. SQLiteCpp) # out-of-source build requires explicit subdir name for compilation artifacts

# Add main.cpp example source code to the executable
add_executable(SQLiteCpp_Example src/main.cpp)

# Link SQLiteCpp_example1 with SQLiteCpp
target_link_libraries(SQLiteCpp_Example SQLiteCpp)
