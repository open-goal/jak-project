# Top Level CMakeLists.txt
cmake_minimum_required(VERSION 3.0) # todo - this was picked randomly
project(jak)

set(CMAKE_CXX_STANDARD 14)

# optimization level can be set here. Note that game/ overwrites this for building game C++ code.
set(CMAKE_CXX_FLAGS "-O0 -ggdb -Wall \
-Wextra -Wcast-align -Wcast-qual -Wdisabled-optimization -Wformat=2 \
-Winit-self -Wmissing-include-dirs -Woverloaded-virtual \
-Wredundant-decls -Wshadow -Wsign-promo ")

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