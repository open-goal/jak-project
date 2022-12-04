# CMake file for compiling the sqlite3 static library under Windows (for ease of use)
#
# Copyright (c) 2012-2022 Sebastien Rombauts (sebastien.rombauts@gmail.com)
#
# Distributed under the MIT License (MIT) (See accompanying file LICENSE.txt
# or copy at http://opensource.org/licenses/MIT) 

# add sources of the "sqlite3" static library
add_library(sqlite3
 sqlite3.c
 sqlite3.h
)

add_library(SQLite::SQLite3 ALIAS sqlite3)

target_include_directories(sqlite3
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<INSTALL_INTERFACE:include/>)

if (SQLITE_ENABLE_COLUMN_METADATA)
    # Enable the use of SQLite column metadata method
    # Require that the sqlite3 library is also compiled with this flag:
    target_compile_definitions(sqlite3 PUBLIC SQLITE_ENABLE_COLUMN_METADATA)
endif (SQLITE_ENABLE_COLUMN_METADATA)

if (SQLITE_ENABLE_JSON1)
    # Enable JSON1 extension when building sqlite3
    # See more here: https://www.sqlite.org/json1.html
    target_compile_definitions(sqlite3 PUBLIC SQLITE_ENABLE_JSON1)
endif (SQLITE_ENABLE_JSON1)

if(SQLITE_ENABLE_RTREE)
    # Enable RTree extension when building sqlite3
    # See more here: https://sqlite.org/rtree.html
    target_compile_definitions(sqlite3 PUBLIC SQLITE_ENABLE_RTREE)
endif (SQLITE_ENABLE_RTREE)

if (UNIX AND (CMAKE_COMPILER_IS_GNUCXX OR ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang"))
    set_target_properties(sqlite3 PROPERTIES COMPILE_FLAGS "-fPIC")

    # Put each function in its own section to allow the linker garbage
    # collection to remove unused section and produced a smaller
    # statically-lined executables.
    target_compile_options(sqlite3 PRIVATE "-ffunction-sections")
endif (UNIX AND (CMAKE_COMPILER_IS_GNUCXX OR ${CMAKE_CXX_COMPILER_ID} STREQUAL "Clang"))

if (UNIX AND CMAKE_COMPILER_IS_GNUCXX)
    if (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER 7.0)
        target_compile_options(sqlite3 PRIVATE "-Wimplicit-fallthrough=0")
    endif()
    if (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER 8.0)
        target_compile_options(sqlite3 PRIVATE "-Wno-cast-function-type")
    endif()
endif()

# Allow the library to be installed via "make install" and found with "find_package"

include(GNUInstallDirs)
install(TARGETS sqlite3
    EXPORT ${PROJECT_NAME}Targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    COMPONENT libraries)
install(FILES sqlite3.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR} COMPONENT headers)
