add_library(sqlite3
 sqlite3.c
 sqlite3.h
)

target_compile_definitions(sqlite3 PUBLIC SQLITE_ENABLE_JSON1)

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
