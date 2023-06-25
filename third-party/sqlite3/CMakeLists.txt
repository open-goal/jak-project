add_library(sqlite3
 sqlite3.c
 sqlite3.h
)

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

# Link target with pthread and dl for Unix
if (UNIX)
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries(sqlite3 PUBLIC Threads::Threads ${CMAKE_DL_LIBS})
endif (UNIX)
