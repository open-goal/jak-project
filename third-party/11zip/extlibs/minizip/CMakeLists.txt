project(minizip)

file(GLOB MZIP_HEADERS include/minizip/*.h)
file(GLOB MZIP_SOURCES src/*.c)

if (NOT WIN32)
    list(REMOVE_ITEM MZIP_HEADERS ${CMAKE_CURRENT_SOURCE_DIR}/include/minizip/iowin32.h)
    list(REMOVE_ITEM MZIP_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/iowin32.c)
ENDIF()

add_library(minizip ${MZIP_HEADERS} ${MZIP_SOURCES})

if (BUILD_ANDROID)
    target_compile_definitions(minizip PRIVATE USE_FILE32API)
endif()


target_include_directories(minizip
    PUBLIC 
        $<INSTALL_INTERFACE:include>    
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    PRIVATE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/minizip>
        ${CMAKE_CURRENT_SOURCE_DIR}/src
)
